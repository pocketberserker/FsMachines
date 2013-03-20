module Machines.Tee

open Plan
open Process
open These
open FSharpx
open FSharpx.Collections

type private T<'a, 'b> = Choice<'a -> obj, 'b -> obj>

type Tee<'a, 'b, 'c> = Machine<T<'a, 'b>, 'c>

let rec tee (ma : Machine<'a, 'aa>) (mb : Machine<'b, 'bb>) (t : Tee<'aa, 'bb, 'c>) : Machine<Choice<'a, 'b>, 'c> =
  match t with
  | Stop -> Stop
  | Emit(o, k) -> Emit(o, fun () -> tee ma mb (k ()))
  | Await(k, Choice1Of2 s, f) ->
    match ma with
    | Stop -> tee ma mb (f ())
    | Emit(a, next) -> tee (next ()) mb (k (s a))
    | Await(g, kg, fg) -> Await((fun a -> tee (g a) mb t), Choice1Of2 kg, fun () -> tee (fg ()) mb t)
  | Await(k, Choice2Of2 s, f) ->
    match mb with
    | Stop -> tee ma mb (f ())
    | Emit(b, next) -> tee ma (next ()) (k (s b))
    | Await(g, kg, fg) -> Await((fun b -> tee ma (g b) t), Choice2Of2 kg, fun () -> tee ma (fg ()) t)
  
let addL (p : Process<'a, 'i>) (t : Tee<'i, 'j, 'o>) : Tee<'a, 'j, 'o> = tee p Process.id t

let addR (p : Process<'b, 'j>) (t : Tee<'i, 'j, 'o>) : Tee<'i, 'b, 'o> = tee Process.id p t

let cappedT = function
| Choice1Of2 f -> f
| Choice2Of2 f -> f

let capL s (t : Tee<'i, 'j, 'o>) : Process<'j, 'o> = addL s t |> Plan.inmap cappedT

let capR s (t : Tee<'i, 'j, 'o>) : Process<'i, 'o> = addR s t |> Plan.inmap cappedT

let left<'a> : Handle<T<'a, obj>, 'a> =
  let g = fun (f : 'a -> obj) -> Choice1Of2 f in g

let right<'a> : Handle<T<obj, 'a>, 'a> =
  let g = fun (f : 'a -> obj) -> Choice2Of2 f in g

let hashJoin (f : _ -> 'k) (g : _ -> 'k) : Tee<_, _, _ * _> =
  let rec build m : Plan<T<_, _>, _, Map<'k, _ seq>> =
    plan {
      let! a = awaits(left<_>)
      let! mp =
        let ak = f a
        build (m |> Map.add ak (m |> Map.findOrDefault ak Seq.empty |> flip Seq.append (seq { yield a })))
      return mp
    }
  in plan {
    let! m = build Map.empty
    let! r =
      awaits(right<_>)
      |> Plan.bind (fun b ->
        let k = g b
        m
        |> Map.findOrDefault k Seq.empty
        |> Seq.toList
        |> flip (List.foldBack (fun a -> fun r -> Emit((a, b), fun () -> r))) (Return ())
        |> repeatedly
      )
    return r
  }
