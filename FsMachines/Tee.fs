namespace Machines

open FSharpx
open FSharpx.Collections

type private T<'a, 'b> = Choice<'a -> obj, 'b -> obj>

type Tee<'a, 'b, 'c> = Machine<T<'a, 'b>, 'c>

module Tee =

  let rec tee (ma : Machine<'a, 'aa>) (mb : Machine<'b, 'bb>) (t : Tee<'aa, 'bb, 'c>)
    : Machine<Choice<'a, 'b>, 'c> =
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

  let left<'a, 'b> : Handle<T<'a, 'b>, 'a> =
    let g = fun (f : 'a -> obj) -> Choice1Of2 f in g

  let right<'a, 'b> : Handle<T<'b, 'a>, 'a> =
    let g = fun (f : 'a -> obj) -> Choice2Of2 f in g

  open Plan

  let hashJoin (f : 'a -> 'k) (g : 'b -> 'k) : Tee<'a, 'b, 'a * 'b> =
    let rec build m : Plan<T<_, _>, _, Map<'k, Vector<_>>> =
      awaits(left<_,_>)
      >>= (fun a ->
        let ak = f a
        m
        |> Map.add ak (m |> Map.findOrDefault ak Vector.empty |> flip Vector.append (Vector.singleton a))
        |> build)
      |> Plan.orElse (Return m)
    plan {
      let! m = build Map.empty
      return!
        awaits(right<_,_>)
        >>= (fun b ->
          let k = g b
          m
          |> Map.findOrDefault k Vector.empty
          |> flip (Vector.foldBack (fun a r -> Emit((a, b), fun () -> r))) (Return ()))
        |> repeatedly
    }

  let private (|LT|GT|EQ|) (a, b) =
    let n = compare a b
    if n < 0 then LT
    elif n > 0 then GT
    else EQ

  let rec mergeOuterChunks<'a, 'b, 'k when 'a : equality and 'b : equality and 'k : comparison>
    : Tee<'k * Vector<'a>, 'k * Vector<'b>, These<'a, 'b>> =
    Plan.awaits left<_, _>
    >>= (fun (ka, as_) ->
      Plan.awaits right<_, _>
      >>= (fun (kb, bs) -> mergeOuterAux ka as_ kb bs)
      |> Plan.orElse (
        Plan.traversePlan_ Vector.foldBack as_ (This >> emit)
        >>. (Machine.flattened Vector.foldBack left<_, _>
        |> Plan.inmap (function
          | Choice1Of2 a -> Choice1Of2 (snd >> a)
          | Choice2Of2 b -> Choice2Of2 b)
        |> Plan.outmap This)))
    |> Plan.orElse (
      Machine.flattened Vector.foldBack right<_, _>
      |> Plan.inmap (Choice.mapSecond (fun y -> snd >> y))
      |> Plan.outmap That)

  and mergeOuterAux ka ca kb cb : Tee<'k * Vector<'a>, 'k * Vector<'b>, These<'a, 'b>> =
    match (ka, kb) with
    | LT ->
      Plan.traversePlan_ Vector.foldBack ca (fun a ->
        emit (This a)
        >>. awaits left<_, _>
        >>= (fun (kap, cap) -> mergeOuterAux kap cap kb cb)
        |> Plan.orElse (
          Plan.traversePlan_ Vector.foldBack cb (That >> emit)
          >>. (Machine.flattened Vector.foldBack right<_, _>
          |> Plan.inmap (Choice.mapSecond (fun y -> snd >> y))
          |> Plan.outmap That)))
    | GT ->
      Plan.traversePlan_ Vector.foldBack cb (fun b ->
        emit (That b)
        >>. awaits right<_, _>
        >>= (fun (kbp, cbp) -> mergeOuterAux ka ca kbp cbp)
        |> Plan.orElse (
          Plan.traversePlan_ Vector.foldBack ca (This >> emit)
          >>. (Machine.flattened Vector.foldBack left<_, _>
          |> Plan.inmap (function
            | Choice1Of2 a -> Choice1Of2 (snd >> a)
            | Choice2Of2 b -> Choice2Of2 b)
          |> Plan.outmap This)))
    | EQ ->
      Plan.traversePlan_ Vector.foldBack
        (Vector.ofSeq (seq { for a in ca do for b in cb do yield Both(a, b) })) emit
      >>. mergeOuterChunks

  let mergeOuterJoin f g : Tee<'a, 'b, These<'a, 'b>> =
    let f = Process.groupingBy f |> Plan.outmap (fun (x,v) -> (x, Vector.ofSeq v))
    let g = Process.groupingBy g |> Plan.outmap (fun (x,v) -> (x, Vector.ofSeq v))
    tee f g mergeOuterChunks
