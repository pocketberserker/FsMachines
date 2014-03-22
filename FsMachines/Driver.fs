module Machines.Driver

open FSharpx
open Plan

[<AbstractClass>]
type AsyncDriver<'T>() =

  abstract member Apply: 'T -> Async<Option<obj>>

  member this.DriveLeft(m: Machine<'T, 'a>, g: 'a -> Async<'b>, initial: 'c, f: 'c * 'b -> 'c) =
    let rec inner (m: Machine<'T, 'a>) (z: 'c) : Async<'c> =
      match m with
      | Stop -> Async.returnM z
      | Emit(a, k) ->
        let next = k ()
        a |> g |> Async.bind (fun x -> inner next (f (z, x)))
      | Await(k, s, f) ->
        this.Apply(s)
        |> Async.map (Option.map k >> Option.getOrElse (f ()))
        |> Async.bind (flip inner z)
    inner m initial
  
  member this.Drive(monoid: Monoid<'b>, m: Machine<'T, 'a>, g: 'a -> Async<'b>) =
    this.DriveLeft(m, g, monoid.Zero(), monoid.Combine)

  member this.Append(d: AsyncDriver<'U>): AsyncDriver<Choice<'T, 'U>> =
    { new AsyncDriver<Choice<'T, 'U>>() with
      member x.Apply(k) =
        match k with
        | Choice1Of2 a -> this.Apply(a)
        | Choice2Of2 b -> d.Apply(b) }
