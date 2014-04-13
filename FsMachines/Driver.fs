namespace Machines

open FSharpx

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

[<AbstractClass>]
type IdDriver<'T>() =

  abstract member Apply: 'T -> Option<obj>

  member this.DriveLeft(m: Machine<'T, 'a>, g: 'a -> 'b, initial: 'c, f: 'c * 'b -> 'c) =
    let rec inner (m: Machine<'T, 'a>) (z: 'c) : 'c =
      match m with
      | Stop -> z
      | Emit(a, k) ->
        let next = k ()
        a |> g |> (fun x -> inner next (f (z, x)))
      | Await(k, s, f) ->
        this.Apply(s)
        |> (Option.map k >> Option.getOrElse (f ()))
        |> (flip inner z)
    inner m initial
  
  abstract member Drive : Monoid<'b> * Machine<'T, 'a> * ('a -> 'b) -> 'b
  default this.Drive(monoid: Monoid<'b>, m: Machine<'T, 'a>, g: 'a -> 'b) =
    this.DriveLeft(m, g, monoid.Zero(), fun (a, b) -> monoid.Combine(a, b))

  abstract member Append : IdDriver<'U> -> IdDriver<Choice<'T, 'U>>
  default this.Append(d: IdDriver<'U>): IdDriver<Choice<'T, 'U>> =
    { new IdDriver<Choice<'T, 'U>>() with
      member x.Apply(k) =
        match k with
        | Choice1Of2 a -> this.Apply(a)
        | Choice2Of2 b -> d.Apply(b) }

module Driver =

  let driveId (monoid: Monoid<'b>) drv (m: Machine<'k, 'a>) g =
    let rec inner acc = function
      | Stop -> acc
      | Emit(h, t) ->
        let r = g h
        inner (monoid.Combine(acc, r)) (t())
      | Await(recv, k, fb) -> inner acc (drv k |> Option.map recv |> Option.getOrElse (fb()))
    inner (monoid.Zero()) m

  let rec id<'T> (drv: 'T -> obj option) : IdDriver<'T> =
    { new IdDriver<'T>() with
      member this.Apply(k) = drv k
      override this.Drive(monoid, m, g) = driveId monoid drv m g
      override this.Append(d) =
        id<Choice<'T, 'U>> (function
          | Choice1Of2 a -> this.Apply(a)
          | Choice2Of2 b -> d.Apply(b)) }
