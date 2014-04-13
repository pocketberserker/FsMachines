namespace Machines

open FSharpx

[<AbstractClass>]
type AsyncProcedure<'T, 'U>() =

  abstract member Machine : Machine<'T, 'U>
  abstract member WithDriver<'b> : (AsyncDriver<'T> -> Async<'b>) -> Async<'b>

  member this.Map(f: 'U -> 'V) =
    { new AsyncProcedure<'T, 'V>() with
      member x.Machine = this.Machine |> Plan.outmap f
      member x.WithDriver(f) = this.WithDriver(f) }

  member this.AndThen(p: Process<'U, 'b>) =
    { new AsyncProcedure<_, _>() with
      member x.Machine = this.Machine |> Plan.andThen p
      member x.WithDriver(f) = this.WithDriver(f) }

  member this.Tee(p: AsyncProcedure<_,_>, t) =
    { new AsyncProcedure<_, _>() with
      member x.Machine = Tee.tee this.Machine p.Machine t
      member x.WithDriver(k) =
        this.WithDriver(fun d1 -> p.WithDriver(fun d2 -> k (d1.Append(d2)))) }

  member this.Execute(monoid: Monoid<'U>) =
    this.WithDriver(fun d -> d.Drive(monoid, this.Machine, Async.returnM))

[<AbstractClass>]
type IdProcedure<'T, 'U>() =

  abstract member Machine : Machine<'T, 'U>
  abstract member WithDriver<'b> : (IdDriver<'T> -> 'b) -> 'b

  member this.Map(f: 'U -> 'V) =
    { new IdProcedure<'T, 'V>() with
      member x.Machine = this.Machine |> Plan.outmap f
      member x.WithDriver(f) = this.WithDriver(f) }

  member this.AndThen(p: Process<'U, 'b>) =
    { new IdProcedure<_, _>() with
      member x.Machine = this.Machine |> Plan.andThen p
      member x.WithDriver(f) = this.WithDriver(f) }

  member this.Tee(p: IdProcedure<_,_>, t) =
    { new IdProcedure<_, _>() with
      member x.Machine = Tee.tee this.Machine p.Machine t
      member x.WithDriver(k) =
        this.WithDriver(fun d1 -> p.WithDriver(fun d2 -> k (d1.Append(d2)))) }

  member this.Execute(monoid: Monoid<'U>) =
    this.WithDriver(fun d -> d.Drive(monoid, this.Machine, id))

module Procedure =

  let idProcedure<'U> (s: Source<_, 'U>) =
    { new IdProcedure<unit, 'U>() with
      member x.Machine = s
      member x.WithDriver(f) = f (Driver.id (box >> Some)) }
