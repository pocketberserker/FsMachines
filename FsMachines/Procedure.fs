module Machines.Procedure

open FSharpx
open Machines.Plan
open Machines.Driver

[<AbstractClass>]
type AsyncProcedure<'T, 'U>() =

  abstract member Machine : Machine<'T, 'U>
  abstract member WithDriver<'b> : (AsyncDriver<'T> -> Async<'b>) -> Async<'b>

  member this.AndThen(p: Process<'U, 'b>) =
    { new AsyncProcedure<_, _>() with
      member x.Machine = this.Machine |> andThen p
      member x.WithDriver(f) = this.WithDriver(f) }

  member this.Tee(p: AsyncProcedure<_,_>, t) =
    { new AsyncProcedure<_, _>() with
      member x.Machine = Tee.tee this.Machine p.Machine t
      member x.WithDriver(k) =
        this.WithDriver(fun d1 -> p.WithDriver(fun d2 -> k (d1.Append(d2)))) }

  member this.Execute(monoid: Monoid<'U>) =
    this.WithDriver(fun d -> d.Drive(monoid, this.Machine, Async.returnM))
