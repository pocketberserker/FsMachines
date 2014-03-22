module ProcedureTest

open System
open NUnit.Framework
open FsUnit
open Machines
open Procedure
open FSharpx

let rReadLn (r: IO.StreamReader) = 
  r.ReadLineAsync()
  |> Task.map (fun x -> if x |> String.IsNullOrEmpty then None else Some x)
  |> Task.toAsync

let getFileLines (s: IO.Stream) m =
  { new AsyncProcedure<_, _>() with
    member x.Machine = m
    member x.WithDriver(k) = async {
      use stream = new IO.StreamReader(s)
      let d =
        { new Driver.AsyncDriver<_>() with
          member y.Apply(k) = rReadLn stream |> Async.map (Option.map k) }
      return! k d
    }
  }

let lineCount (st: string) =
  let stream = new IO.MemoryStream(Text.Encoding.UTF8.GetBytes(st))
  (getFileLines stream (Process.apply (konst 1))).Execute Monoid.sumInt

let lineCharCount (st: string) =
  let stream = new IO.MemoryStream(Text.Encoding.UTF8.GetBytes(st))
  (getFileLines stream (Process.apply (fun x -> (1, x.Length)))).Execute
    (Monoid.tuple2 Monoid.sumInt Monoid.sumInt)

let words: Plan.Process<string, string> =
  Plan.plan {
    let! s = Plan.await<string, _>
    let xs = s.Split([|"\\W"|], StringSplitOptions.None) |> Array.toList
    do! Plan.traversePlan_ List.foldBack xs Plan.emit
    return ()
  } |> Plan.repeatedly

let lineWordCount (st: string) =
  let stream = new IO.MemoryStream(Text.Encoding.UTF8.GetBytes(st))
  let fuga =
    Plan.split words Process.id
    |> Plan.outmap (function
      | Choice1Of2 _ -> (1, 0)
      | Choice2Of2 _ -> (0, 1))
  (getFileLines stream fuga).Execute (Monoid.tuple2 Monoid.sumInt Monoid.sumInt)

let input =
  Text.StringBuilder().AppendLine("hoge").AppendLine("fuga").AppendLine("piyo").ToString()

[<Test>]
let ``line`` () =
  input
  |> lineCount
  |> Async.RunSynchronously
  |> should equal 3

[<Test>]
let ``line char`` () =
  input
  |> lineCharCount
  |> Async.RunSynchronously
  |> should equal (3, 12)

[<Test>]
let ``line word`` () =
  input
  |> lineWordCount
  |> Async.RunSynchronously
  |> should equal (3, 3)
