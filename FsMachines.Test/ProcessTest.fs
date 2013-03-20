module ProcessTest

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open Machines
open FSharpx

let fsCheck t = fsCheck "" t
let infiniteIntSeq = Seq.unfold (fun i -> Some(i+1, i+1)) 0 |> Seq.cache

[<Test>]
let ``transduce doesn't stack overflow given huge input`` () = 
  fsCheck <| fun () ->
    Process.transduce [0 .. 100000] (Plan.await |> Plan.bind Plan.emit |> Plan.compile) |> ignore
    true

[<Test>]
let ``repeated emits don't stack overflow given huge input`` () = 
  fsCheck <| fun () ->
    Process.transduce [0] (Plan.traversePlan_ List.foldBack [0 .. 100000] Plan.emit |> Plan.compile) |> ignore
    true

[<Test>]
let ``filtered`` () = 
  fsCheck <| fun () ->
    Process.transduce infiniteIntSeq (Process.filtered (flip (%) 2 >> (=) 0) |> Plan.andThen (Process.taking 100)) |>ignore
    Process.transduce infiniteIntSeq (Process.takingWhile (flip (<) 100)) |> ignore
    true
