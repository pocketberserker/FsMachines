module Machines.Source

open Plan

type Source<'k, 'o> = Machine<'k, 'o>

let repeated o : Source<'k, 'o> = o |> emit |> repeatedly

let cap (r : Source<'k, 'i>) (p : Process<'i, 'o>) : Source<'k, 'o> = r |> andThen p

let cycled foldBack os : Source<'k, 'o> =
  traversePlan_ foldBack os emit |>repeatedly

let source foldBack os : Source<'k, 'o> =
  traversePlan_ foldBack os emit |> compile
