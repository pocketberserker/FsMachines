module Machines.Automation

open Plan

let rec process_ = function
  | Mealy step -> bind (fun i -> match step i with | (o, next) -> emit o >>. process_ next) await
  | Moore(msg, next) -> emit msg >>. bind (fun i -> process_ (next i)) await
 