module Machines.These

type These<'a, 'b> =
  | This of 'a
  | That of 'b
  | Both of 'a * 'b
