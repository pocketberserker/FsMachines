namespace Machines

[<AutoOpen>]
module Seq =

  let (|EmptySeq|Cons|) xs =
    if xs |> Seq.isEmpty then EmptySeq
    else Cons ((Seq.head xs), (Seq.skip 1 xs))

  let rec foldBack f xs init =
    let xs = Seq.cache xs
    match xs with
    | EmptySeq -> init
    | Cons(x, xs) -> f x (lazy foldBack f xs init)

  let cons x (xs: Lazy<'a seq>) = seq { yield x; yield! xs.Force() }
