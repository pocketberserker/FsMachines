namespace Machines.Tests

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open Machines
open FSharpx

[<TestFixture>]
module TeeTest =

  let q1 = Source.source List.foldBack ([("3", 1); ("5", 2); ("7", 2)]) |> Procedure.idProcedure
  let q2 = Source.source List.foldBack ([(2, "10"); (1, "9"); (12, "14")]) |> Procedure.idProcedure
  
  let listMonoid<'a> =
    { new Monoid<'a list>() with
      override this.Zero() = []
      override this.Combine(a, b) = List.append a b }

  [<Test>]
  let ``hashjoin emits something, in order`` () =
    let expected = [(("5", 2), (2, "10")); (("7", 2), (2, "10")); (("3", 1), (1, "9"))]
    let t = q1.Tee(q2, Tee.hashJoin snd fst)
    let actual = t.Map(Seq.singleton >> Seq.toList).Execute(listMonoid)
    actual |> should equal expected

  let randomPairs =
    Gen.listOf (gen {
      let! k = Gen.choose (0, 10)
      let! v = Gen.choose (0, 10)
      return (k, v)
    })
    |> Gen.map (List.sortBy fst)

  type RandomPairs =
    static member Tuple() =
      Arb.fromGen (Gen.two randomPairs)

  [<Property(Arbitrary = [| typeof<RandomPairs> |])>]
  let ``merge outer join doesn't lose any keys`` (l1: (int * int) list, l2: (int * int) list) =
    let results =
      Tee.mergeOuterJoin fst fst
      |> Tee.capL (Source.source List.foldBack l1)
      |> Source.cap (Source.source List.foldBack l2)
      |> Plan.foldMap listMonoid (Seq.singleton >> Seq.toList)
    let reference = Set.ofList <| List.append (l1 |> List.map fst) (l2 |> List.map fst)

    results
    |> List.map (function
      | Both(a, b) -> fst a
      | This a -> fst a
      | That b -> fst b)
    |> Set.ofList
    |> ((=) reference)

  [<Property(Arbitrary = [| typeof<RandomPairs> |])>]
  let ``merge outer join doesn't lose any values`` (l1: (int * int) list, l2: (int * int) list) =
    let rawResult =
      Tee.mergeOuterJoin fst fst
      |> Tee.capL (Source.source List.foldBack l1)
      |> Source.cap (Source.source List.foldBack l2)
      |> Plan.foldMap listMonoid (Seq.singleton >> Seq.toList)
    let result =
      rawResult
      |> List.collect (function
        | Both(a, b) -> [a; b]
        | This a -> [a]
        | That b -> [b])
      |> Seq.groupBy fst
      |> Seq.map (fun (k, v) -> (k, Set.ofSeq v))
      |> Seq.sortBy fst
      |> Seq.toList
    let reference =
      List.append l1 l2
      |> Seq.groupBy fst
      |> Seq.map (fun (k, v) -> (k, Set.ofSeq v))
      |> Seq.sortBy fst
      |> Seq.toList

    result = reference

  let intersectWithKey m1 m2 f =
    m1
    |> Seq.choose (fun (k, v) ->
      if m2 |> Seq.exists (fst >> ((=) k)) then
        Some (k, f k v (m2 |> Seq.find (fst >> ((=) k)) |> snd))
      else None)

  let intersectWith m2 f m1 = intersectWithKey m1 m2 (fun _ x y -> f (x, y))

  [<Property(Arbitrary = [| typeof<RandomPairs> |])>]
  let ``hash join works`` (l1: (int * int) list, l2: (int * int) list) =
    let result =
      Tee.hashJoin fst fst
      |> Tee.capL (Source.source List.foldBack l1)
      |> Source.cap (Source.source List.foldBack l2)
      |> Plan.foldMap listMonoid (Seq.singleton >> Seq.toList)
      |> List.map (fun ((k, v1), (_, v2)) -> (k, (v1, v2)))
      |> Seq.sort
      |> Seq.toList

    let cartesian a b = seq { for aa in a do for bb in b do yield (aa, bb) }

    let reference =
      l1
      |> Seq.groupBy fst
      |> intersectWith (l2 |> Seq.groupBy fst) (fun (v1, v2) -> cartesian v1 v2)
      |> Seq.collect (fun (k, pairs) ->
        pairs |> Seq.map (fun (v1, v2) -> (k, (snd v1,  snd v2))))
      |> Seq.sort
      |> Seq.toList

    result = reference
