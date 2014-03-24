namespace Machines

open Plan

module Machine =

  let pass (h : Handle<'k, 'o>) = awaits h >>= (fun x -> emit x) |> repeatedly

  let stopped : Machine<'a, 'a> = Stop

  let flattened foldBack (h : Handle<'k, _>) =
    awaits h |> bind (fun is -> traversePlan_ foldBack is emit) |> repeatedly

module Process =

  let echo<'a> : Process<'a, 'a> = repeatedly await

  let id<'a> : Process<'a, 'a> = await >>= emit |> repeatedly

  let compose (m : Process<'b, 'c>) (n : Process<'a, 'b>) : Process<'a, 'c> = n |> andThen m

  let transduce (l : 'a seq) (p : Process<'a, 'b>): 'b seq =
    let rec go (acc: 'b seq) (s: Process<'a, 'b>) (in_ : 'a seq) : 'b seq =
      match s with
      | Stop -> acc
      | Emit(h, t) -> go (Seq.append acc [ h ]) (t ()) in_
      | Await(k, f, fb) ->
        if Seq.isEmpty in_ then go acc (fb ()) in_
        else go acc (k (f (Seq.head in_))) (Seq.skip 1 in_)
    go Seq.empty p l

  let filtered (p : 'a -> bool) : Process<'a, 'a> =
    repeatedly <| plan {
      let! i = await
      return! if p i then emit i else Return ()
    }

  let dropping n : Process<'a, 'a> = replicate n await >>. id

  let taking n : Process<'a, 'a> = replicate n (await >>= emit) |> compile

  let takingWhile (p : 'a -> bool): Process<'a, 'a> =
    await >>= (fun v -> if p v then emit v else Stop) |> repeatedly

  let buffered n : Process<'a, 'a seq> =
    let rec go (xs : 'a seq) c =
      match xs, c with
      | acc, 0 when Seq.isEmpty acc -> Stop
      | acc, 0 -> emit acc
      | acc, n -> plan {
        let! i = await |> orElse (emit acc >>. Stop)
        return! go (Seq.append acc [i]) (n - 1)
    }
    go Seq.empty<'a> n |> repeatedly

  let grouping (p : 'a * 'a -> bool): Process<'a, 'a seq> =
    let rec collect (acc: 'a seq) x : Process<'a, 'a seq> =
      await
      |> orElse (emit (Seq.append acc [x]) >>. Stop)
      >>= (fun y ->
        if p (x, y) then collect (Seq.append acc [x]) y
        else emit(Seq.append acc [x]) >>. collect Seq.empty y
      )
    await >>= (fun x -> collect Seq.empty x)

  let groupingBy (f: 'a -> 'k): Process<'a, 'k * 'a seq> =
    grouping (fun (x, y) -> f x = f y) |> outmap (fun v -> (f (Seq.head v), v))

  let wrapping<'a> : Process<'a, 'a seq> = grouping (fun (_, _) -> true)

  let rec supply x = function
    | m when Seq.isEmpty x -> m
    | Stop -> Stop
    | Emit(o, next) -> Emit(o, fun () -> supply x (next ()))
    | Await(f, g, _) -> supply (Seq.skip 1 x) (f (g (Seq.head x)))

  let apply f : Process<'a, 'b> = await >>= (f >> emit) |> repeatedly
