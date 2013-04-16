module Machines.Plan

type Plan<'k, 'o, 'a> =
  | Return of 'a
  | Emit of 'o * (unit -> Plan<'k, 'o, 'a>)
  | Await of (obj -> Plan<'k, 'o, 'a>) * 'k * (unit -> Plan<'k, 'o, 'a>)
  | Stop

type Handle<'k, 'o> = ('o -> obj) -> 'k

type Machine<'k, 'o> = Plan<'k, 'o, unit>

type Process<'i, 'o> = Machine<'i -> obj, 'o>

type Automation<'i, 'o> =
  | Mealy of ('i -> 'o * Automation<'i, 'o>)
  | Moore of 'o * ('i -> Automation<'i, 'o>)

type Joiner<'i, 'j, 'o> = Machine<'i -> obj * 'j -> obj, 'o>

type Splitter<'i, 'o, 'p> = Machine<'i, Choice<'o, 'p>>

let rec andThen (p : Process<'o, 'p>) (pl : Plan<'k, 'o, 'a>) : Plan<'k, 'p, 'a> =
  let rec inner pl = function
  | Emit(o, next) -> Emit(o, fun () -> andThen (next ()) pl)
  | Stop -> Stop
  | Await(k, s, f) ->
    match pl with
    | Stop -> inner Stop (f ())
    | Emit(o, next) -> inner (next ()) (k (s o))
    | Await(l, t, g) -> Await(l >> (andThen p), t, fun () -> andThen p (g ()))
    | Return x -> Return x
  in inner pl p

let rec bind f = function
| Return x -> f x
| Emit(o, next) -> Emit(o, fun () -> bind f (next ()))
| Await(k, success, failure) -> Await(k >> (bind f), success, fun () -> bind f (failure ()))
| Stop -> Stop

let inline (>>=) m f = bind f m
let inline (=<<) f m = bind f m
let inline (>>.) p next = p >>= (fun _ -> next)

let rec orElse p = function
| Return _ as r -> r
| Emit(o, next) -> Emit(o, fun () -> next () |> orElse p)
| Await(k, success, failure) -> Await(k, success, fun () -> failure () |> orElse p)
| Stop -> p

let map f p = p >>= (fun x -> Return (f x))

let rec inmap p = function
| Return x -> Return x
| Stop -> Stop
| Emit(o, next) -> Emit(o, fun () -> next () |> inmap p)
| Await(k, s, f) -> Await(k >> (inmap p), p s, fun () -> f () |> inmap p)

let rec outmap f = function
| Return x -> Return x
| Stop -> Stop
| Emit(o, next) -> Emit(f o, fun () -> next () |> outmap f)
| Await(k, success, failure) -> Await(k >> (outmap f), success, fun () -> failure () |> outmap f)

let rec fold acc f = function
| Emit(o, next) -> next () |> fold (f acc o) f
| Await(_, _, failure) -> failure () |> fold acc f
| _ -> acc

let rec foldBack f p acc =
  match p with
  | Emit(o, next) -> f o (foldBack f (next ()) acc)
  | Await(_, _, failure) -> foldBack f (failure ()) acc
  | _ -> acc

let compile p : Machine<'k, 'o> = p >>. Stop

let rec repeatedly p : Machine<'k, 'o> =
  let inline (>>.) p (next : Lazy<Plan<'k, 'o, 'a>>) = p >>= (fun _ -> next.Value)
  let rec r = lazy (p >>. r)
  r.Value

let rec replicate n p =
  match n with
  | n when n <= 0 -> Return ()
  | n -> p >>. (replicate (n - 1) p)

let rec iterate h = function
| Return x -> iterate h (h x)
| Stop -> Stop
| Emit(o, next) -> Emit(o, fun () -> next () |> iterate h)
| Await(k, s, f) -> Await(k >> (iterate h), s, fun () -> f () |> iterate h)

let rec split (y : Process<'o, 'p>) pl =
  match pl, y with
  | Emit(o, h), Emit(p, k) ->
    Emit(Choice1Of2 o, fun () -> Emit(Choice2Of2 p, fun () -> h () |> split (k ())))
  | Emit(o, h), Await(k, s, f) -> Emit(Choice1Of2 o, fun () -> h () |> split (k (s o)))
  | Emit(o, h), _ -> Emit(Choice1Of2 o, fun () -> h() |> split y)
  | _, Emit(p, k) -> Emit(Choice2Of2 p, fun () -> pl |> split (k ()))
  | Await(k1, s1, f1), _ -> Await(k1 >> (split y), s1, fun () -> f1 () |> split y)
  | Stop, _ -> Stop
  | Return x, _ -> Return x

let rec sink (Moore(_, next) as m) = function
| Emit(o, k) -> k () |> sink (next o)
| Await(_, _, f) -> f () |> sink m
| _ -> m

let empty = Stop

let point a = Return a

let plus m1 m2 = m1 |> orElse m2
 
let fail = fun () -> Stop

let emit a = Emit(a, fun () -> Return ())

let await<'a, 'b> : Plan<'a -> obj, 'b, 'a> = Await((fun (a : obj) -> Return (a :?> 'a)), (fun x -> box x), fail)

let awaits (f : Handle<'k, 'j>) : Plan<'k,'o, 'j> =
  Await((fun (a : obj) -> Return (a :?> 'j)), f (fun x -> box x), fail)

type PlanBuilder() =
  member this.Return(x) = point x
  member this.Bind(m, f) = bind f m
  member this.Zero() = empty

let plan = PlanBuilder()

let traversePlan_ foldBack as_ (f : 'a -> Plan<'k, 'o, unit>) : Plan<'k, 'o, unit> =
  foldBack (fun a p -> f a >>. p) as_ (Return ())
 
let filter f p =
  bind (fun x -> if f x then Return x else Stop) p
