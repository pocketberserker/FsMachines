namespace Machines

open Plan

type Wye<'i, 'j, 'o> = Machine<These<'i -> obj, 'j -> obj>, 'o>

module Wye =

  let rec wye (pa : Process<'a, 'aa>) (pb : Process<'b, 'bb>) (y : Wye<'aa, 'bb, 'o>)
    : Wye<'a, 'b, 'o> =
    match y with
    | Stop -> Stop
    | Emit(o, next) -> Emit(o, fun () -> wye pa pb (next ()))
    | Await(k, This kl, f) ->
      match pa with
      | Stop -> wye Stop pb (f ())
      | Emit(a, next) -> wye (next ()) pb (k (kl a))
      | Await(l, t, g) ->
        Await begin
          (fun (paa : obj) -> wye (paa :?> Process<'a, 'aa>) pb y),
          This (fun a -> a |> t |> l |> box),
          fun () -> wye (g ()) pb y
        end
    | Await(k, That kr, f) ->
      match pb with
      | Stop -> wye pa Stop (f ())
      | Emit(b, next) -> wye pa (next ()) (k (kr b))
      | Await(l, t, g) ->
        Await begin
          (fun (pbb : obj) -> wye pa (pbb :?> Process<'b, 'bb>) y),
          That (fun b -> b |> t |> l |> box),
          fun () -> wye pa (g ()) y
        end
    | Await(k, Both(kl, kr), f) ->
      match pa with
      | Emit(a, next) -> wye (next ()) pb (k (kl a))
      | Stop ->
        match pb with
        | Emit(b, next) -> wye Stop (next ()) (k (kr b))
        | Stop -> wye Stop Stop (f ())
        | Await(l, t, g) ->
          Await begin
            (fun (pbb : obj) -> wye Stop (pbb :?> Process<'b, 'bb>) y),
            That (fun b -> b |> t |> l |> box),
            fun () -> wye Stop (g ()) y
          end
      | Await(la, ta, ga) ->
        match pb with
        | Emit(b, next) -> wye pa (next ()) (k (kr b))
        | Stop ->
          Await begin
            (fun (paa : obj) -> wye (paa :?> Process<'a, 'aa>) Stop y),
            This (fun a -> a |> ta |> la |> box),
            fun () -> wye (ga ()) Stop y
          end
        | Await(lb, tb, gb) ->
          Await begin
            (fun (x : obj) -> x :?> Wye<'a, 'b, 'o>),
            Both((fun a -> wye (la (ta a)) pb y |> box), fun b -> wye pa (lb (tb b)) y |> box),
            fun () -> wye (ga ()) (gb ()) y
          end
