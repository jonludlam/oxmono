open! Core

type t =
  | Linear of
      { y_intercept : float
      ; slope : float
      }
  | Logarithmic of
      { scale : float
      ; zero_value : float
      }
[@@deriving quickcheck, sexp_of]

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let bound = 1_000_000. in
  let%bind make_linear = Bool.quickcheck_generator in
  if make_linear
  then (
    let%bind y_intercept = Float.gen_incl (-.bound) bound in
    let%bind slope =
      let%bind sign = Bool.quickcheck_generator in
      let%bind magnitude = Float.gen_incl 0.0001 bound in
      return (if sign then magnitude else -.magnitude)
    in
    return (Linear { y_intercept; slope }))
  else (
    let%bind scale = Float.gen_incl 0.1 bound in
    let%bind zero_value = Float.gen_incl (-.bound) bound in
    return (Logarithmic { scale; zero_value }))
;;

let assert_min_value_lt_max_value ~min_value ~max_value =
  if Float.( >= ) min_value max_value
  then
    raise_s
      [%message "min_value must be < max_value" (min_value : float) (max_value : float)]
;;

let create_linear_exn ~min_value ~max_value ~max_bar_height =
  assert_min_value_lt_max_value ~min_value ~max_value;
  let slope = Float.of_int max_bar_height /. (max_value -. min_value) in
  Linear { y_intercept = slope *. min_value *. -1.; slope }
;;

let create_logarithmic_exn ~min_value ~max_value ~max_bar_height =
  assert_min_value_lt_max_value ~min_value ~max_value;
  let scale = Float.of_int max_bar_height /. Float.log (max_value -. min_value) in
  Logarithmic { zero_value = min_value; scale }
;;

let f t x =
  match t with
  | Linear { y_intercept; slope } -> y_intercept +. (slope *. x)
  | Logarithmic { scale; zero_value } ->
    let x = x -. zero_value in
    (* If x < 1. (meaning that log(x) < 0), we switch to a linearly decreasing value. This
       is to make the function invertible for every value, which is primarily useful for
       the quicktest. *)
    if Float.( < ) x 1. then x -. 1. else scale *. Float.log x
;;

let inverse_f t y =
  match t with
  | Linear { y_intercept; slope } -> (y -. y_intercept) /. slope
  | Logarithmic { scale; zero_value } ->
    if Float.( <= ) y 0.
    then y +. 1. +. zero_value
    else Float.exp (y /. scale) +. zero_value
;;

let height t value = f t value |> Float.iround_exn ~dir:`Nearest

let precise_height t value =
  let num_eighths = f t value *. 8. |> Float.iround_exn ~dir:`Nearest in
  if num_eighths >= 0
  then ~whole_blocks:(num_eighths / 8), ~extra_eighths:(num_eighths % 8)
  else ~whole_blocks:(num_eighths / 8), ~extra_eighths:(-(-num_eighths % 8))
;;

let value_at_height = inverse_f

module For_testing = struct
  let f = f
  let inverse_f = inverse_f
end
