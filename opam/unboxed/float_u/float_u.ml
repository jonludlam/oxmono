open Ocaml_intrinsics_kernel.Conditional.Unboxed
module F = Base.Float
module A = Base.Array
module FA = Float_array

type t = float#

module Boxed = Core.Float

let globalize (local_ t) : t = t

external of_float : (float[@local_opt]) -> float# @@ portable = "%unbox_float"
external to_float : float# -> (float[@local_opt]) @@ portable = "%box_float"

(* Note about the implementation strategy:

   Most functions in this file are implemented by boxing the float, calling the equivalent
   function on boxed floats, and then unboxing the result. This may seem surprising: isn't
   the point of unboxed types to avoid boxes? But it's fine; the compiler's middle-end
   will reliably eliminate these boxing and unboxing steps, and the testsuite checks there
   are no allocations here. If you add new functions, you should add similar tests.

   Why is it done this way? Just because it was less work than adding many new primitives
   to the compiler. But it is likely we will do that work, one day.
*)

module Shared_derived = struct
  let%template[@alloc a = (heap, stack)] [@inline] sexp_of_t t : Base.Sexp.t =
    (F.sexp_of_t [@alloc a] [@inlined hint]) (to_float t) [@exclave_if_stack a]
  ;;

  let[@inline] t_of_sexp sexp : t = of_float ((F.t_of_sexp [@inlined hint]) sexp)
  let t_sexp_grammar = Sexplib0.Sexp_grammar.coerce F.t_sexp_grammar

  include Bin_prot_unboxed_numbers.Float_u

  let[@inline] hash_fold_t state t = (F.hash_fold_t [@inlined hint]) state (to_float t)
  let[@inline] hash t = (F.hash [@inlined hint]) (to_float t)
  let typerep_of_t = Typerep_lib.Std.Typerep.Float_u
  let typename_of_t = Typerep_lib.Std.typename_of_float_u
  let[@inline] to_string t : string = (F.to_string [@inlined hint]) (to_float t)
  let[@inline] of_string s : t = of_float ((F.of_string [@inlined hint]) s)

  [%%template
  [@@@mode.default m = (global, local)]

  let[@inline] [@zero_alloc] equal t1 t2 : bool = F.equal (to_float t1) (to_float t2)
  let[@inline] [@zero_alloc] compare t1 t2 : int = F.compare (to_float t1) (to_float t2)]
end

include Shared_derived

let[@inline] ascending t1 t2 : int =
  (F.ascending [@inlined hint]) (to_float t1) (to_float t2)
;;

let[@inline] descending t1 t2 : int =
  (F.descending [@inlined hint]) (to_float t1) (to_float t2)
;;

let[@inline] between t ~low ~high : bool =
  (F.between [@inlined hint]) (to_float t) ~low:(to_float low) ~high:(to_float high)
;;

let[@inline] [@zero_alloc strict] clamp_exn t ~min ~max : t =
  of_float
    ((F.clamp_exn [@inlined hint]) (to_float t) ~min:(to_float min) ~max:(to_float max))
;;

let[@inline] pp ppf t : unit = (F.pp [@inlined hint]) ppf (to_float t)
let[@inline] invariant t : unit = (F.invariant [@inlined hint]) (to_float t)
let[@inline] [@zero_alloc strict] nan () : t = of_float F.nan
let[@inline] [@zero_alloc strict] infinity () : t = of_float F.infinity
let[@inline] [@zero_alloc strict] neg_infinity () : t = of_float F.neg_infinity
let[@inline] [@zero_alloc strict] max_value () : t = of_float F.max_value
let[@inline] [@zero_alloc strict] min_value () : t = of_float F.min_value
let[@inline] [@zero_alloc strict] zero () : t = of_float F.zero
let[@inline] [@zero_alloc strict] one () : t = of_float F.one
let[@inline] [@zero_alloc strict] minus_one () : t = of_float F.minus_one
let[@inline] [@zero_alloc strict] pi () : t = of_float F.pi
let[@inline] [@zero_alloc strict] sqrt_pi () : t = of_float F.sqrt_pi
let[@inline] [@zero_alloc strict] sqrt_2pi () : t = of_float F.sqrt_2pi

let[@inline] [@zero_alloc strict] euler_gamma_constant () : t =
  of_float F.euler_gamma_constant
;;

let[@inline] [@zero_alloc strict] epsilon_float () : t = of_float F.epsilon_float
let[@inline] [@zero_alloc strict] max_finite_value () : t = of_float F.max_finite_value

let[@inline] [@zero_alloc strict] min_positive_subnormal_value () : t =
  of_float F.min_positive_subnormal_value
;;

let[@inline] [@zero_alloc strict] min_positive_normal_value () : t =
  of_float F.min_positive_normal_value
;;

let[@inline] to_int64_preserve_order t : int64 option =
  (F.to_int64_preserve_order [@inlined hint]) (to_float t)
;;

let[@inline] to_int64_preserve_order_exn t : int64 =
  (F.to_int64_preserve_order_exn [@inlined hint]) (to_float t)
;;

let[@inline] of_int64_preserve_order i : t =
  of_float ((F.of_int64_preserve_order [@inlined hint]) i)
;;

let[@inline] one_ulp ud t : t = of_float ((F.one_ulp [@inlined hint]) ud (to_float t))
let[@inline] [@zero_alloc] to_int t : int = (F.to_int [@inlined hint]) (to_float t)

let[@inline] [@zero_alloc] to_int_unchecked t : int =
  (Base.Int.of_float_unchecked [@inlined hint]) (to_float t)
;;

let[@inline] truncate t : int = Stdlib.truncate (to_float t)
let[@inline] of_int63 i : t = of_float ((F.of_int63 [@inlined hint]) i)
let[@inline] of_int64 i : t = of_float ((F.of_int64 [@inlined hint]) i)
let[@inline] to_int64 t : int64 = (F.to_int64 [@inlined hint]) (to_float t)

external unbox_f32 : (float32[@local_opt]) -> float32# @@ portable = "%unbox_float32"
external box_f32 : float32# -> (float32[@local_opt]) @@ portable = "%box_float32"

external float32_of_float
  :  local_ float
  -> (float32[@local_opt])
  @@ portable
  = "%float32offloat"

external float_of_float32
  :  local_ float32
  -> (float[@local_opt])
  @@ portable
  = "%floatoffloat32"

let[@inline] [@zero_alloc] to_float32_u t : float32# =
  unbox_f32 (float32_of_float (to_float t))
;;

let[@inline] [@zero_alloc] of_float32_u f : t = of_float (float_of_float32 (box_f32 f))
let[@inline] round ?dir t : t = of_float ((F.round [@inlined hint]) ?dir (to_float t))
let[@inline] iround ?dir t : int option = (F.iround [@inlined hint]) ?dir (to_float t)
let[@inline] iround_exn ?dir t : int = (F.iround_exn [@inlined hint]) ?dir (to_float t)

let[@inline] [@zero_alloc assume_unless_opt] round_towards_zero t : t =
  of_float ((F.round_towards_zero [@inlined hint]) (to_float t))
;;

let[@inline] round_down t : t = of_float ((F.round_down [@inlined hint]) (to_float t))
let[@inline] round_up t : t = of_float ((F.round_up [@inlined hint]) (to_float t))

(** Rounds half integers up. *)
let[@inline] [@zero_alloc assume_unless_opt] round_nearest t : t =
  of_float ((F.round_nearest [@inlined hint]) (to_float t))
;;

(** Rounds half integers to the even integer. *)
let[@inline] [@zero_alloc assume_unless_opt] round_nearest_half_to_even t : t =
  of_float ((F.round_nearest_half_to_even [@inlined hint]) (to_float t))
;;

let[@inline] iround_towards_zero t : int option =
  (F.iround_towards_zero [@inlined hint]) (to_float t)
;;

let[@inline] iround_down t : int option = (F.iround_down [@inlined hint]) (to_float t)
let[@inline] iround_up t : int option = (F.iround_up [@inlined hint]) (to_float t)

let[@inline] iround_nearest t : int option =
  (F.iround_nearest [@inlined hint]) (to_float t)
;;

let[@inline] iround_towards_zero_exn t : int =
  (F.iround_towards_zero_exn [@inlined hint]) (to_float t)
;;

let[@inline] iround_down_exn t : int = (F.iround_down_exn [@inlined hint]) (to_float t)
let[@inline] iround_up_exn t : int = (F.iround_up_exn [@inlined hint]) (to_float t)

let[@inline] iround_nearest_exn t : int =
  (F.iround_nearest_exn [@inlined hint]) (to_float t)
;;

let[@inline] int63_round_down_exn t : Base.Int63.t =
  (F.int63_round_down_exn [@inlined hint]) (to_float t)
;;

let[@inline] int63_round_up_exn t : Base.Int63.t =
  (F.int63_round_up_exn [@inlined hint]) (to_float t)
;;

let[@inline] int63_round_nearest_exn t : Base.Int63.t =
  (F.int63_round_nearest_exn [@inlined hint]) (to_float t)
;;

let[@inline] iround_lbound () = of_float F.iround_lbound
let[@inline] iround_ubound () = of_float F.iround_ubound
let[@inline] int63_round_lbound () = of_float F.int63_round_lbound
let[@inline] int63_round_ubound () = of_float F.int63_round_ubound

let[@inline] round_significant t ~significant_digits : t =
  of_float ((F.round_significant [@inlined hint]) (to_float t) ~significant_digits)
;;

let[@inline] round_decimal t ~decimal_digits : t =
  of_float ((F.round_decimal [@inlined hint]) (to_float t) ~decimal_digits)
;;

let[@inline] [@zero_alloc] is_nan t : bool = (F.is_nan [@inlined hint]) (to_float t)
let[@inline] [@zero_alloc] is_inf t : bool = (F.is_inf [@inlined hint]) (to_float t)
let[@inline] [@zero_alloc] is_finite t : bool = (F.is_finite [@inlined hint]) (to_float t)

let[@inline] [@zero_alloc] is_integer t : bool =
  (F.is_integer [@inlined hint]) (to_float t)
;;

let%template[@inline] [@zero_alloc] min_inan t1 t2 : t =
  of_float ((F.min_inan [@mode local] [@inlined hint]) (to_float t1) (to_float t2))
;;

let%template[@inline] [@zero_alloc] max_inan t1 t2 : t =
  of_float ((F.max_inan [@mode local] [@inlined hint]) (to_float t1) (to_float t2))
;;

let[@inline] [@zero_alloc] mod_float t1 t2 : t =
  of_float (F.mod_float (to_float t1) (to_float t2))
;;

let[@inline] [@zero_alloc] add t1 t2 : t = of_float (F.add (to_float t1) (to_float t2))
let[@inline] [@zero_alloc] sub t1 t2 : t = of_float (F.sub (to_float t1) (to_float t2))

let[@inline] [@zero_alloc] scale t1 t2 : t =
  of_float (F.scale (to_float t1) (to_float t2))
;;

module O = struct
  external unbox : (float[@local_opt]) -> float# @@ portable = "%unbox_float"
  external box : float# -> (float[@local_opt]) @@ portable = "%box_float"

  let[@inline] [@zero_alloc strict] ( + ) t1 t2 : t =
    of_float (F.O.( + ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc strict] ( - ) t1 t2 : t =
    of_float (F.O.( - ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc strict] ( * ) t1 t2 : t =
    of_float (F.O.( * ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc strict] ( / ) t1 t2 : t =
    of_float (F.O.( / ) (to_float t1) (to_float t2))
  ;;

  let[@inline] ( % ) t1 t2 : t = of_float (F.O.( % ) (to_float t1) (to_float t2))

  let[@inline] [@zero_alloc strict] ( ** ) t1 t2 : t =
    of_float (F.O.( ** ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc strict] ( ~- ) t : t = of_float (F.O.( ~- ) (to_float t))

  let[@inline] [@zero_alloc strict] ( >= ) t1 t2 : bool =
    F.O.( >= ) (to_float t1) (to_float t2)
  ;;

  let[@inline] [@zero_alloc strict] ( <= ) t1 t2 : bool =
    F.O.( <= ) (to_float t1) (to_float t2)
  ;;

  let[@inline] [@zero_alloc strict] ( = ) t1 t2 : bool =
    F.O.( = ) (to_float t1) (to_float t2)
  ;;

  let[@inline] [@zero_alloc strict] ( > ) t1 t2 : bool =
    F.O.( > ) (to_float t1) (to_float t2)
  ;;

  let[@inline] [@zero_alloc strict] ( < ) t1 t2 : bool =
    F.O.( < ) (to_float t1) (to_float t2)
  ;;

  let[@inline] [@zero_alloc strict] ( <> ) t1 t2 : bool =
    F.O.( <> ) (to_float t1) (to_float t2)
  ;;

  let[@inline] [@zero_alloc strict] abs t : t = of_float (F.O.abs (to_float t))
  let[@inline] [@zero_alloc strict] neg t : t = of_float (F.O.neg (to_float t))

  let[@inline] [@zero_alloc strict] of_int i : t =
    of_float ((F.O.of_int [@inlined hint]) i)
  ;;
end

include O

module O_dot = struct
  let[@inline] [@zero_alloc] ( +. ) t1 t2 : t =
    of_float (F.O_dot.( +. ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc] ( -. ) t1 t2 : t =
    of_float (F.O_dot.( -. ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc] ( *. ) t1 t2 : t =
    of_float (F.O_dot.( *. ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc] ( /. ) t1 t2 : t =
    of_float (F.O_dot.( /. ) (to_float t1) (to_float t2))
  ;;

  let[@inline] ( %. ) t1 t2 : t = of_float (F.O_dot.( %. ) (to_float t1) (to_float t2))

  let[@inline] [@zero_alloc] ( **. ) t1 t2 : t =
    of_float (F.O_dot.( **. ) (to_float t1) (to_float t2))
  ;;

  let[@inline] [@zero_alloc] ( ~-. ) t : t = of_float (F.O_dot.( ~-. ) (to_float t))
end

let[@inline] to_string_hum ?delimiter ?decimals ?strip_zero ?explicit_plus t : string =
  (F.to_string_hum [@inlined hint])
    ?delimiter
    ?decimals
    ?strip_zero
    ?explicit_plus
    (to_float t)
;;

let[@inline] to_padded_compact_string t : string =
  (F.to_padded_compact_string [@inlined hint]) (to_float t)
;;

let[@inline] to_padded_compact_string_custom t ?prefix ~kilo ~mega ~giga ~tera ?peta ()
  : string
  =
  (F.to_padded_compact_string_custom [@inlined hint])
    (to_float t)
    ?prefix
    ~kilo
    ~mega
    ~giga
    ~tera
    ?peta
    ()
;;

let[@inline] int_pow t i : t = of_float ((F.int_pow [@inlined hint]) (to_float t) i)
let[@inline] square t : t = of_float ((F.square [@inlined hint]) (to_float t))
let[@inline] ldexp t i : t = of_float (F.ldexp (to_float t) i)
let[@inline] log10 t : t = of_float (F.log10 (to_float t))
let[@inline] log2 t : t = of_float (F.log2 (to_float t))
let[@inline] expm1 t : t = of_float (F.expm1 (to_float t))
let[@inline] log1p t : t = of_float (F.log1p (to_float t))
let[@inline] copysign t1 t2 : t = of_float (F.copysign (to_float t1) (to_float t2))
let[@inline] cos t = of_float (F.cos (to_float t))
let[@inline] sin t : t = of_float (F.sin (to_float t))
let[@inline] tan t : t = of_float (F.tan (to_float t))
let[@inline] acos t : t = of_float (F.acos (to_float t))
let[@inline] asin t : t = of_float (F.asin (to_float t))
let[@inline] atan t : t = of_float (F.atan (to_float t))
let[@inline] atan2 t1 t2 : t = of_float (F.atan2 (to_float t1) (to_float t2))
let[@inline] hypot t1 t2 : t = of_float (F.hypot (to_float t1) (to_float t2))
let[@inline] cosh t : t = of_float (F.cosh (to_float t))
let[@inline] sinh t : t = of_float (F.sinh (to_float t))
let[@inline] tanh t : t = of_float (F.tanh (to_float t))
let[@inline] acosh t : t = of_float (F.acosh (to_float t))
let[@inline] asinh t : t = of_float (F.asinh (to_float t))
let[@inline] atanh t : t = of_float (F.atanh (to_float t))
let[@inline] sqrt t : t = of_float (F.sqrt (to_float t))
let[@inline] exp t : t = of_float (F.exp (to_float t))
let[@inline] log t : t = of_float (F.log (to_float t))

module Class = F.Class

let[@inline] classify t : Class.t = (F.classify [@inlined hint]) (to_float t)

let[@inline] sign t : Base.Sign.t =
  (F.sign [@inlined hint]) (to_float t) [@alert "-deprecated"]
;;

let[@inline] sign_exn t : Base.Sign.t = (F.sign_exn [@inlined hint]) (to_float t)

let[@inline] sign_or_nan t : Base.Sign_or_nan.t =
  (F.sign_or_nan [@inlined hint]) (to_float t)
;;

let[@inline] create_ieee_exn ~negative ~exponent ~mantissa : t =
  of_float ((F.create_ieee_exn [@inlined hint]) ~negative ~exponent ~mantissa)
;;

let[@inline] ieee_negative t : bool = (F.ieee_negative [@inlined hint]) (to_float t)
let[@inline] ieee_exponent t : int = (F.ieee_exponent [@inlined hint]) (to_float t)

let[@inline] ieee_mantissa t : Base.Int63.t =
  (F.ieee_mantissa [@inlined hint]) (to_float t)
;;

external box_int64 : int64# -> (int64[@local_opt]) @@ portable = "%box_int64"
external unbox_int64 : (int64[@local_opt]) -> int64# @@ portable = "%unbox_int64"

let[@inline] to_bits x = Base.Int64.bits_of_float (to_float x) |> unbox_int64
let[@inline] of_bits x = Base.Int64.float_of_bits (box_int64 x) |> of_float

let[@inline] [@zero_alloc] select b (ifso : t) (ifnot : t) : t =
  select_int64 b (to_bits ifso) (to_bits ifnot) |> of_bits
;;

let[@inline] [@zero_alloc] first_non_nan (x : t) (y : t) : t = select (is_nan x) y x

(* Both of these implementations are the same up to direction.

   [if (x < y) then x else y] is the correct min, except that if either is nan we get y
   (because comparisons with nan are false). If y is nan, that's correct. If x is nan, we
   need to pick it, so explicitly test that.

   Implement that branchlessly with [select]. The only weird thing here is that we want to
   do two selects, but not pass back and forth to [int64] twice, so we write out the
   selects in int64-space.
*)

let[@inline] [@zero_alloc] min (x : t) (y : t) : t =
  let xb = to_bits x in
  let yb = to_bits y in
  let minb = select_int64 (x < y) xb yb in
  select_int64 (is_nan x) xb minb |> of_bits
;;

let[@inline] [@zero_alloc] max (x : t) (y : t) : t =
  let xb = to_bits x in
  let yb = to_bits y in
  let maxb = select_int64 (x > y) xb yb in
  select_int64 (is_nan x) xb maxb |> of_bits
;;

module type Array = sig
  type elt : float64
  type t

  val get : local_ t -> int -> elt [@@zero_alloc]
  val set : local_ t -> int -> elt -> unit [@@zero_alloc]
  val unsafe_get : local_ t -> int -> elt [@@zero_alloc]
  val unsafe_set : local_ t -> int -> elt -> unit [@@zero_alloc]
  val create : len:int -> elt -> t
  val length : local_ t -> int

  val unsafe_blit
    :  src:local_ t
    -> src_pos:int
    -> dst:local_ t
    -> dst_pos:int
    -> len:int
    -> unit

  val compare : t -> t -> int
  val copy : t -> t
  val t_of_sexp : Core.Sexp.t -> t @@ portable
  val sexp_of_t : t -> Core.Sexp.t @@ portable
  val custom_sexp_of_t : (elt -> Core.Sexp.t) -> t -> Core.Sexp.t
  val custom_t_of_sexp : (Core.Sexp.t -> elt) -> Core.Sexp.t -> t
  val init : int -> f:(int -> elt) -> t
  val iter : t -> f:(elt -> unit) -> unit
  val iteri : t -> f:(int -> elt -> unit) -> unit

  val%template to_float_u_array : t @ m -> elt array @ m [@@mode m = (local, global)]
  val%template of_float_u_array : elt array @ m -> t @ m [@@mode m = (local, global)]
end

module Array = struct
  type t = Float_array.t [@@deriving bin_io]

  let[@zero_alloc assume_unless_opt] get a i : float# = of_float (FA.get a i)
  let[@zero_alloc assume_unless_opt] set a i t : unit = FA.set a i (to_float t)

  let[@zero_alloc assume_unless_opt] unsafe_get a i : float# =
    of_float (FA.unsafe_get a i)
  ;;

  let[@zero_alloc assume_unless_opt] unsafe_set a i t : unit =
    FA.unsafe_set a i (to_float t)
  ;;

  let create ~len t : t = FA.create ~len (to_float t)
  let init len ~f = (FA.init [@inlined hint]) len ~f:(fun [@inline] i -> f i |> to_float)
  let length = FA.length
  let copy = FA.copy
  let unsafe_blit = FA.unsafe_blit
  let t_of_sexp = FA.t_of_sexp
  let sexp_of_t = FA.sexp_of_t
  let compare = FA.compare

  (* The use of %identity, as opposed to %obj_magic, is safe because the internal type
     system used by the compiler's middle-end treats these two types identically (this is
     a stronger requirement than just that they have the same runtime representation). *)
  external to_float_u_array_external
    :  (t[@local_opt])
    -> (float# array[@local_opt])
    @@ portable
    = "%identity"

  external of_float_u_array_external
    :  (float# array[@local_opt])
    -> (t[@local_opt])
    @@ portable
    = "%identity"

  let%expect_test "[Float_array.t] is the same as [float# array], so [%identity] is safe" =
    let tag_float_array = Obj.tag (Obj.repr (Float_array.create ~len:1 1.)) in
    let tag_float_u_array = Obj.tag (Obj.repr [| #1. |]) in
    assert (Int.equal tag_float_array tag_float_u_array);
    print_endline (Int.to_string tag_float_array);
    [%expect {| 254 |}]
  ;;

  let%template to_float_u_array = to_float_u_array_external [@@mode m = (local, global)]
  let%template of_float_u_array = of_float_u_array_external [@@mode m = (local, global)]

  let custom_sexp_of_t sexp_of_a t =
    let sexp_of_a a = sexp_of_a (of_float a) in
    FA.custom_sexp_of_t sexp_of_a t
  ;;

  let custom_t_of_sexp a_of_sexp t =
    let a_of_sexp sexp = to_float (a_of_sexp sexp) in
    FA.custom_t_of_sexp a_of_sexp t
  ;;

  let iter t ~f = (FA.iter [@inlined hint]) t ~f:(fun [@inline] x -> f (of_float x))
  let iteri t ~f = (FA.iteri [@inlined hint]) t ~f:(fun [@inline] i x -> f i (of_float x))

  module Permissioned = struct
    (* Note: Perms are from in [Core] so this [Permissioned] module will need to be placed
       into the Core part when this gets added to base/core. *)
    type -'perms t = 'perms Float_array.Permissioned.t

    let[@zero_alloc assume_unless_opt] get a i : float# =
      of_float (FA.Permissioned.get a i)
    ;;

    let[@zero_alloc assume_unless_opt] set a i t : unit =
      FA.Permissioned.set a i (to_float t)
    ;;

    let[@zero_alloc assume_unless_opt] unsafe_get a i : float# =
      of_float (FA.Permissioned.unsafe_get a i)
    ;;

    let[@zero_alloc assume_unless_opt] unsafe_set a i t : unit =
      FA.Permissioned.unsafe_set a i (to_float t)
    ;;
  end
end

module Polymorphic_array_helpers = struct
  let get a i : float# = of_float (A.get a i)
  let set a i t : unit = A.set a i (to_float t)
  let unsafe_get a i : float# = of_float (A.unsafe_get a i)
  let unsafe_set a i t : unit = A.unsafe_set a i (to_float t)
end

module type Ref = Ref_intf.T

module Ref = struct
  type nonrec t = { mutable contents : t }

  let[@inline] [@zero_alloc] get t = t.contents
  let[@inline] [@zero_alloc] set t x = t.contents <- x
  let[@inline] [@zero_alloc] add t x = set t (get t + x)
  let[@inline] create contents = { contents }
  let[@inline] [@zero_alloc] create_local contents = exclave_ { contents }
  let[@inline] create_zero () = create #0.0

  module O = struct
    let[@inline] [@zero_alloc] ref x = exclave_ create_local x
    let[@inline] [@zero_alloc] ( ! ) t = get t
    let[@inline] [@zero_alloc] ( := ) t x = set t x
    let[@inline] [@zero_alloc] ( += ) t x = t := !t + x
  end
end

let number_of_exponent_bits = 11
let number_of_mantissa_bits = 52
let sign_mask () : int64# = #0x8000_0000_0000_0000L

let%test _ =
  let shift : int = Int.add number_of_mantissa_bits number_of_exponent_bits in
  Int64_u.(sign_mask () = #1L lsl shift)
;;

include struct
  open Base_quickcheck

  let%template quickcheck_generator =
    (Generator.Via_thunk.map [@mode portable]) Generator.float ~f:(fun f () ->
      of_float (f ()))
  ;;

  let%template quickcheck_observer =
    (Observer.Via_thunk.unmap [@mode portable]) Observer.float ~f:(fun f () ->
      to_float (f ()))
  ;;

  let quickcheck_shrinker = Shrinker.atomic
end

module Option = struct
  type value = t
  type t = value

  let typerep_of_t = typerep_of_t
  let typename_of_t = typename_of_t

  (* The magic value for [none] is a signaling nan, which will cause many floating point
     operations to fail. In particular, comparisons should fail rather than return false. *)
  let none () : t = of_bits (#0x7ff0_1234_5678_90ABL : int64#)
  let%test_unit "none is nan" = [%test_eq: Base.Bool.t] (is_nan (none ())) true

  let some x =
    if is_nan x
    then (
      let sign_mask : int64# = sign_mask () in
      (* Flatten all nan values to either [nan] or [-nan]. The purpose is to prevent
         calling [some] on the magic value for [none] from accidentally returning [none],
         while preserving the sign of nan. Note that this maps all signalling nans into a
         quiet nan -- we're preserving this behavior for consistency with
         [Float63.Option]. *)
      if Int64_u.(to_bits x land sign_mask = #0L) then nan () else neg (nan ()))
    else x
  ;;

  let is_none t =
    (* We need to be careful here - the compiler is clever enough to see that [none ()] is
       nan and will try to optimize comparisons away *)
    Int64_u.equal (to_bits t) (to_bits (none ()))
  ;;

  let%test_unit "none is none" = [%test_eq: Base.Bool.t] (is_none (none ())) true
  let is_some t = not (is_none t)

  let value_exn t =
    if is_some t
    then t
    else (
      match Core.raise_s [%message "Float_u.Option.value_exn none"] with
      | (_ : Core.Nothing.t) -> .)
  ;;

  let unchecked_value t = t
  let value t ~default = if is_some t then t else default
  let to_float_option t = if is_some t then Some (to_float t) else None
  let sexp_of_t t = Base.Option.sexp_of_t Base.Float.sexp_of_t (to_float_option t)
  let some_is_representable _ = true

  module Optional_syntax = struct
    type nonrec value = value
    type nonrec t = t

    module Optional_syntax = struct
      let is_none t = is_none t
      let unsafe_value t = unchecked_value t
    end
  end

  module Array = Array
end

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving globalize]

    include Shared_derived

    let stable_witness = Ppx_stable_witness_runtime.Stable_witness.assert_stable
  end
end
