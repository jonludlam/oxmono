open! Core

(** For records not consisting purely of floats, fields of [t] are preferable to mutable
    fields of [Packed_float_option.t] because their value can be mutated without crossing
    the GC write barrier.

    This is because float storage in a record of mixed types must be boxed, so changing a
    mutable [Packed_float_option.t] requires a [caml_modify] call to point the mutable
    field to a different float. In contrast, a [t] stores an unboxed float which may be
    modified directly. *)

type t [@@deriving bin_io, sexp]

val create : Packed_float_option0.t -> t
val get : t -> Packed_float_option0.t
val set : t -> Packed_float_option0.t -> unit
