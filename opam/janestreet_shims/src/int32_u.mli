(** Shim module for [Int32_u]. Add declarations that don't exist in [Stdlib.Int32] here. *)
include module type of Stdlib.Int32

val to_int32 : t -> int32
val of_int32 : int32 -> t
