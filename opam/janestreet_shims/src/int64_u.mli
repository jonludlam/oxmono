(** Shim module for [Int64_u]. Add declarations that don't exist in [Stdlib.Int64] here. *)
include module type of Stdlib.Int64

val to_int64 : t -> int64
val of_int64 : int64 -> t
