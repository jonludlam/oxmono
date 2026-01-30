(** Shim module for [Float_u]. Add declarations that don't exist in [Stdlib.Float] here. *)
include module type of Stdlib.Float

val to_float : t -> float
val of_float : float -> t
