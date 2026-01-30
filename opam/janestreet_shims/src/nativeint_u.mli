(** Shim module for [Nativeint_u]. Add declarations that don't exist in [Stdlib.Nativeint]
    here. *)
include module type of Stdlib.Nativeint

val to_nativeint : t -> nativeint
val of_nativeint : nativeint -> t
