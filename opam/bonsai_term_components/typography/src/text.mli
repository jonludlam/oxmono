open! Core

type 'attr t =
  { chars : Uchar.t iarray
  ; attr : 'attr
  }
[@@deriving sexp_of]

val of_string : attr:'attr -> string -> 'attr t
val to_string : _ t -> string
