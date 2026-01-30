open Base

type t =
  | LVCMOS33
  | LVCMOS18
[@@deriving sexp]

val to_string : t -> string
val of_string : string -> t
