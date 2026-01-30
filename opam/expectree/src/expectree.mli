@@ portable

open! Base

type t =
  | Leaf of string
  | Branch of string * t list
  | Split of t list

val to_string : t -> string
val sexp_to_string : ?inline_pairs:bool -> Sexp.t -> string
