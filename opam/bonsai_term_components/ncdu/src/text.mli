open! Core
open Bonsai_term

val component
  :  local_ Bonsai_term.Bonsai.graph
  -> (?attrs:Attr.t list -> string -> View.t) Bonsai_term.Bonsai.t
