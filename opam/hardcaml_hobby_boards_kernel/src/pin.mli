open Base

type t =
  { name : string
  ; loc : string
  ; iostandard : Iostandard.t
  }
[@@deriving sexp]
