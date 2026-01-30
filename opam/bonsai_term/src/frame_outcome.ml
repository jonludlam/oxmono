open! Core

type 'exit t =
  | Exit of 'exit
  | Stdin_closed
  | Continue
[@@deriving sexp_of]
