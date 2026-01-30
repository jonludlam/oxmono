open! Core

(** [Captured_or_ignored.t] is a tiny utility type meant to help libraries and apps
    represent "event capturing".

    - [ Ignored  ] represents a handler ignoring an event / it being a no-op.
    - [ Captured ] represents a handler that "used" the event. *)

type t =
  | Captured
  | Ignored
[@@deriving sexp_of]
