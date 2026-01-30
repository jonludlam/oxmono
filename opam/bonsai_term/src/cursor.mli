open! Core

module Kind : sig
  type t =
    | Default
    | Bar
    | Bar_blinking
    | Block
    | Block_blinking
    | Underline
    | Underline_blinking
  [@@deriving sexp_of, equal]
end

type t =
  { position : Geom.Position.t
  ; kind : Kind.t
  }
[@@deriving sexp_of, equal]

val set_cursor_position : local_ Bonsai.graph -> (t option -> unit Ui_effect.t) Bonsai.t

val register
  :  Term.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

module For_mock_tests : sig
  val register
    :  (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end
