open! Core
open Bonsai_term
open Bonsai

(** This library contains hard-coded constants for Catppuccin colors:
    https://github.com/catppuccin/catppuccin *)

type t =
  | Rosewater
  | Flamingo
  | Pink
  | Mauve
  | Red
  | Maroon
  | Peach
  | Yellow
  | Green
  | Teal
  | Sky
  | Sapphire
  | Blue
  | Lavender
  | Text
  | Subtext1
  | Subtext0
  | Overlay2
  | Overlay1
  | Overlay0
  | Surface2
  | Surface1
  | Surface0
  | Base
  | Mantle
  | Crust
[@@deriving sexp, equal, enumerate, compare]

include Comparable.S_plain with type t := t

module Flavor : sig
  type t =
    | Mocha
    | Macchiato
    | Frappe
    | Latte
  [@@deriving sexp ~portable, equal ~portable, enumerate, compare ~portable]
end

val color : flavor:Flavor.t -> t -> Attr.Color.t
val color' : t Bonsai.t -> local_ Bonsai.graph -> Attr.Color.t Bonsai.t
val flavor : local_ Bonsai.graph -> Flavor.t Bonsai.t

val set_flavor_within
  :  Flavor.t Bonsai.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

(** A version of [set_flavor_within] that is specialized for the common "application" type *)
val set_flavor_within_app
  :  Flavor.t Bonsai.t
  -> (local_ Bonsai.graph -> view:View.t Bonsai.t * handler:'b Bonsai.t)
  -> local_ Bonsai.graph
  -> view:View.t Bonsai.t * handler:'b Bonsai.t

module Rgb : sig
  type t =
    { r : int
    ; b : int
    ; g : int
    }
  [@@deriving globalize]
end

val rgb : Flavor.t -> t -> Rgb.t
