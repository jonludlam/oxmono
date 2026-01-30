open! Core
open Bonsai_term
open Bonsai.Let_syntax

let component (local_ graph) =
  let%arr flavor = Bonsai_tui_catppuccin.flavor graph in
  let crust = Bonsai_tui_catppuccin.color ~flavor Crust in
  let text = Bonsai_tui_catppuccin.color ~flavor Text in
  fun ?(attrs = []) string ->
    View.text ~attrs:([ Attr.bg crust; Attr.fg text ] @ attrs) string
;;
