open! Core
open Bonsai_term

module Action : sig
  type t =
    | Scroll_to of
        { bottom : int
        ; top : int
        }
    | Up
    | Down
    | Top
    | Bottom
    | Up_half_screen
    | Down_half_screen
    | Stick_to_bottom
end

type t =
  view:View.t
  * inject:(Action.t -> unit Effect.t)
  * less_keybindings_handler:(Event.t -> unit Effect.t)
  * is_at_bottom:bool
  * stuck_to_bottom:bool
(* [less_keybindings] will provide "less-like" keybindings. The keybindings are:

   [ up arrow ] or [ k ] -> Up [ down arrow ] or [ j ] -> Down [ d ] or [ ctrl + d ] ->
   Down_half_screen [ u ] or [ ctrl + u ] -> Up_half_screen [ gg ] -> Top [ G ] -> Bottom *)

(** [component ~dimensions view], will make a region of size [dimensions] containing
    [view]. If [view] is vertically bigger than [dimensions.height] then the region will
    be "scrollable". You can scroll by scheduling [inject] actions.

    A default "handler" for events with less-like navigation is provided as a helper
    utility. *)
val component
  :  ?default_stuck_to_bottom:bool
  -> dimensions:Dimensions.t Bonsai.t
  -> View.t Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t
