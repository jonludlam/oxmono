open! Core
open Bonsai_term

(** This is a tiny library that implements [on_click] handlers in bonsai_term.

    There is __no__ event bubbling, so when there are on_clicks attached on parent-child
    overlapping elements which handler will execute will be undefined behavior. Please
    only attach these to children-most, non-overlapping views. *)

(** [on_click ~on_click view] will trigger [on_click] if there are consecutive Left ->
    Release mouse events on [view].

    NOTE: In order for this library to do anything, you need to make a call to [handler]
    at the top-level of your app:

    e.g.:

    {[
      let ~view, ~handler = ... in
      let handler = Aide_cli_click_handler.handler ~view ~handler graph in
      ~view, ~handler
    ]} *)
val add_click_handler
  :  local_ Bonsai.graph
  -> (View.t -> on_click:unit Effect.t -> View.t) Bonsai.t

(** In order for this library to work, you must add a call to [handler] at the top level
    of your app like:

    {[
      let ~view, ~handler = ... in
      let handler = Bonsai_tui_click_handler.handler ~view ~handler graph in
      ~view, ~handler
    ]} *)
val handler
  :  view:View.t Bonsai.t
  -> handler:(Event.t -> unit Effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> (Event.t -> unit Effect.t) Bonsai.t
