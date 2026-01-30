open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Region_with_handler = struct
  type t =
    { region : Region.t
    ; on_click : unit Effect.t
    ; path : Bonsai.Path.t
    }
end

let button_tag : (Bonsai.Path.t, Region_with_handler.t list) View.Tag.t =
  View.Tag.create
    (module Bonsai.Path)
    ~transform_regions:(fun (regions_with_handlers : Region_with_handler.t list) f ->
      let%map.List { region; on_click; path } = regions_with_handlers in
      let region = f region in
      { Region_with_handler.region; on_click; path })
    ~reduce:(fun (a : Region_with_handler.t list) b -> a @ b)
;;

let add_click_handler (local_ graph) =
  let path = Bonsai.path graph in
  let%arr path in
  fun view ~on_click ->
    View.Tag.mark view ~id:button_tag ~key:path ~f:(fun region ->
      [ { Region_with_handler.on_click; region; path } ])
;;

module Captured_or_not = struct
  type t =
    | Captured
    | Ignored
end

module State_machine = struct
  module Model = struct
    type t = { last_handler_that_was_clicked : Region_with_handler.t option }

    let default = { last_handler_that_was_clicked = None }
  end

  module Action = struct
    type t =
      | Mouse of
          { kind : Event.mouse_kind
          ; position : Position.t
          ; mods : Event.Modifier.t list
          }
  end

  let find_region_that_was_clicked_at_position view position =
    let id = button_tag in
    let keys = View.Tag.keys view ~id in
    List.find_map keys ~f:(fun key ->
      let%bind.Option regions = View.Tag.find view ~id key in
      List.find_map regions ~f:(fun region_with_handler ->
        match Region.contains region_with_handler.region position with
        | false -> None
        | true -> Some region_with_handler))
  ;;

  let apply_action
    ctx
    (input : View.t Bonsai.Computation_status.t)
    (model : Model.t)
    (action : Action.t)
    =
    match action with
    | Mouse { kind = Left; position; mods = [] } ->
      (match input with
       | Inactive -> Model.default, Captured_or_not.Captured
       | Active view ->
         let last_handler_that_was_clicked =
           find_region_that_was_clicked_at_position view position
         in
         { Model.last_handler_that_was_clicked }, Captured)
    | Mouse { kind = Release; position; mods = [] } ->
      (match input with
       | Inactive -> Model.default, Captured
       | Active view ->
         (match model.last_handler_that_was_clicked with
          | None -> Model.default, Captured
          | Some
              { region
              ; on_click =
                  (* NOTE: We ignore the on_click that happend during the mouse_down
                     event, and instead only use the effect at "mouse-up" time. *)
                  _
              ; path
              } ->
            let current_click = find_region_that_was_clicked_at_position view position in
            (match current_click with
             | None -> Model.default, Captured
             | Some current_mouse_up ->
               let same_region =
                 [%equal: Region.t * Bonsai.Path.t]
                   (current_mouse_up.region, current_mouse_up.path)
                   (region, path)
               in
               (match same_region with
                | false -> Model.default, Captured
                | true ->
                  Bonsai.Apply_action_context.schedule_event ctx current_mouse_up.on_click;
                  Model.default, Captured))))
    | Mouse { kind = Drag; position = _; mods = [] } -> model, Ignored
    | Mouse { kind = Scroll _ | Middle | Right; position = _; mods = _ } ->
      Model.default, Ignored
    | Mouse { kind = Drag; position = _; mods = _ :: _ } -> Model.default, Ignored
    | Mouse { kind = Left | Release; position = _; mods = _ :: _ } ->
      Model.default, Ignored
  ;;

  let component (view : View.t Bonsai.t) (local_ graph) =
    let _model, inject =
      Bonsai.actor_with_input ~default_model:Model.default ~recv:apply_action view graph
    in
    inject
  ;;
end

let handler' ~view (local_ graph) =
  let inject = State_machine.component view graph in
  let%arr inject in
  fun ~kind ~position ~mods -> inject (Mouse { kind; position; mods })
;;

let handler ~view ~handler:default_handler (local_ graph) =
  let handle_click_event = handler' ~view graph in
  let%arr default_handler and handle_click_event in
  fun (event : Event.t) ->
    match event with
    | Mouse { kind; position; mods } ->
      (match%bind.Effect handle_click_event ~kind ~position ~mods with
       | Captured -> Effect.Ignore
       | Ignored -> default_handler event)
    | event -> default_handler event
;;
