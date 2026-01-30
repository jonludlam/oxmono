open! Core
open Bonsai
open Bonsai.Let_syntax
open Bonsai_term

module Action = struct
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
  [@@deriving sexp_of]

  type inner_action =
    | Public_action of t
    | Set_offset of int
    | Single_g
    | Not_g
  [@@deriving sexp_of]
end

module Input = struct
  type t =
    { dimensions : Dimensions.t
    ; content_height : int
    }
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    { offset : int
    ; last_time_g_was_pressed : Time_ns.t option
    ; stuck_to_bottom : bool
    }
  [@@deriving sexp_of]
end

type t =
  view:View.t
  * inject:(Action.t -> unit Effect.t)
  * less_keybindings_handler:(Event.t -> unit Effect.t)
  * is_at_bottom:bool
  * stuck_to_bottom:bool

let apply_action
  ctx
  input
  ({ Model.offset; last_time_g_was_pressed; stuck_to_bottom } as model)
  action
  =
  let time_source = Bonsai.Apply_action_context.time_source ctx in
  match input with
  | Bonsai.Computation_status.Inactive -> model
  | Active { Input.dimensions = { Dimensions.height; width = _ }; content_height } ->
    (match stuck_to_bottom, action with
     | true, Action.Public_action Stick_to_bottom -> model
     | _ ->
       let time_interval = Time_ns.Span.of_sec 0.3 in
       let now = Bonsai.Time_source.now time_source in
       let max_bounded_offset = Int.max 0 (content_height - height) in
       let prev_offset = offset in
       let stuck_to_bottom =
         match action with
         | Action.Public_action Stick_to_bottom -> true
         | _ -> stuck_to_bottom
       in
       let offset =
         match action with
         | Action.Public_action (Scroll_to { bottom; top }) ->
           (* NOTE: I am unsure if defaulting to always scrolling to the top when the
              element is bigger than the viewport is totally correct... *)
           let min_visible = offset in
           let max_visible = offset + height - 1 in
           if bottom <= max_visible && top >= min_visible
           then offset
           else if bottom < max_visible
           then top
           else offset + (bottom - max_visible)
         | Public_action Up -> Int.max 0 (offset - 1)
         | Public_action Up_half_screen -> Int.max 0 (offset - (height / 2))
         | Public_action Down -> Int.min max_bounded_offset (offset + 1)
         | Public_action Down_half_screen ->
           Int.min
             max_bounded_offset
             (Int.min (content_height - 1) (offset + (height / 2)))
         | Public_action Top -> 0
         | Public_action Bottom -> max_bounded_offset
         | Public_action Stick_to_bottom -> max_bounded_offset
         | Set_offset offset -> offset
         | Not_g -> offset
         | Single_g ->
           (match last_time_g_was_pressed with
            | None -> offset
            | Some last ->
              (match Time_ns.Span.O.(Time_ns.diff now last < time_interval) with
               | true -> 0
               | false -> offset))
       in
       let stuck_to_bottom =
         match action with
         | Action.Set_offset _ ->
           (* don't re-compute [stuck_to_bottom] if the offset was set directly *)
           stuck_to_bottom
         | _ -> stuck_to_bottom && offset >= prev_offset
       in
       let last_time_g_was_pressed =
         match action with
         | Single_g ->
           (match last_time_g_was_pressed with
            | None -> Some now
            | Some last ->
              (match Time_ns.Span.O.(Time_ns.diff now last < time_interval) with
               | true -> None
               | false -> Some now))
         | Not_g -> None
         | _ -> last_time_g_was_pressed
       in
       { offset; last_time_g_was_pressed; stuck_to_bottom })
;;

let use_less_keybindings (event : Event.t) (inject : Action.inner_action -> unit Effect.t)
  =
  let%bind.Effect () =
    match event with
    | Key_press { key = ASCII 'g'; mods = [] } -> Effect.Ignore
    | _ -> inject Not_g
  in
  match event with
  | Key_press { key = ASCII 'j'; mods = [] }
  | Key_press { key = Arrow `Down; mods = [] }
  | Key_press { key = ASCII ('e' | 'E'); mods = [ Ctrl ] } -> inject (Public_action Down)
  | Key_press { key = ASCII 'd'; mods = [ Ctrl ] | [] }
  | Key_press { key = ASCII 'D'; mods = [ Ctrl ] }
  | Key_press { key = Page `Down; mods = [] } -> inject (Public_action Down_half_screen)
  | Key_press { key = ASCII 'u'; mods = [ Ctrl ] | [] }
  | Key_press { key = ASCII 'U'; mods = [ Ctrl ] }
  | Key_press { key = Page `Up; mods = [] } -> inject (Public_action Up_half_screen)
  | Key_press { key = ASCII 'k'; mods = [] }
  | Key_press { key = Arrow `Up; mods = [] }
  | Key_press { key = ASCII ('y' | 'Y'); mods = [ Ctrl ] } -> inject (Public_action Up)
  | Key_press { key = ASCII 'g'; mods = [] } -> inject Single_g
  | Key_press { key = ASCII 'G'; mods = [] } -> inject (Public_action Bottom)
  | Mouse { kind = Scroll `Down; position = _; mods = [] } ->
    Effect.all_unit (List.create ~len:5 (inject (Public_action Down)))
  | Mouse { kind = Scroll `Up; position = _; mods = [] } ->
    Effect.all_unit (List.create ~len:5 (inject (Public_action Up)))
  | _ -> Effect.Ignore
;;

let component ?(default_stuck_to_bottom = false) ~dimensions view (local_ graph) =
  let content_height =
    let%arr view in
    View.height view
  in
  let%sub { offset = offset_state; last_time_g_was_pressed = _; stuck_to_bottom }, inject =
    let input =
      let%arr content_height and dimensions in
      { Input.content_height; dimensions }
    in
    let state, inject =
      Bonsai.state_machine_with_input
        ~default_model:
          { Model.offset = 0
          ; last_time_g_was_pressed = None
          ; stuck_to_bottom = default_stuck_to_bottom
          }
        ~apply_action
        input
        graph
    in
    let%arr state and inject in
    state, inject
  in
  let offset =
    let%arr offset_state and stuck_to_bottom and content_height and dimensions in
    if stuck_to_bottom && content_height > dimensions.Dimensions.height
    then content_height - dimensions.Dimensions.height
    else offset_state
  in
  (* Keep the internal offset up to date so that scrolling _out_ of a sticky state still
     works. *)
  Bonsai.Edge.on_change
    ~trigger:`After_display
    ~equal:[%equal: int * int]
    (Bonsai.both offset_state offset)
    graph
    ~callback:
      (let%arr inject and stuck_to_bottom in
       fun (offset_state, offset) ->
         if stuck_to_bottom && not (Int.equal offset_state offset)
         then inject (Set_offset offset)
         else Effect.Ignore);
  let view =
    let%arr view and offset and content_height and dimensions in
    let view = View.crop ~t:offset view in
    let b_crop = Int.max 0 (content_height - dimensions.Dimensions.height - offset) in
    View.crop ~b:b_crop view
  in
  let less_keybindings_handler =
    let%arr inject in
    fun event -> use_less_keybindings event inject
  in
  let inject =
    let%arr inject in
    fun action -> inject (Public_action action)
  in
  let is_at_bottom =
    let%arr offset and content_height and dimensions in
    let max_bounded_offset = content_height - dimensions.Dimensions.height in
    (* If content fits entirely in viewport, we're always at bottom *)
    if max_bounded_offset <= 0 then true else offset >= max_bounded_offset
  in
  let%arr view
  and inject
  and less_keybindings_handler
  and is_at_bottom
  and stuck_to_bottom in
  ~view, ~inject, ~less_keybindings_handler, ~is_at_bottom, ~stuck_to_bottom
;;
