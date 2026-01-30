open! Core
open Bonsai_term
open Bonsai.Let_syntax
open Zed

let cursor_tag : (Bonsai.Path.t, Region.t) View.Tag.t =
  View.Tag.create
    (module Bonsai.Path)
    ~reduce:(fun _ t -> t)
    ~transform_regions:(fun region f -> f region)
;;

module Action = struct
  type t =
    | Insert of string
    | Newline
    | Next_char
    | Prev_char
    | Next_line
    | Prev_line
    | Goto_bol
    | Goto_eol
    | Goto_bot
    | Goto_eot
    | Delete_next_char
    | Delete_prev_char
    | Delete_next_line
    | Delete_prev_line
    | Kill_next_char
    | Kill_next_line
    | Kill_prev_line
    | Next_word
    | Prev_word
    | Delete_next_word
    | Delete_prev_word
    | Kill_next_word
    | Kill_prev_word
    | Yank
    | Undo
    | Replace_char of string
    | Prev_char_in_line
  [@@deriving sexp_of]

  let to_zed_action
    :  t
    -> [ `Forward_to_zed of Zed_edit.action
       | `Custom of
         [ `prev_line
         | `next_line
         | `replace_char of string
         | `kill_next_char
         | `prev_char_in_line
         ]
       ]
    = function
    | Insert s -> `Forward_to_zed (`insert s)
    | Newline -> `Forward_to_zed `newline
    | Next_char -> `Forward_to_zed `next_char
    | Prev_char -> `Forward_to_zed `prev_char
    | Next_line -> `Custom `next_line
    | Prev_line -> `Custom `prev_line
    | Goto_bol -> `Forward_to_zed `goto_bol
    | Goto_eol -> `Forward_to_zed `goto_eol
    | Goto_bot -> `Forward_to_zed `goto_bot
    | Goto_eot -> `Forward_to_zed `goto_eot
    | Delete_next_char -> `Forward_to_zed `delete_next_char
    | Delete_prev_char -> `Forward_to_zed `delete_prev_char
    | Delete_next_line -> `Forward_to_zed `delete_next_line
    | Delete_prev_line -> `Forward_to_zed `delete_prev_line
    | Kill_next_char -> `Custom `kill_next_char
    | Kill_next_line -> `Forward_to_zed `kill_next_line
    | Kill_prev_line -> `Forward_to_zed `kill_prev_line
    | Next_word -> `Forward_to_zed `next_word
    | Prev_word -> `Forward_to_zed `prev_word
    | Delete_next_word -> `Forward_to_zed `delete_next_word
    | Delete_prev_word -> `Forward_to_zed `delete_prev_word
    | Kill_next_word -> `Forward_to_zed `kill_next_word
    | Kill_prev_word -> `Forward_to_zed `kill_prev_word
    | Yank -> `Forward_to_zed `yank
    | Undo -> `Forward_to_zed `undo
    | Replace_char s -> `Custom (`replace_char s)
    | Prev_char_in_line -> `Custom `prev_char_in_line
  ;;
end

module Cursor = struct
  type t =
    { logical_line : int
    ; logical_column : int
    ; visual_column : int (* Some characters (e.g. some emojis) will be twice as wide! *)
    ; visual_line : int (* Visual line number considering wrapped lines *)
    ; position : int
    }
  [@@deriving sexp_of]
end

let maximum_character_size = 5

let wrap_text ~text ~max_width : ([ `Not_wrapped | `Wrapped ] * string) list =
  let lines = String.split ~on:'\n' text in
  let max_width =
    (* NOTE: Some characters are bigger than 1 char visually, so [width = 1] could result
       in a forever loop. Setting it to a minimum [max_width] of 5 makes it so that every
       character is guaranteed to visually fit. *)
    Int.max max_width maximum_character_size
  in
  let wrap_line line =
    if max_width <= 0
    then [ line ]
    else if String.is_empty line
    then [ "" ] (* Preserve empty lines *)
    else (
      let utf8_chars = String.Utf8.to_list (String.Utf8.of_string line) in
      let rec wrap_chars ~chars ~current_line ~current_width ~acc =
        match chars with
        | [] ->
          (* Always include the current line, even if empty (though it shouldn't be empty
             here) *)
          List.rev (current_line :: acc)
        | char :: rest ->
          let char_width = Int.min maximum_character_size (View.uchar_tty_width char) in
          let char_str = String.Utf8.of_list [ char ] |> String.Utf8.to_string in
          if current_width + char_width <= max_width
          then
            wrap_chars
              ~chars:rest
              ~current_line:(current_line ^ char_str)
              ~current_width:(current_width + char_width)
              ~acc
          else if String.is_empty current_line
          then
            (* Character is too wide for the line, but we must include it to avoid
               infinite loop *)
            wrap_chars
              ~chars:rest
              ~current_line:char_str
              ~current_width:char_width
              ~acc:(current_line :: acc)
          else
            wrap_chars ~chars ~current_line:"" ~current_width:0 ~acc:(current_line :: acc)
      in
      wrap_chars ~chars:utf8_chars ~current_line:"" ~current_width:0 ~acc:[])
  in
  List.concat_map lines ~f:(fun line ->
    let sub_lines = wrap_line line in
    match sub_lines with
    | [] -> []
    | first :: rem -> (`Not_wrapped, first) :: List.map rem ~f:(fun x -> `Wrapped, x))
;;

let calculate_visual_position ~text ~logical_line ~logical_column ~max_width =
  let max_width = Int.max maximum_character_size max_width in
  let lines = String.split ~on:'\n' text in
  if logical_line >= List.length lines
  then
    ( ~visual_line:logical_line
    , ~visual_column:logical_column (* Fallback to logical position *) )
  else (
    (* First, count the visual lines from all previous logical lines *)
    let visual_lines_before =
      List.take lines logical_line
      |> List.sum (module Int) ~f:(fun line_text ->
        List.length (wrap_text ~text:line_text ~max_width))
    in
    (* Now find the position within the current logical line *)
    let line_text = List.nth_exn lines logical_line in
    let utf8_chars = String.Utf8.to_list (String.Utf8.of_string line_text) in
    let chars_before_cursor = List.take utf8_chars logical_column in
    let rec find_visual_line ~chars_remaining ~current_line_chars ~visual_line_offset =
      match chars_remaining with
      | [] ->
        (* Cursor is at the end of this logical line *)
        let visual_column =
          List.sum (module Int) ~f:View.uchar_tty_width current_line_chars
        in
        let visual_line = visual_lines_before + visual_line_offset in
        if visual_column >= max_width
        then ~visual_line:(visual_line + 1), ~visual_column:0
        else ~visual_line, ~visual_column
      | char :: rest ->
        let char_width = View.uchar_tty_width char in
        let current_width =
          List.sum (module Int) ~f:View.uchar_tty_width current_line_chars
        in
        if current_width + char_width <= max_width
        then
          find_visual_line
            ~chars_remaining:rest
            ~current_line_chars:(current_line_chars @ [ char ])
            ~visual_line_offset
        else
          (* Move to next visual line *)
          find_visual_line
            ~chars_remaining
            ~current_line_chars:[]
            ~visual_line_offset:(visual_line_offset + 1)
    in
    find_visual_line
      ~chars_remaining:chars_before_cursor
      ~current_line_chars:[]
      ~visual_line_offset:0)
;;

type t =
  { text : string Bonsai.t
  ; send_actions : (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
  ; view : View.t Bonsai.t
  ; cursor : Cursor.t Bonsai.t
  ; set_text : (string -> unit Effect.t) Bonsai.t
  ; rope : Zed.Zed_rope.t Bonsai.t
  ; get_cursor_position : (View.t -> Position.t option) Bonsai.t
  }

let compute_text ~zed_context =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let text = Zed_rope.to_string rope in
  ~text, ~rope
;;

let compute_cursor ~zed_context ~text ~width =
  let cursor = Zed_edit.cursor zed_context in
  let position = Zed_edit.position zed_context in
  let logical_line = Zed_cursor.get_line cursor
  and logical_column = Zed_cursor.get_column cursor in
  let ~visual_line, ~visual_column =
    calculate_visual_position ~text ~logical_line ~logical_column ~max_width:width
  in
  let cursor =
    { Cursor.logical_line; logical_column; visual_column; visual_line; position }
  in
  cursor
;;

let utf8_chars_of_string s = String.Utf8.to_list (String.Utf8.of_string s)

let count_chars_before_width ~width:visual_width utf_chars =
  List.fold_until
    utf_chars
    ~finish:(fun (~width:_, ~count) -> count)
    ~init:(~width:0, ~count:0)
    ~f:(fun (~width, ~count) a ->
      match width >= visual_width with
      | true -> Stop count
      | false ->
        let width = width + View.uchar_tty_width a in
        Continue (~width, ~count:(count + 1)))
;;

let prev_line ~ideal_visual_column ~zed_context ~width =
  (* NOTE: We are repeatedly computing [compute_text] - consider de-duplicating this
     compoutation. This is also very much not optimized at all. The priority here is a
     nice UX. *)
  let ~text, .. = compute_text ~zed_context in
  let%tydi { visual_line; visual_column; logical_column; logical_line = _; _ } =
    compute_cursor ~zed_context ~text ~width
  in
  let is_at_first_line = visual_line = 0 in
  match is_at_first_line with
  | true -> Zed_edit.prev_line zed_context
  | false ->
    let wrapped_lines = wrap_text ~text ~max_width:width in
    (match List.nth wrapped_lines visual_line with
     | None ->
       if visual_column = 0
       then (
         (* NOTE: We are in the "fake" last virtual line right after a wrapped line. *)
         match List.nth wrapped_lines (visual_line - 1) with
         | None -> Zed_edit.prev_line zed_context
         | Some (_, prev_line) ->
           Zed_edit.move
             zed_context
             ~set_wanted_column:false
             (-List.length (utf8_chars_of_string prev_line)))
       else Zed_edit.prev_line zed_context
     | Some (_, current_line) ->
       (* NOTE: We have a 0 check previously, so this should always be true. *)
       assert (visual_line >= 1);
       let _, prev_line = List.nth_exn wrapped_lines (visual_line - 1) in
       let prev_line_chars = utf8_chars_of_string prev_line in
       let current_line_chars_before_cursor =
         count_chars_before_width ~width:visual_column (utf8_chars_of_string current_line)
       in
       let num_chars_on_prev_line_before_target =
         count_chars_before_width ~width:ideal_visual_column prev_line_chars
       in
       let is_wrapped = logical_column <> visual_column in
       let delta =
         List.length prev_line_chars
         - num_chars_on_prev_line_before_target
         + current_line_chars_before_cursor
         + if is_wrapped then 0 else 1
       in
       let delta = -delta in
       Zed_edit.move zed_context ~set_wanted_column:false delta)
;;

let next_line ~ideal_visual_column ~zed_context ~width =
  (* NOTE: We are repeatedly computing [compute_text] - consider de-duplicating this
     compoutation. This is also very much not optimized at all. The priority here is a
     nice UX. *)
  let ~text, .. = compute_text ~zed_context in
  let%tydi { visual_line; visual_column; logical_column = _; logical_line = _; _ } =
    compute_cursor ~zed_context ~text ~width
  in
  let wrapped_lines = wrap_text ~text ~max_width:width in
  match List.nth wrapped_lines visual_line with
  | None -> Zed_edit.next_line zed_context
  | Some (_, current_line) ->
    let current_line_chars = utf8_chars_of_string current_line in
    (match List.nth wrapped_lines (visual_line + 1) with
     | None -> Zed_edit.next_line zed_context
     | Some (next_line_is_wrapped, next_line) ->
       let next_line_chars = utf8_chars_of_string next_line in
       let current_line_chars_before_cursor =
         count_chars_before_width ~width:visual_column current_line_chars
       in
       let num_chars_on_next_line_before_target =
         count_chars_before_width ~width:ideal_visual_column next_line_chars
       in
       let next_line_is_wrapped =
         match next_line_is_wrapped with
         | `Wrapped -> true
         | `Not_wrapped -> false
       in
       let delta =
         List.length current_line_chars
         - current_line_chars_before_cursor
         + num_chars_on_next_line_before_target
         + if next_line_is_wrapped then 0 else 1
       in
       Zed_edit.move zed_context ~set_wanted_column:false delta)
;;

module State_manager = struct
  module Model = struct
    type t =
      { update_count : int
      ; ideal_visual_column : int
      ; scroll_offset : int
      }
  end

  module Input = struct
    type t =
      { zed_context : unit Zed_edit.context
      ; width : int
      ; max_height : int
      }
  end

  module Editor_action = Action

  module Action = struct
    type t =
      | Send_actions of Action.t Nonempty_list.t
      | Set_text of string
  end

  let apply_action
    _
    (input : Input.t Bonsai.Computation_status.t)
    (model : Model.t)
    (action : Action.t)
    =
    let update_count = succ model.update_count in
    let get_visual_lines () =
      match input with
      | Inactive -> None
      | Active { zed_context; width; _ } ->
        let ~text, .. = compute_text ~zed_context in
        Some (wrap_text ~text ~max_width:width)
    in
    let lines_before = get_visual_lines () in
    let () =
      match action with
      | Send_actions actions ->
        let apply_action action =
          match input with
          | Inactive -> ()
          | Active { zed_context; width; max_height = _ } ->
            (match Editor_action.to_zed_action action with
             | `Forward_to_zed action -> (Zed.Zed_edit.get_action action) zed_context
             | `Custom `prev_line ->
               prev_line
                 ~ideal_visual_column:model.ideal_visual_column
                 ~width
                 ~zed_context
             | `Custom `next_line ->
               next_line
                 ~ideal_visual_column:model.ideal_visual_column
                 ~width
                 ~zed_context
             | `Custom `kill_next_char ->
               (Zed.Zed_edit.get_action `set_mark) zed_context;
               (Zed.Zed_edit.get_action `next_char) zed_context;
               (Zed.Zed_edit.get_action `kill) zed_context
             | `Custom (`replace_char s) ->
               (Zed.Zed_edit.get_action `delete_next_char) zed_context;
               (Zed.Zed_edit.get_action (`insert s)) zed_context;
               (Zed.Zed_edit.get_action `prev_char) zed_context
             | `Custom `prev_char_in_line ->
               if Zed_edit.at_bol zed_context
               then ()
               else (Zed.Zed_edit.get_action `prev_char) zed_context)
        in
        Nonempty_list.iter actions ~f:apply_action
      | Set_text text ->
        (match input with
         | Inactive -> ()
         | Active { zed_context; width = _; max_height = _ } ->
           let rope = Zed.Zed_rope.of_string text in
           Zed.Zed_edit.set_text_and_forget_history zed_context rope)
    in
    let ideal_visual_column =
      let should_update_ideal_visual_column =
        (* We do not want to update the visual column if for [prev_line/next_line] events. *)
        match action with
        | Set_text _ -> `Update_ideal_column
        | Send_actions actions ->
          (match
             Nonempty_list.for_all actions ~f:(function
               | Prev_line | Next_line -> true
               | _ -> false)
           with
           | true -> `Keep_ideal_column
           | false -> `Update_ideal_column)
      in
      match should_update_ideal_visual_column with
      | `Keep_ideal_column -> model.ideal_visual_column
      | `Update_ideal_column ->
        (match input with
         | Inactive -> model.ideal_visual_column
         | Active { zed_context; width; max_height = _ } ->
           let%tydi { visual_column; _ } =
             let ~text, .. = compute_text ~zed_context in
             compute_cursor ~zed_context ~text ~width
           in
           visual_column)
    in
    let scroll_offset =
      match input with
      | Inactive -> model.scroll_offset
      | Active { zed_context; width; max_height } ->
        let ~text, .. = compute_text ~zed_context in
        let%tydi { visual_line; _ } = compute_cursor ~zed_context ~text ~width in
        let current_offset = model.scroll_offset in
        let visible_top = current_offset in
        let visible_bottom = current_offset + max_height - 1 in
        (* Check if cursor is out of view and adjust scroll offset *)
        if visual_line < visible_top
        then (* Cursor is above visible area, scroll up *)
          Int.max 0 visual_line
        else if visual_line > visible_bottom
        then
          (* Cursor is below visible area, scroll down *)
          Int.max 0 (visual_line - max_height + 1)
        else (
          (* Cursor is still in view, don't change offset *)
          match current_offset with
          | 0 -> current_offset
          | _ ->
            (match
               let%map.Option lines_after = get_visual_lines ()
               and lines_before
               and max_height =
                 match input with
                 | Inactive -> None
                 | Active { max_height; _ } -> Some max_height
               in
               let num_lines_before = List.length lines_before
               and num_lines_after = List.length lines_after in
               let diff =
                 match num_lines_after > max_height with
                 | false -> 0
                 | true -> Int.max 0 (num_lines_before - num_lines_after)
               in
               diff
             with
             | None -> current_offset
             | Some diff -> current_offset - diff))
    in
    { Model.update_count; ideal_visual_column; scroll_offset }
  ;;
end

let component ?match_word ?undo_size ~text_attrs ~width ~max_height (local_ graph) =
  let path_id = Bonsai.path graph in
  let width =
    let%arr width in
    Int.max width maximum_character_size
  in
  let max_height =
    let%arr max_height in
    Int.max 1 max_height
  in
  let zed_context =
    Bonsai.Expert.thunk
      ~f:(fun () ->
        let edit = Zed_edit.create ?match_word ?undo_size () in
        let cursor = Zed_edit.new_cursor edit in
        Zed_edit.context edit cursor)
      graph
  in
  let model, inject =
    Bonsai.state_machine_with_input
      ~default_model:
        { State_manager.Model.update_count = 0
        ; ideal_visual_column = 0
        ; scroll_offset = 0
        }
      ~apply_action:State_manager.apply_action
      (let%arr zed_context and width and max_height in
       { State_manager.Input.zed_context; width; max_height })
      graph
  in
  let%sub { update_count = count; ideal_visual_column = _; scroll_offset } = model in
  let send_actions =
    let%arr inject in
    fun actions -> inject (Send_actions actions)
  in
  let set_text =
    let%arr inject in
    fun text -> inject (Set_text text)
  in
  let%sub ~text, ~rope =
    let%arr zed_context and count in
    let _ = count in
    compute_text ~zed_context
  in
  let cursor =
    let%arr zed_context and count and text and width in
    let _ = count in
    let cursor = compute_cursor ~zed_context ~text ~width in
    { cursor with visual_line = Int.max 0 cursor.visual_line }
  in
  let full_view =
    let full_view =
      let%arr text and text_attrs and width in
      let wrapped_lines = wrap_text ~text ~max_width:width in
      View.vcat
      @@ List.map ~f:(fun (_, line) -> View.text ~attrs:text_attrs line)
      @@ wrapped_lines
    in
    let%arr full_view
    and { visual_column; visual_line; _ } = cursor
    and path_id in
    let cursor = View.transparent_rectangle ~width:1 ~height:1 in
    let cursor = View.Tag.mark cursor ~id:cursor_tag ~key:path_id ~f:Fn.id in
    let view = View.zcat [ View.pad ~l:visual_column ~t:visual_line cursor; full_view ] in
    view
  in
  let view =
    let%arr scroll_offset and full_view in
    View.crop ~t:scroll_offset full_view
  in
  let view =
    let%arr view and max_height in
    let too_big_by = Int.max 0 (View.height view - max_height) in
    View.crop ~b:too_big_by view
  in
  let get_cursor_position =
    let%arr path_id in
    fun view ->
      let%map.Option { x; y; _ } = View.Tag.find view ~id:cursor_tag path_id in
      { Position.x; y }
  in
  { text; send_actions; view; cursor; set_text; rope; get_cursor_position }
;;

let default_keybindings_handler
  : (Action.t Nonempty_list.t -> unit Effect.t) -> Event.t -> unit Effect.t
  =
  fun send_actions ->
  let send_action action = send_actions [ action ] in
  let handler (event : Event.t) =
    match event with
    | Key_press { key = ASCII c; mods = [] } -> send_action (Insert (Char.to_string c))
    | Key_press { key = Uchar c; mods = [] } ->
      send_action (Insert (Uchar.Utf8.to_string c))
    | Key_press { key = Backspace; mods = [] } -> send_action Delete_prev_char
    | Key_press { key = Backspace; mods = [ Meta ] }
    | Key_press { key = Backspace; mods = [ Ctrl ] }
    | Key_press
        { key = ASCII 'W' (* NOTE: In VSCode Ctrl+Backspace is Ctrl+W. *)
        ; mods = [ Ctrl ]
        } -> send_action Delete_prev_word
    | Key_press { key = Delete; mods = [ Ctrl ] }
    | Key_press { key = ASCII 'd'; mods = [ Meta ] } -> send_action Delete_next_word
    | Key_press { key = Arrow `Left; mods = [ Ctrl ] } -> send_action Prev_word
    | Key_press { key = Arrow `Right; mods = [ Ctrl ] } -> send_action Next_word
    | Key_press { key = ASCII 'b'; mods = [ Meta ] } -> send_action Prev_word
    | Key_press { key = ASCII 'f'; mods = [ Meta ] } -> send_action Next_word
    | Key_press { key = Arrow `Left; mods = [] } -> send_action Prev_char
    | Key_press { key = Arrow `Right; mods = [] } -> send_action Next_char
    | Key_press { key = Arrow `Up; mods = [] } -> send_action Prev_line
    | Key_press { key = Arrow `Down; mods = [] } -> send_action Next_line
    | Key_press { key = Enter; mods = [] } -> send_action Newline
    | Key_press { key = ASCII 'U'; mods = [ Ctrl ] } -> send_action Delete_prev_line
    | Key_press { key = ASCII 'Z'; mods = [ Ctrl ] } -> send_action Undo
    | Key_press { key = Home; mods = [] } -> send_action Goto_bol
    | Key_press { key = End; mods = [] } -> send_action Goto_eol
    | Key_press { key = Delete; mods = [] } -> send_action Delete_next_char
    | _ -> Effect.Ignore
  in
  handler
;;

module Vim = struct
  module Mode = struct
    type t =
      | Normal
      | Insert
    [@@deriving sexp_of]
  end

  module Command_state = struct
    type t =
      | None
      | Waiting_for_second_char of char
    [@@deriving sexp_of]
  end

  module Yank_type = struct
    type t =
      | Line
      | Char
    [@@deriving sexp_of]
  end

  type t =
    { handler : (Event.t -> unit Effect.t) Bonsai.t
    ; mode : Mode.t Bonsai.t
    }

  module Model = struct
    type t =
      { mode : Mode.t
      ; command_state : Command_state.t
      ; last_yank_type : Yank_type.t
      }

    module Input = struct
      type t = { send_action : Action.t -> unit Effect.t }
    end

    let apply_action
      context
      (input : Input.t Bonsai.Computation_status.t)
      (model : t)
      (event : Event.t)
      =
      let send_action action =
        Bonsai.Apply_action_context.schedule_event
          context
          (match input with
           | Inactive -> Effect.Ignore
           | Active { send_action } -> send_action action)
      in
      let model =
        let%tydi { mode; command_state; last_yank_type } = model in
        match mode, event with
        (* Insert mode handling *)
        | Insert, Key_press { key = Escape; mods = [] } ->
          (* NOTE: This is not intuitive, but repeatedly pressing [esc,i,esc,i,esc,i] will
             gradually move your cursor backwards because pressing [esc] will put you into
             normal mode, but _also_ move your cursor backwards! *)
          send_action Prev_char_in_line;
          { mode = Normal; command_state = None; last_yank_type }
        | Insert, Key_press { key = ASCII c; mods = [] } ->
          send_action (Insert (Char.to_string c));
          model
        | Insert, Key_press { key = Uchar c; mods = [] } ->
          send_action (Insert (Uchar.Utf8.to_string c));
          model
        | Insert, Key_press { key = Backspace; mods = [] } ->
          send_action Delete_prev_char;
          model
        | Insert, Key_press { key = Enter; mods = [] } ->
          send_action Newline;
          model
        | Insert, Key_press { key = ASCII 'U'; mods = [ Ctrl ] } ->
          send_action Delete_prev_line;
          model
        | Insert, Key_press { key = ASCII 'W'; mods = [ Ctrl ] } ->
          send_action Delete_prev_word;
          model
        | Insert, Key_press { key = Backspace; mods = [ Ctrl ] } ->
          send_action Delete_prev_word;
          model
        (* Normal mode handling *)
        | Normal, Key_press { key = ASCII c; mods = [] } ->
          (match command_state, c with
           (* Movement commands *)
           | None, 'h' ->
             send_action Prev_char;
             model
           | None, 'j' ->
             send_action Next_line;
             model
           | None, 'k' ->
             send_action Prev_line;
             model
           | None, 'l' ->
             send_action Next_char;
             model
           | None, 'w' ->
             send_action Next_word;
             model
           | None, 'b' ->
             send_action Prev_word;
             model
           | None, '0' ->
             send_action Goto_bol;
             model
           | None, '$' ->
             send_action Goto_eol;
             model
           | None, 'G' ->
             send_action Goto_eot;
             model
           (* Mode switching *)
           | None, 'i' -> { model with mode = Insert }
           | None, 'I' ->
             send_action Goto_bol;
             { model with mode = Insert }
           | None, 'a' ->
             send_action Next_char;
             { model with mode = Insert }
           | None, 'A' ->
             send_action Goto_eol;
             { model with mode = Insert }
           | None, 'o' ->
             send_action Goto_eol;
             send_action Newline;
             { model with mode = Insert }
           | None, 'S' ->
             send_action Goto_bol;
             send_action Kill_next_line;
             { mode = Insert; command_state = None; last_yank_type = Line }
           | None, 'D' ->
             send_action Kill_next_line;
             { model with last_yank_type = Char }
           | None, 'C' ->
             send_action Kill_next_line;
             { mode = Insert; command_state = None; last_yank_type = Char }
           | None, 'O' ->
             send_action Goto_bol;
             send_action (Insert "\n");
             send_action Prev_line;
             { model with mode = Insert }
           (* Single character commands *)
           | None, 'x' ->
             send_action Kill_next_char;
             { model with last_yank_type = Char }
           | None, 'X' ->
             send_action Delete_prev_char;
             model
           | None, 's' ->
             send_action Delete_next_char;
             { model with mode = Insert }
           | None, 'u' ->
             send_action Undo;
             model
           | None, 'p' ->
             (match last_yank_type with
              | Line ->
                (* line-wise paste below current line *)
                send_action Goto_eol;
                send_action Newline;
                send_action Yank
              | Char ->
                (* character-wise paste after cursor, cursor on last char of pasted text *)
                send_action Next_char;
                send_action Yank;
                send_action Prev_char);
             model
           | None, 'P' ->
             (match last_yank_type with
              | Line ->
                (* line-wise paste above current line *)
                send_action Goto_bol;
                send_action Yank
              | Char ->
                (* character-wise paste at cursor, cursor on last char of pasted text *)
                send_action Yank;
                send_action Prev_char);
             model
           (* Multi-character commands - first char *)
           | None, 'd' -> { model with command_state = Waiting_for_second_char 'd' }
           | None, 'c' -> { model with command_state = Waiting_for_second_char 'c' }
           | None, 'y' -> { model with command_state = Waiting_for_second_char 'y' }
           | None, 'g' -> { model with command_state = Waiting_for_second_char 'g' }
           | None, 'r' -> { model with command_state = Waiting_for_second_char 'r' }
           (* Multi-character commands - second char *)
           | Waiting_for_second_char 'd', 'd' ->
             send_action Goto_bol;
             send_action Kill_next_line;
             { mode = Normal; command_state = None; last_yank_type = Line }
           | Waiting_for_second_char 'd', 'b' ->
             send_action Kill_prev_word;
             { mode = Normal; command_state = None; last_yank_type = Char }
           | Waiting_for_second_char 'd', '0' ->
             send_action Kill_prev_line;
             { mode = Normal; command_state = None; last_yank_type = Char }
           | Waiting_for_second_char 'd', '$' ->
             send_action Kill_next_line;
             { mode = Normal; command_state = None; last_yank_type = Char }
           | Waiting_for_second_char 'd', 'w' ->
             send_action Kill_next_word;
             { model with command_state = None }
           | Waiting_for_second_char 'c', 'e' ->
             send_action Kill_next_word;
             { mode = Insert; command_state = None; last_yank_type = Line }
           | Waiting_for_second_char 'c', 'w' ->
             send_action Kill_next_word;
             { mode = Insert; command_state = None; last_yank_type = Char }
           | Waiting_for_second_char 'c', 'b' ->
             send_action Kill_prev_word;
             { mode = Insert; command_state = None; last_yank_type = Char }
           | Waiting_for_second_char 'c', 'c' ->
             send_action Goto_bol;
             send_action Kill_next_line;
             { mode = Insert; command_state = None; last_yank_type = Line }
           | Waiting_for_second_char 'g', 'g' ->
             send_action Goto_bot;
             { model with command_state = None }
           | Waiting_for_second_char 'r', c ->
             send_action (Replace_char (Char.to_string c));
             { model with command_state = None }
           (* Cancel multi-character command on any other key *)
           | Waiting_for_second_char _, _ -> { model with command_state = None }
           | None, _ -> model)
        (* Handle arrow keys in both modes *)
        | Insert, Key_press { key = Arrow `Left; mods = [] } ->
          send_action Prev_char;
          model
        | Insert, Key_press { key = Arrow `Right; mods = [] } ->
          send_action Next_char;
          model
        | Insert, Key_press { key = Arrow `Up; mods = [] } ->
          send_action Prev_line;
          model
        | Insert, Key_press { key = Arrow `Down; mods = [] } ->
          send_action Next_line;
          model
        (* Ctrl+Arrow for word movement in insert mode *)
        | Insert, Key_press { key = Arrow `Left; mods = [ Ctrl ] } ->
          send_action Prev_word;
          model
        | Insert, Key_press { key = Arrow `Right; mods = [ Ctrl ] } ->
          send_action Next_word;
          model
        | Normal, Key_press { key = Arrow `Left; mods = [] } ->
          send_action Prev_char;
          model
        | Normal, Key_press { key = Arrow `Right; mods = [] } ->
          send_action Next_char;
          model
        | Normal, Key_press { key = Arrow `Up; mods = [] } ->
          send_action Prev_line;
          model
        | Normal, Key_press { key = Arrow `Down; mods = [] } ->
          send_action Next_line;
          model
        | Normal, Key_press { key = Escape; mods = [] } ->
          { model with command_state = None }
        | (Normal | Insert), Key_press { key = Home; mods = [] } ->
          send_action Goto_bol;
          { model with command_state = None }
        | (Normal | Insert), Key_press { key = End; mods = [] } ->
          send_action Goto_eol;
          { model with command_state = None }
        | (Normal | Insert), Key_press { key = Delete; mods = [] } ->
          send_action Delete_next_char;
          { model with command_state = None }
        | _, _ -> model
      in
      model
    ;;
  end

  let vim_keybindings_handler
    :  default_mode:Mode.t -> (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> local_ Bonsai.graph -> t
    =
    fun ~default_mode send_actions (local_ graph) ->
    let send_action =
      let%arr send_actions in
      fun action -> send_actions [ action ]
    in
    let model, handler =
      (* NOTE: We intentionally use [state_machine_with_input] to avoid frame gaps when
         switching modes / performing sequences of keystrokes. It is somewhat important to
         make this as snappy as we can (within reason). *)
      Bonsai.state_machine_with_input
        ~default_model:
          { Model.mode = default_mode; command_state = None; last_yank_type = Char }
        (let%arr send_action in
         { Model.Input.send_action })
        ~apply_action:Model.apply_action
        graph
    in
    let mode =
      let%arr { mode; command_state = _; last_yank_type = _ } = model in
      mode
    in
    { mode; handler }
  ;;
end

module Emacs = struct
  let emacs_keybindings_handler
    :  (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t -> local_ Bonsai.graph
    -> (Event.t -> unit Effect.t) Bonsai.t
    =
    fun send_actions (local_ _graph) ->
    let%arr send_actions in
    let send_action action = send_actions [ action ] in
    let handler (event : Event.t) =
      match event with
      (* Basic character insertion *)
      | Key_press { key = ASCII c; mods = [] } -> send_action (Insert (Char.to_string c))
      | Key_press { key = Uchar c; mods = [] } ->
        send_action (Insert (Uchar.Utf8.to_string c))
      | Key_press { key = Enter; mods = [] } -> send_action Newline
      | Key_press { key = Backspace; mods = [] } -> send_action Delete_prev_char
      (* Emacs-style Ctrl keybindings *)
      | Key_press { key = ASCII 'A'; mods = [ Ctrl ] } -> send_action Goto_bol
      | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } -> send_action Goto_eol
      | Key_press { key = ASCII 'F'; mods = [ Ctrl ] } -> send_action Next_char
      | Key_press { key = ASCII 'B'; mods = [ Ctrl ] } -> send_action Prev_char
      | Key_press { key = ASCII 'N'; mods = [ Ctrl ] } -> send_action Next_line
      | Key_press { key = ASCII 'P'; mods = [ Ctrl ] } -> send_action Prev_line
      | Key_press { key = ASCII 'D'; mods = [ Ctrl ] } -> send_action Delete_next_char
      | Key_press { key = ASCII 'H'; mods = [ Ctrl ] } -> send_action Delete_prev_char
      | Key_press { key = ASCII 'K'; mods = [ Ctrl ] } -> send_action Kill_next_line
      | Key_press { key = ASCII 'U'; mods = [ Ctrl ] } -> send_action Kill_prev_line
      | Key_press { key = ASCII 'Y'; mods = [ Ctrl ] } -> send_action Yank
      (* Word movement *)
      | Key_press { key = Arrow `Right; mods = [ Meta ] } -> send_action Next_word
      | Key_press { key = Arrow `Left; mods = [ Meta ] } -> send_action Prev_word
      | Key_press { key = ASCII 'f'; mods = [ Meta ] } -> send_action Next_word
      | Key_press { key = ASCII 'b'; mods = [ Meta ] } -> send_action Prev_word
      (* Word deletion *)
      | Key_press { key = Delete; mods = [ Ctrl ] }
      | Key_press { key = ASCII 'd'; mods = [ Meta ] } -> send_action Kill_next_word
      | Key_press { key = ASCII 'W'; mods = [ Ctrl ] }
      | Key_press { key = Backspace; mods = [ Meta ] } -> send_action Kill_prev_word
      (* Document navigation *)
      | Key_press { key = ASCII '<'; mods = [ Meta; Shift ] } -> send_action Goto_bot
      | Key_press { key = ASCII '>'; mods = [ Meta; Shift ] } -> send_action Goto_eot
      (* Undo *)
      | Key_press { key = ASCII 'Z'; mods = [ Ctrl ] } -> send_action Undo
      | Key_press { key = ASCII '_'; mods = [ Ctrl ] } -> send_action Undo
      (* Arrow keys (standard behavior) *)
      | Key_press { key = Arrow `Left; mods = [] } -> send_action Prev_char
      | Key_press { key = Arrow `Right; mods = [] } -> send_action Next_char
      | Key_press { key = Arrow `Up; mods = [] } -> send_action Prev_line
      | Key_press { key = Arrow `Down; mods = [] } -> send_action Next_line
      (* Ctrl+Arrow for word movement (common in many editors) *)
      | Key_press { key = Arrow `Right; mods = [ Ctrl ] } -> send_action Next_word
      | Key_press { key = Arrow `Left; mods = [ Ctrl ] } -> send_action Prev_word
      (* Home/End/Delete keys *)
      | Key_press { key = Home; mods = [] } -> send_action Goto_bol
      | Key_press { key = End; mods = [] } -> send_action Goto_eol
      | Key_press { key = Delete; mods = [] } -> send_action Delete_next_char
      | _ -> Effect.Ignore
    in
    handler
  ;;
end

module Buffer_and_apply_paste_events_in_bulk = struct
  (** [Buffer_and_apply_paste_events_in_bunk.f] will make pasting fast.

      When a paste begins, it will not immediately send the event to the editor, and will
      instead buffer the paste event's keystrokes. It will send the events to the text
      editor only when it sees that the paste has finished. *)
  let f
    :  send_actions:(Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> handler:(Event.t -> unit Effect.t) Bonsai.t -> local_ Bonsai.graph
    -> (Event.t -> unit Effect.t) Bonsai.t
    =
    fun ~send_actions ~handler (local_ graph) ->
    let open struct
      module Model = struct
        type t =
          { inside_of_paste : bool
          ; paste_buffer :
              [ `Char of char | `Uchar of Uchar.t | `Newline ] Reversed_list.t
          }
      end

      module Input = struct
        type t =
          { handler : Event.t -> unit Effect.t
          ; send_actions : Action.t Nonempty_list.t -> unit Effect.t
          }
      end

      let apply_action
        context
        (input : Input.t Bonsai.Computation_status.t)
        (model : Model.t)
        (event : Event.t)
        =
        let[@inline always] forward_event event =
          match input with
          | Inactive -> ()
          | Active { handler; _ } ->
            Bonsai.Apply_action_context.schedule_event context (handler event)
        in
        let[@inline always] send_actions actions =
          match input with
          | Inactive -> ()
          | Active { send_actions; _ } ->
            Bonsai.Apply_action_context.schedule_event context (send_actions actions)
        in
        let commit_paste_buffer ~paste_buffer =
          (* Combine all paste events into a single string to create only one undo
             checkpoint. This makes undo undo the entire paste operation atomically. *)
          let combined_string =
            paste_buffer
            |> Reversed_list.rev
            |> List.map ~f:(function
              | `Char c -> Char.to_string c
              | `Uchar uchar -> Uchar.Utf8.to_string uchar
              | `Newline -> "\n")
            |> String.concat ~sep:""
          in
          match combined_string with
          | "" -> ()
          | s -> send_actions [ Action.Insert s ]
        in
        let paste_buffer =
          match model.inside_of_paste with
          | false ->
            let () =
              match event with
              | Paste _ -> ()
              | _ -> forward_event event
            in
            model.paste_buffer
          | true ->
            let event =
              match event with
              | Key_press { key = ASCII char; mods = [] } -> Some (`Char char)
              | Key_press { key = Uchar uchar; mods = [] } -> Some (`Uchar uchar)
              | Key_press { key = Enter; mods = [] } -> Some `Newline
              | _ ->
                (* NOTE: we drop all other events that happen inside of a paste. I think
                   this is fine. *)
                None
            in
            (match event with
             | None -> model.paste_buffer
             | Some event -> event :: model.paste_buffer)
        in
        let inside_of_paste, paste_buffer =
          match event with
          | Paste `Start -> true, Reversed_list.[]
          | Paste `End ->
            commit_paste_buffer ~paste_buffer;
            false, Reversed_list.[]
          | _ -> model.inside_of_paste, paste_buffer
        in
        { Model.inside_of_paste; paste_buffer }
      ;;
    end in
    let _model, inject =
      Bonsai.state_machine_with_input
        ~default_model:{ Model.inside_of_paste = false; paste_buffer = [] }
        ~apply_action
        (let%arr send_actions and handler in
         { Input.send_actions; handler })
        graph
    in
    inject
  ;;
end

module For_testing = struct
  let wrap_text = wrap_text
end
