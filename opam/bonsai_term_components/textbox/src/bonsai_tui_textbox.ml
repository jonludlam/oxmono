open! Core
open! Bonsai
open! Bonsai_term
open Bonsai.Let_syntax

type t =
  { view : View.t
  ; string : string
  ; handler : Event.t -> unit Effect.t
  ; set : string -> unit Effect.t
  }

type action =
  | Char of char
  | Uchar of Uchar.t
  | Backspace
  | Clear
  | Set of string

let component
  ?(cursor_attrs = Bonsai.return [])
  ?(text_attrs = Bonsai.return [])
  ?(default_model = "")
  ~is_focused
  (local_ graph)
  =
  let string, inject =
    Bonsai.state_machine
      ~default_model
      ~apply_action:(fun _ (model : string) (action : action) ->
        match action with
        | Clear -> ""
        | Backspace ->
          if String.is_empty model
          then model
          else String.sub ~pos:0 ~len:(String.length model - 1) model
        | Char char ->
          (* NOTE: This is O(n^2) which is sad... I can't think of anything better for
             now... maybe a reversed list or maybe even a rope could be better, but way
             more complex... *)
          model ^ Char.to_string char
        | Uchar uchar ->
          (* NOTE: This is O(n^2) and also sad... *)
          model ^ Uchar.Utf8.to_string uchar
        | Set s -> s)
      graph
  in
  let set =
    let%arr inject in
    fun value -> inject (Set value)
  in
  let handler =
    let%arr inject in
    fun (event : Event.t) ->
      (* TODO: Implement the abilityof moving the cursor around. *)
      match event with
      | Mouse _ | Paste _ -> Effect.Ignore
      | Key_press { key = ASCII char; mods = [] } -> inject (Char char)
      | Key_press { key = Uchar uchar; mods = [] } -> inject (Uchar uchar)
      | Key_press { key = ASCII ('U' | 'u'); mods = [ Ctrl ] } -> inject Clear
      | Key_press { key = Backspace; mods = [] } -> inject Backspace
      | _ -> Effect.Ignore
  in
  let view =
    let%arr string and is_focused and cursor_attrs and text_attrs in
    View.hcat
      [ View.text ~attrs:text_attrs string
      ; (if is_focused
         then View.text ~attrs:(cursor_attrs @ [ Attr.blink ]) " "
         else View.none)
      ]
  in
  let%arr string and view and handler and set in
  { view; string; handler; set }
;;
