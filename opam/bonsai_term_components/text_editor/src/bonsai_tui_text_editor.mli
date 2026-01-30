open! Core
open Bonsai_term

(** [bonsai_tui_text_editor] is a tiny library that provides a tiny text editor.

    - UTF-8 support
    - "Standard" keybindings
    - VIM keybindings
    - Line wrapping (via word breaking)

    It's main purpose is to provide a rich text edciting experience in apps with
    complicated text editing requirements. If the current library does not fulfill your
    requirements, please reach out!

    You can see a demo of this component in [./examples/text_editor/bin/main.exe]. *)

module Action : sig
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
end

module Cursor : sig
  type t =
    { logical_line : int
    ; logical_column : int
    ; visual_column : int
    ; visual_line : int
    ; position : int
    }
  [@@deriving sexp_of]
end

(** [text] is the inputted string.

    [send_actions] lets you send commands to the text editor. NOTE: For opinionated
    "normal", and "vim" keybindings, you can pass [send_actions] to either
    [Vim.vim_keybindings_handler] or [default_keybindings_handler].

    [view] is the view for the text editor.

    [cursor] let's you know the location of the cursor according to the text editor.

    NOTE: if you would like to get the cursor position "visualy" relative to other
    elements in the view, you can use [get_cursor_relative_position].

    [set_text] let's you update the string value of the textbox. Undo history is lost, but
    cursor position will be attempted to be preserved.

    [get_cursor_position view] will give you the location of the text editor's cursor
    relative top-left of the view that you give it. The text editor can be embedded
    anywhere inside of [view] and [get_cursor_position] will give you a location relative
    to the top-left corner of the view. You can use this if you would like to set the
    cursor positioning. *)
type t = private
  { text : string Bonsai.t
  ; send_actions : (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
  ; view : View.t Bonsai.t
  ; cursor : Cursor.t Bonsai.t
  ; set_text : (string -> unit Effect.t) Bonsai.t
  ; rope : Zed.Zed_rope.t Bonsai.t
  ; get_cursor_position : (View.t -> Position.t option) Bonsai.t
  }

(** [Bonsai_tui_text_editor.component] is a tiny text editor.

    [match_word] is used to recognize 'words' (some commands operate on words). It must
    returns the end of the matched word if any.

    [undo_size] is the size of the undo buffer. It is the number of state zed will
    remember. It defaults to [1000].

    [text_attrs] is the default attributes attaced to text. We do not currently support
    syntax highlighting.

    [width] The editor will attempt to have a width of [width] and will start wrapping
    lines when a line reaches [width].

    [max_height]. The text editor's height will be [min (height view) max_height]. If
    there are more lines visually in the editor than max_height, the element will start
    scrolling. *)
val component
  :  ?match_word:(Zed.Zed_rope.t -> int -> int option)
  -> ?undo_size:int
  -> text_attrs:Attr.t list Bonsai.t
  -> width:int Bonsai.t
  -> max_height:int Bonsai.t
  -> local_ Bonsai.graph
  -> t

val default_keybindings_handler
  :  (Action.t Nonempty_list.t -> unit Effect.t)
  -> Event.t
  -> unit Effect.t

module Vim : sig
  module Mode : sig
    type t =
      | Normal
      | Insert
    [@@deriving sexp_of]
  end

  type t = private
    { handler : (Event.t -> unit Effect.t) Bonsai.t
    ; mode : Mode.t Bonsai.t
    }

  val vim_keybindings_handler
    :  default_mode:Mode.t
    -> (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> local_ Bonsai.graph
    -> t
end

module Emacs : sig
  val emacs_keybindings_handler
    :  (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> local_ Bonsai.graph
    -> (Event.t -> unit Effect.t) Bonsai.t
end

module Buffer_and_apply_paste_events_in_bulk : sig
  (** [Buffer_and_apply_paste_events_in_bunk.f] will make pasting fast.

      When a paste begins, it will not immediately send the event to the editor, and will
      instead buffer the paste event's keystrokes. It will send the events to the text
      editor only when it sees that the paste has finished. *)
  val f
    :  send_actions:(Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> handler:(Event.t -> unit Effect.t) Bonsai.t
    -> local_ Bonsai.graph
    -> (Event.t -> unit Effect.t) Bonsai.t
end

module For_testing : sig
  val wrap_text
    :  text:string
    -> max_width:int
    -> ([ `Wrapped | `Not_wrapped ] * string) list
end
