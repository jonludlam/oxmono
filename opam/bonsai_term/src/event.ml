open! Core

module Modifier = struct
  type t =
    | Meta
    | Ctrl
    | Shift
  [@@deriving sexp_of]
end

module Key = struct
  type t =
    | Escape
    | Enter
    | Tab
    | Backspace
    | Insert
    | Delete
    | Home
    | End
    | Arrow of [ `Up | `Down | `Left | `Right ]
    | Page of [ `Up | `Down ]
    | Function of int
    | Uchar of Uchar.t
    | ASCII of char
  [@@deriving sexp_of]
end

type mouse_kind =
  | Left
  | Middle
  | Right
  | Scroll of [ `Up | `Down ]
  | Drag
  | Release
[@@deriving sexp_of]

type t =
  | Key_press of
      { key : Key.t
      ; mods : Modifier.t list [@sexp.list]
      }
  | Mouse of
      { kind : mouse_kind
      ; position : Geom.Position.t
      ; mods : Modifier.t list [@sexp.list]
      }
  | Paste of [ `Start | `End ]
[@@deriving sexp_of]

module Root_event = struct
  type nonrec t =
    | Event of t
    | Resize of Geom.Dimensions.t
    | Stdin_closed
    | Timer
  [@@deriving sexp_of]
end
