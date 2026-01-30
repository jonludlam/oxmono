open! Core

let is_in_emacs = Option.is_some (Sys.getenv "INSIDE_EMACS")
