open! Core
open! Async

type t

val all : t list
val arg_type : t Command.Arg_type.t

(** Filename of original SVG file *)
val basename : t -> Filename.t

(** contents of the original SVG file *)
val contents : t -> string

(** name of variable that the SVG contents will be embedded under *)
val variable_name : t -> string

(** Name to pass on the command line *)
val arg_name : t -> string
