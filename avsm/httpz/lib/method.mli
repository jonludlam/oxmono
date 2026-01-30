(** HTTP request method. *)

type t =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch

(** Convert to string. *)
val to_string : t -> string

(** Pretty-print method. *)
val pp : Stdlib.Format.formatter -> t -> unit
