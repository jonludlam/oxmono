(** HTTP version. *)

type t =
  | Http_1_0
  | Http_1_1

(** Convert to string representation. *)
val to_string : t -> string

(** Pretty-print version. *)
val pp : Stdlib.Format.formatter -> t -> unit
