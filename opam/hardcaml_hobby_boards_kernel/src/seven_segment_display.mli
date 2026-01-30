(** Functions for writing numeric and hex values to the seven segment display. *)

open Base

type code

(** [0..9] *)
val numeric_codes : code array

(** [0..9a..f] *)
val hex_codes : code array

(** Add a dot after the code *)
val add_dot : code -> code

(** Convert code to a signal. *)
val to_signal : code -> Hardcaml.Signal.t

(** Print code for debugging *)
val print : code -> unit
