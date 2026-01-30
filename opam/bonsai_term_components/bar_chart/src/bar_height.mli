open! Core

(** Represents a function from a float value to how tall a bar should be in rows. *)
type t [@@deriving quickcheck, sexp_of]

(** The bar height will increase linearly with an increase in the input float. Raises if
    [min_value] >= [max_value]. *)
val create_linear_exn : min_value:float -> max_value:float -> max_bar_height:int -> t

val create_logarithmic_exn : min_value:float -> max_value:float -> max_bar_height:int -> t

(** Rounded to the closest integer. *)
val height : t -> float -> int

(** There are unicode characters for each eighth of a cell, so this is the greatest
    precision we can achieve when rendering a bar. *)
val precise_height : t -> float -> whole_blocks:int * extra_eighths:int

(** Returns the value that would be associated with a bar if it were x units tall. *)
val value_at_height : t -> float -> float

module For_testing : sig
  val f : t -> float -> float
  val inverse_f : t -> float -> float
end
