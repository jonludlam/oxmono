open! Core
open Bonsai_term

module Make_label_string : sig
  type t = local_ float -> string

  (** The returned function uses a metrix prefix (K, M, B, T) determined by the magnitude
      of the greatest magnitude of [min_value] and [max_value].

      There are enough decimals that there will be a different label string for each row. *)
  val make_reasonable_linear
    :  max_bar_height:int
    -> min_value:float
    -> max_value:float
    -> t

  val make_reasonable_logarithmic : base:int -> t
end

module Layout : sig
  (** Values that map to rows less than 0 or greater than the [max_bar_height] are
      discarded. If there are two labels that should go on the same row, only one will be
      displayed. *)
  type t =
    | Every_n_rows of int
    | Custom_by_row of int list
    | Custom_by_percent_of_max_height of Percent.t list
    | Every_x_units of float
    | Custom_by_value of float list
end

type t =
  { layout : Layout.t
  ; make_label_string : Make_label_string.t
  }

val view
  :  t
  -> max_bar_height:int
  -> bar_height:Bar_height.t
  -> label_color:Attr.Color.t option
  -> View.t
