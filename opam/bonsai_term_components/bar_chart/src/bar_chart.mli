open! Core
open Bonsai_term

module Bar : sig
  type t =
    { value : float
    ; label : string option
    ; color : Attr.Color.t option
    }
end

module Y_labels_config : sig
  type t =
    | Hidden
    | Shown_use_reasonable_default (** Places a label on every 5th row *)
    | Shown_custom of Y_labels.t
end

module Y_range : sig
  (** The min/max values on the y axis. [Use_most_extreme] will find the min / max value
      from the given data. *)
  type t =
    | Constant of float
    | Use_most_extreme_value
end

module Bar_height_config : sig
  type t =
    | Default
    (** Defaults to setting the minimum value on the y axis to 0 if all of the data is >=
        0 or the minimum value of the data if the value of a bar is < 0. The max value
        will be the maximum of 0 and the greatest value from the data. *)
    | Linear of
        { min_value : Y_range.t
        ; max_value : Y_range.t
        }
    | Logarithmic of
        { min_value : Y_range.t
        ; max_value : Y_range.t
        ; base : int
        }
end

module Bar_width_config : sig
  type t =
    | Custom of
        { width : int
        ; padding : int
        } (** There will be [padding] blank columns between each bar. *)
    | Choose_for_me_from_max_total_width of int
    (** Chooses a width and padding for the bars that will bring the total width of the
        graph (including labels / borders) as close to the max total width as possible
        without going over. However, if the graph is still wider than the provided total
        width when the bar width is 1 and the padding is 0, the graph will be longer than
        the given total width. *)
end

val view
  :  ?theme:Theme.t (** Default: [Theme.catppuccin ~flavor:Mocha ~data_color:Blue] *)
  -> ?y_labels_config:Y_labels_config.t (** Default [Shown_use_reasonable_default]. *)
  -> ?show_x_labels:bool (** Default [true]. *)
  -> ?title:string option (** Default [None]. *)
  -> ?show_border:bool (** Default [true]. *)
  -> ?bar_height_config:Bar_height_config.t (** See [Bar_height_config.Default]. *)
  -> Bar.t list (** Each [Bar.t] in the list will be graphed from left to right. *)
  -> max_bar_height:int (** The number of vertical cells the y axis will take up. *)
  -> bar_width_config:Bar_width_config.t
  -> View.t

module For_testing : sig
  val calculate_bar_width : width_for_bars:int -> num_bars:int -> width:int * padding:int
end
