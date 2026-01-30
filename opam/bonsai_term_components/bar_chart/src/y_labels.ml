open! Core
open Bonsai_term

module Make_label_string = struct
  type t = local_ float -> string

  let make_reasonable_linear ~max_bar_height ~min_value ~max_value =
    let suffix, denominator =
      match Float.max (Float.abs min_value) (Float.abs max_value) with
      | x when Float.( < ) x 1_000.0 -> "", 1.0
      | x when Float.( < ) x 1_000_000.0 -> "K", 1_000.0
      | x when Float.( < ) x 1_000_000_000.0 -> "M", 1_000_000.0
      | x when Float.( < ) x 1_000_000_000_000.0 -> "B", 1_000_000_000.0
      | _ -> "T", 1_000_000_000_000.0
    in
    let units_per_row =
      (max_value -. min_value) /. (Float.of_int max_bar_height *. denominator)
      |> Float.abs
    in
    let rec get_num_decimals acc x =
      if Float.( >= ) (Float.abs x) 1.0
      then acc
      else get_num_decimals (acc + 1) (x *. 10.0)
    in
    let decimals = get_num_decimals 0 units_per_row in
    fun x ->
      let x = x /. denominator in
      Float.to_string_hum ~strip_zero:true ~delimiter:',' ~decimals x ^ suffix
  ;;

  let make_reasonable_logarithmic ~base =
    let format_power power =
      let superscript_digit = function
        | '0' -> "⁰"
        | '1' -> "¹"
        | '2' -> "²"
        | '3' -> "³"
        | '4' -> "⁴"
        | '5' -> "⁵"
        | '6' -> "⁶"
        | '7' -> "⁷"
        | '8' -> "⁸"
        | '9' -> "⁹"
        | '-' -> "⁻"
        | _ -> ""
      in
      if power = 1
      then ""
      else Int.to_string power |> String.concat_map ~f:superscript_digit
    in
    fun x ->
      if Float.( <= ) x 0.
      then Float.to_string_hum ~decimals:2 x ~strip_zero:true
      else (
        let power =
          Float.log x /. Float.log (Float.of_int base) |> Float.iround_exn ~dir:`Nearest
        in
        let coefficient =
          x /. Float.( ** ) (Float.of_int base) (Float.of_int power)
          |> Float.to_string_hum ~decimals:1 ~strip_zero:true
        in
        if power = 0
        then coefficient
        else if String.equal coefficient "1"
        then [%string "%{base#Int}%{format_power power}"]
        else [%string "%{coefficient}×%{base#Int}%{format_power power}"])
  ;;
end

module Label = struct
  type t =
    { row_num : int
    ; text : string
    }
  [@@deriving fields ~getters]
end

module Layout = struct
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

let get_labels { layout; make_label_string } ~max_bar_height ~bar_height =
  let label_from_row_num row_num =
    { Label.row_num
    ; text =
        make_label_string (Bar_height.value_at_height bar_height (Int.to_float row_num))
    }
  in
  let label_from_value value =
    { Label.row_num = Bar_height.height bar_height value; text = make_label_string value }
  in
  let labels =
    match layout with
    | Custom_by_row row_nums -> List.map row_nums ~f:label_from_row_num
    | Custom_by_percent_of_max_height height_percents ->
      List.map height_percents ~f:(fun percent ->
        Percent.to_mult percent *. Int.to_float max_bar_height
        |> Float.iround_exn ~dir:`Nearest)
      |> List.map ~f:label_from_row_num
    | Every_n_rows n ->
      List.range ~stride:n 0 (max_bar_height + 1) |> List.map ~f:label_from_row_num
    | Every_x_units x ->
      let rec get_labels acc value =
        let row_num = Bar_height.height bar_height value in
        if not (Int.between row_num ~low:0 ~high:max_bar_height)
        then acc
        else get_labels (label_from_value value :: acc) (value +. x)
      in
      let start_value = Bar_height.value_at_height bar_height 0. in
      get_labels [] start_value
    | Custom_by_value values -> List.map values ~f:label_from_value
  in
  List.filter labels ~f:(fun { row_num; text = _ } ->
    Int.between row_num ~low:0 ~high:max_bar_height)
  |> List.dedup_and_sort ~compare:(Comparable.lift Int.compare ~f:Label.row_num)
;;

let view t ~max_bar_height ~bar_height ~label_color =
  let labels = get_labels t ~max_bar_height ~bar_height in
  let row_num_dif =
    List.fold_map labels ~init:(-1) ~f:(fun prev_height { row_num; text } ->
      row_num, (row_num - prev_height - 1, text))
    |> snd
  in
  let labels =
    List.map row_num_dif ~f:(fun (bottom_padding, label) ->
      View.text ~attrs:(Option.map ~f:Attr.fg label_color |> Option.to_list) label
      |> View.pad ~b:bottom_padding)
  in
  (* right justifying *)
  let longest_label_length =
    List.map labels ~f:View.width
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  List.map labels ~f:(fun label ->
    View.pad ~l:(longest_label_length - View.width label) label)
  |> List.rev
  |> View.vcat
;;
