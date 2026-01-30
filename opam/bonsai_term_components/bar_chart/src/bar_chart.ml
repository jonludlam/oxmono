open! Core
open Bonsai_term

let truncate_text s max_length =
  let utf8_string = String.Utf8.of_string s in
  let total_width =
    String.Utf8.fold utf8_string ~init:0 ~f:(fun acc uchar ->
      let width = View.uchar_tty_width uchar in
      acc + width)
  in
  if total_width = 0 || total_width <= max_length
  then s
  else (
    (* Reserve space for the ellipsis character *)
    let max_width = max_length - 1 in
    let truncated, _ =
      String.Utf8.fold utf8_string ~init:([], 0) ~f:(fun (acc, current_width) uchar ->
        let char_width = View.uchar_tty_width uchar in
        if current_width + char_width <= max_width
        then uchar :: acc, current_width + char_width
        else acc, current_width)
    in
    String.Utf8.to_string (String.Utf8.of_list (List.rev truncated)) ^ "…")
;;

let get_fg_attr color = Option.map color ~f:Attr.fg |> Option.to_list

module Bar = struct
  type t =
    { value : float
    ; label : string option
    ; color : Attr.Color.t option
    }
  [@@deriving fields ~getters]

  let solid_block = Uchar.Utf8.of_string "█"
  let down_arrow = Uchar.Utf8.of_string "↓"
  let up_arrow = Uchar.Utf8.of_string "↑"

  let block_eighths =
    Array.map [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] ~f:Uchar.Utf8.of_string
  ;;

  let view { value; color; label = _ } ~max_bar_height ~bar_height ~bar_width =
    let ~whole_blocks, ~extra_eighths = Bar_height.precise_height bar_height value in
    let num_eighths = (whole_blocks * 8) + extra_eighths in
    let max_num_eighths = max_bar_height * 8 in
    let make_row c =
      View.text
        ~attrs:(get_fg_attr color)
        (String.concat (List.create ~len:bar_width (Uchar.Utf8.to_string c)))
    in
    let make_block height =
      (* Fill the rectangle with a character so it shows up in tests. *)
      View.vcat @@ List.init height ~f:(fun _ -> make_row solid_block)
    in
    let bar =
      if num_eighths < 0
      then make_row down_arrow
      else if num_eighths = 0
      then View.transparent_rectangle ~width:bar_width ~height:1
      else if num_eighths > max_num_eighths
      then View.vcat [ make_row up_arrow; make_block (max_bar_height - 1) ]
      else View.vcat [ make_row block_eighths.(extra_eighths); make_block whole_blocks ]
    in
    View.pad bar ~t:(max_bar_height - View.height bar)
  ;;
end

module Y_range = struct
  type t =
    | Constant of float
    | Use_most_extreme_value

  let get_y_value
    t
    ~bars
    ~(get_value : float list -> compare:local_ (float -> float -> int) -> float option)
    ~default
    =
    match t with
    | Constant x -> x
    | Use_most_extreme_value ->
      List.map bars ~f:Bar.value
      |> get_value ~compare:Float.compare
      |> Option.value ~default
  ;;

  let y_min t bars = get_y_value t ~bars ~get_value:List.min_elt ~default:0.
  let y_max t bars = get_y_value t ~bars ~get_value:List.max_elt ~default:1.
end

module Bar_width_config = struct
  type t =
    | Custom of
        { width : int
        ; padding : int
        }
    | Choose_for_me_from_max_total_width of int

  let calculate_bar_width ~width_for_bars ~num_bars =
    (* In order of importance, our priorities are that:
       1. [bar_width] >= 1.
       2. Total width of bars and padding does not exceed [width_for_bars].
       3. [padding] >= 1.
       4. [padding] is as large as possible, but the total width taken up by bars is at
          least 3x the total width taken up by padding.

       This logic is slightly complicated by the fact that there are N bars but N + 1
       segments of padding. *)
    if num_bars <= 0
    then ~width:1, ~padding:0
    else if width_for_bars <= 2 * num_bars
    then ~width:(Int.max 1 (width_for_bars / num_bars)), ~padding:0
    else (
      let padding = Int.max 1 (width_for_bars / (4 * (num_bars + 1))) in
      let width = (width_for_bars - ((num_bars + 1) * padding)) / num_bars in
      (* Because we're rounding down when calculating the width, we sometimes we violate
         the condition that bars to take up 3x the space that the padding does so we need
         to reduce the padding by 1. *)
      if padding = 1 || num_bars * width >= 3 * (num_bars + 1) * padding
      then ~width, ~padding
      else (
        let padding = padding - 1 in
        ~width:((width_for_bars - ((num_bars + 1) * padding)) / num_bars), ~padding))
  ;;

  let get_bar_width t ~length_of_labels_and_border ~num_bars =
    match t with
    | Custom { width; padding } -> ~width, ~padding
    | Choose_for_me_from_max_total_width total_width ->
      let width_for_bars = total_width - length_of_labels_and_border in
      calculate_bar_width ~width_for_bars ~num_bars
  ;;
end

module Bar_height_config = struct
  type t =
    | Default
    | Linear of
        { min_value : Y_range.t
        ; max_value : Y_range.t
        }
    | Logarithmic of
        { min_value : Y_range.t
        ; max_value : Y_range.t
        ; base : int
        (* The base does not affect bar height calculations. It only determines the scale
           used when auto-generating y-axis labels. We include it here so that a
           reasonable [Y_label_config] can be derived from this config without requiring
           the user to specify the base separately. *)
        }

  let extrema t ~data =
    match t with
    | Linear { min_value; max_value } | Logarithmic { min_value; max_value; base = _ } ->
      ~min_value:(Y_range.y_min min_value data), ~max_value:(Y_range.y_max max_value data)
    | Default ->
      let min_value = Y_range.y_min Use_most_extreme_value data |> Float.min 0. in
      let max_value = Y_range.y_max Use_most_extreme_value data |> Float.max 0. in
      if Float.( = ) min_value 0. && Float.( = ) max_value 0.
      then ~min_value:0., ~max_value:1.
      else ~min_value, ~max_value
  ;;

  let get_bar_height t ~data ~max_bar_height =
    let ~min_value, ~max_value = extrema t ~data in
    match t with
    | Linear _ | Default ->
      Bar_height.create_linear_exn ~min_value ~max_value ~max_bar_height
    | Logarithmic _ ->
      Bar_height.create_logarithmic_exn ~min_value ~max_value ~max_bar_height
  ;;
end

module Y_labels_config = struct
  type t =
    | Hidden
    | Shown_use_reasonable_default
    | Shown_custom of Y_labels.t

  let get_y_labels t ~max_bar_height ~min_value ~max_value ~bar_height_config =
    match t with
    | Hidden -> None
    | Shown_use_reasonable_default ->
      let make_label_string =
        match bar_height_config with
        | Bar_height_config.Linear _ | Default ->
          Y_labels.Make_label_string.make_reasonable_linear
            ~max_bar_height
            ~min_value
            ~max_value
        | Logarithmic { base; min_value = _; max_value = _ } ->
          Y_labels.Make_label_string.make_reasonable_logarithmic ~base
      in
      Some { Y_labels.layout = Every_n_rows 5; make_label_string }
    | Shown_custom y_labels -> Some y_labels
  ;;
end

let title_view title ~max_width ~text_color ~border_color =
  (* awkward special casing for one line titles to make it so that the border box does not
     stretch the whole width if the title is skinnier than that. *)
  if String.length title <= max_width
  then
    View.text ~attrs:(get_fg_attr text_color) title
    |> Bonsai_tui_border_box.view ~attrs:(get_fg_attr border_color)
    |> View.center ~within:{ Dimensions.height = 3; width = max_width }
  else
    (* center each line rather (as opposed to centering the whole view) *)
    Bonsai_tui_typography.Text.of_string ~attr:() title
    |> List.singleton
    |> List.singleton
    |> Bonsai_tui_typography.typeset ~max_width
    |> List.map ~f:(fun line ->
      List.map line ~f:Bonsai_tui_typography.Text.to_string |> String.concat ~sep:" ")
    |> List.map ~f:(View.text ~attrs:(get_fg_attr text_color))
    |> List.map ~f:(View.center ~within:{ Dimensions.height = 1; width = max_width })
    |> View.vcat
    |> Bonsai_tui_border_box.view ~attrs:(get_fg_attr text_color)
;;

let x_labels ~label_color ~bar_width ~bar_padding ~labels =
  let pad_string s =
    (* This is necessary to make sure that the label is shifted to the left is there's no
       way to perfectly center the label. For example, if there's two characters available
       for the x label and a one character label, we want the label to be in the first
       position rather than the second. *)
    let s =
      if bar_padding % 2 = 1 && bar_width % 2 <> String.length s % 2 then s ^ " " else s
    in
    View.text ~attrs:(get_fg_attr label_color) s
    |> View.center ~within:{ Dimensions.height = 1; width = bar_width + bar_padding }
  in
  let text_width =
    if bar_padding = 0
    then bar_width - 2
    else bar_width + bar_padding - 2 + (bar_padding % 2)
  in
  List.map labels ~f:(fun s -> truncate_text s text_width |> pad_string) |> View.hcat
;;

let view
  ?(theme = Theme.catppuccin ~flavor:Mocha ~data_color:Blue)
  ?(y_labels_config = Y_labels_config.Shown_use_reasonable_default)
  ?(show_x_labels = true)
  ?(title = None)
  ?(show_border = true)
  ?(bar_height_config = Bar_height_config.Default)
  data
  ~max_bar_height
  ~bar_width_config
  =
  let bar_height =
    Bar_height_config.get_bar_height bar_height_config ~data ~max_bar_height
  in
  let data =
    List.map data ~f:(fun bar ->
      { bar with color = (if Option.is_some bar.color then bar.color else theme.data) })
  in
  let y_labels_view =
    let ~min_value, ~max_value = Bar_height_config.extrema bar_height_config ~data in
    Y_labels_config.get_y_labels
      y_labels_config
      ~max_bar_height
      ~min_value
      ~max_value
      ~bar_height_config
    |> Option.map
         ~f:(Y_labels.view ~max_bar_height ~bar_height ~label_color:theme.label_text)
  in
  let ~width:bar_width, ~padding:bar_padding =
    let length_of_labels_and_border =
      Bool.to_int show_border
      + (Option.map y_labels_view ~f:View.width |> Option.value ~default:0)
    in
    Bar_width_config.get_bar_width
      bar_width_config
      ~length_of_labels_and_border
      ~num_bars:(List.length data)
  in
  let bars =
    match List.is_empty data with
    | true -> View.transparent_rectangle ~width:1 ~height:max_bar_height
    | false ->
      List.map data ~f:(Bar.view ~bar_height ~max_bar_height ~bar_width)
      |> List.mapi ~f:(fun i bar ->
        let l = if i = 0 then bar_padding else 0 in
        View.pad bar ~l ~r:bar_padding)
      |> View.hcat
  in
  let maybe_with_border =
    if not show_border
    then bars
    else
      Bonsai_tui_border_box.view
        ~attrs:(get_fg_attr theme.border)
        ~line_type:Thick
        ~hide_right:true
        ~hide_top:true
        bars
  in
  let x_labels =
    if not show_x_labels
    then View.none
    else (
      let left_pad = ((bar_padding + 1) / 2) + Bool.to_int show_border in
      let labels = List.map data ~f:Bar.label |> List.map ~f:(Option.value ~default:"") in
      x_labels ~label_color:theme.label_text ~bar_width ~bar_padding ~labels
      |> View.pad ~l:left_pad)
  in
  let maybe_with_x_labels = View.vcat [ maybe_with_border; x_labels ] in
  let maybe_with_title =
    match title with
    | None -> maybe_with_x_labels
    | Some title ->
      let left_pad = Bool.to_int show_border in
      let max_width = View.width maybe_with_x_labels - left_pad - 2 in
      let title_view =
        title_view
          title
          ~max_width
          ~text_color:theme.title
          ~border_color:theme.title_border
        |> View.pad ~l:left_pad
      in
      View.vcat [ title_view; maybe_with_x_labels ]
  in
  let maybe_with_y_labels =
    match y_labels_view with
    | None -> maybe_with_title
    | Some y_labels_view ->
      (* The [View.width x_labels > 1] is to make labels line up properly for a bar chart
         with no bars. *)
      let b_pad = Bool.to_int show_border + Bool.to_int (View.width x_labels > 1) in
      let y_labels_view = View.pad ~b:b_pad y_labels_view in
      View.hcat
        [ View.pad
            ~t:(View.height maybe_with_title - View.height y_labels_view + 1)
            y_labels_view
        ; maybe_with_title
        ]
  in
  maybe_with_y_labels
;;

module For_testing = struct
  let calculate_bar_width = Bar_width_config.calculate_bar_width
end
