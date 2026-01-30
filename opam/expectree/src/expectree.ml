open! Base

type t =
  | Leaf of string
  | Branch of string * t list
  | Split of t list

module Left = struct
  type t =
    | Gap
    | Line
end

let print_gap ~buf left =
  left
  |> List.rev
  |> List.iter ~f:(function
    | Left.Gap -> Buffer.add_string buf "   "
    | Line -> Buffer.add_string buf "│  ")
;;

let connector ~buf prefix child =
  Buffer.add_string buf prefix;
  Buffer.add_string buf "─";
  let half =
    match child with
    | Split [ Leaf _ ] -> true
    | Split _ -> false
    | Leaf _ | Branch _ -> true
  in
  if half then Buffer.add_string buf "╴" else Buffer.add_string buf "─"
;;

let is_all_whitespace x = String.for_all ~f:Char.is_whitespace x

let count_leading_empty_lines s =
  s |> String.split ~on:'\n' |> List.take_while ~f:is_all_whitespace |> List.length
;;

let count_trailing_empty_lines s = count_leading_empty_lines (String.rev s)

let print_label ~is_branch_label ~buf ~left s =
  String.split s ~on:'\n'
  |> List.drop_while ~f:is_all_whitespace
  |> List.rev
  |> List.drop_while ~f:is_all_whitespace
  |> List.rev
  |> List.iteri ~f:(fun i s ->
    if i <> 0
    then (
      Buffer.add_char buf '\n';
      print_gap ~buf left);
    Buffer.add_string buf s);
  Fn.apply_n_times
    ~n:(count_trailing_empty_lines s)
    (fun () ->
      Buffer.add_char buf '\n';
      let left = if is_branch_label then Left.Line :: left else left in
      print_gap ~buf left)
    ()
;;

let count_next_newlines t =
  let rec loop = function
    | Leaf s -> Some (count_leading_empty_lines s)
    | Branch (s, _) -> Some (count_leading_empty_lines s)
    | Split [] -> None
    | Split children -> List.find_map children ~f:loop
  in
  Option.value (loop t) ~default:0
;;

let print_upper_whitespace ~buf ~left child =
  Fn.apply_n_times
    ~n:(count_next_newlines child)
    (fun () ->
      Buffer.add_char buf '\n';
      print_gap ~buf (Line :: left))
    ()
;;

let rec loop ~buf ~(left : Left.t list) (t : t) ~at_top ~first_node ~split_depth =
  match t with
  | Leaf s | Branch (s, []) -> print_label ~buf ~left ~is_branch_label:false s
  | Split [ child ] -> loop ~buf ~left ~at_top ~first_node ~split_depth child
  | Split children ->
    let children_len = List.length children in
    List.iteri children ~f:(fun i child ->
      let is_first_node = i = 0 in
      let is_last_node = i + 1 = children_len in
      if not is_first_node
      then (
        print_upper_whitespace ~buf ~left child;
        Buffer.add_char buf '\n');
      (match at_top, is_first_node, is_last_node with
       | _, true, true -> ()
       | true, true, false -> connector ~buf "╭" child
       | false, true, false -> connector ~buf "┬" child
       | _, false, false ->
         print_gap ~buf left;
         connector ~buf "├" child
       | _, _, true ->
         print_gap ~buf left;
         connector ~buf "╰" child);
      let next_left = if is_last_node then Left.Gap :: left else Line :: left in
      loop
        ~buf
        ~left:next_left
        ~first_node
        ~at_top:false
        ~split_depth:(split_depth + 1)
        child)
  | Branch (s, children) ->
    print_label ~is_branch_label:true ~buf ~left s;
    let children_len = List.length children in
    List.iteri children ~f:(fun i child ->
      let is_last_node = i + 1 = children_len in
      print_upper_whitespace ~buf ~left child;
      Buffer.add_char buf '\n';
      print_gap ~buf left;
      (match is_last_node with
       | false -> connector ~buf "├" child
       | true -> connector ~buf "╰" child);
      let next_left = if is_last_node then Left.Gap :: left else Line :: left in
      loop ~buf ~left:next_left ~first_node:false ~at_top:false ~split_depth:0 child)
;;

let rec conv_sexp ~inline_pairs = function
  | Sexp.Atom s -> Leaf s
  | List [ Atom s; List [] ] when inline_pairs -> Leaf (s ^ "╶╴•")
  | List [ Atom a; Atom b ] when inline_pairs -> Leaf (Printf.sprintf "%s╶╴%s" a b)
  | List [ Atom s; (List [ Atom _; List _ ] as child) ] ->
    Branch (s, [ conv_sexp ~inline_pairs child ])
  | List [ Atom s; List children ] ->
    Branch (s, List.map children ~f:(conv_sexp ~inline_pairs))
  | List [] -> Leaf "•"
  | List children -> Split (List.map children ~f:(conv_sexp ~inline_pairs))
;;

let to_string t =
  let buf = Buffer.create 64 in
  loop ~buf ~left:[] ~first_node:true ~at_top:true ~split_depth:0 t;
  Buffer.contents buf
;;

let sexp_to_string ?(inline_pairs = true) sexp = to_string (conv_sexp ~inline_pairs sexp)
