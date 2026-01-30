open! Core

(** This is a trie that remembers the insertion order of its values, but only within a
    single prefix. *)
type t =
  { children : t String.Table.t
  ; mutable insertion_order : string list (* latest first *)
  ; mutable counts_always : bool
  ; mutable counts_if_no_children : bool
  }
[@@deriving sexp_of]

let create () =
  { children = String.Table.create ()
  ; insertion_order = []
  ; counts_always = false
  ; counts_if_no_children = false
  }
;;

let get_or_add_one t =
  Hashtbl.find_and_call t.children ~if_found:Fn.id ~if_not_found:(fun key ->
    let data = create () in
    Hashtbl.set t.children ~key ~data;
    t.insertion_order <- key :: t.insertion_order;
    data)
;;

let rec add t ?(only_if_empty = false) = function
  | [] ->
    if only_if_empty then t.counts_if_no_children <- true else t.counts_always <- true
  | key :: rest -> add (get_or_add_one t key) rest ~only_if_empty
;;

let depth_first_traversal t =
  let rec helper t key_path =
    let children =
      List.concat_map (List.rev t.insertion_order) ~f:(fun key ->
        let child = Hashtbl.find_exn t.children key in
        helper child (key :: key_path))
    in
    if t.counts_always || (t.counts_if_no_children && List.is_empty t.insertion_order)
    then key_path :: children
    else children
  in
  helper t [] |> List.map ~f:List.rev
;;

module%test _ = struct
  let show t = print_s [%sexp (depth_first_traversal t : string list list)]

  let%expect_test "can insert the empty path" =
    let t = create () in
    add t [];
    show t;
    [%expect {| (()) |}]
  ;;

  let%expect_test "nodes always sort before their children regardless of insertion order" =
    let t = create () in
    add t [ "a"; "b" ];
    add t [ "a"; "c" ];
    add t [ "a" ];
    show t;
    [%expect {| ((a) (a b) (a c)) |}]
  ;;

  let%expect_test "sibling nodes are sorted in insertion order" =
    let t = create () in
    add t [ "a"; "z" ];
    add t [ "a"; "b" ];
    show t;
    [%expect {| ((a z) (a b)) |}]
  ;;

  let%expect_test "traversal order is based on the earliest inserted prefix" =
    let t = create () in
    add t [ "a"; "c" ];
    add t [ "b" ];
    add t [ "a"; "b" ];
    show t;
    [%expect {| ((a c) (a b) (b)) |}]
  ;;
end
