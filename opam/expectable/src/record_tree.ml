open! Core
include Composition_infix

let is_probably_constructor_name str =
  if String.is_empty str then false else Char.is_uppercase (String.get str 0)
;;

let%expect_test "is_probably_constructor_name" =
  [%test_result: bool] (is_probably_constructor_name "") ~expect:false;
  [%test_result: bool] (is_probably_constructor_name "1") ~expect:false;
  [%test_result: bool] (is_probably_constructor_name "foo") ~expect:false;
  [%test_result: bool] (is_probably_constructor_name "Foo") ~expect:true
;;

let is_probably_field_name str =
  if String.is_empty str
  then false
  else (
    match String.get str 0 with
    | '~' -> true
    | '_' -> true
    | char -> Char.is_lowercase char)
;;

let%expect_test "is_probably_field_name" =
  [%test_result: bool] (is_probably_field_name "") ~expect:false;
  [%test_result: bool] (is_probably_field_name "1") ~expect:false;
  [%test_result: bool] (is_probably_field_name "1") ~expect:false;
  [%test_result: bool] (is_probably_field_name "~foo") ~expect:true;
  [%test_result: bool] (is_probably_field_name "_foo") ~expect:true;
  [%test_result: bool] (is_probably_field_name "Foo") ~expect:false
;;

type 'leaf t =
  | Choose of (string * 'leaf t) list
  | Leaf of 'leaf
  | All of 'leaf t list
[@@deriving sexp_of]

type alist = (string * Sexp.t) list [@@deriving of_sexp]

let rec parse_sexp' ?max_depth depth sexp ~parse_leaf =
  match max_depth with
  | Some max_depth when depth >= max_depth -> Leaf (parse_leaf sexp)
  | _ ->
    (match alist_of_sexp sexp with
     | [] -> non_branch sexp depth ~parse_leaf
     | alist ->
       if List.exists alist ~f:(fst >> is_probably_field_name >> not)
          || List.contains_dup alist ~compare:(Comparable.lift [%compare: string] ~f:fst)
       then non_branch sexp depth ~parse_leaf
       else
         Choose (List.Assoc.map alist ~f:(parse_sexp' ?max_depth (depth + 1) ~parse_leaf))
     | exception _ -> non_branch sexp depth ~parse_leaf)

and non_branch sexp depth ~parse_leaf =
  match (sexp : Sexp.t) with
  | Atom _ -> Leaf (parse_leaf sexp)
  | List [] -> All []
  | List (Atom hd :: _ :: _) when is_probably_constructor_name hd ->
    Leaf (parse_leaf sexp)
  | List list -> All (List.map list ~f:(parse_sexp' (depth + 1) ~parse_leaf))
;;

let parse_sexp ?max_depth sexp ~parse_leaf = parse_sexp' ?max_depth 0 sexp ~parse_leaf

let%expect_test "parse_sexp" =
  let t = parse_sexp [%sexp { a = "foo" }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (Choose ((a (Leaf foo)))) |}]
;;

let%expect_test "record-looking sexps that contain duplicate fields parse as leaves" =
  let t = parse_sexp [%sexp { a = "foo"; a = "bar" }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (All ((All ((Leaf a) (Leaf foo))) (All ((Leaf a) (Leaf bar))))) |}]
;;

let%expect_test "empty lists parse as a leaf" =
  let t = parse_sexp [%sexp { a = (None : _ option) }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (Choose ((a (All ())))) |}]
;;

let%expect_test "variant constructors parse as leaves" =
  let t = parse_sexp [%sexp { a = `Foo 123 }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (Choose ((a (Leaf (Foo 123))))) |}]
;;

let%expect_test "optional variant constructors still parse as leaves" =
  let t = parse_sexp [%sexp { a = [ `Foo 123 ] }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (Choose ((a (All ((Leaf (Foo 123))))))) |}]
;;

let%expect_test "optional variants parse as leaves" =
  let t = parse_sexp [%sexp { a = [ `Foo ] }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (Choose ((a (All ((Leaf Foo)))))) |}]
;;

let%expect_test "lists of variants aren't parsed as leaves due to ambiguity with variant \
                 constructors"
  =
  (* this is sad but there's no way to disambigute this from [`Foo `Bar] in the sexp
     representation, so whatever *)
  let t = parse_sexp [%sexp { a = [ `Foo; `Bar ] }] ~parse_leaf:Fn.id in
  print_s [%sexp (t : Sexp.t t)];
  [%expect {| (Choose ((a (Leaf (Foo Bar))))) |}]
;;

let rec get_keypath t path =
  match t, path with
  | All [], _ -> [ None ]
  | All trees, _ -> List.concat_map trees ~f:(fun tree -> get_keypath tree path)
  | _, [] -> [ Some t ]
  | Leaf _, _ :: _ -> [ None ]
  | Choose kvps, key :: rest ->
    (let%map.Option tree = List.Assoc.find kvps key ~equal:[%equal: string] in
     get_keypath tree rest)
    |> Option.value ~default:[ None ]
;;

let%expect_test "get_keypath" =
  let t = parse_sexp [%sexp { a = { b = "hello" } }] ~parse_leaf:Fn.id in
  print_s [%sexp (get_keypath t [ "a" ] : Sexp.t t option list)];
  [%expect {| (((Choose ((b (Leaf hello)))))) |}];
  print_s [%sexp (get_keypath t [ "a"; "b" ] : Sexp.t t option list)];
  [%expect {| (((Leaf hello))) |}];
  print_s [%sexp (get_keypath t [ "a"; "b"; "c" ] : Sexp.t t option list)];
  [%expect {| (()) |}];
  print_s [%sexp (get_keypath t [ "a"; "c" ] : Sexp.t t option list)];
  [%expect {| (()) |}]
;;

let%expect_test "get keypath traverses through lists" =
  let t =
    parse_sexp [%sexp { name = "a"; foo = [ { buy = 10; sell = 11 } ] }] ~parse_leaf:Fn.id
  in
  print_s [%sexp (get_keypath t [ "a" ] : Sexp.t t option list)];
  [%expect {| (()) |}];
  print_s [%sexp (get_keypath t [ "foo" ] : Sexp.t t option list)];
  [%expect {| (((Choose ((buy (Leaf 10)) (sell (Leaf 11)))))) |}];
  print_s [%sexp (get_keypath t [ "foo"; "buy" ] : Sexp.t t option list)];
  [%expect {| (((Leaf 10))) |}]
;;

let%expect_test "heterogeneous nested paths" =
  let t =
    parse_sexp
      ~parse_leaf:Fn.id
      [%sexp { name = "second"; foo = [ { buy = 10 }; { sell = 12 } ] }]
  in
  print_s [%sexp (get_keypath t [ "foo" ] : Sexp.t t option list)];
  [%expect {| (((Choose ((buy (Leaf 10))))) ((Choose ((sell (Leaf 12)))))) |}];
  print_s [%sexp (get_keypath t [ "foo"; "buy" ] : Sexp.t t option list)];
  [%expect {| (((Leaf 10)) ()) |}];
  print_s [%sexp (get_keypath t [ "foo"; "sell" ] : Sexp.t t option list)];
  [%expect {| (() ((Leaf 12))) |}]
;;
