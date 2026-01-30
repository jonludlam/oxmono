(* [%test_result] is used in place of [%test_eq] when equality is tested over
   [Semantic_equal.t]s because [Semantic_equal.sexp_of_t] does not exist. *)

open! Core

let incr = Int.( + ) 1
let even x = [%equal: int] 0 (x mod 2)
let incr_even x = if even x then Some (incr x) else None

let%test_unit "[map]" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let mapped_insertion_ordered_map =
      Insertion_ordered_map.map insertion_ordered_map ~f:incr
    in
    let mapped_simple = simple |> By_int.Simple.map ~f:incr |> By_int.of_alist_exn in
    [%test_result: bool]
      ([%equal: int By_int.Semantic_equal.t] mapped_insertion_ordered_map mapped_simple)
      ~expect:true)
;;

let%test_unit "[filter]" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let filtered_insertion_ordered_map =
      Insertion_ordered_map.filter insertion_ordered_map ~f:even
    in
    let filtered_simple = simple |> By_int.Simple.filter ~f:even |> By_int.of_alist_exn in
    [%test_result: bool]
      ([%equal: int By_int.Semantic_equal.t]
         filtered_insertion_ordered_map
         filtered_simple)
      ~expect:true)
;;

let%test_unit "[filter_map]" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let filter_mapped_insertion_ordered_map =
      Insertion_ordered_map.filter_map insertion_ordered_map ~f:incr_even
    in
    let filter_mapped_simple =
      simple |> By_int.Simple.filter_map ~f:incr_even |> By_int.of_alist_exn
    in
    [%test_result: bool]
      ([%equal: int By_int.Semantic_equal.t]
         filter_mapped_insertion_ordered_map
         filter_mapped_simple)
      ~expect:true)
;;
