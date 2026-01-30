open! Core

let%test_unit _ =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let insertion_ordered_map', simple' = Common.create_random_canonical_and_simple () in
    let compared_insertion_ordered_map =
      By_int.Semantic_compare.compare
        Int.compare
        insertion_ordered_map
        insertion_ordered_map'
    in
    let compared_simple = By_int.Simple.compare Int.compare simple simple' in
    [%test_eq: int] compared_insertion_ordered_map compared_simple)
;;
