(* [%test_result] is used in place of [%test_eq] when equality is tested over
   [Semantic_equal.t]s because [Semantic_equal.sexp_of_t] does not exist. *)

open! Core

module%test [@name "serialization"] _ = struct
  let%test_unit "roundtrippable [sexp_of_t] and [t_of_sexp]" =
    Quickcheck.test
      Common.gen_alist_with_unique_keys
      ~sexp_of:[%sexp_of: (int * int) list]
      ~f:(fun alist ->
        let insertion_ordered_map = By_int.of_alist_exn alist in
        let roundtripped_insertion_ordered_map =
          insertion_ordered_map |> [%sexp_of: int By_int.t] |> [%of_sexp: int By_int.t]
        in
        [%test_result: bool]
          ([%equal: int By_int.Semantic_equal.t]
             insertion_ordered_map
             roundtripped_insertion_ordered_map)
          ~expect:true)
  ;;

  module T = struct
    type t = int By_int.t [@@deriving bin_io]
  end

  let of_ = Binable.of_string (module T)
  let to_ = Binable.to_string (module T)

  let%test_unit "roundtrippable []" =
    let insertion_ordered_map =
      By_int.of_alist_exn [ Int.max_value, Int.zero; Int.min_value, Int.zero ]
    in
    let roundtripped_insertion_ordered_map = insertion_ordered_map |> to_ |> of_ in
    [%test_result: bool]
      ([%equal: int By_int.Semantic_equal.t]
         insertion_ordered_map
         roundtripped_insertion_ordered_map)
      ~expect:true
  ;;

  (* [Insertion_ordered_map] changes shape with a [Bin_prot.Shape.Uuid.t]. *)

  let%expect_test _ =
    print_endline [%bin_digest: int By_int.Simple.t];
    [%expect {| 5ab148322655e56a6e20439dc5e9245d |}]
  ;;

  let%expect_test _ =
    print_endline [%bin_digest: int By_int.t];
    [%expect {| fd8e616be4e001584d0367264d7cf658 |}]
  ;;
end

let%test_unit "roundtrippable [of_alist_exn] and [to_alist]" =
  Quickcheck.test
    Common.gen_alist_with_unique_keys
    ~sexp_of:[%sexp_of: (int * int) list]
    ~f:(fun alist ->
      let roundtripped_alist =
        alist |> By_int.of_alist_exn |> Insertion_ordered_map.to_alist
      in
      [%test_eq: (int * int) list] alist roundtripped_alist)
;;

let%test_unit "roundtrippable [of_map] -> [to_map]" =
  Quickcheck.test
    Common.gen_alist_with_unique_keys
    ~sexp_of:[%sexp_of: (int * int) list]
    ~f:(fun alist ->
      let map = Map.of_alist_exn (module Int) alist in
      let roundtripped_map =
        map |> Insertion_ordered_map.of_map |> Insertion_ordered_map.to_map
      in
      [%test_eq: int Int.Map.t] map roundtripped_map)
;;

let%test_unit "roundtrippable [to_map] -> [of_map] iff insertion-order follows compare" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun state ->
    Random.init state;
    let size = Random.int_incl Int.zero 100 in
    let insertion_ordered_map, (_ : int By_int.Simple.t) =
      Common.create_sequential_canonical_and_simple ~size
    in
    let roundtripped_insertion_ordered_map =
      insertion_ordered_map
      |> Insertion_ordered_map.to_map
      |> Insertion_ordered_map.of_map
    in
    [%test_result: bool]
      ([%equal: int By_int.Semantic_equal.t]
         insertion_ordered_map
         roundtripped_insertion_ordered_map)
      ~expect:true)
;;

let%test_unit "failed roundtrippable [to_map] -> [of_map] iff insertion-order does not \
               follow compare"
  =
  let insertion_ordered_map =
    By_int.of_alist_exn [ Int.max_value, Int.zero; Int.min_value, Int.zero ]
  in
  let roundtripped_insertion_ordered_map =
    insertion_ordered_map |> Insertion_ordered_map.to_map |> Insertion_ordered_map.of_map
  in
  [%test_result: bool]
    ([%equal: int By_int.Semantic_equal.t]
       insertion_ordered_map
       roundtripped_insertion_ordered_map)
    ~expect:false
;;
