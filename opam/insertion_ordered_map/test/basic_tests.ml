open! Core

let%test "[empty] is empty" = Insertion_ordered_map.is_empty By_int.empty
let%test "[] is empty" = Insertion_ordered_map.is_empty (By_int.of_alist_exn [])

let%test_unit "[singleton] is a singleton" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun int ->
    let singleton = By_int.singleton int int |> Insertion_ordered_map.to_alist in
    [%test_result: int] (List.length singleton) ~expect:1)
;;

let%test_unit "always [mem]" =
  let size = 10_000 in
  let insertion_ordered_map = Common.create_sequential_canonical ~size in
  Quickcheck.test
    (Int.gen_incl Int.zero (size - 1))
    ~sexp_of:[%sexp_of: int]
    ~f:(fun int ->
      [%test_result: bool]
        (Insertion_ordered_map.mem insertion_ordered_map int)
        ~expect:true)
;;

let%test_unit "never [mem]" =
  let insertion_ordered_map = Common.create_sequential_canonical ~size:0 in
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun int ->
    [%test_result: bool]
      (Insertion_ordered_map.mem insertion_ordered_map int)
      ~expect:false)
;;

let%test_unit "[min_elt]" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let insertion_ordered_map_min_elt =
      Insertion_ordered_map.min_elt insertion_ordered_map
    in
    let simple_min_elt = By_int.Simple.min_elt simple in
    [%test_eq: (int * int) option] insertion_ordered_map_min_elt simple_min_elt)
;;

let%test_unit "[max_elt]" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let insertion_ordered_map_max_elt =
      Insertion_ordered_map.max_elt insertion_ordered_map
    in
    let simple_max_elt = By_int.Simple.max_elt simple in
    [%test_eq: (int * int) option] insertion_ordered_map_max_elt simple_max_elt)
;;

let%test_unit "[nth]" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple = Common.create_random_canonical_and_simple () in
    let n = Random.int_incl 0 Int.max_value in
    let insertion_ordered_map_nth = Insertion_ordered_map.nth insertion_ordered_map n in
    let simple_nth = By_int.Simple.nth simple n in
    [%test_eq: (int * int) option] insertion_ordered_map_nth simple_nth)
;;

let%test_unit "[change] removes when [f] returns [None]" =
  let size = 10_000 in
  let insertion_ordered_map = Common.create_sequential_canonical ~size in
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~f:(fun int ->
    let insertion_ordered_map_changed =
      Insertion_ordered_map.change insertion_ordered_map int ~f:(Fn.const None)
    in
    let insertion_ordered_map_removed =
      Insertion_ordered_map.remove insertion_ordered_map int
    in
    [%test_result: bool]
      ([%equal: int By_int.Semantic_equal.t]
         insertion_ordered_map_changed
         insertion_ordered_map_removed)
      ~expect:true)
;;
