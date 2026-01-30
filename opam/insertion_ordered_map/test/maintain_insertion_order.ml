open! Core

let%test_unit "[add_exn], [set], [remove], [update], and [change] do not alter relative \
               order"
  =
  Quickcheck.test
    (List.gen_non_empty By_int.Accessors.quickcheck_generator)
    ~sexp_of:[%sexp_of: By_int.Accessors.t list]
    ~trials:100
    ~f:(fun operators ->
      let offset = 1_000 in
      (* Initial values are front-loaded with 1_000 initial elements to ensure [set] and
         [update] are not always adding elements. *)
      let by_int_init, by_int_simple_init =
        Common.create_permuted_canonical_and_simple ~size:offset
      in
      let insertion_ordered_map, simple =
        List.foldi
          operators
          ~init:(by_int_init, by_int_simple_init)
          ~f:(fun index (insertion_ordered_map, simple) operator ->
            let next_key = Int.(index + offset) in
            By_int.Accessors.apply_update
              operator
              ~insertion_ordered_map
              ~simple
              ~next_key
              ~data:next_key)
      in
      [%test_eq: (int * int) list]
        simple
        (Insertion_ordered_map.to_alist insertion_ordered_map))
;;

let%test_unit "[iter] follows insertion order" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple =
      Common.create_permuted_canonical_and_simple ~size:100
    in
    let counter = ref 0 in
    Insertion_ordered_map.iter insertion_ordered_map ~f:(fun data ->
      let (_ : int), simple_data = By_int.Simple.nth_exn simple !counter in
      [%test_eq: int] data simple_data;
      counter := !counter + 1))
;;

let%test_unit "[fold] follows insertion order" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let insertion_ordered_map, simple =
      Common.create_permuted_canonical_and_simple ~size:100
    in
    let counter = ref 0 in
    Insertion_ordered_map.fold
      insertion_ordered_map
      ~init:()
      ~f:(fun ~key:(_ : int) ~data (_ : unit) ->
        let (_ : int), simple_data = By_int.Simple.nth_exn simple !counter in
        [%test_eq: int] data simple_data;
        counter := !counter + 1))
;;

let%test_unit "[fold_right] follows insertion order" =
  Quickcheck.test Common.gen_int ~sexp_of:[%sexp_of: int] ~trials:100 ~f:(fun state ->
    Random.init state;
    let size = 100 in
    let insertion_ordered_map, simple =
      Common.create_permuted_canonical_and_simple ~size
    in
    let counter = ref 0 in
    Insertion_ordered_map.fold_right
      insertion_ordered_map
      ~init:()
      ~f:(fun ~key:(_ : int) ~data (_ : unit) ->
        let (_ : int), simple_data = By_int.Simple.nth_exn simple (size - 1 - !counter) in
        [%test_eq: int] data simple_data;
        counter := !counter + 1))
;;
