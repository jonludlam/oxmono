open! Core

let create =
  List.fold
    ~init:(By_int.empty, By_int.Simple.empty)
    ~f:(fun (insertion_ordered_map, simple) int ->
      ( Insertion_ordered_map.add_exn insertion_ordered_map ~key:int ~data:int
      , By_int.Simple.add_exn simple ~key:int ~data:int ))
;;

let create_sequential_canonical_and_simple ~size = create (List.init size ~f:Fn.id)

let create_permuted_canonical_and_simple ~size =
  create (List.init size ~f:Fn.id |> List.permute)
;;

let gen_int = Int.gen_incl Int.min_value Int.max_value

let gen_alist_with_unique_keys =
  let%map.Quickcheck.Generator list =
    List.gen_non_empty (Quickcheck.Generator.tuple2 gen_int gen_int)
  in
  List.stable_dedup ~compare:[%compare: Int.t * _] list
;;

let create_random_canonical_and_simple () =
  create (List.map (Quickcheck.random_value gen_alist_with_unique_keys) ~f:fst)
;;

let create_sequential_canonical ~size =
  List.fold
    (List.init size ~f:Fn.id)
    ~init:By_int.empty
    ~f:(fun insertion_ordered_map int ->
      Insertion_ordered_map.add_exn insertion_ordered_map ~key:int ~data:int)
;;
