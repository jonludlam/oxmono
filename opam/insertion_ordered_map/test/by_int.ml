open! Core
include Insertion_ordered_map.Make_binable (Core.Int)

module Simple = struct
  type 'value t = (int * 'value) list [@@deriving bin_io, compare]

  let empty = []

  let add_exn t ~key ~data =
    List.iter t ~f:(fun (key', (_ : _)) -> assert (key <> key'));
    t @ [ key, data ]
  ;;

  let remove t key = List.filter t ~f:(fun (key', (_ : _)) -> key <> key')

  let mem t key =
    let t = List.map t ~f:fst in
    List.mem t key ~equal:Int.equal
  ;;

  let set t ~key ~data =
    if mem t key
    then
      List.map t ~f:(fun (key', data') -> if key = key' then key, data else key', data')
    else add_exn t ~key ~data
  ;;

  let update t key ~f =
    if mem t key
    then
      List.map t ~f:(fun (key', data') ->
        if key = key' then key, f (Some data') else key', data')
    else add_exn t ~key ~data:(f None)
  ;;

  let map t ~f = List.map t ~f:(fun (key, data) -> key, f data)
  let filter t ~f = List.filter t ~f:(fun ((_ : int), data) -> f data)

  let filter_map t ~f =
    List.filter_map t ~f:(fun (key, data) ->
      Option.map (f data) ~f:(fun data -> key, data))
  ;;

  let nth t n = List.nth t n

  let nth_exn t n =
    match nth t n with
    | None -> invalid_arg "n larger than size of [Simple.t]"
    | Some elt -> elt
  ;;

  let compare_tuple = [%compare: Int.t * _]
  let max_elt t = List.max_elt t ~compare:compare_tuple
  let min_elt t = List.min_elt t ~compare:compare_tuple
end

module Accessors = struct
  type t =
    | Add_exn
    | Remove
    | Set
    | Update
    | Change
  [@@deriving quickcheck, sexp_of]

  let apply_update t ~insertion_ordered_map ~simple ~next_key ~data =
    let existing_key = Random.int_incl Int.zero Int.(max (next_key - 1) zero) in
    match t with
    | Add_exn ->
      ( Insertion_ordered_map.add_exn insertion_ordered_map ~key:next_key ~data
      , Simple.add_exn simple ~key:next_key ~data )
    | Remove ->
      ( Insertion_ordered_map.remove insertion_ordered_map existing_key
      , Simple.remove simple existing_key )
    | Set ->
      ( Insertion_ordered_map.set insertion_ordered_map ~key:existing_key ~data
      , Simple.set simple ~key:existing_key ~data )
    | Update ->
      ( Insertion_ordered_map.update insertion_ordered_map existing_key ~f:(Fn.const data)
      , Simple.update simple existing_key ~f:(Fn.const data) )
    | Change ->
      ( Insertion_ordered_map.change
          insertion_ordered_map
          existing_key
          ~f:(Fn.const (Some data))
      , Simple.update simple existing_key ~f:(Fn.const data) )
  ;;
end
