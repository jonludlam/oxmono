open! Core
include Tag_intf

module Key = struct
  type 'a t =
    | T :
        { type_id : 'a Type_equal.Id.t
        ; proof : ('a, 'key * 'data) Type_equal.t
        ; key_id : 'key Type_equal.Id.t
        ; data_id : 'data Type_equal.Id.t
        ; cmp_id : 'cmp Type_equal.Id.t
        ; comparator : ('key, 'cmp) Comparator.Module.t
        ; empty_map : ('key, 'data, 'cmp) Map.t
        ; reduce : 'data -> 'data -> 'data
        ; transform_regions : 'data -> (Geom.Region.t -> Geom.Region.t) -> 'data
        }
        -> 'a t

  let type_id (T { type_id; _ }) = type_id
  let sexp_of_t _ = sexp_of_opaque

  let sexp_of_key
    (type k v)
    (T { proof = T; comparator = (module Cmp); _ } : (k * v) t)
    (key : k)
    =
    Comparator.sexp_of_t Cmp.comparator key
  ;;

  let create
    (type k cmp)
    ~(here : [%call_pos])
    (module M : Comparator.S with type t = k and type comparator_witness = cmp)
    ~transform_regions
    ~reduce
    =
    let sexp_of_key = Comparator.sexp_of_t M.comparator in
    let type_id =
      Type_equal.Id.create
        ~name:(Source_code_position.to_string here)
        (Tuple2.sexp_of_t sexp_of_key sexp_of_opaque)
    in
    let key_id = Type_equal.Id.create ~name:"" sexp_of_key in
    let data_id = Type_equal.Id.create ~name:"" sexp_of_opaque in
    let cmp_id = Type_equal.Id.create ~name:"" sexp_of_opaque in
    T
      { type_id
      ; proof = T
      ; key_id
      ; data_id
      ; cmp_id
      ; comparator = (module M)
      ; empty_map = Map.empty (module M)
      ; reduce
      ; transform_regions
      }
  ;;
end

module Data = struct
  type 'a t =
    | T :
        { proof : ('a, 'key * 'data) Type_equal.t
        ; map : ('key, 'data, 'cmp) Map.t
        ; key_id : 'key Type_equal.Id.t
        ; cmp_id : 'cmp Type_equal.Id.t
        ; data_id : 'data Type_equal.Id.t
        ; reduce : 'data -> 'data -> 'data
        ; transform_regions : 'data -> (Geom.Region.t -> Geom.Region.t) -> 'data
        }
        -> 'a t

  let sexp_of_t _ = sexp_of_opaque
end

module Store = Univ_map.Make (Key) (Data)
module Merge = Univ_map.Merge (Key) (Data) (Data) (Data)

let empty = Store.empty
let is_empty = Store.is_empty

let set (type k v) store (id : (k * v) Key.t) ~(key : k) ~(data : v) =
  let (T { reduce; data_id = data_id_from_key; _ }) = id in
  Store.update store id ~f:(function
    | None ->
      let (Key.T { comparator; transform_regions; cmp_id; proof = T; key_id; data_id; _ })
        =
        id
      in
      let T = Type_equal.Id.same_witness_exn data_id data_id_from_key in
      Data.T
        { map = Map.singleton comparator key data
        ; cmp_id
        ; key_id
        ; data_id
        ; proof = T
        ; reduce
        ; transform_regions
        }
    | Some (Data.T ({ proof = T; data_id; _ } as t)) ->
      let map =
        Map.update t.map key ~f:(function
          | None -> data
          | Some prev_data ->
            let T = Type_equal.Id.same_witness_exn data_id data_id_from_key in
            reduce prev_data data)
      in
      Data.T { t with map })
;;

let remove (type k v) store (id : (k * v) Key.t) (key : k) =
  Store.change store id ~f:(function
    | None -> None
    | Some (Data.T ({ proof = T; _ } as t)) ->
      let map = Map.remove t.map key in
      if Map.is_empty map then None else Some (Data.T { t with map }))
;;

let remove_all store id = Store.remove store id

let get (type k v) store (id : (k * v) Key.t) (key : k) : v option =
  match Store.find store id with
  | None -> None
  | Some (T { map; proof = T; _ }) -> Map.find map key
;;

let mem (type k v) store (id : (k * v) Key.t) (key : k) : bool =
  match Store.find store id with
  | None -> false
  | Some (T { map; proof = T; _ }) -> Map.mem map key
;;

let keys (type k v) store (id : (k * v) Key.t) : k list =
  match Store.find store id with
  | None -> []
  | Some (T { map; proof = T; _ }) -> Map.keys map
;;

let merge a b =
  match is_empty a, is_empty b with
  | true, true -> empty
  | true, false -> b
  | false, true -> a
  | false, false ->
    let f ~key:_ = function
      | `Left data -> Some data
      | `Right data -> Some data
      | `Both
          ( Data.T
              { map = a
              ; data_id = data_id_a
              ; key_id = key_id_a
              ; cmp_id = cmp_id_a
              ; proof
              ; reduce
              ; transform_regions
              }
          , Data.T
              { map = b
              ; data_id = data_id_b
              ; key_id = key_id_b
              ; cmp_id = cmp_id_b
              ; proof = _
              ; reduce = _
              ; transform_regions = _
              } ) ->
        let T = Type_equal.Id.same_witness_exn cmp_id_a cmp_id_b in
        let T = Type_equal.Id.same_witness_exn key_id_a key_id_b in
        let T = Type_equal.Id.same_witness_exn data_id_a data_id_b in
        let map = Map.merge_skewed a b ~combine:(fun ~key:_ a b -> reduce a b) in
        Some
          (Data.T
             { map
             ; key_id = key_id_a
             ; data_id = data_id_a
             ; cmp_id = cmp_id_a
             ; proof
             ; reduce
             ; transform_regions
             })
    in
    Merge.merge a b ~f:{ Merge.f }
;;

let transform_regions store ~f =
  if is_empty store
  then store
  else
    Type_equal.conv Store.type_equal store
    |> Map.map ~f:(fun (Store.Packed.T (key, Data.T t)) ->
      Store.Packed.T
        ( key
        , Data.T
            { t with map = Map.map t.map ~f:(fun data -> t.transform_regions data f) } ))
    |> Type_equal.conv (Type_equal.sym Store.type_equal)
;;

module Id = struct
  type ('key, 'data) t = ('key * 'data) Key.t

  let create = Key.create
  let sexp_of_key = Key.sexp_of_key
end

type t = Store.t
