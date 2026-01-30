open! Core

(** This API allows the user to associate arbitrary data with a [view] and then look up
    that data even after the view has been composed with other views. Additionally, if the
    lookup succeeds, you'll retrieve the _current_ location of the view that has been
    tagged, accounting for any layout that has been performed on the view. *)
module type For_view = sig
  (* The type of the view *)
  type view

  (** This type represents an id that can be used to set and lookup values of arbitrary
      types on a view. *)
  type ('key, 'data) t

  (** sexp a key *)
  val sexp_of_key : ('key, _) t -> 'key -> Sexp.t

  (** Create a tag id, allowing you to tag a [view] with arbitrary key/value pairs. Each
      tag id is unique, meaning that you need to use the same one to lookup a value that
      you previously set.

      - When the tagged view is transformed by being included in a view composition, the
        [transform_regions] callback is invoked, allowing you to process the data as you
        see fit.
      - If the multiple views are tagged with the same key, the [reduce] function is
        called on both pairs of data and location, allowing you to choose how to combine
        them. *)
  val create
    :  here:[%call_pos]
    -> ('key, _) Comparator.Module.t
    -> transform_regions:('data -> (Geom.Region.t -> Geom.Region.t) -> 'data)
    -> reduce:('data -> 'data -> 'data)
    -> ('key, 'data) t

  (** Associate a k/v pair to a view *)
  val mark : view -> id:('key, 'data) t -> key:'key -> f:(Geom.Region.t -> 'data) -> view

  (** Attempt to retrieve some data from the view, along with the current location of the
      tagged item. *)
  val find : view -> id:('key, 'data) t -> 'key -> 'data option

  (** Returns [true] if the view has been marked with [key] *)
  val mem : view -> id:('key, 'data) t -> 'key -> bool

  (** Returns all the keys that have been tagged on a given view *)
  val keys : view -> id:('key, _) t -> 'key list

  (** Removes a tagged key from the view *)
  val remove : view -> id:('key, _) t -> key:'key -> view

  (** Removes all the tags from this view for the given tag id. *)
  val remove_all : view -> id:_ t -> view
end

module type Tag = sig
  module type For_view = For_view

  module Id : sig
    type ('key, 'data) t

    val create
      :  here:[%call_pos]
      -> ('key, _) Comparator.Module.t
      -> transform_regions:('data -> (Geom.Region.t -> Geom.Region.t) -> 'data)
      -> reduce:('data -> 'data -> 'data)
      -> ('key, 'data) t

    val sexp_of_key : ('key, _) t -> 'key -> Sexp.t
  end

  type t

  val empty : t
  val is_empty : t -> bool
  val set : t -> ('key, 'data) Id.t -> key:'key -> data:'data -> t
  val get : t -> ('key, 'data) Id.t -> 'key -> 'data option
  val mem : t -> ('key, _) Id.t -> 'key -> bool
  val remove : t -> ('key, _) Id.t -> 'key -> t
  val merge : t -> t -> t
  val transform_regions : t -> f:(Geom.Region.t -> Geom.Region.t) -> t
  val keys : t -> ('key, _) Id.t -> 'key list
  val remove_all : t -> _ Id.t -> t
end
