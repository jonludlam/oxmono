open! Core
include Insertion_ordered_map.S_binable with module Key := Int

(** [Simple] is a simple, inefficient representation of [Insertion_ordered_map] that is
    easier to reason about. It is used to run property tests against
    [Insertion_ordered_map]. *)
module Simple : sig
  type 'a t = (int * 'a) list [@@deriving bin_io, compare]

  val empty : 'a t
  val add_exn : 'a t -> key:int -> data:'a -> 'a t
  val nth : 'a t -> int -> (int * 'a) option
  val nth_exn : 'a t -> int -> int * 'a
  val max_elt : 'a t -> (int * 'a) option
  val min_elt : 'a t -> (int * 'a) option
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
end

module Accessors : sig
  type 'a by_int := 'a t
  type t [@@deriving quickcheck, sexp_of]

  val apply_update
    :  t
    -> insertion_ordered_map:'a by_int
    -> simple:'a Simple.t
    -> next_key:int
    -> data:'a
    -> 'a by_int * 'a Simple.t
end
