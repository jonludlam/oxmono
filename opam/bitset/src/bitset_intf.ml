open! Core

(** [Bitset] provides a space-efficient set of non-negative integer values. *)

module type S = sig @@ portable
  type -'perms t : mutable_data

  (** [create ~len] creates a bitset with at least the given initial length (in bits). The
      set is initially empty, and does not grow. *)
  val create : len:int -> [< read_write ] t

  val create_local : len:int -> local_ [< read_write ] t [@@zero_alloc]
  val capacity : local_ [> read ] t -> int [@@zero_alloc]

  (** [is_empty t] returns [true] iff [mem t i] returns false on every [i] *)
  val is_empty : local_ [> read ] t -> bool
  [@@zero_alloc]

  (** [add t i] adds [i] to the set. *)
  val add : local_ [> write ] t -> int -> unit
  [@@zero_alloc]

  val unsafe_add : local_ [> write ] t -> int -> unit [@@zero_alloc]

  (** [remove t i] removes [i] from set. *)
  val remove : local_ [> write ] t -> int -> unit
  [@@zero_alloc]

  val unsafe_remove : local_ [> write ] t -> int -> unit [@@zero_alloc]

  (** [assign t i x = if x then add t i else remove t i], but branch-free. *)
  val assign : local_ [> write ] t -> int -> bool -> unit
  [@@zero_alloc]

  val unsafe_assign : local_ [> write ] t -> int -> bool -> unit [@@zero_alloc]

  (** [mem t i] returns [true] iff [i] is in [t]. *)
  val mem : local_ [> read ] t -> int -> bool
  [@@zero_alloc]

  val unsafe_mem : local_ [> read ] t -> int -> bool [@@zero_alloc]

  (** [clear t] empties the set. *)
  val clear : local_ [> write ] t -> unit [@@zero_alloc]

  (** [set_all t] adds everything to the set. *)
  val set_all : local_ [> write ] t -> unit
  [@@zero_alloc]

  (** [union a b] combines two bitsets; it creates a new bitset where [i] is present iff
      [i] is in [a] or [b] *)
  val union : local_ [> read ] t -> local_ [> read ] t -> [< read_write ] t

  val union_local : local_ [> read ] t -> local_ [> read ] t -> local_ [< read_write ] t
  [@@zero_alloc]

  (** [union_into ~dst ~src] adds all elements in [src] into [dst]. All elements set in
      [src] must be below [capacity dst]. This function raises if that is not the case. *)
  val union_into : dst:local_ [> write ] t -> src:local_ [> read ] t -> unit
  [@@zero_alloc]

  (** [inter a b] intersects two bitsets; it creates a new bitset where [i] is present iff
      [i] is in [a] and [b] *)
  val inter : local_ [> read ] t -> local_ [> read ] t -> [< read_write ] t

  val inter_local : local_ [> read ] t -> local_ [> read ] t -> local_ [< read_write ] t
  [@@zero_alloc]

  val inter_into : dst:local_ [> write ] t -> src:local_ [> read ] t -> unit
  [@@zero_alloc]

  (** [is_inter_empty a b = is_empty (inter_local a b)], but skips the intermediate
      allocation.

      This function is optimized for small bitsets and minimizes branching. For large
      bitsets, note that the entire intersection is computed (no early exit for non-zero
      intersection). *)
  val is_inter_empty : local_ [> read ] t -> local_ [> read ] t -> bool
  [@@zero_alloc]

  (** [diff a b] finds the elements in one but not the other bitset; it creates a new
      bitset where [i] is present iff [i] is in [a] and not [b] *)
  val diff : local_ [> read ] t -> local_ [> read ] t -> [< read_write ] t

  val diff_local : local_ [> read ] t -> local_ [> read ] t -> local_ [< read_write ] t
  [@@zero_alloc]

  (** [remove_all ~dst ~src] removes all elements set in [src] from [dst]. It is the
      in-place version of [diff] *)
  val remove_all : dst:local_ [> write ] t -> src:local_ [> read ] t -> unit
  [@@zero_alloc]

  (** [complement t] creates a new bitset where [i] is present iff [i] is not in [t]. This
      operates only on [i]s from 0 to [capacity t] (which may be larger than the initial
      [len]) *)
  val complement : local_ [> read ] t -> [< read_write ] t

  val complement_local : local_ [> read ] t -> local_ [< read_write ] t [@@zero_alloc]
  val complement_inplace : local_ [> read_write ] t -> unit [@@zero_alloc]

  (** [is_subset t1 ~of_:t2] returns true iff [t1] is a subset of [t2]. *)
  val is_subset : local_ [> read ] t -> of_:local_ [> read ] t -> bool
  [@@zero_alloc]

  (** returns the number of members -- values of [i] for which [mem t i] is true *)
  val num_members : local_ [> read ] t -> int
  [@@zero_alloc]

  (** {v
 returns the number of members -- values of [i] for which [mem t i] is true --
      within the range defined by start and end

      Raises if start is outside of [0, capacity t) or end is outside of [0, capacity t].
      v} *)
  val num_members_in_range
    :  local_ [> read ] t
    -> start:local_ int Maybe_bound.t
    -> end_:local_ int Maybe_bound.t
    -> int
  [@@zero_alloc]

  (** returns the index of the first element set in [t] *)
  val first_member : local_ [> read ] t -> local_ int option
  [@@zero_alloc]

  (** [iter t ~f] calls [f] for all elements in set [t] *)
  val iter_set : local_ [> read ] t -> f:local_ (int -> unit) -> unit
  [@@zero_alloc]

  (** [fold_set_local t ~init ~f] returns [f] folded over all the elements in set [t] *)
  val fold_set_local
    :  [> read ] t @ local
    -> init:'acc @ local
    -> f:('acc @ local -> int -> 'acc @ local) @ local
    -> 'acc @ local
  [@@zero_alloc]

  (** [grow t ~new_len] creates a new set from [t] with capacity [new_len]. *)
  val grow : local_ [> read ] t -> new_len:int -> [< read_write ] t

  val grow_local : local_ [> read ] t -> new_len:int -> local_ [< read_write ] t
  [@@zero_alloc]

  (** [copy_and_truncate t ~new_len] creates a new set from [t] with capacity [new_len].
      Bits above [new_len] may be cleared. *)
  val copy_and_truncate : local_ [> read ] t -> new_len:int -> [< read_write ] t

  val copy_and_truncate_local
    :  local_ [> read ] t
    -> new_len:int
    -> local_ [< read_write ] t
  [@@zero_alloc]

  val copy : local_ [> read ] t -> [< read_write ] t
  val copy_local : local_ [> read ] t -> local_ [< read_write ] t [@@zero_alloc]

  (** Converts [t] into a string with length [capacity t], where each char is either '0'
      for an unset bit or '1' for a set bit. *)
  val to_string : local_ [> read ] t -> string

  (** Like [to_string], but returns a local [string]. *)
  val to_string_local : local_ [> read ] t -> local_ string
  [@@zero_alloc]

  (** Opposite of [to_string]. *)
  val of_string : local_ string -> [< read_write ] t

  (** Like [of_string], but returns a local [t]. *)
  val of_string_local : local_ string -> local_ [< read_write ] t
  [@@zero_alloc]

  val sexp_of_t : local_ [> read ] t -> Sexp.t
  val t_of_sexp : Sexp.t -> [< read_write ] t
  val quickcheck_generator : [< read_write ] t Quickcheck.Generator.t
end

module type S_plain = sig
  type t : mutable_data [@@deriving bin_io, globalize, compare ~localize, equal ~localize]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, stable_witness]
    end
  end

  include S with type 'perms t := t

  module As_bit_array : sig
    type nonrec t = t [@@deriving sexp]
  end
end

module type S_permissioned = sig
  type -'perms t : mutable_data [@@deriving globalize, compare ~localize, equal ~localize]

  module Stable : sig
    module V1 : sig
      type nonrec -'perms t = 'perms t [@@deriving bin_io]
    end
  end

  include S with type 'perms t := 'perms t

  module As_bit_array : sig
    type nonrec 'rw t = 'rw t [@@deriving sexp]
  end
end
