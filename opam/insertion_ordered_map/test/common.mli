open! Core

val create_sequential_canonical_and_simple
  :  size:int
  -> int By_int.t * int By_int.Simple.t

val create_sequential_canonical : size:int -> int By_int.t

(** [create_permuted_canonical_and_simple] creates values with keys a permutation of
    [0, ..., size].

    This in contrast to [create_random_canonical_and_simple], which creates values with
    random keys but has no guarantee on the size of returned values. *)
val create_permuted_canonical_and_simple : size:int -> int By_int.t * int By_int.Simple.t

val create_random_canonical_and_simple : unit -> int By_int.t * int By_int.Simple.t
val gen_alist_with_unique_keys : (int * int) list Quickcheck.Generator.t
val gen_int : int Quickcheck.Generator.t
