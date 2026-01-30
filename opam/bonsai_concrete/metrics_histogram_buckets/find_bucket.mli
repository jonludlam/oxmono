open! Core

(*$
  open Core

  let signature fname typename =
    printf
      {|
        (** Precondition: array is sorted in strictly increasing order and is not empty.
      Returns the smallest index i such that [x <= array.(i)], or [Array.length array]
      if no such index exists. This function has a complexity of O(Array.length array). *)
        val %s : %s array -> %s -> int [@@zero_alloc]
        |}
      fname
      typename
      typename
  ;;

  List.iter
    [ "float", "float"; "int", "int"; "span", "Time_ns.Span.t" ]
    ~f:(fun (fname, typename) -> signature fname typename)
*)

(** Precondition: array is sorted in strictly increasing order and is not empty. Returns
    the smallest index i such that [x <= array.(i)], or [Array.length array] if no such
    index exists. This function has a complexity of O(Array.length array). *)
val float : float array -> float -> int
[@@zero_alloc]

(** Precondition: array is sorted in strictly increasing order and is not empty. Returns
    the smallest index i such that [x <= array.(i)], or [Array.length array] if no such
    index exists. This function has a complexity of O(Array.length array). *)
val int : int array -> int -> int
[@@zero_alloc]

(** Precondition: array is sorted in strictly increasing order and is not empty. Returns
    the smallest index i such that [x <= array.(i)], or [Array.length array] if no such
    index exists. This function has a complexity of O(Array.length array). *)
val span : Time_ns.Span.t array -> Time_ns.Span.t -> int
[@@zero_alloc]
(*$*)
