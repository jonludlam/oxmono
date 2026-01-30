(** Unboxed span into the buffer. *)

(** Span type - offset and length into buffer.
    Uses int16# internally since max buffer size is 32KB. *)
type t =
  #{ off : int16#
   ; len : int16#
   }

(** Convert int to int16#. *)
val of_int : int -> int16#

(** Convert int16# to int. *)
val to_int : int16# -> int

(** Create a span from int16# offset and length. *)
val make : off:int16# -> len:int16# -> t

(** Get offset as int16#. *)
val off16 : t -> int16#

(** Get length as int16#. *)
val len16 : t -> int16#

(** Get offset as int (for array indexing). *)
val off : t -> int

(** Get length as int (for comparisons with String.length etc). *)
val len : t -> int

(** Case-sensitive comparison with string. *)
val equal : local_ Base_bigstring.t -> t -> string -> bool

(** Case-insensitive comparison with string. *)
val equal_caseless : local_ Base_bigstring.t -> t -> string -> bool

(** Parse decimal integer from span. Returns [-1L] on error.
    Note: This does NOT check for overflow. Use [parse_int64_limited] for security. *)
val parse_int64 : local_ Base_bigstring.t -> t -> int64#

(** Parse decimal integer from span with overflow protection and maximum value limit.
    Returns unboxed tuple: [#(value, overflow_flag)]
    - value: parsed value or [-1L] if empty/invalid
    - overflow_flag: [true] if value exceeds [max_value] or has too many digits *)
val parse_int64_limited : local_ Base_bigstring.t -> t -> max_value:int64# -> #(int64# * bool)

(** Copy span to string. Allocates. *)
val to_string : local_ Base_bigstring.t -> t -> string

(** Copy span to bytes. Allocates. *)
val to_bytes : local_ Base_bigstring.t -> t -> bytes

(** Pretty-print span contents using buffer. *)
val pp_with_buf : local_ Base_bigstring.t -> Stdlib.Format.formatter -> t -> unit

(** Pretty-print span structure (offset and length). *)
val pp : Stdlib.Format.formatter -> t -> unit
