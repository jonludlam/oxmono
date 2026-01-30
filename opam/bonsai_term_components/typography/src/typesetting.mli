open! Core

val typeset_line : max_width:int -> 'attr Text.t list -> 'attr Text.t list list

module For_testing : sig
  module Mutable_segment : sig
    type 'attr t =
      { chars : Uchar.t Vec.t
      ; attr : 'attr
      }
    [@@deriving sexp_of]

    val of_string : attr:'attr -> string -> 'attr t
  end

  module Chunk : sig
    type 'attr t =
      { segments : 'attr Text.t iarray
      ; total_width : int
      ; visual_width : int
      }
    [@@deriving sexp_of]
  end

  val chunkenize : 'attr Mutable_segment.t list -> 'attr Chunk.t list
end
