open! Core

module Dimensions : sig
  (** Represents the dimensions of some bounding box on the terminal grid. [width] and
      [height] must be non-negative. *)
  type t =
    { height : int
    ; width : int
    }
  [@@deriving sexp, equal, compare]

  include Comparator.S with type t := t
end

module Position : sig
  (** Represents a location on the terminal grid. [x] and [y] may be negative *)
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, equal, compare]

  include Comparator.S with type t := t
end

module Region : sig
  (** Represents a region of space on the terminal grid. [width] and [height] must be
      non-negative, but [x] and [y] may be negative. *)
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    }
  [@@deriving sexp, equal, compare]

  include Comparator.S with type t := t

  val contains : t -> Position.t -> bool
end
