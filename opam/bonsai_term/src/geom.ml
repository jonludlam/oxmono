open! Core

module Dimensions = struct
  type t =
    { height : int
    ; width : int
    }
  [@@deriving sexp, equal, compare]

  include functor Comparator.Make
end

module Position = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, equal, compare]

  include functor Comparator.Make
end

module Region = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    }
  [@@deriving sexp, equal, compare]

  include functor Comparator.Make

  let contains { x; y; width; height } (position : Position.t) =
    position.x >= x
    && position.y >= y
    && position.x < x + width
    && position.y < y + height
  ;;
end
