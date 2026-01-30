open! Base
open Bin_prot.Std

module Boxed = struct
  type t = int64 [@@deriving bin_io ~localize]
end

module T = struct
  type t = int64#

  let name = "int64#"

  external%template box : int64# @ m -> int64 @ m @@ portable = "%box_int64"
  [@@mode m = (global, local)]

  external unbox : int64 -> int64# @@ portable = "%unbox_int64"
end

include%template Binable_any.Of_binable [@kind bits64] (Boxed) (T)
