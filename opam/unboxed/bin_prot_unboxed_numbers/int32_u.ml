open! Base
open Bin_prot.Std

module Boxed = struct
  type t = int32 [@@deriving bin_io ~localize]
end

module T = struct
  type t = int32#

  let name = "int32#"

  external%template box : int32# @ m -> int32 @ m @@ portable = "%box_int32"
  [@@mode m = (global, local)]

  external unbox : int32 -> int32# @@ portable = "%unbox_int32"
end

include%template Binable_any.Of_binable [@kind bits32] (Boxed) (T)
