open! Base
open Float32.Util

module Boxed = struct
  type t = float32 [@@deriving bin_io ~localize]
end

module T = struct
  type t = float32#

  let name = "float32#"

  external%template box : float32# @ m -> float32 @ m @@ portable = "%box_float32"
  [@@mode m = (global, local)]

  external unbox : float32 -> float32# @@ portable = "%unbox_float32"
end

include%template Binable_any.Of_binable [@kind float32] (Boxed) (T)
