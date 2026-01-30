open! Base
open Bin_prot.Std

module Boxed = struct
  type t = float [@@deriving bin_io ~localize]
end

module T = struct
  type t = float#

  let name = "float#"

  external%template box : float# @ m -> float @ m @@ portable = "%box_float"
  [@@mode m = (global, local)]

  external unbox : float -> float# @@ portable = "%unbox_float"
end

include%template Binable_any.Of_binable [@kind float64] (Boxed) (T)
