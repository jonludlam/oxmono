open! Base
open Bin_prot.Std

module Boxed = struct
  type t = nativeint [@@deriving bin_io ~localize]
end

module T = struct
  type t = nativeint#

  let name = "nativeint#"

  external%template box : nativeint# @ m -> nativeint @ m @@ portable = "%box_nativeint"
  [@@mode m = (global, local)]

  external unbox : nativeint -> nativeint# @@ portable = "%unbox_nativeint"
end

include%template Binable_any.Of_binable [@kind word] (Boxed) (T)
