open! Core

module type T = sig
  type elt : float64
  type t

  include Float_u.Ref with type elt := elt and type t := t

  val create_none : unit -> t
  val set_none : local_ t -> unit [@@zero_alloc]
  val set_float_nan_as_none : local_ t -> Float_u.t -> unit [@@zero_alloc]
end
