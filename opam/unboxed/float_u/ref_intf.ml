open! Core

module type T = sig
  type elt : float64
  type t

  val create : elt -> t
  val create_local : elt -> local_ t [@@zero_alloc]
  val create_zero : unit -> t
  val get : local_ t -> elt [@@zero_alloc]
  val set : local_ t -> elt -> unit [@@zero_alloc]
  val add : local_ t -> elt -> unit [@@zero_alloc]

  module O : sig
    val ref : elt -> local_ t [@@zero_alloc]
    val ( ! ) : local_ t -> elt [@@zero_alloc]
    val ( := ) : local_ t -> elt -> unit [@@zero_alloc]
    val ( += ) : local_ t -> elt -> unit [@@zero_alloc]
  end
end
