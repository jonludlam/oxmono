open! Core
open Bonsai

(* NOTE: This module contains some helper functions for "things we want to keep in sync"
   inside of each [loop.ml] iteration. *)

module For_dimensions : sig
  type t

  val create : term:Term.t -> t
  val update : t -> unit
  val set : t -> Geom.Dimensions.t -> unit
  val value : t -> Geom.Dimensions.t Bonsai.t
end

module For_clock : sig
  type t = Bonsai.Time_source.t

  val create : Async.Time_source.t -> t
  val advance_to : t -> Time_ns.t -> unit
end

module For_exit : sig
  type 'exit t

  module Status : sig
    type 'exit t =
      | Not_yet_exited
      | Exited of 'exit
  end

  val create : unit -> 'exit t
  val exit_status : 'exit t -> 'exit Status.t
  val exit : 'exit t -> 'exit -> unit
  val exit_effect : 'exit t -> 'exit -> unit Effect.t
  val warn_if_already_exited : here:[%call_pos] -> _ t -> unit
end
