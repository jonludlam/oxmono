open! Core

(** [Testable_timeout] provides an API that looks like the browser's [setTimeout], but is
    fuelled by async, so the time source can be overriden in tests. *)

type handle

val set_timeout : Time_ns.Span.t -> f:(unit -> unit) -> handle
val cancel : handle -> unit

module For_running_tests : sig
  open Async_kernel

  val with_ : Time_source.t -> f:(unit -> unit Deferred.t) -> unit Deferred.t
end
