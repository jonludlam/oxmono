open! Core
open Bonsai
open Async

type common_app_fn :=
  dimensions:Geom.Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

type 'ret common_start_args :=
  ?dispose:bool
  -> ?nosig:bool
  -> ?mouse:bool
  -> ?bpaste:bool
  -> ?reader:Reader.t
  -> ?writer:Writer.t
  -> ?time_source:Time_source.t
  -> ?optimize:bool
  -> ?target_frames_per_second:int
  -> ?for_mocking:Notty_async.For_mocking.t
  -> 'ret

val start : (common_app_fn -> unit Deferred.Or_error.t) common_start_args

val start_with_exit
  : ((exit:('exit -> unit Effect.t) -> common_app_fn) -> 'exit Deferred.Or_error.t)
      common_start_args

module For_testing : sig
  val make_app_exit_on_ctrlc
    :  common_app_fn
    -> (exit:(unit -> unit Effect.t) -> common_app_fn)

  val with_driver
    :  dispose:bool option
    -> nosig:bool option
    -> mouse:bool option
    -> bpaste:bool option
    -> reader:Reader.t option
    -> writer:Writer.t option
    -> time_source:Time_source.t option
    -> for_mocking:Notty_async.For_mocking.t option
    -> optimize:bool option
    -> target_frames_per_second:int option
    -> (exit:('exit -> unit Effect.t)
        -> dimensions:Geom.Dimensions.t Bonsai.t
        -> local_ Bonsai.graph
        -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t)
    -> ('exit Driver.t -> 'a Deferred.Or_error.t)
    -> 'a Deferred.Or_error.t
end
