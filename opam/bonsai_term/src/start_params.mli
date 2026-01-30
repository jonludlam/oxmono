open! Core
open Bonsai
open Async

type 'exit t = private
  { dispose : bool option
  ; nosig : bool option
  ; mouse : bool option
  ; bpaste : bool option
  ; optimize : bool
  ; target_frames_per_second : int
  ; reader : Reader.t option
  ; writer : Writer.t option
  ; time_source : Time_source.t
  ; for_mocking : Notty_async.For_mocking.t option
  ; app :
      exit:('exit -> unit Effect.t)
      -> dimensions:Geom.Dimensions.t Bonsai.t
      -> local_ Bonsai.graph
      -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  }

val create_exn
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
  -> app:
       (exit:('exit -> unit Effect.t)
        -> dimensions:Geom.Dimensions.t Bonsai.t
        -> local_ Bonsai.graph
        -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t)
  -> 'exit t
