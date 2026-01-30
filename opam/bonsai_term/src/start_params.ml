open! Core
open Bonsai
open Async

type 'exit t =
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

let sanity_check_exn
  { dispose = _
  ; nosig = _
  ; mouse = _
  ; bpaste = _
  ; reader = _
  ; writer = _
  ; time_source = _
  ; optimize = _
  ; target_frames_per_second
  ; for_mocking = _
  ; app = _
  }
  =
  if target_frames_per_second < 1
  then
    raise_s
      [%message
        "Assertion failure: [target_frames_per_second < 1]"
          (target_frames_per_second : int)
          "please pick a value >= 1"]
;;

let create_exn
  ~dispose
  ~nosig
  ~mouse
  ~bpaste
  ~reader
  ~writer
  ~time_source
  ~for_mocking
  ~optimize
  ~target_frames_per_second
  ~app
  =
  let optimize = Option.value ~default:true optimize
  and target_frames_per_second = Option.value ~default:60 target_frames_per_second
  and time_source = Option.value_or_thunk ~default:Time_source.wall_clock time_source in
  let out =
    { dispose
    ; nosig
    ; mouse
    ; bpaste
    ; reader
    ; writer
    ; time_source
    ; for_mocking
    ; optimize
    ; target_frames_per_second
    ; app
    }
  in
  sanity_check_exn out;
  out
;;
