open! Base

(** Specification of horizontal/vertical VGA video timing. *)
module Timing : sig
  type t =
    { sync : int
    ; back_porch : int
    ; active : int
    ; front_porch : int
    }
  [@@deriving sexp_of]
end

(** VGA specification - nominal pixel clock rate and the horizontal and vertical video
    timings. *)
module Spec : sig
  type t =
    { clock_hz : int
    ; horizontal_timing : Timing.t
    ; vertical_timing : Timing.t
    }
  [@@deriving sexp_of]

  val testing : t
  val testing_no_scaling : t
  val testing_4x4y : t
  val testing_2x4y : t
  val testing_4x2y : t
  val testing_3x5y : t
  val t640x480_60hz : t
  val t800x600_60hz : t
  val t1024x768_60hz : t
  val t1280x720_60hz : t
  val t1920x1080_30hz : t
  val t1920x1080_60hz : t
end

module Scan : sig
  open Hardcaml

  module O : sig
    type 'a t =
      { first : 'a
      (** Pulsed at the beginning of sync, back porch, active video and front porch. *)
      ; is_sync : 'a (** High during sync *)
      ; is_back_porch : 'a (** High during back porch *)
      ; is_active : 'a (** High during active video *)
      ; is_front_porch : 'a (** High during front porch *)
      ; last : 'a
      (** Pulsed at the beginning of sync, back porch, active video and front porch. *)
      ; counter : 'a
      (** Raw counter - can be used to derive row/column indices during active video *)
      }
    [@@deriving hardcaml]
  end

  (** Create a statemachine which implements horizonatal or vertical scan timing from a
      [Timing.t] specification. *)
  val create : Scope.t -> Signal.Reg_spec.t -> enable:Signal.t -> Timing.t -> Signal.t O.t
end
