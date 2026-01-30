open! Core
open Async

(** A [Driver.t] contains the "core" logic for driving a single cycle for the "bonsai
    loop."

    We have this intermediate abstraction so that we can "drive" bonsai term in:
    1. The "real way" when users call [Bonsai_term.start] (defined in [loop.ml])
    2. In "testing" when users use [bonsai_term_integration_test].

    importantly, because both [Bonsai_term.start] and the [bonsai_term_integration_test]
    handle use this to "drive", this allows for "integration tests" with minimal amounts
    of mocking. *)
type 'exit t

val create : 'exit Start_params.t -> 'exit t Deferred.t

(** [compute_first_frame] is a nuance. The "real" bonsai term calls it right after
    creation, but making it a separate function allows for the test handle to do things
    after [create], but before the first computation. *)
val compute_first_frame : 'exit t -> unit

(** [compute_frame] returns a "staged" deferred. You can think of this as a
    [Frame_outcome.t Deferred.t Deferred.t].

    [Deferred.bind]'ing on the outer effect will "complete" after the frame is "painted"
    (when Term.image is called). [bind]'ing again on the inner deferred will return after
    the frame is "finished" (after the next "event" (key, timer, win resize) is received
    and its events are processed by bonsai_term as specified in
    [docs/how_does_bonsai_term_work.md]). *)
val compute_frame
  :  'exit t
  -> [ `Frame_painted of [ `Frame_finished of 'exit Frame_outcome.t ] Deferred.t ]
       Deferred.t

(** Calls [Term.release] - needs to get called for cleanup purposes. *)
val release : 'exit t -> unit Deferred.t

(** [prev_view] returns the previous view that was drawn to the screen. Useful for expect
    test purposes. *)
val prev_view : 'exit t -> View.t option

val dimensions : 'exit t -> Geom.Dimensions.t
