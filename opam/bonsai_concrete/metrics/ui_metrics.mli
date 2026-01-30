open! Core
module Histogram_bucket_definitions = Histogram_bucket_definitions
module Histogram = Histogram

(** This module is used for collecting metrics from the currently running Bonsai app. It
    uses a global store of values so that we don't need to thread a handle through all the
    places in Bonsai where we might want to record data.

    As a side effect, this module listens to the number of incremental nodes annotated by
    Bonsai. *)

module Over_time : sig
  module Collected_at : sig
    module Why : sig
      type t =
        | Bonsai_started
        | On_interval
        | Tab_hidden
        | Tab_shown
        | Page_was_closed
      [@@deriving sexp_of, string]
    end

    type t =
      { why : Why.t
      ; timestamp : Time_ns.t
      ; time_since_navigation_start : Time_ns.Span.t
      }
    [@@deriving sexp_of]

    val create : Why.t -> t
  end

  module Snapshot : sig
    type 'a t =
      { delta : 'a (** [delta = cumulative] when [reason = First_contentful_paint] *)
      ; cumulative : 'a
      ; collected_at : Collected_at.t
      }
    [@@deriving sexp_of]
  end

  module Cumulative_only_snapshot : sig
    type 'a t =
      { cumulative : 'a
      ; collected_at : Collected_at.t
      }
    [@@deriving sexp_of]

    val of_snapshot : 'a Snapshot.t -> 'a t
  end
end

module One_off : sig
  module Collected_at : sig
    type t =
      { timestamp : Time_ns.t
      ; time_since_navigation_start : Time_ns.Span.t
      ; was_backgrounded : bool
      }
    [@@deriving sexp_of]
  end
end

module Timing_histograms : sig
  module Kind : sig
    module Input : sig
      type t =
        | Telemetry_idle_callback
        (** Measures the amount of time that the app can be "idle". This will be measured
            when the browser is able to call into [requestIdleCallback]. *)
        | Bonsai_whole_frame_loop
        (** Total time that it takes for bonsai to render a frame from executing actions,
            stabilizing, vdom diffing, etc... This may be the most useful "global" frame
            time. *)
        | Bonsai_stabilization_update_visibility
        (** Time taken to stabilize bonsai graph nodes linked to layout changes *)
        | Bonsai_stabilization_clock
        (** Time taken to stabilize bonsai graph nodes linked to clock changes *)
        | Bonsai_stabilization_action
        (** Time taken to stabilize bonsai graph nodes linked to executing an action *)
        | Bonsai_stabilization_after_apply_actions
        (** Time taken to stabilize bonsai graph nodes after actions and stabilizations
            have changed graph inputs *)
        | Bonsai_update_visibility
        (** Time taken to recompute which parts of the DOM have had their layout changed *)
        | Bonsai_apply_action
        (** Time taken by bonsai to evaluate state machines (apply action) *)
        | Bonsai_diff_vdom
        (** Time taken by the virtual dom implementation to diff the changed nodes *)
        | Bonsai_patch_vdom
        (** Time taken by the virtual dom implementation to actually mutate the dom *)
        | Bonsai_display_handlers (** Time taken to trigger on_display edges *)
        | Browser_long_task
        (** Measures the time spent in a "long task" as defined by the browser (50ms or
            more). This is meassured using the [PerformanceLongTaskTiming] API. *)
        | Bonsai_start_of_frame_to_start_of_next_frame
        (** Time that occurs between the start of each frame. It will not be less than
            16ms on a 60hz refresh rate. This measure is also very representative that
            performance felt by users. *)
        | Bonsai_end_of_frame_to_start_of_next_frame
        (** Time between the end of a frame and a new frame. Contrarily to other measures,
            the larger this is, the better this will be. It will top at 16ms maximum at
            60hz refresh rate. *)
        | Metrics_count_dom_nodes
        (** At page close only: Counts the number of dom nodes at page close. *)
        | Bonsai_graph_application
        (** At startup only: Total time to construct the initial graph *)
        | Bonsai_preprocess (** At startup only: Total time to optimize the graph *)
        | Bonsai_gather (** At startup only: Total time to make the graph runnable *)
      [@@deriving string, sexp_of, equal, compare, enumerate]
    end

    module Aggregated : sig
      (** A [Kind.Aggregated.t] represents the union of some [Kind.Input.t]s, that we want
          to explcitly collect because aggregating the collected data is impractical. *)
      type t =
        | Bonsai_stabilization_all
        (** Aggregates all phases of the bonsai stabilization *)
      [@@deriving string, sexp_of, equal, compare, enumerate]
    end

    type t =
      | Input of Input.t
      | Aggregated of Aggregated.t
    [@@deriving string, sexp_of, equal, compare, enumerate]
  end

  val observe : Kind.Input.t -> Time_ns.Span.t -> unit

  module Snapshots : sig
    type t =
      | On_interval_foregrounded_only of
          Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
      | On_init_visibility_change_or_hide of
          { foregrounded : Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
          ; backgrounded : Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
          }
    [@@deriving sexp_of]
  end

  val take_snapshots : why:Over_time.Collected_at.Why.t -> (Kind.t * Snapshots.t) list
end

module Counters : sig
  module Kind : sig
    type t =
      (* Incr node counts *)
      | Incr_node_input
      | Incr_node_value
      | Incr_node_result
      | Incr_node_lifecycle
      | Incr_node_empty_lifecycle
      | Incr_node_model
      | Incr_node_model_and_input
      | Incr_node_switch_model
      | Incr_node_assoc_key
      | Incr_node_assoc_input
      | Incr_node_assoc_results
      | Incr_node_assoc_lifecycles
      | Incr_node_assoc_inputs
      | Incr_node_path
      | Incr_node_lifecycle_apply_action_pair
      (* Statistics *)
      | Incr_skipped_stabilizations
      (* Unexpected situations *)
      | Ui_time_source_went_backwards
      | Ui_time_source_and_async_time_source_out_of_sync
      | Bonsai_switch_action_dropped
      | Bonsai_leaf0_apply_action_got_dynamic_action
      | Bonsai_leaf1_apply_action_got_static_action
    [@@deriving string, sexp_of, equal, compare]

    include Comparator.S with type t := t
  end

  val observe : Kind.t -> unit

  module Snapshots : sig
    type t =
      { all : int Over_time.Snapshot.t
      ; foregrounded : int Over_time.Snapshot.t
      }
    [@@deriving sexp_of]
  end

  val take_snapshots : why:Over_time.Collected_at.Why.t -> (Kind.t * Snapshots.t) list
end

module One_off_timings : sig
  module Kind : sig
    type t =
      | Bonsai_graph_application
      | Bonsai_preprocess
      | Bonsai_gather
      | Incr_app_creation
      | First_stabilization
      | Mount_initial_dom
    [@@deriving sexp_of, equal, string, compare]

    include Comparator.S with type t := t
  end

  module Timing : sig
    type t =
      { kind : Kind.t
      ; value : Time_ns.Span.t
      ; collected_at : One_off.Collected_at.t
      }
    [@@deriving sexp_of]
  end

  val observe : Kind.t -> Time_ns.Span.t -> unit
  val new_values : unit -> Timing.t list
  val all_values : unit -> Timing.t list
end

module For_debugging_histograms : sig
  val collect_all_timings : bool ref
  val analyze_histogram_quantile_accuracy : unit -> unit
  val print_data : Timing_histograms.Kind.t -> unit
end

module For_testing : sig
  val clear : unit -> unit
  val set_document_is_hidden : bool -> unit
end

module Private : sig
  val set_get_time_since_navigation_start : (unit -> Time_ns.Span.t) -> unit
  val set_document_is_hidden : bool -> unit
  val num_backgrounding_changes : unit -> int
end
