open! Core
open! Async_rpc_kernel

module Query = struct
  module type Unstable = sig
    type t [@@deriving equal]
  end

  module type Stable = sig
    type t [@@deriving bin_io, stable_witness]
  end

  module type Conv = sig
    module Unstable : Unstable
    module Stable : Stable

    val to_stable : Unstable.t -> Stable.t
    val of_stable : Stable.t -> Unstable.t
  end

  module type S = sig
    type t [@@deriving equal, bin_io]
  end
end

module Response = struct
  module type Unstable = sig
    (** This is like [Polling_state_rpc.Response] but the types aren't binable *)
    type t

    module Update : sig
      type t [@@deriving sexp_of]
    end

    val diffs : from:t -> to_:t -> Update.t
    val update : t -> Update.t -> t
  end

  module type Stable = sig
    type t [@@deriving bin_io, stable_witness]

    module Update : sig
      type t [@@deriving bin_io, stable_witness]
    end
  end

  module type Conv = sig
    module Unstable : Unstable
    module Stable : Stable

    val to_stable : Unstable.t -> Stable.t
    val of_stable : Stable.t -> Unstable.t

    module Update : sig
      val to_stable : Unstable.Update.t -> Stable.Update.t
      val of_stable : Stable.Update.t -> Unstable.Update.t
    end
  end

  module type S = sig
    module Unstable : Unstable

    include
      Polling_state_rpc.Response
      with type t = Unstable.t
       and type Update.t = Unstable.Update.t
  end
end

module type Versioned_polling_state_rpc = sig
  module Query = Query
  module Response = Response

  module Make_stable_query
      (Unstable : Query.Unstable)
      (Stable : Query.Stable)
      (Conv : Query.Conv with module Unstable := Unstable and module Stable := Stable) :
    Query.S with type t = Unstable.t

  (** Here is how [Make_stable_response] works, from the client's perspective:

      {v
      ┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
      ╎                                Unstable                                   ╎
      ╎                                                                           ╎
      ╎ ┌──────────┐     ┌──────┐     ┌───────────┐     ┌──────┐     ┌──────────┐ ╎
      ╎ │ Response │ ──▶ │ Diff │ ──▶ │ Response  │ ──▶ │ Diff │ ──▶ │ Response │ ╎
      ╎ └──────────┘     └──────┘     └───────────┘     └──────┘     └──────────┘ ╎
      ╎                                                                           ╎
      └−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
              ▲             ▲                              ▲
              │             │                              │
              │             │                              │
      ┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
      ╎                                Stable (V1)                                ╎
      ╎                                                                           ╎
      ╎ ┌──────────┐     ┌──────┐                       ┌──────┐                  ╎
      ╎ │ Response │     │ Diff │                       │ Diff │                  ╎
      ╎ └──────────┘     └──────┘                       └──────┘                  ╎
      ╎                                                                           ╎
      └−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
      v}

      Importantly, stable responses and diffs are what get sent over the wire, but they
      are immediately converted to unstable responses and diffs before applying the diffs.
      This performs well when the responses are large relative to the diffs (which is
      expected to be the common case). Small diffs are sent over the wire, converted to
      the unstable diff type, and then applied to the previous (possibly very large)
      response. *)
  module Make_stable_response
      (Unstable : Response.Unstable)
      (Stable : Response.Stable)
      (Conv : Response.Conv with module Unstable := Unstable and module Stable := Stable) :
    Response.S with module Unstable := Unstable

  module Client : sig
    (** A [('query, 'response) caller] represents a nonempty list of creator functions,
        each representing a single Polling_state_rpc.t. *)
    type ('query, 'response) caller =
      (?initial_query:'query -> unit -> ('query, 'response) Polling_state_rpc.Client.t)
        Babel.Caller.t

    (** Create a [caller] from a single Polling_state_rpc.t. Callers can be combined in
        the typical babel way, e.g. using [Babel.Caller.of_list_decreasing_preference]. *)
    val create_caller
      :  ('query, 'response) Polling_state_rpc.t
      -> ('query, 'response) caller

    (** This function will use the menu embedded in [Versioned_rpc.Connection_with_menu]
        to choose the most up-to-date Polling_state_rpc.t that both you and the server
        know and will use that Polling_state_rpc.t under the hood. *)
    val negotiate_client
      :  ('query, 'response) caller
      -> ?initial_query:'query
      -> Versioned_rpc.Connection_with_menu.t
      -> ('query, 'response) Polling_state_rpc.Client.t Or_error.t
  end
end
