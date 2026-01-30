open! Core
open! Async_rpc_kernel
include Versioned_polling_state_rpc_intf

module Make_stable_query
    (Unstable : Query.Unstable)
    (Stable : Query.Stable)
    (Conv : Query.Conv with module Unstable := Unstable and module Stable := Stable) :
  Query.S with type t = Unstable.t = struct
  type t = Unstable.t [@@deriving equal]

  include
    Binable.Of_binable_with_uuid
      (Stable)
      (struct
        type t = Unstable.t

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string {|8266b996-7a30-436c-b045-00c02f7f3db7|}
        ;;

        let to_binable = Conv.to_stable
        let of_binable = Conv.of_stable
      end)
end

module Make_stable_response
    (Unstable : Response.Unstable)
    (Stable : Response.Stable)
    (Conv : Response.Conv with module Unstable := Unstable and module Stable := Stable) :
  Response.S with module Unstable := Unstable = struct
  type t = Unstable.t

  include
    Binable.Of_binable_with_uuid
      (Stable)
      (struct
        type t = Unstable.t

        let to_binable = Conv.to_stable
        let of_binable = Conv.of_stable

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string {|3e367da4-b851-422a-b4b0-6a1833eb941c|}
        ;;
      end)

  module Update = struct
    type t = Unstable.Update.t [@@deriving sexp_of]

    include
      Binable.Of_binable_with_uuid
        (Stable.Update)
        (struct
          type t = Unstable.Update.t

          let to_binable = Conv.Update.to_stable
          let of_binable = Conv.Update.of_stable

          let caller_identity =
            Bin_prot.Shape.Uuid.of_string {|db6dae4d-cb00-4727-ab1b-f0f34b37945c|}
          ;;
        end)
  end

  let diffs = Unstable.diffs
  let update = Unstable.update
end

module Client = struct
  type ('query, 'response) creator =
    ?initial_query:'query -> unit -> ('query, 'response) Polling_state_rpc.Client.t

  type ('query, 'response) caller = ('query, 'response) creator Babel.Caller.t

  let create_caller rpc =
    Babel.Caller.Expert.return
      (Polling_state_rpc.babel_generic_rpc rpc)
      (fun ?initial_query () -> Polling_state_rpc.Client.create ?initial_query rpc)
  ;;

  let negotiate_client caller ?initial_query conn =
    let menu = Versioned_rpc.Connection_with_menu.menu conn in
    let%map.Or_error f = Babel.Caller.to_dispatch_fun caller menu in
    let conn = Versioned_rpc.Connection_with_menu.connection conn in
    let creator = f conn in
    creator ?initial_query ()
  ;;
end
