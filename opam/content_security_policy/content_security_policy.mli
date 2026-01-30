open! Core

module Source : sig
  type t =
    | Self
    | Unsafe_inline
    | Unsafe_eval
    | Wasm_unsafe_eval
    | Strict_dynamic
    | Report_sample
    | Inline_content of string
    | Host_or_scheme of string
  [@@deriving compare ~localize, sexp_of]
end

module Fetch_type : sig
  type t =
    | Connect
    | Default
    | Font
    | Frame
    | Img
    | Manifest
    | Media
    | Object
    | Prefetch
    | Script
    | Style
    | Worker
  [@@deriving compare ~localize, sexp_of]
end

(** The type representing a security policy *)
type t [@@deriving sexp_of]

(** Create a Content Security Policy, which can be enforced by using it as a response
    header. The default behavior for all of the optional parameters is to allow everything
    (which matches the behavior if you have no CSP). Thus a no-op policy can be created
    by:

    {[
      create ~insecure_requests:`Allow []
    ]}

    While a maximally restrictive policy (except for the [sandbox] directive; see below)
    can be created by:

    {[
      create
        ~base_uri:[]
        ~form_action:[]
        ~frame_ancestors:[]
        ~insecure_requests:`Block
        [ Default, [ None ] ]
    ]}

    The [sandbox] directive isn't exposed because we don't understand how to use it
    properly. Please contact the library owners if this would be useful to you. *)
val create
  :  ?report_uri:string
  -> ?base_uri:Source.t list
  -> ?form_action:Source.t list
  -> ?frame_ancestors:Source.t list
  -> insecure_requests:[ `Block | `Upgrade | `Allow ]
  -> (Fetch_type.t, Source.t list) List.Assoc.t
  -> t

val to_string : t -> string
val header_name : string
val header_name_report_only : string

module Monoid : sig
  (** {v
 The [empty] policy.  This policy is equivalent to the following:

      base-uri 'self';
      form-action 'self';
      frame-ancestors 'self';
      block-all-mixed-content;
      default-src 'self';
      v} *)
  val empty : t

  (** [combine] merges the two policies, producing a policy that accepts the set of
      requests that is the union of both inputs. *)
  val combine : t -> t -> t

  (** An operator alias for [combine]. *)
  val ( |.| ) : t -> t -> t

  (** [reduce] merges all the items in the list *)
  val reduce : t list -> t

  (** returns a Csp.t for use in our various web-server libraries. *)
  val finalize : t -> t

  (** Constructors *)

  val report_uri : string -> t
  val base_uri : string -> t
  val form_action : string -> t
  val frame_ancestor : string -> t
  val insecure_requests : [ `Allow | `Block | `Upgrade ] -> t
  val fetch : Fetch_type.t -> Source.t -> t
end
