(** Segment-based HTTP routing with Base_trie.

    Routes match against path segments (string lists) rather than raw bytes.
    Response callbacks use local headers for stack allocation.

    {2 Quick Start}

    {[
      open Httpz_server.Route

      let routes = of_list [
        get_ [] (fun _ctx respond -> html respond "Welcome!");
        get_ ["api"; "status"] (fun _ctx respond -> json respond {|{"ok":true}|});

        get ("users" / seg End) (fun (user_id, ()) _ctx respond ->
          html respond (sprintf "User: %s" user_id));

        get ("static" / tail) (fun path _ctx respond ->
          serve_file (String.concat "/" path) respond);
      ]
    ]}

    {2 Pattern Syntax}

    {[
      "users" / seg End           (* /users/:id *)
      "api" / "v1" / seg End      (* /api/v1/:resource *)
      "static" / tail             (* /static/** *)
      root                        (* / *)
    ]} *)

(** {1 Response Body} *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }

(** {1 Response Headers} *)

type resp_header = Httpz.Header_name.t * string

(** {1 Response Writing} *)

type respond = status:Httpz.Res.status -> headers:local_ resp_header list -> body -> unit
(** Response writer. Headers are [local_] for stack allocation. *)

(** {2 Response Helpers}

    All helpers take [local_ respond] to enable stack-allocated closures. *)

val html : local_ respond -> string -> unit
val json : local_ respond -> string -> unit
val xml : local_ respond -> string -> unit
val atom : local_ respond -> string -> unit
val plain : local_ respond -> string -> unit
val redirect : local_ respond -> status:Httpz.Res.status -> location:string -> unit
val not_found : local_ respond -> unit
val respond_string : local_ respond -> status:Httpz.Res.status -> ?headers:local_ resp_header list -> string -> unit
val stream : local_ respond -> status:Httpz.Res.status -> ?headers:local_ resp_header list -> ?length:int ->
  ((string -> unit) -> unit) -> unit

(** {1 Request Context} *)

type ctx
(** Request context providing access to path, query, and HTTP method. *)

val meth : ctx -> Httpz.Method.t
(** Get the HTTP method of the request. *)

val is_head : ctx -> bool
(** Returns [true] if this is a HEAD request. Use this to skip expensive
    body generation - the routing layer automatically matches HEAD requests
    to GET routes. *)

val path : ctx -> string
(** Get request path as string (e.g., "/users/123"). *)

val query_param : ctx -> string -> string option
(** Find first query parameter by name. *)

val query_params : ctx -> string -> string list
(** Find all query parameters by name. *)

val query : ctx -> (string * string) list
(** Get all query parameters. *)

(** {2 Lazy Response Helpers}

    These variants skip body generation for HEAD requests. The thunk is only
    called for non-HEAD requests. Use these when body generation is expensive. *)

val html_gen : ctx -> local_ respond -> (unit -> string) -> unit
val json_gen : ctx -> local_ respond -> (unit -> string) -> unit
val xml_gen : ctx -> local_ respond -> (unit -> string) -> unit
val atom_gen : ctx -> local_ respond -> (unit -> string) -> unit
val plain_gen : ctx -> local_ respond -> (unit -> string) -> unit

(** {1 Path Patterns} *)

type _ pat
(** Path pattern. Type parameter encodes captured values. *)

val root : unit pat
(** Match root path [/]. *)

val ( / ) : string -> 'a pat -> 'a pat
(** [prefix / rest] matches literal segment, then continues. *)

val lit : string -> 'a pat -> 'a pat
(** Same as [( / )]. *)

val seg : 'a pat -> (string * 'a) pat
(** Capture any segment, then continue. *)

val tail : string list pat
(** Capture all remaining segments. *)

(** {1 Header Requirements} *)

type _ hdr

val h0 : unit hdr
val ( +> ) : Httpz.Header_name.t -> 'a hdr -> (string option * 'a) hdr

(** {1 Handlers} *)

type ('a, 'h) handler = 'a -> 'h -> ctx -> local_ respond -> unit
(** Handler function with local respond for stack allocation. *)

(** {1 Routes} *)

type route

val get : 'a pat -> ('a -> ctx -> local_ respond -> unit) -> route
val post : 'a pat -> ('a -> ctx -> local_ respond -> unit) -> route
val put : 'a pat -> ('a -> ctx -> local_ respond -> unit) -> route
val delete : 'a pat -> ('a -> ctx -> local_ respond -> unit) -> route

val get_ : string list -> (ctx -> local_ respond -> unit) -> route
val post_ : string list -> (ctx -> local_ respond -> unit) -> route

val get_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val post_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val put_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val delete_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route

val get_h1 : 'a pat -> Httpz.Header_name.t -> ('a -> string option -> ctx -> local_ respond -> unit) -> route
val post_h1 : 'a pat -> Httpz.Header_name.t -> ('a -> string option -> ctx -> local_ respond -> unit) -> route

val route : Httpz.Method.t -> 'a pat -> 'h hdr -> ('a, 'h) handler -> route

(** {1 Route Collections} *)

type t

val empty : t
val of_list : route list -> t
val add : route -> t -> t

(** {1 Dispatch} *)

val dispatch :
  bytes ->
  meth:Httpz.Method.t ->
  target:Httpz.Target.t ->
  headers:local_ Httpz.Header.t list ->
  t ->
  respond:local_ respond ->
  bool
(** [dispatch buf ~meth ~target ~headers routes ~respond] dispatches a request.
    Returns [true] if a route matched. *)
