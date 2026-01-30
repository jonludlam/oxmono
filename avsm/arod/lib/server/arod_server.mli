(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz + Eio server adapter for arod routes

    This module adapts the framework-agnostic {!Arod.Route} abstraction to
    work with httpz for HTTP parsing and Eio for async I/O. It provides
    a zero-allocation HTTP parser with Eio-based connection handling. *)

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  config:Arod.Config.t ->
  ?memo_cache:Arod.Route.Response.t Arod.Memo.t ->
  ?memoized_paths:string list ->
  Arod.Route.Routes.t ->
  unit
(** [run ~sw ~net ~config ?memo_cache ?memoized_paths routes] starts the
    httpz + Eio server with the given routes.

    @param sw Eio switch for managing server lifecycle.
    @param net Eio network for creating sockets.
    @param config Server configuration with host, port, and paths.
    @param memo_cache Optional memoization cache for caching responses.
    @param memoized_paths List of path prefixes to memoize (default:
      ["/feeds/"; "/sitemap"; "/perma."; "/bushel/graph.json"]).
    @raise exn on server errors (Eio propagates exceptions) *)
