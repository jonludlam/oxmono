open Async

(** This library helps debug a connection between two parties speaking a protocol by
    capturing the traffic that passes between them. *)

type angstrom_exit_status = (unit, string) Result.t

module Peer = struct
  type t =
    { name : string
    ; reader : Reader.t
    ; writer : Writer.t
    }
end

module type Protocol = sig
  type message

  val parser_ : message Angstrom.t
  val to_string : message -> string
end

module type S = sig
  module Peer = Peer

  type message

  (** Given a [Reader.t] and [Writer.t] connected to a peer, return a new [Reader.t] and
      [Writer.t] that represents the connection to the peer but that will run [f] on any
      messages that are read or written. *)
  val wrap_connection_to_peer
    :  Peer.t
    -> my_name:string
    -> f:([ `Sent | `Received ] -> message -> unit)
    -> (Reader.t
       * Writer.t
       * [ `Stopped_reading of angstrom_exit_status Deferred.t ]
       * [ `Stopped_writing of angstrom_exit_status Deferred.t ])
         Deferred.t

  (** Connect two peers together and listen to the messages that pass between them. Both
      readers and writers will be closed when either side closes the connection. *)
  val connect_peers_and_listen
    :  peer1:Peer.t
    -> peer2:Peer.t
    -> f:([ `Peer_1_to_2 | `Peer_2_to_1 ] -> message -> unit)
    -> ([ `Peer1 of angstrom_exit_status ] * [ `Peer2 of angstrom_exit_status ])
         Deferred.t
end

module type Man_in_the_middle_debugger = sig
  module type S = S

  module Peer = Peer
  module Make (Protocol : Protocol) : S with type message := Protocol.message
end
