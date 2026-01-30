(** Types file that defines the different fields in the ethernet, ipv4 and udp headers.
    Common network constants and the udp packet structure are also defined in this module. *)

open! Base
open! Hardcaml

module Ethernet_header : sig
  type 'a t =
    { dst_mac : 'a
    ; src_mac : 'a
    ; ethertype : 'a
    }
  [@@deriving hardcaml]
end

module Ipv4 : sig
  type 'a t =
    { version : 'a
    ; ihl : 'a
    ; dscp : 'a
    ; ecn : 'a
    ; length : 'a
    ; identification : 'a
    ; flags : 'a
    ; fragment_offset : 'a
    ; ttl : 'a
    ; protocol : 'a
    ; checksum : 'a
    ; src_ip : 'a
    ; dst_ip : 'a
    }
  [@@deriving hardcaml]
end

module Udp : sig
  type 'a t =
    { src_port : 'a
    ; dst_port : 'a
    ; length : 'a
    ; checksum : 'a
    }
  [@@deriving hardcaml]
end

val max_data_bits : int
val udp_protocol : int
val tcp_protocol : int
val ipv4_ethertype : int
val preamble_sfd_bits : int
val fcs_bits : int
val ifg_bits : int

module Packet : sig
  type 'a t =
    { ethernet_header : 'a Ethernet_header.t
    ; ipv4_header : 'a Ipv4.t
    ; udp_header : 'a Udp.t
    ; data : 'a
    }
  [@@deriving hardcaml]
end
