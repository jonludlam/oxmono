open Base
open! Hardcaml

module Ethernet_header = struct
  type 'a t =
    { dst_mac : 'a [@bits 6 * 8]
    ; src_mac : 'a [@bits 6 * 8]
    ; ethertype : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module Ipv4 = struct
  type 'a t =
    { version : 'a [@bits 4]
    ; ihl : 'a [@bits 4]
    ; dscp : 'a [@bits 6]
    ; ecn : 'a [@bits 2]
    ; length : 'a [@bits 16]
    ; identification : 'a [@bits 16]
    ; flags : 'a [@bits 3]
    ; fragment_offset : 'a [@bits 13]
    ; ttl : 'a [@bits 8]
    ; protocol : 'a [@bits 8]
    ; checksum : 'a [@bits 16]
    ; src_ip : 'a [@bits 32]
    ; dst_ip : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module Udp = struct
  type 'a t =
    { src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; length : 'a [@bits 16]
    ; checksum : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

let max_data_bits = (1500 * 8) - Udp.sum_of_port_widths - Ipv4.sum_of_port_widths
let udp_protocol = 17
let tcp_protocol = 6
let ipv4_ethertype = 2048
let preamble_sfd_bits = 64
let fcs_bits = 32
let ifg_bits = 96

module Packet = struct
  type 'a t =
    { ethernet_header : 'a Ethernet_header.t
    ; ipv4_header : 'a Ipv4.t
    ; udp_header : 'a Udp.t
    ; data : 'a [@bits max_data_bits]
    }
  [@@deriving hardcaml]
end
