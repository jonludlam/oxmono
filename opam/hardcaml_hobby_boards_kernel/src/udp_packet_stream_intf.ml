(** Module that takes in an ethernet data stream that contains data from the start of the
    ethernet header to the end of the FCS and:

    - filters incoming traffic for UDP packets
    - swaps the source and destination MAC, IP and port fields
    - strips the FCS
    - outptus the result as 32-bit axi stream

    This module is useful to create a loopback UDP data stream that can be forwarded to
    the ethernet.tx module to send a loopback packet *)

open! Base
open! Hardcaml
open! Ethernet_types

module type Config = sig
  type 'a t = { dst_mac : 'a } [@@deriving hardcaml]
end

module type Udp_packet_stream = sig
  module Config : Config

  module I : sig
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; config : 'a Config.t
      ; axi_rx : 'a Ethernet.Axi32.Source.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { axi_udp_packet : 'a Ethernet.Axi32.Source.t } [@@deriving hardcaml]
  end

  val create_internal : Scope.t -> Interface.Create_fn(I)(O).t
  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t
end
