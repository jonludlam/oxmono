(* Module that produces a UDP packet (from ethernet header to the end of the UDP data
   field) with the following fixed data:
   AAAA_BBBB_CCCC_1111_2222_AAAA_BBBB_CCCC_1111_2222_0000_0000_0000_0000_0000 (hex)

   This UDP packet is generate on a counter (every 2^19 cycles) and the FPGA and host mac
   addresses, ip addresses and ports are configurable.
*)

open! Base
open! Hardcaml
open! Ethernet_types

module type Config = sig
  type 'a t =
    { fpga_mac : 'a
    ; fpga_ip : 'a
    ; fpga_port : 'a
    ; host_mac : 'a
    ; host_ip : 'a
    ; host_port : 'a
    ; ip_header_checksum : 'a
    }
  [@@deriving hardcaml]
end

module type Udp_packet_generator = sig
  module Config : Config

  module I : sig
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; config : 'a Config.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { axi_tx : 'a Ethernet.Axi32.Source.t } [@@deriving hardcaml]
  end

  val data_string : string
  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t
end
