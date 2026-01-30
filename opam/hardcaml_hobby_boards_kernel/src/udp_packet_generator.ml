open Base
open Hardcaml
open Signal
open Ethernet_types
module Axi = Ethernet.Axi32

let word_size = Ethernet.Config.data_bits
let word_bytes = word_size / 8

let header_bits =
  Ethernet_header.sum_of_port_widths + Ipv4.sum_of_port_widths + Udp.sum_of_port_widths
;;

let header_cycles = header_bits / word_size
let udp_header_bytes = Udp.sum_of_port_widths / 8

let data_string =
  "240'hAAAA_BBBB_CCCC_1111_2222_AAAA_BBBB_CCCC_1111_2222_0000_0000_0000_0000_0000"
;;

let data = of_string data_string

module Config = struct
  type 'a t =
    { fpga_mac : 'a [@bits 6 * 8]
    ; fpga_ip : 'a [@bits 32]
    ; fpga_port : 'a [@bits 16]
    ; host_mac : 'a [@bits 6 * 8]
    ; host_ip : 'a [@bits 32]
    ; host_port : 'a [@bits 16]
    ; ip_header_checksum : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; config : 'a Config.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { axi_tx : 'a Axi.Source.t } [@@deriving hardcaml]
end

module States = struct
  type t =
    | Wait
    | Header
    | Data
  [@@deriving sexp_of, enumerate, compare ~localize]
end

module Var = Always.Variable

let create scope ({ clocking; config } : _ I.t) =
  let spec = Types.Clocking.to_spec clocking in
  let%hw_var state_counter = Var.reg spec ~width:Udp.port_widths.length in
  let ethernet_header = Ethernet_header.Of_always.reg spec in
  let ipv4_header = Ipv4.Of_always.reg spec in
  let%hw.Udp.Of_always udp_header = Udp.Of_always.reg spec in
  let payload_data = Var.reg spec ~width:(width data) in
  let wait_counter = Var.reg spec ~width:19 in
  let first_word = Var.reg spec ~width:1 in
  let header = Var.reg spec ~width:header_bits in
  let%hw.Always.State_machine state =
    Always.State_machine.create (module States) ~enable:vdd spec
  in
  let axi_tx = Axi.Source.Of_always.wire zero in
  Always.(
    compile
      [ Ethernet_header.Of_always.assign
          ethernet_header
          { dst_mac = config.host_mac
          ; src_mac = config.fpga_mac
          ; ethertype =
              of_int_trunc ~width:Ethernet_header.port_widths.ethertype ipv4_ethertype
          }
      ; Ipv4.Of_always.assign
          ipv4_header
          Ipv4.
            { (Of_signal.zero ()) with
              version = of_string "4'h4"
            ; ihl = of_string "4'h5"
            ; length =
                of_int_trunc
                  ~width:port_widths.length
                  ((sum_of_port_widths + Udp.sum_of_port_widths + width data) / 8)
            ; ttl = of_string "8'h40"
            ; protocol = of_int_trunc ~width:port_widths.protocol udp_protocol
            ; checksum = config.ip_header_checksum
            ; src_ip = config.fpga_ip
            ; dst_ip = config.host_ip
            }
      ; Udp.Of_always.assign
          udp_header
          { src_port = config.fpga_port
          ; dst_port = config.host_port
          ; length =
              of_int_trunc
                ~width:Udp.port_widths.length
                ((Udp.sum_of_port_widths + width data) / 8)
          ; checksum = zero Udp.port_widths.checksum
          }
      ; state_counter <-- state_counter.value +:. 1
      ; axi_tx.tstrb <-- zero 4 (* not used *)
      ; first_word <-- gnd
      ; state.switch
          [ ( Wait
            , [ payload_data <-- data
              ; wait_counter <-- wait_counter.value +:. 1
              ; header
                <-- Ethernet_header.Of_signal.pack
                      ~rev:true
                      (Ethernet_header.Of_always.value ethernet_header)
                    @: Ipv4.Of_signal.pack ~rev:true (Ipv4.Of_always.value ipv4_header)
                    @: Udp.Of_signal.pack ~rev:true (Udp.Of_always.value udp_header)
              ; when_
                  (all_bits_set wait_counter.value)
                  [ state_counter <--. 0; first_word <-- vdd; state.set_next Header ]
              ] )
          ; ( Header
            , [ axi_tx.tvalid <-- vdd
              ; axi_tx.tuser <-- gnd @: first_word.value
              ; axi_tx.tdata <-- sel_top header.value ~width:word_size
              ; header <-- sll header.value ~by:word_size
              ; when_
                  (state_counter.value ==:. header_cycles)
                  [ axi_tx.tdata
                    <-- sel_top header.value ~width:(word_size / 2)
                        @: sel_top payload_data.value ~width:(word_size / 2)
                  ; payload_data <-- sll payload_data.value ~by:(word_size / 2)
                  ; state_counter <--. 0
                  ; state.set_next Data
                  ]
              ] )
          ; ( Data
            , [ axi_tx.tvalid <-- vdd
              ; axi_tx.tdata <-- sel_top payload_data.value ~width:word_size
              ; payload_data <-- sll payload_data.value ~by:word_size
              ; when_
                  (sll state_counter.value ~by:2
                   ==: (Udp.Of_always.value udp_header).length
                       -:. (udp_header_bytes + word_bytes + (word_bytes / 2)))
                  [ axi_tx.tkeep <-- ones word_bytes
                  ; axi_tx.tlast <-- vdd
                  ; wait_counter <--. 0
                  ; state.set_next Wait
                  ]
              ] )
          ]
      ]);
  { O.axi_tx = Axi.Source.Of_always.value axi_tx }
;;

let hierarchical ?instance scope inputs =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"udp_packet_generator" ?instance create inputs
;;
