open Base
open Hardcaml
open Signal
open Ethernet_types
module Var = Always.Variable
module Axi = Ethernet.Axi32
module Strip_fcs_32 = Ethernet_utils.Strip_fcs.Make (Ethernet.Config)
module Swap_addressing = Ethernet_utils.Swap_addressing

let word_size = Ethernet.Config.data_bits
let ethernet_header_cycles = (Ethernet_header.sum_of_port_widths / word_size) + 1
let ipv4_header_cycles = Ipv4.sum_of_port_widths / word_size
let udp_header_cycles = Udp.sum_of_port_widths / word_size
let pipeline_n = ethernet_header_cycles + ipv4_header_cycles + 1

let max_cycles =
  Int.max ethernet_header_cycles ipv4_header_cycles |> Int.max udp_header_cycles
;;

let log_max_cycles = Int.ceil_log2 max_cycles

module Config = struct
  type 'a t = { dst_mac : 'a [@bits Ethernet_header.port_widths.dst_mac] }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Types.Clocking.t
    ; config : 'a Config.t
    ; axi_rx : 'a Axi.Source.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { axi_udp_packet : 'a Axi.Source.t } [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Ethernet_header
    | Ipv4_Header
    | Udp_Header
    | Data
  [@@deriving sexp_of, enumerate, compare ~localize]
end

let create_internal scope ({ clocking; config; axi_rx } : _ I.t) =
  let spec = Types.Clocking.to_spec clocking in
  let%hw_var state_counter = Var.reg spec ~width:log_max_cycles in
  let%hw.Ethernet_header.Of_always ethernet_header = Ethernet_header.Of_always.reg spec in
  let%hw.Ethernet_header.Of_always ethernet_header_next =
    Ethernet_header.Of_always.wire zero
  in
  let%hw.Ipv4.Of_always ipv4_header = Ipv4.Of_always.reg spec in
  let%hw.Ipv4.Of_always ipv4_header_next = Ipv4.Of_always.wire zero in
  let udp_header = Udp.Of_always.reg spec in
  let%hw.Udp.Of_always udp_header_next = Udp.Of_always.wire zero in
  let%hw rx_tvalid_delayed = reg spec axi_rx.tvalid in
  let%hw rx_tdata_delayed = reg ~enable:axi_rx.tvalid spec axi_rx.tdata in
  let%hw start_of_frame = axi_rx.tvalid &: lsb axi_rx.tuser in
  let%hw end_of_frame = axi_rx.tvalid &: lsb axi_rx.tlast in
  let%hw.Ethernet_header.Of_signal ( ethernet_header_next_value
                                   , ethernet_header_next_with_split_data )
    =
    let module Shift_in = Ethernet_utils.Shift_in_data (Ethernet_header) in
    Shift_in.next rx_tvalid_delayed rx_tdata_delayed ethernet_header
  in
  let%hw.Ipv4.Of_signal ipv4_header_next_value, ipv4_header_next_with_split_data =
    let module Shift_in = Ethernet_utils.Shift_in_data (Ipv4) in
    Shift_in.next rx_tvalid_delayed rx_tdata_delayed ipv4_header
  in
  let%hw.Udp.Of_signal udp_header_next_value, udp_header_next_with_split_data =
    let module Shift_in = Ethernet_utils.Shift_in_data (Udp) in
    Shift_in.next rx_tvalid_delayed rx_tdata_delayed udp_header
  in
  let%hw.Always.State_machine state =
    Always.State_machine.create (module States) ~enable:vdd spec
  in
  let%hw_var udp_frame_seen = Var.reg spec ~width:1 in
  let%hw.Axi.Source.Of_signal buffered_axi_rx =
    Axi.Source.Of_signal.pipeline
      ~enable:(axi_rx.tvalid |: udp_frame_seen.value |: state.is Idle)
      spec
      ~n:pipeline_n
      axi_rx
  in
  Always.(
    compile
      [ state.switch
          [ ( Idle
            , [ state_counter <-- zero log_max_cycles
              ; Ethernet_header.Of_always.(
                  ethernet_header <-- Ethernet_header.Of_signal.zero ())
              ; Ipv4.Of_always.(ipv4_header <-- Ipv4.Of_signal.zero ())
              ; Udp.Of_always.(udp_header <-- Udp.Of_signal.zero ())
              ; when_
                  start_of_frame
                  [ udp_frame_seen <-- gnd; state.set_next Ethernet_header ]
              ] )
          ; ( Ethernet_header
            , [ Ethernet_header.Of_always.(
                  ethernet_header_next <-- ethernet_header_next_value)
              ; Ethernet_header.Of_always.(ethernet_header <-- value ethernet_header_next)
              ; state_counter
                <-- state_counter.value +: uextend rx_tvalid_delayed ~width:log_max_cycles
              ; if_
                  (rx_tvalid_delayed
                   &: (state_counter.value ==:. ethernet_header_cycles - 1))
                  [ Ethernet_header.Of_always.(
                      ethernet_header_next <-- ethernet_header_next_with_split_data)
                  ; if_
                      (ethernet_header_next.ethertype.value
                       ==:. ipv4_ethertype
                       &: ((Ethernet_header.Of_always.value ethernet_header_next).dst_mac
                           ==: config.dst_mac))
                      [ Ipv4.Of_always.(ipv4_header_next <-- ipv4_header_next_value)
                      ; Ipv4.Of_always.(ipv4_header <-- value ipv4_header_next)
                      ; state_counter <-- zero log_max_cycles
                      ; state.set_next Ipv4_Header
                      ]
                      [ state.set_next Idle ]
                  ]
                  [ state.set_next Ethernet_header ]
              ] )
          ; ( Ipv4_Header
            , [ Ipv4.Of_always.(ipv4_header_next <-- ipv4_header_next_value)
              ; Ipv4.Of_always.(ipv4_header <-- value ipv4_header_next)
              ; state_counter
                <-- state_counter.value +: uextend rx_tvalid_delayed ~width:log_max_cycles
              ; if_
                  (rx_tvalid_delayed &: (state_counter.value ==:. ipv4_header_cycles - 1))
                  [ Ipv4.Of_always.(ipv4_header_next <-- ipv4_header_next_with_split_data)
                  ; if_
                      (ipv4_header_next.protocol.value ==:. udp_protocol)
                      [ Udp.Of_always.(udp_header_next <-- udp_header_next_value)
                      ; Udp.Of_always.(udp_header <-- value udp_header_next)
                      ; state_counter <-- zero log_max_cycles
                      ; udp_frame_seen <-- vdd
                      ; state.set_next Udp_Header
                      ]
                      [ state.set_next Idle ]
                  ]
                  [ state.set_next Ipv4_Header ]
              ] )
          ; ( Udp_Header
            , [ Udp.Of_always.(udp_header_next <-- udp_header_next_value)
              ; Udp.Of_always.(udp_header <-- value udp_header_next)
              ; state_counter
                <-- state_counter.value +: uextend rx_tvalid_delayed ~width:log_max_cycles
              ; when_
                  (rx_tvalid_delayed &: (state_counter.value ==:. udp_header_cycles - 1))
                  [ Udp.Of_always.(udp_header_next <-- udp_header_next_with_split_data)
                  ; state_counter <-- zero log_max_cycles
                  ; state.set_next Data
                  ]
              ] )
          ; Data, [ when_ end_of_frame [ state.set_next Idle ] ]
          ]
      ]);
  { O.axi_udp_packet =
      { buffered_axi_rx with tvalid = buffered_axi_rx.tvalid &: udp_frame_seen.value }
  }
;;

let create scope ({ clocking; config; axi_rx } : _ I.t) =
  let%tydi { axi_udp_packet } = create_internal scope { clocking; config; axi_rx } in
  let%tydi { axi_source_dn; axi_dest_up = _ } =
    Strip_fcs_32.create
      { clock = clocking.clock
      ; clear = clocking.clear
      ; axi_source_up = axi_udp_packet
      ; axi_dest_dn = { tready = vdd }
      }
  in
  let%tydi { axi } =
    Swap_addressing.create
      { clk = clocking.clock
      ; clr = clocking.clear
      ; axi = { axi_source_dn with tdata = bswap axi_source_dn.tdata }
      }
  in
  { O.axi_udp_packet = { axi with tdata = bswap axi.tdata } }
;;

let hierarchical ?instance scope inputs =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"udp_packet_decoder" ?instance create inputs
;;
