open Base
open Hardcaml
open Signal
open Ethernet_types
module Var = Always.Variable
module Axi = Ethernet.Axi32

module Make (M : sig
    val filter_config : _ Udp_packet_decoder_intf.Filter_config.t
  end) =
struct
  let word_size = Ethernet.Config.data_bits

  let ethernet_header_cycles =
    Int.round_up ~to_multiple_of:word_size Ethernet_header.sum_of_port_widths / word_size
  ;;

  let ipv4_header_cycles =
    Int.round_up ~to_multiple_of:word_size Ipv4.sum_of_port_widths / word_size
  ;;

  let udp_header_cycles =
    Int.round_up ~to_multiple_of:word_size Udp.sum_of_port_widths / word_size
  ;;

  let data_cycles = Int.round_up ~to_multiple_of:word_size max_data_bits / word_size

  let max_cycles =
    Int.max ethernet_header_cycles ipv4_header_cycles
    |> Int.max udp_header_cycles
    |> Int.max data_cycles
  ;;

  let log_max_cycles = Int.ceil_log2 max_cycles

  let filter_enabled, dst_mac_address =
    match M.filter_config with
    | No_filter -> false, zero Ethernet_header.port_widths.dst_mac
    | Destination_mac_filter mac_address -> true, mac_address
  ;;

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; axi_rx : 'a Axi.Source.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { axi_tx : 'a Axi.Source.t
      ; ready : 'a Axi.Dest.t
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Ethernet_header
      | Ipv4_header
      | Udp_header
      | Data
      | Fcs
    [@@deriving sexp_of, enumerate, compare ~localize]
  end

  let create scope ({ clocking; axi_rx } : _ I.t) =
    let spec = Types.Clocking.to_spec clocking in
    let%hw_var state_counter = Var.reg spec ~width:log_max_cycles in
    let ready = Axi.Dest.Of_always.wire zero in
    let%hw_var udp_data_counter = Var.reg spec ~width:Udp.port_widths.length in
    let%hw_var udp_data_counter_next =
      Var.wire ~default:(zero Udp.port_widths.length) ()
    in
    let%hw.Ethernet_header.Of_always ethernet_header =
      Ethernet_header.Of_always.reg spec
    in
    let%hw.Ethernet_header.Of_always ethernet_header_next =
      Ethernet_header.Of_always.wire zero
    in
    let%hw.Ipv4.Of_always ipv4_header = Ipv4.Of_always.reg spec in
    let%hw.Ipv4.Of_always ipv4_header_next = Ipv4.Of_always.wire zero in
    let udp_header = Udp.Of_always.reg spec in
    let%hw.Udp.Of_always udp_header_next = Udp.Of_always.wire zero in
    let%hw_var data_length = Var.reg spec ~width:Udp.port_widths.length in
    let%hw rx_tvalid_delayed = reg spec (axi_rx.tvalid &: ready.tready.value) in
    let%hw rx_tdata_delayed = reg ~enable:axi_rx.tvalid spec axi_rx.tdata in
    let%hw rx_tdata_delayed_2 = reg spec rx_tdata_delayed in
    let start_of_frame = axi_rx.tvalid &: lsb axi_rx.tuser in
    let error = msb axi_rx.tuser in
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
    let dst_mac_address_valid =
      if filter_enabled
      then ethernet_header_next.dst_mac.value ==: dst_mac_address
      else vdd
    in
    let axi_udp_data = Axi.Source.Of_always.wire zero in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module States) ~enable:vdd spec
    in
    Always.(
      compile
        [ ready.tready <-- vdd
        ; if_
            rx_tvalid_delayed
            [ udp_data_counter_next <-- udp_data_counter.value +:. (word_size / 8) ]
            [ udp_data_counter_next <-- udp_data_counter.value ]
        ; state.switch
            [ ( Idle
              , [ state_counter <-- zero log_max_cycles
                ; udp_data_counter <-- zero Udp.port_widths.length
                ; Ethernet_header.Of_always.(
                    ethernet_header <-- Ethernet_header.Of_signal.zero ())
                ; Ipv4.Of_always.(ipv4_header <-- Ipv4.Of_signal.zero ())
                ; Udp.Of_always.(udp_header <-- Udp.Of_signal.zero ())
                ; when_ start_of_frame [ state.set_next Ethernet_header ]
                ] )
            ; ( Ethernet_header
              , [ Ethernet_header.Of_always.(
                    ethernet_header_next <-- ethernet_header_next_value)
                ; Ethernet_header.Of_always.(
                    ethernet_header <-- value ethernet_header_next)
                ; state_counter
                  <-- state_counter.value
                      +: uextend rx_tvalid_delayed ~width:log_max_cycles
                ; if_
                    (rx_tvalid_delayed
                     &: (state_counter.value ==:. ethernet_header_cycles - 1))
                    [ Ethernet_header.Of_always.(
                        ethernet_header_next <-- ethernet_header_next_with_split_data)
                    ; if_
                        (ethernet_header_next.ethertype.value
                         ==:. ipv4_ethertype
                         &: dst_mac_address_valid)
                        [ Ipv4.Of_always.(ipv4_header_next <-- ipv4_header_next_value)
                        ; Ipv4.Of_always.(ipv4_header <-- value ipv4_header_next)
                        ; state_counter <-- zero log_max_cycles
                        ; state.set_next Ipv4_header
                        ]
                        [ state.set_next Idle ]
                    ]
                    [ state.set_next Ethernet_header ]
                ; when_ error [ state.set_next Idle ]
                ] )
            ; ( Ipv4_header
              , [ Ipv4.Of_always.(ipv4_header_next <-- ipv4_header_next_value)
                ; Ipv4.Of_always.(ipv4_header <-- value ipv4_header_next)
                ; state_counter
                  <-- state_counter.value
                      +: uextend rx_tvalid_delayed ~width:log_max_cycles
                ; if_
                    (rx_tvalid_delayed &: (state_counter.value ==:. ipv4_header_cycles - 1)
                    )
                    [ Ipv4.Of_always.(
                        ipv4_header_next <-- ipv4_header_next_with_split_data)
                    ; if_
                        (ipv4_header_next.protocol.value ==:. udp_protocol)
                        [ Udp.Of_always.(udp_header_next <-- udp_header_next_value)
                        ; Udp.Of_always.(udp_header <-- value udp_header_next)
                        ; state_counter <-- zero log_max_cycles
                        ; state.set_next Udp_header
                        ]
                        [ state.set_next Idle ]
                    ]
                    [ state.set_next Ipv4_header ]
                ; when_ error [ state.set_next Idle ]
                ] )
            ; ( Udp_header
              , [ Udp.Of_always.(udp_header_next <-- udp_header_next_value)
                ; Udp.Of_always.(udp_header <-- value udp_header_next)
                ; state_counter
                  <-- state_counter.value
                      +: uextend rx_tvalid_delayed ~width:log_max_cycles
                ; udp_data_counter <-- udp_data_counter_next.value
                ; if_
                    (rx_tvalid_delayed &: (state_counter.value ==:. udp_header_cycles - 1))
                    [ Udp.Of_always.(udp_header_next <-- udp_header_next_with_split_data)
                    ; data_length <-- (Udp.Of_always.value udp_header_next).length
                    ; state_counter <-- zero log_max_cycles
                    ; state.set_next Data
                    ]
                    [ state.set_next Udp_header ]
                ; when_ error [ state.set_next Idle ]
                ] )
            ; ( Data
              , [ state_counter
                  <-- state_counter.value
                      +: uextend rx_tvalid_delayed ~width:log_max_cycles
                ; udp_data_counter <-- udp_data_counter_next.value
                ; axi_udp_data.tvalid <-- (vdd &: rx_tvalid_delayed)
                ; axi_udp_data.tuser <-- gnd @: (state_counter.value ==:. 0)
                ; axi_udp_data.tdata
                  <-- sel_bottom rx_tdata_delayed_2 ~width:(word_size / 2)
                      @: sel_top rx_tdata_delayed ~width:(word_size / 2)
                ; axi_udp_data.tstrb <-- ones 4
                ; if_
                    (rx_tvalid_delayed
                     &: (udp_data_counter.value >=: data_length.value -:. (word_size / 8))
                    )
                    [ axi_udp_data.tlast <-- vdd
                    ; (let%hw valid_data_bytes =
                         sel_bottom (data_length.value -: udp_data_counter.value) ~width:4
                       in
                       axi_udp_data.tkeep
                       <-- (List.init 4 ~f:(fun i -> valid_data_bytes >:. i) |> concat_msb))
                    ; state_counter <-- zero log_max_cycles
                    ; state.set_next Fcs
                    ]
                    []
                ; when_
                    error
                    [ axi_udp_data.tvalid <-- vdd
                    ; axi_udp_data.tuser <-- vdd @: (state_counter.value ==:. 0)
                    ; axi_udp_data.tlast <-- vdd
                    ; axi_udp_data.tkeep <-- ones 4
                    ; state.set_next Idle
                    ]
                ] )
            ; Fcs, [ state.set_next Idle ]
            ]
        ]);
    { O.axi_tx = Axi.Source.Of_always.value axi_udp_data
    ; O.ready = Axi.Dest.Of_always.value ready
    }
  ;;

  let hierarchical ?instance scope inputs =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"udp_packet_decoder" ?instance create inputs
  ;;
end
