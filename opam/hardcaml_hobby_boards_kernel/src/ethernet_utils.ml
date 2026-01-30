open Base
open! Core
open Hardcaml
open Signal
open Ethernet_types

module Interfaces (Config : Hardcaml_axi.Stream_config) = struct
  module Stream = Hardcaml_axi.Stream.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; axi_source_up : 'a Stream.Source.t [@rtlprefix "source_up_"]
      ; axi_dest_dn : 'a Stream.Dest.t [@rtlprefix "dest_dn_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { axi_source_dn : 'a Stream.Source.t [@rtlprefix "source_dn_"]
      ; axi_dest_up : 'a Stream.Dest.t [@rtlprefix "dest_up_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end
end

module Strip_fcs = struct
  module Make (Config : Hardcaml_axi.Stream_config) = struct
    include Interfaces (Config)

    let fcs_bytes = 4
    let tdata_bytes = Stream.Source.port_widths.tdata / 8
    let () = assert (tdata_bytes <= 4)
    let () = assert (fcs_bytes % tdata_bytes = 0)
    let fcs_words = fcs_bytes / tdata_bytes

    let create (i : Signal.t I.t) =
      let spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
      let tbadframe, tfirst = i.axi_source_up.tuser.:(1), i.axi_source_up.tuser.:(0) in
      let enable = i.axi_source_up.tvalid &: i.axi_dest_dn.tready in
      let tdata_reg = pipeline ~n:fcs_words spec ~enable i.axi_source_up.tdata in
      let tfirst_regs =
        reg_fb spec ~enable ~width:fcs_words ~f:(fun d ->
          sll d ~by:1 |: uresize tfirst ~width:fcs_words)
        @: tfirst
      in
      { O.axi_source_dn =
          { Stream.Source.tvalid = i.axi_source_up.tvalid &: (lsbs tfirst_regs ==:. 0)
          ; tdata = tdata_reg
          ; tkeep = i.axi_source_up.tkeep
          ; tstrb = i.axi_source_up.tstrb
          ; tlast = i.axi_source_up.tlast
          ; tuser = tbadframe @: msb tfirst_regs
          }
      ; axi_dest_up = { tready = i.axi_dest_dn.tready }
      }
    ;;

    let hierarchical ?instance scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical
        ~scope
        ~name:("strip_fcs_" ^ Int.to_string Config.data_bits)
        ?instance
        (fun _scope -> create)
        i
    ;;

    module For_testing = struct
      let fcs_words = fcs_words
    end
  end
end

module Swap_addressing = struct
  (* {v
      Ethernet |  0 |  6 bytes mac -\
               |  6 |  6 bytes mac -/
               | 12 |  2 +
      IPV4     | 14 |  12 +
               | 26 |  4 bytes ip -\
               | 30 |  4 bytes ip -/
      UDP      | 34 |  2 bytes port -\
               | 36 |  2 bytes port -/
               | 38 |  4 +

         3 2 1 0
       +---------+
       | A A A A |  0
       | B B A A |  4
       | B B B B |  8
       | - - - - | 12
       | - - - - | 16
       | - - - - | 20
       | C C - - | 24
       | D D C C | 28
       | E E D D | 32
       | - - F F | 36
       |     - - | 40
       +---------+
     v}
  *)

  (* This code could be generalised to perform arbitrary byte swapping based on a given
     byte order. *)
  let byte_order =
    let range ofs n = List.init n ~f:(fun i -> i + ofs) in
    [ range 6 6 (* mac *)
    ; range 0 6 (* max *)
    ; range 12 14
    ; range 30 4 (* ip *)
    ; range 26 4 (* ip *)
    ; range 36 2 (* port *)
    ; range 34 2 (* port *)
    ; range 38 6
    ]
    (* align to 4 bytes and add pass through for final bytes *)
    |> List.concat
    |> List.chunks_of ~length:4
  ;;

  let byte_deltas =
    List.mapi byte_order ~f:(fun i offsets ->
      List.map offsets ~f:(fun offset -> offset - (i * 4)))
  ;;

  let min_byte_delta =
    List.fold byte_deltas ~init:0 ~f:(fun init l -> List.fold l ~init ~f:min)
  ;;

  let max_byte_delta =
    List.fold byte_deltas ~init:0 ~f:(fun init l -> List.fold l ~init ~f:max)
  ;;

  let min_word_delta = -(min_byte_delta - 3) / 4
  let max_word_delta = (max_byte_delta + 3) / 4

  let offset_deltas =
    List.map byte_deltas ~f:(List.map ~f:(fun x -> x - (max_word_delta * 4)))
  ;;

  type byte_in_past =
    { word_in_past : int
    ; byte : int
    }
  [@@deriving sexp_of]

  let byte_in_past =
    let get_byte_in_past i =
      let i = -i - 1 in
      let word_in_past = i / 4 in
      let byte = 3 - (i % 4) in
      { word_in_past; byte }
    in
    List.map offset_deltas ~f:(List.map ~f:get_byte_in_past)
  ;;

  let words_required_at_cycle =
    let word = List.map byte_in_past ~f:(List.map ~f:(fun x -> x.word_in_past)) in
    let max_word = List.map word ~f:(fun x -> List.fold x ~init:0 ~f:max) in
    let min_word = List.map word ~f:(fun x -> List.fold x ~init:Int.max_value ~f:min) in
    List.zip_exn min_word max_word
  ;;

  module Make_select_bytes (B : Hardcaml.Comb.S) = struct
    open B

    let word_control_pipeline_mask =
      List.map words_required_at_cycle ~f:(fun (min, max) ->
        List.init (min_word_delta + max_word_delta) ~f:(fun i ->
          if i >= min && i <= max then 1 else 0)
        |> List.rev
        |> B.of_bit_list)
    ;;

    let select_bytes ~prev_words ~word_count =
      let prev_words =
        List.map prev_words ~f:(fun w ->
          Array.init 4 ~f:(fun i -> w.:[(i * 8) + 7, i * 8]))
        |> Array.of_list
      in
      let words =
        List.map byte_in_past ~f:(fun byte_in_past ->
          concat_lsb
          @@ List.map byte_in_past ~f:(fun { word_in_past; byte } ->
            prev_words.(word_in_past).(byte)))
      in
      mux word_count words
    ;;
  end

  (* Simple pipelined implementation, which doesn't respect framing signals. *)
  let _swap ~clock ~clear ~shift ~d ~word_count =
    let module Select_bytes = Make_select_bytes (Signal) in
    let spec = Signal.Reg_spec.create () ~clock ~clear in
    let reg ~enable = Signal.reg spec ~enable in
    let r0 = reg ~enable:shift d in
    let r1 = reg ~enable:shift r0 in
    let r2 = reg ~enable:shift r1 in
    let r3 = reg ~enable:shift r2 in
    let r4 = reg ~enable:shift r3 in
    Select_bytes.select_bytes ~prev_words:[ r0; r1; r2; r3; r4 ] ~word_count
  ;;

  module I = struct
    type 'a t =
      { clk : 'a
      ; clr : 'a
      ; axi : 'a Ethernet.Axi32.Source.t [@rtlprefix "axi_in_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t = { axi : 'a Ethernet.Axi32.Source.t [@rtlprefix "axi_out_"] }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let create ?scope (i : Signal.t I.t) =
    let open Signal in
    let ( -- ) =
      match scope with
      | None -> Signal.( -- )
      | Some scope -> Scope.naming scope
    in
    let spec = Reg_spec.create () ~clock:i.clk ~clear:i.clr in
    let module Select_bytes = Make_select_bytes (Signal) in
    (* Pipeline control.

       - Data is pushed by the RX interface. We cannot stall this interface, though it
         will not provide data every cycle.

       - We must buffer a few words in order to reorder some bytes

       - This poses a problem for the last few words of a packet which we must somehow
         flush through the pipeline, even though the RX interface isn't pushing data.

       - In the following we distingiush a few cases;

       - RX data valid - pipeline shifts

       - RX data not valid - the words in the pipeline shift towards the output and bunch
         up

       - depending on which word is being processed, we check that the appropriate enables
         are set in the pipeline. If they are, a word is output. Otherwise we stall,
         unless ...

       - The last word of a packet is in the pipeline therefore we shift the pipeline in
         order to flush it. In this state we may also be buffering the first words of the
         next packet.
    *)
    let reg_axi ~enable axi = Ethernet.Axi32.Source.map axi ~f:(reg spec ~enable) in
    let e = Array.init 5 ~f:(fun i -> wire 1 -- ("ENABLE" ^ Int.to_string i)) in
    let axi0 = reg_axi ~enable:e.(0) i.axi in
    let axi1 = reg_axi ~enable:e.(1) axi0 in
    let axi2 = reg_axi ~enable:e.(2) axi1 in
    let axi3 = reg_axi ~enable:e.(3) axi2 in
    let axi4 = reg_axi ~enable:e.(4) axi3 in
    let count = wire 4 -- "COUNT" in
    let masks = Select_bytes.word_control_pipeline_mask in
    let prev_words = List.map [ axi0; axi1; axi2; axi3; axi4 ] ~f:(fun x -> x.tdata) in
    let tdata = Select_bytes.select_bytes ~prev_words ~word_count:count in
    let mask = (mux count masks).:[2, 0] -- "MASK" in
    let valid =
      (List.map [ axi0; axi1; axi2 ] ~f:(fun a -> a.tvalid) |> concat_lsb) -- "VALID"
    in
    let last =
      (List.map [ axi0; axi1; axi2 ] ~f:(fun a -> a.tlast &: a.tvalid) |> concat_lsb)
      -- "LAST"
    in
    let last =
      mux last (List.map ~f:of_bit_string [ "000"; "001"; "011"; "011"; "111" ])
      -- "LAST_MASK"
    in
    let valid = (valid |: last) -- "VLD_LAST" in
    (* shift out when the current output word can be constructed - or must be constructed
       because of tlast *)
    let shift_out = axi2.tvalid &: (valid &: mask ==: mask) in
    (* clear count on tlast - potentially it might be useful to also clear on tfirst. *)
    let clr_count = i.clr |: (shift_out &: axi2.tlast) in
    let en_count = shift_out in
    e.(0) <-- vdd;
    (* always shift in, regardless of valid *)
    e.(1) <-- (shift_out |: ~:(axi2.tvalid) |: ~:(axi1.tvalid));
    (* shift into empty slots *)
    e.(2) <-- (shift_out |: ~:(axi2.tvalid));
    e.(3) <-- shift_out;
    (* always shift as data is produced *)
    e.(4) <-- shift_out;
    count
    <-- reg_fb
          (Reg_spec.override ~clear:clr_count spec)
          ~enable:en_count
          ~width:4
          ~f:(fun d -> mux2 (d ==:. 10) d (d +:. 1));
    { O.axi = { axi2 with tvalid = shift_out; tdata } }
  ;;
end

module Shift_in_data (X : Interface.S) = struct
  (* Shift in the whole of tdata, or the top half of tdata *)
  let next delayed tdata x =
    let word_size = width tdata in
    let x_unpacked = X.Of_always.value x in
    let x_packed = X.Of_signal.pack ~rev:true (X.Of_always.value x) in
    let next =
      sel_bottom x_packed ~width:(X.sum_of_port_widths - word_size) @: tdata
      |> X.Of_signal.unpack ~rev:true
    in
    let split =
      sel_bottom x_packed ~width:(X.sum_of_port_widths - (word_size / 2))
      @: sel_top tdata ~width:(word_size / 2)
      |> X.Of_signal.unpack ~rev:true
    in
    X.Of_signal.mux2 delayed next x_unpacked, X.Of_signal.mux2 delayed split x_unpacked
  ;;
end

module For_testing = struct
  module Ethernet_tx = Ethernet.Tx.Make (struct
      let max_packets = 4
      let average_packet_size = 128
    end)

  module Crc = Ethernet_tx.Make_comb (Bits)
  open Bits

  let preamble_sfd_value = of_string "64'h5555_5555_5555_5557"

  let swap_addressing packet =
    let swapped_packet =
      Packet.
        { packet with
          ethernet_header =
            { packet.ethernet_header with
              dst_mac = packet.ethernet_header.src_mac
            ; src_mac = packet.ethernet_header.dst_mac
            }
        ; ipv4_header =
            { packet.ipv4_header with
              src_ip = packet.ipv4_header.dst_ip
            ; dst_ip = packet.ipv4_header.src_ip
            }
        ; udp_header =
            { packet.udp_header with
              src_port = packet.udp_header.dst_port
            ; dst_port = packet.udp_header.src_port
            }
        }
    in
    swapped_packet
  ;;

  let get_random_config =
    Udp_packet_generator.Config.
      { fpga_mac = random ~width:Ethernet_header.port_widths.src_mac
      ; fpga_ip =
          of_int_trunc
            ~width:Ipv4.port_widths.src_ip
            (Random.int_incl 3232235520 3232301055)
      ; fpga_port =
          of_int_trunc ~width:Udp.port_widths.src_port (Random.int_incl 49152 65535)
      ; host_mac = random ~width:Ethernet_header.port_widths.dst_mac
      ; host_ip =
          of_int_trunc
            ~width:Ipv4.port_widths.dst_ip
            (Random.int_incl 3232235520 3232301055)
      ; host_port =
          of_int_trunc ~width:Udp.port_widths.dst_port (Random.int_incl 49152 65535)
      ; ip_header_checksum = random ~width:Ipv4.port_widths.checksum
      }
  ;;

  let get_config =
    Udp_packet_generator.Config.
      { fpga_mac = of_string "48'h0018_3e04_c882"
      ; fpga_ip = of_string "32'hc0a8_0164"
      ; fpga_port = of_int_trunc ~width:Ethernet_types.Udp.port_widths.src_port 49152
      ; host_mac = of_string "48'h607d_09a4_0854"
      ; host_ip = of_string "32'hc0a8_0165"
      ; host_port = of_int_trunc ~width:Ethernet_types.Udp.port_widths.dst_port 49152
      ; ip_header_checksum = of_string "16'hd8ad"
      }
  ;;

  let create_packet_from_fpga
    ?(config : Bits.t Udp_packet_generator.Config.t option)
    ?protocol
    ?dst_mac
    ?data
    ()
    =
    let default_data =
      of_string
        "240'hAAAA_BBBB_CCCC_1111_2222_AAAA_BBBB_CCCC_1111_2222_0000_0000_0000_0000_0000"
    in
    let data = Option.value ~default:default_data data in
    let default_config =
      { get_random_config with
        host_mac =
          Option.value
            ~default:(random ~width:Ethernet_header.port_widths.dst_mac)
            dst_mac
      }
    in
    let address_config = Option.value ~default:default_config config in
    let ethernet_header : _ Ethernet_header.t =
      { dst_mac = address_config.host_mac
      ; src_mac = address_config.fpga_mac
      ; ethertype =
          of_int_trunc ~width:Ethernet_header.port_widths.ethertype ipv4_ethertype
      }
    in
    let ipv4_header : _ Ipv4.t =
      { version = of_string "4'h4"
      ; ihl = of_string "4'h5"
      ; dscp = zero Ipv4.port_widths.dscp
      ; ecn = zero Ipv4.port_widths.ecn
      ; length =
          of_int_trunc
            ~width:Ipv4.port_widths.length
            ((Ipv4.sum_of_port_widths + Udp.sum_of_port_widths + width data) / 8)
      ; identification = zero Ipv4.port_widths.identification
      ; flags = zero Ipv4.port_widths.flags
      ; fragment_offset = zero Ipv4.port_widths.fragment_offset
      ; ttl = of_string "8'h40"
      ; protocol =
          Option.value
            ~default:(of_int_trunc ~width:Ipv4.port_widths.protocol udp_protocol)
            protocol
      ; checksum = address_config.ip_header_checksum
      ; src_ip = address_config.fpga_ip
      ; dst_ip = address_config.host_ip
      }
    in
    let udp_header : _ Udp.t =
      { src_port = address_config.fpga_port
      ; dst_port = address_config.host_port
      ; length =
          of_int_trunc
            ~width:Udp.port_widths.length
            ((Udp.sum_of_port_widths + width data) / 8)
      ; checksum = zero Udp.port_widths.checksum
      }
    in
    let data = data @: zero (max_data_bits - width data) in
    Packet.{ ethernet_header; ipv4_header; udp_header; data }
  ;;

  let create_packet_from_host
    ?(config : Bits.t Udp_packet_generator.Config.t option)
    ?protocol
    data
    =
    create_packet_from_fpga ?config ?protocol ~data () |> swap_addressing
  ;;

  let update_axi_output_data
    ~output_data
    ~(axi_output : Bits.t ref Ethernet.Axi32.Source.t)
    =
    let open Bits in
    if to_bool !(axi_output.tvalid)
    then
      if to_bool !(axi_output.tlast)
      then (
        let valid_bytes = leading_ones !(axi_output.tkeep) in
        output_data
        := sel_top !(axi_output.tdata) ~width:(to_int_trunc valid_bytes * 8)
           :: !output_data)
      else output_data := !(axi_output.tdata) :: !output_data
    else ();
    !output_data
  ;;

  let check_tx_data
    ~output_data
    ~expected_data
    ?(rx_error = false)
    ?(print_data_as_string = false)
    ()
    =
    let preamble_received = sel_top (concat_lsb output_data) ~width:preamble_sfd_bits in
    let fcs_received = sel_top (concat_msb output_data) ~width:fcs_bits in
    let data_received =
      bswap
        (concat_msb output_data
         |> drop_top ~width:fcs_bits
         |> drop_bottom ~width:preamble_sfd_bits)
    in
    let fcs_data = if rx_error then data_received else expected_data in
    let byte_list = split_msb ~exact:true ~part_width:8 fcs_data in
    let l =
      List.fold byte_list ~init:(ones 32) ~f:(fun crc byte ->
        Crc.update_bits ~polynomial:Ethernet_tx.crc_polynomial_802_3 ~crc byte)
    in
    let expected_fcs = ~:l in
    if rx_error
    then [%test_result: Bits.t] (fcs_received <>: expected_fcs) ~expect:vdd
    else
      [%test_result: Bits.Hex.t]
        (preamble_received @: data_received @: fcs_received)
        ~expect:(preamble_sfd_value @: expected_data @: expected_fcs);
    if print_data_as_string
    then (
      printf "%s" (data_received |> to_ascii_string_msb);
      printf "\n")
  ;;

  let check_tx_packet
    ~output_data
    ~(packet : Bits.t Packet.t)
    ~loopback
    ?(rx_error = false)
    ?(print_data_as_string = false)
    ()
    =
    let total_width =
      Ethernet_header.sum_of_port_widths + (to_int_trunc packet.ipv4_header.length * 8)
    in
    let expected_packet = if loopback then swap_addressing packet else packet in
    let expected_packet_vector = Packet.Of_bits.pack ~rev:true expected_packet in
    check_tx_data
      ~output_data
      ~expected_data:(sel_top expected_packet_vector ~width:total_width)
      ~rx_error
      ~print_data_as_string
      ()
  ;;

  let sim_preamble_sfd sim (inputs : _ Ethernet.Rx.I.t) =
    let open Bits in
    for _ = 0 to (preamble_sfd_bits / width !(inputs.rxd)) - 2 do
      inputs.rxd := of_string "01";
      inputs.crsdv := vdd;
      Cyclesim.cycle sim
    done;
    inputs.rxd := of_string "11";
    inputs.crsdv := vdd;
    Cyclesim.cycle sim
  ;;

  let sim_packet sim ~packet (inputs : _ Ethernet.Rx.I.t) =
    let all_inputs =
      let packet = String.to_list packet in
      let data =
        List.map ~f:Char.to_int packet
        |> List.map ~f:(fun input -> of_int_trunc ~width:8 input)
        |> concat_lsb
      in
      split_lsb ~exact:true ~part_width:2 data
    in
    List.iter
      ~f:(fun input ->
        inputs.crsdv := vdd;
        inputs.rxd := input;
        Cyclesim.cycle sim)
      all_inputs;
    inputs.crsdv := gnd;
    (* Wait some cycles for the writes to all finalize. *)
    Cyclesim.cycle ~n:1000 sim
  ;;

  let sim_udp_packet sim ~packet (inputs : _ Ethernet.Rx.I.t) =
    let all_inputs =
      let data = of_ascii_string_msb packet in
      let packet = create_packet_from_host ~config:get_config data in
      let total_width =
        Packet.sum_of_port_widths - (Ethernet_types.max_data_bits - width data)
      in
      let packet_vector =
        bswap (Packet.Of_bits.pack ~rev:true packet) |> sel_bottom ~width:total_width
      in
      split_lsb ~exact:true ~part_width:2 packet_vector
    in
    List.iter
      ~f:(fun input ->
        inputs.crsdv := vdd;
        inputs.rxd := input;
        Cyclesim.cycle sim)
      all_inputs;
    inputs.crsdv := gnd;
    (* Wait some cycles for the writes to all finalize. *)
    Cyclesim.cycle ~n:2500 sim
  ;;
end
