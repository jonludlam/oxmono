open! Core

let snakes =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_snakes.test_snakes ()
        |> Hardcaml_waveterm_interactive.run]
;;

let vga =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_vga.test_video_timing 10_000
        |> Hardcaml_waveterm_interactive.run]
;;

let uart_tx =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_uart.test_tx_waves Eight Even One
        |> Hardcaml_waveterm_interactive.run]
;;

let uart =
  Command.basic
    ~summary:""
    [%map_open.Command
      let clocks_per_bit = flag "-clocks-per-bit" (optional_with_default 8 int) ~doc:"" in
      fun () ->
        let waves, result =
          Hardcaml_hobby_boards_test.Test_uart.send_string
            ~top:true
            ~clocks_per_bit
            Eight
            Even
            One
            "hello world"
        in
        print_s [%message result];
        Hardcaml_waveterm_interactive.run waves]
;;

let ethernet_loopback =
  Command.basic
    ~summary:""
    [%map_open.Command
      let rx_error =
        flag "-rx-error" (optional_with_default false bool) ~doc:"Enable an rx_error"
      in
      fun () ->
        Hardcaml_hobby_boards_test.Test_ethernet.test_loopback ~rx_error ()
        |> Hardcaml_waveterm_interactive.run]
;;

let ethernet_rx =
  Command.basic
    ~summary:""
    [%map_open.Command
      let data_bits = flag "-data-bits" (optional_with_default 64 int) ~doc:"" in
      let data_lengths = [ data_bits ] in
      fun () ->
        fst (Hardcaml_hobby_boards_test.Test_ethernet.test_rx_waves data_lengths)
        |> Hardcaml_waveterm_interactive.run]
;;

let ethernet_rx_packets =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        fst Hardcaml_hobby_boards_test.Test_ethernet.test_udp_packet_decoder
        |> Hardcaml_waveterm_interactive.run]
;;

let ethernet_tx =
  Command.basic
    ~summary:""
    [%map_open.Command
      let data_bits = flag "-data-bits" (optional_with_default (50 * 8) int) ~doc:""
      and with_data_gaps =
        flag
          "-with-data-gaps"
          (optional_with_default false bool)
          ~doc:"Enable a random number (10-30) cycle gap between each data valid"
      in
      let data_lengths = [ data_bits ] in
      fun () ->
        Hardcaml_hobby_boards_test.Test_ethernet.test_tx_waves
          data_lengths
          ~with_data_gaps
        |> Hardcaml_waveterm_interactive.run]
;;

let ethernet_tx_packets =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_ethernet.test_udp_packet_generator
        |> Hardcaml_waveterm_interactive.run]
;;

let udp_packet_decoder =
  Command.basic
    ~summary:""
    [%map_open.Command
      let with_data_gaps =
        flag
          "-with-data-gaps"
          (optional_with_default false bool)
          ~doc:"Enable a 15 cycle gap between each data valid"
      in
      fun () ->
        fst
          (Hardcaml_hobby_boards_test.Test_udp_packet_decoder.test_rx_waves
             ~with_data_gaps
             ())
        |> Hardcaml_waveterm_interactive.run]
;;

let udp_packet_generator =
  Command.basic
    ~summary:""
    [%map_open.Command
      let num_udp_packets =
        flag
          "-num-udp-packets"
          (optional_with_default 1 int)
          ~doc:"Number of udp packets to generate"
      in
      fun () ->
        Hardcaml_hobby_boards_test.Test_udp_packet_generator.test_tx_waves
          ~num_udp_packets
        |> Hardcaml_waveterm_interactive.run]
;;

let udp_packet_stream =
  Command.basic
    ~summary:""
    [%map_open.Command
      let with_data_gaps =
        flag
          "-with-data-gaps"
          (optional_with_default false bool)
          ~doc:"Enable a 15 cycle gap between each data valid"
      and strip_fcs_and_swap_address =
        flag
          "-strip-fcs-and-swap-address"
          (optional_with_default false bool)
          ~doc:
            "Enable checking of the output after the fcs has been removed and the source \
             and destination fields swapped"
      in
      fun () ->
        Hardcaml_hobby_boards_test.Test_udp_packet_stream.test_udp_packet_stream_waves
          ~with_data_gaps
          ~strip_fcs_and_swap_address
          ()
        |> Hardcaml_waveterm_interactive.run]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "ethernet-loopback", ethernet_loopback
       ; "ethernet-rx", ethernet_rx
       ; "ethernet-rx-packets", ethernet_rx_packets
       ; "ethernet-tx", ethernet_tx
       ; "ethernet-tx-packets", ethernet_tx_packets
       ; "snakes", snakes
       ; "uart-tx", uart_tx
       ; "uart", uart
       ; "udp-packet-decoder", udp_packet_decoder
       ; "udp-packet-generator", udp_packet_generator
       ; "udp-packet-stream", udp_packet_stream
       ; "vga", vga
       ])
;;
