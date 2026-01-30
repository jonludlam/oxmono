open Hardcaml

val test_loopback : ?rx_error:bool -> unit -> Hardcaml_waveterm.Waveform.t
val test_rx_waves : int list -> Hardcaml_waveterm.Waveform.t * Bits.t list list
val test_tx_waves : int list -> with_data_gaps:bool -> Hardcaml_waveterm.Waveform.t
val test_udp_packet_generator : Hardcaml_waveterm.Waveform.t
val test_udp_packet_decoder : Hardcaml_waveterm.Waveform.t * Bits.t
