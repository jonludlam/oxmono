val test_udp_packet_stream_waves
  :  ?with_data_gaps:bool
  -> ?strip_fcs_and_swap_address:bool
  -> unit
  -> Hardcaml_waveterm.Waveform.t
