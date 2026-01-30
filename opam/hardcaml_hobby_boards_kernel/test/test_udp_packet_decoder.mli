open Hardcaml

val test_rx_waves
  :  ?with_data_gaps:bool
  -> unit
  -> Hardcaml_waveterm.Waveform.t * Bits.t list list
