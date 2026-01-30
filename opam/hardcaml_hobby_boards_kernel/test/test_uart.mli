open Hardcaml_hobby_boards_kernel.Uart_types

val test_tx_waves
  :  Data_bits.Cases.t
  -> Parity.Cases.t
  -> Stop_bits.Cases.t
  -> Hardcaml_waveterm.Waveform.t

val test_tx_rx
  :  ?top:bool
  -> ?clocks_per_bit:int
  -> Data_bits.Cases.t
  -> Parity.Cases.t
  -> Stop_bits.Cases.t
  -> Hardcaml_waveterm.Waveform.t * (int -> (int, int) result)

val send_string
  :  ?top:bool
  -> ?clocks_per_bit:int
  -> Data_bits.Cases.t
  -> Parity.Cases.t
  -> Stop_bits.Cases.t
  -> string
  -> Hardcaml_waveterm.Waveform.t * string
