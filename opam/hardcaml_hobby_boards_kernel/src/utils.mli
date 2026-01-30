open! Base
open Hardcaml

val generate_clear : Signal.t Clock_and_reset.I.t -> Signal.t
val sync_reg_spec : Signal.t Clock_and_reset.I.t -> Signal.Reg_spec.t
