open! Base

module Part_and_pins : sig
  type t =
    { part : string
    ; pins : Pin.t list
    }
  [@@deriving sexp, fields ~getters]
end

val load : string -> Part_and_pins.t
