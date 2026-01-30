open! Base
open! Hardcaml
open! Board

module type S = Clock_and_reset_intf.S

module I = struct
  type 'a t =
    { clock_100 : 'a
    ; reset_n : 'a
    }
  [@@deriving hardcaml]
end

include
  Make_I
    (struct
      let core = "clocking"
    end)
    (I)
