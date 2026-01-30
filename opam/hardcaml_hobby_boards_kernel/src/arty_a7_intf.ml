open! Base
open Hardcaml
open Board

module type Arty_a7 = sig
  module Clock_and_reset : Clock_and_reset.S with type 'a I.t = 'a Clock_and_reset.I.t

  module Switches : sig
    module I : Interface.S with type 'a t = 'a
    include M_I(I).S with type board := Board.t
  end

  module Buttons : sig
    module I : Interface.S with type 'a t = 'a
    include M_I(I).S with type board := Board.t
  end

  module Leds : sig
    module O : Interface.S with type 'a t = 'a
    include M_O(O).S with type board := Board.t
  end

  module Rgb_leds : sig
    module Rgb : sig
      type 'a t =
        { red : 'a
        ; green : 'a
        ; blue : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t = { leds : 'a Rgb.t array } [@@deriving hardcaml]
    end

    include M_O(O).S with type board := Board.t
  end

  module Ethernet : sig
    module I : sig
      type 'a t =
        { crsdv : 'a
        ; rxerr : 'a
        ; rxd : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { txen : 'a
        ; txd : 'a
        }
      [@@deriving hardcaml]
    end

    include M_IO(I)(O).S with type board := Board.t
  end

  val generate_top : ?dir:string -> part:[ `a35 | `a100 ] -> Board.t -> unit
end
