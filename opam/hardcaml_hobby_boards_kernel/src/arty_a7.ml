(* 35 and 100 variants - only the part actually differs - the pins are the same *)

open! Base
open Hardcaml
open Board
module Clock_and_reset = Clock_and_reset

module Switches = struct
  module I = Types.Value (struct
      let port_name = "dip_switches_4bits_tri_i"
      let port_width = 4
    end)

  include
    Make_I
      (struct
        let core = "switches"
      end)
      (I)
end

module Leds = struct
  module O = Types.Value (struct
      let port_name = "led_4bits_tri_o"
      let port_width = 4
    end)

  include
    Make_O
      (struct
        let core = "leds"
      end)
      (O)
end

module Buttons = struct
  module I = Types.Value (struct
      let port_name = "dip_switches_4bits_tri_i"
      let port_width = 4
    end)

  include
    Make_I
      (struct
        let core = "switches"
      end)
      (I)
end

module Rgb_leds = struct
  module Rgb = struct
    type 'a t =
      { red : 'a [@rtlname "r"]
      ; green : 'a [@rtlname "g"]
      ; blue : 'a [@rtlname "b"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { leds : 'a Rgb.t array [@length 4] }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  (* The port names require a flat array, so we convert to something nicer at the
     interface level. *)
  module O' = Types.Value (struct
      let port_name = "rbg_led_tri_o"
      let port_width = 12
    end)

  include
    Make_O
      (struct
        let core = "leds"
      end)
      (O')

  let complete board o = complete board (Signal.concat_lsb (O.to_list o))
end

module Ethernet = struct
  module I = struct
    type 'a t =
      { crsdv : 'a [@rtlname "eth_crs"]
      ; rxerr : 'a [@rtlname "eth_rx_er"]
      ; rxd : 'a [@bits 4] [@rtlname "eth_rxd"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { txen : 'a [@rtlname "eth_tx_en"]
      ; txd : 'a [@bits 4] [@rtlname "eth_txd"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_IO
      (struct
        let core = "ethernet"
      end)
      (I)
      (O)
end

(* Ethernet, I2C, shield, qspi, spi, usbuart, pmod *)

let part_info p = Xml_pins.Part_and_pins.t_of_sexp (Parsexp.Single.parse_string_exn p)

let custom_constraints =
  Rope.concat
    ~sep:[%rope "\n"]
    [ [%rope "set_property BITSTREAM.GENERAL.COMPRESS true [current_design];"]
    ; [%rope "set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design];"]
    ; [%rope "set_property CFGBVS VCCO [current_design]"]
    ; [%rope "set_property CONFIG_VOLTAGE 3.3 [current_design]"]
    ; [%rope "create_clock -add -name clock_100 -period 10.00 [get_ports {clock_100}];"]
    ]
;;

let generate_top ?dir ~(part : [ `a35 | `a100 ]) board =
  let board_info, name =
    match part with
    | `a35 -> Board_info.arty_a7_35_dot_sexp, "arty_a7_35t"
    | `a100 -> Board_info.arty_a7_100_dot_sexp, "arty_a7_100t"
  in
  let part_info = part_info board_info in
  Xilinx_top.generate
    ?dir
    ~name
    ~part:(Xml_pins.Part_and_pins.part part_info)
    ~pins:(Xml_pins.Part_and_pins.pins part_info)
    ~custom_constraints
    board
;;
