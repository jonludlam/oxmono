open! Base
open! Hardcaml

module type Ethernet = sig
  module Config : Hardcaml_axi.Stream.Config
  module Axi32 : module type of Hardcaml_axi.Stream.Make (Config)

  module Rx : sig
    module I : sig
      type 'a t =
        { clocking : 'a Types.Clocking.t
        ; crsdv : 'a
            (* Carrier sense / receive data valid signal that is asserted when rxd is
               valid *)
        ; rxerr : 'a
            (* RX error signal that is asserted when an error is detected somewhere in the
               frame being transferred *)
        ; rxd : 'a (* 2-bit wide data signal *)
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { axi_tx : 'a Axi32.Source.t
        ; rx_error : 'a
        }
      [@@deriving hardcaml]
    end

    val create : Scope.t -> Interface.Create_fn(I)(O).t
    val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t
  end

  module Tx : sig
    module Make (M : sig
        val max_packets : int
        val average_packet_size : int
      end) : sig
      module I : sig
        type 'a t =
          { clocking : 'a Types.Clocking.t
          ; data_stream : 'a Axi32.Source.t
          }
        [@@deriving hardcaml]
      end

      module O : sig
        type 'a t =
          { txen : 'a (* Signal that is asserted when the data present on txd is valid *)
          ; txd : 'a (* 2-bit wide data signal *)
          ; ready : 'a Axi32.Dest.t
          (* Axi ready signal that is asserted when the ethernet TX FIFO is not full *)
          }
        [@@deriving hardcaml]
      end

      val crc_polynomial_802_3 : Bits.t

      module Make_comb (C : Comb.S) : sig
        val update_bit : polynomial:Bits.t -> crc:C.t -> C.t -> C.t
        val update_bits : polynomial:Bits.t -> crc:C.t -> C.t -> C.t
      end

      val create : Scope.t -> Interface.Create_fn(I)(O).t
      val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t
    end
  end
end
