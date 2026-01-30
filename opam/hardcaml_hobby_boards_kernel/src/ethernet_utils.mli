(** Utils file that includes copies of the Strip_fcs and Swap_addressing modules from
    lib/hardcaml/networking as these are not currently part of the public release. It also
    includes helpful testing functions to create input data and check the outputs. *)

open Hardcaml
open Ethernet_types

module Interfaces (Config : Hardcaml_axi.Stream_config) : sig
  module Stream : module type of Hardcaml_axi.Stream.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; axi_source_up : 'a Stream.Source.t
      ; axi_dest_dn : 'a Stream.Dest.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { axi_source_dn : 'a Stream.Source.t
      ; axi_dest_up : 'a Stream.Dest.t
      }
    [@@deriving hardcaml]
  end
end

module Strip_fcs : sig
  module Make (Config : Hardcaml_axi.Stream_config) : sig
    include module type of Interfaces (Config)

    val create : Interface.Create_fn(I)(O).t
    val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t

    module For_testing : sig
      val fcs_words : int
    end
  end
end

module Swap_addressing : sig
  module I : sig
    type 'a t =
      { clk : 'a
      ; clr : 'a
      ; axi : 'a Ethernet.Axi32.Source.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { axi : 'a Ethernet.Axi32.Source.t } [@@deriving hardcaml]
  end

  module Make_select_bytes (B : Comb.S) : sig
    (** Select output bytes from [prev_words] at given [word_count] *)
    val select_bytes : prev_words:B.t list -> word_count:B.t -> B.t
  end

  (** Performs swapping of MAC and IP address and UDP ports in an ethernet packet framed
      as an AXI packet stream. Logic is required to ensure the self flushing behaviour of
      the pipeline and validity of the output AXI stream framing signals.

      Thankfully, this logic is only required for a specific ping-pong demo, otherwise I
      would reconsider it completely. *)
  val create : ?scope:Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Shift_in_data (X : Interface.S) : sig
  val next : Signal.t -> Signal.t -> Always.Variable.t X.t -> Signal.t X.t * Signal.t X.t
end

module For_testing : sig
  val swap_addressing : Bits.t Packet.t -> Bits.t Packet.t
  val get_random_config : Bits.t Udp_packet_generator.Config.t
  val get_config : Bits.t Udp_packet_generator.Config.t
  val preamble_sfd_value : Bits.t

  val create_packet_from_fpga
    :  ?config:Bits.t Udp_packet_generator.Config.t
    -> ?protocol:Bits.t
    -> ?dst_mac:Bits.t
    -> ?data:Bits.t
    -> unit
    -> Bits.t Packet.t

  val create_packet_from_host
    :  ?config:Bits.t Udp_packet_generator.Config.t
    -> ?protocol:Bits.t
    -> Bits.t
    -> Bits.t Packet.t

  val update_axi_output_data
    :  output_data:Bits.t list ref
    -> axi_output:Bits.t ref Ethernet.Axi32.Source.t
    -> Bits.t list

  val check_tx_data
    :  output_data:Bits.t list
    -> expected_data:Bits.t
    -> ?rx_error:bool
    -> ?print_data_as_string:bool
    -> unit
    -> unit

  val check_tx_packet
    :  output_data:Bits.t list
    -> packet:Bits.t Packet.t
    -> loopback:bool
    -> ?rx_error:bool
    -> ?print_data_as_string:bool
    -> unit
    -> unit

  val sim_preamble_sfd : ('a, 'b) Cyclesim.t -> Bits.t ref Ethernet.Rx.I.t -> unit

  val sim_packet
    :  ('a, 'b) Cyclesim.t
    -> packet:string
    -> Bits.t ref Ethernet.Rx.I.t
    -> unit

  val sim_udp_packet
    :  ('a, 'b) Cyclesim.t
    -> packet:string
    -> Bits.t ref Ethernet.Rx.I.t
    -> unit
end
