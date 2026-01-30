(** Xilinx Clock utils *)

(** Xilinx BUFR component (available in series 7 devices) *)
module BUFR : sig
  module type P = sig
    val bufr_divide : string
    val sim_device : string
  end

  module P : P

  module Make (P : P) : sig
    val params : Hardcaml.Parameter.t list

    module I : sig
      type 'a t =
        { ce : 'a
        ; clr : 'a
        ; i : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t = { o : 'a } [@@deriving hardcaml]
    end

    module T : Hardcaml.Interface.S with type 'a t = 'a Hardcaml.Interface.Empty.t

    val create
      :  ?lib:string
      -> ?arch:string
      -> ?attributes:Hardcaml.Rtl_attribute.t list
      -> ?instance:string
      -> ?name:string
      -> ?parameters:Hardcaml.Parameter.t list
      -> Hardcaml.Signal.t I.t
      -> Hardcaml.Signal.t O.t
  end
end
