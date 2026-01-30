module BUFR = struct
  module type P = sig
    val bufr_divide : string
    val sim_device : string
  end

  module P : P = struct
    let bufr_divide = "BYPASS"
    let sim_device = "7SERIES"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"BUFR_DIVIDE" ~value:(String P.bufr_divide)
      ; Hardcaml.Parameter.create ~name:"SIM_DEVICE" ~value:(String P.sim_device)
      ]
    ;;

    module I = struct
      type 'a t =
        { ce : 'a [@bits 1] [@rtlname "CE"]
        ; clr : 'a [@bits 1] [@rtlname "CLR"]
        ; i : 'a [@bits 1] [@rtlname "I"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "BUFR") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end
