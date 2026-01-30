open! Base
open! Hardcaml

include struct
  open Board_intf
  module M_IOT = M_IOT
  module M_I = M_I
  module M_O = M_O
  module M_T = M_T
  module M_IO = M_IO
  module M_IT = M_IT
  module M_OT = M_OT

  module type Subsystem_name = Subsystem_name
  module type Subsystem = Subsystem
end

module Subsystem = struct
  type t =
    { inputs : Signal.t list
    ; outputs : Signal.t list
    ; input_tristates : Signal.t list
    ; output_tristates : Signal.t list
    ; complete : bool
    }
  [@@deriving sexp_of]
end

type t =
  { subsystems : (string, Subsystem.t) Hashtbl.t
  ; pins : Pin.t list
  ; scope : Scope.t
  }
[@@deriving sexp_of, fields ~getters]

let add_inputs { subsystems; pins = _; scope = _ } subsystem inputs input_tristates =
  match
    Hashtbl.add
      subsystems
      ~key:subsystem
      ~data:
        { inputs; outputs = []; input_tristates; output_tristates = []; complete = false }
  with
  | `Ok -> ()
  | `Duplicate ->
    raise_s
      [%message
        "Cannot add inputs - subsystem has already been created" (subsystem : string)]
;;

let set_outputs { subsystems; pins = _; scope = _ } subsystem outputs output_tristates =
  match Hashtbl.find subsystems subsystem with
  | None -> raise_s [%message "Cannot complete subsystem" (subsystem : string)]
  | Some e ->
    Hashtbl.set
      subsystems
      ~key:subsystem
      ~data:{ e with outputs; output_tristates; complete = true }
;;

let create ?(flatten_design = false) () =
  let subsystems = Hashtbl.create (module String) in
  { subsystems; pins = []; scope = Scope.create ~flatten_design () }
;;

module Make_IOT (C : Subsystem_name) (I : Interface.S) (O : Interface.S) (T : Interface.S) =
struct
  module T_enabled = With_valid.Fields.Make (T)

  let create board =
    let inputs = I.Of_signal.inputs () in
    let tristate_inputs = T.Of_signal.inputs () in
    add_inputs board C.core (I.to_list inputs) (T.to_list tristate_inputs);
    inputs, tristate_inputs
  ;;

  let complete board outputs tristate_outputs =
    let subsystem = C.core in
    let outputs = O.Of_signal.outputs outputs in
    let tristate_outputs = T_enabled.Of_signal.outputs tristate_outputs in
    O.iter3 O.port_widths O.port_names outputs ~f:(fun expected_width port_name output ->
      let got_width = Signal.width output in
      if expected_width <> got_width
      then
        raise_s
          [%message
            "Output port width is incorrect"
              ~subsystem:(C.core : string)
              (expected_width : int)
              (got_width : int)
              (port_name : string)]);
    T.iter3
      T.port_widths
      T.port_names
      tristate_outputs
      ~f:(fun expected_width port_name { With_valid.valid; value } ->
        if Signal.width valid <> 1
        then
          raise_s
            [%message
              "Tristate enable width must be 1" (subsystem : string) (port_name : string)];
        let got_width = Signal.width value in
        if expected_width <> got_width
        then
          raise_s
            [%message
              "Tristate port width is incorrect"
                (subsystem : string)
                (expected_width : int)
                (got_width : int)
                (port_name : string)]);
    set_outputs board C.core (O.to_list outputs) (T_enabled.to_list tristate_outputs)
  ;;
end

module Make_I (C : Subsystem_name) (I : Interface.S) = struct
  include Make_IOT (C) (I) (Interface.Empty) (Interface.Empty)

  let create board =
    let i, _ = create board in
    complete board Interface.Empty.Empty Interface.Empty.Empty;
    i
  ;;
end

module Make_O (C : Subsystem_name) (O : Interface.S) = struct
  include Make_IOT (C) (Interface.Empty) (O) (Interface.Empty)

  let complete board o =
    let _, _ = create board in
    complete board o Interface.Empty.Empty
  ;;
end

module Make_T (C : Subsystem_name) (T : Interface.S) = struct
  include Make_IOT (C) (Interface.Empty) (Interface.Empty) (T)

  let create board =
    let _, t = create board in
    t
  ;;

  let complete board t = complete board Interface.Empty.Empty t
end

module Make_IO (C : Subsystem_name) (I : Interface.S) (O : Interface.S) = struct
  include Make_IOT (C) (I) (O) (Interface.Empty)

  let create board =
    let inputs, _ = create board in
    inputs
  ;;

  let complete board outputs = complete board outputs Interface.Empty.Empty
end

module Make_IT (C : Subsystem_name) (I : Interface.S) (T : Interface.S) = struct
  include Make_IOT (C) (I) (Interface.Empty) (T)

  let create board =
    let inputs, tristates = create board in
    inputs, tristates
  ;;

  let complete board tristates = complete board Interface.Empty.Empty tristates
end

module Make_OT (C : Subsystem_name) (O : Interface.S) (T : Interface.S) = struct
  include Make_IOT (C) (Interface.Empty) (O) (T)

  let create board =
    let _, tristates = create board in
    tristates
  ;;

  let complete board outputs tristates = complete board outputs tristates
end
