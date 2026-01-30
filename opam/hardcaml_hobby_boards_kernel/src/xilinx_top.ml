open! Base
open! Hardcaml
open Board
open Subsystem

let hardcaml_circuit name (board : (_, Board.Subsystem.t) Hashtbl.t) =
  let cores = Hashtbl.to_alist board in
  Circuit.create_exn
    ~name
    (List.map cores ~f:(fun (_, core) -> core.outputs @ core.output_tristates)
     |> List.concat)
;;

let rtl_of_hardcaml_circuit board hardcaml_circuit =
  Rtl.create
    ~database:(Board.scope board |> Scope.circuit_database)
    Verilog
    [ hardcaml_circuit ]
  |> Rtl.full_hierarchy
;;

let port_name signal =
  match Signal.names signal with
  | [ name ] -> name
  | names ->
    (* Shouldn't happen as these were already circuit ports *)
    raise_s [%message "Top level circuit port requires one name" (names : string list)]
;;

let map_inputs cores ~f =
  List.map cores ~f:(fun core -> List.map core.inputs ~f) |> List.concat
;;

let map_input_tristates cores ~f =
  List.map cores ~f:(fun core -> List.map core.input_tristates ~f) |> List.concat
;;

let map_outputs cores ~f =
  List.map cores ~f:(fun core -> List.map core.outputs ~f) |> List.concat
;;

let structural_circuit name board circuit =
  Structural.reset_circuit_database ();
  Structural.create_circuit [%string "%{name}_top"] (fun () ->
    let input_set =
      Set.of_list (module String) (List.map (Circuit.inputs circuit) ~f:port_name)
    in
    let cores = Hashtbl.data board in
    let i =
      map_inputs cores ~f:(fun i ->
        let name = port_name i in
        name, Structural.mk_input name (Signal.width i))
    in
    let o =
      map_outputs cores ~f:(fun o ->
        let name = port_name o in
        name, Structural.mk_output name (Signal.width o))
    in
    let i_t =
      map_input_tristates cores ~f:(fun t ->
        let name = port_name t in
        name, Structural.mk_tristate name (Signal.width t))
    in
    let o_t =
      map_input_tristates cores ~f:(fun t ->
        let name = port_name t in
        ( ([%string "%{name}$value"], Structural.mk_wire (Signal.width t))
        , ([%string "%{name}$valid"], Structural.mk_wire 1) ))
    in
    let i = i @ i_t |> List.filter ~f:(fun (name, _) -> Set.mem input_set name) in
    let o = List.concat (o :: List.map o_t ~f:(fun (d, v) -> [ d; v ])) in
    List.iter2_exn i_t o_t ~f:(fun (_, t) ((_, d), (_, v)) ->
      Structural.(t <-- mux v [ z (width d); d ]));
    Structural.inst name ~i ~o)
;;

let generate_xdc_pins (pins : Pin.t list) board =
  let cores = Hashtbl.data board in
  let find_pin name =
    match List.find pins ~f:(fun pin -> String.equal name pin.name) with
    | None -> raise_s [%message "Failed to find pin" (name : string) (pins : Pin.t list)]
    | Some pin -> pin
  in
  let pin_to_xdc (pin : Pin.t) =
    [%rope
      "set_property -dict { PACKAGE_PIN %{pin.loc#String} IOSTANDARD \
       %{pin.iostandard#Iostandard} } [ get_ports { %{pin.name#String} } ];\n"]
  in
  let port_to_xdc p =
    let width = Signal.width p in
    if width = 1
    then [ port_name p |> find_pin |> pin_to_xdc ]
    else
      List.init width ~f:(fun idx ->
        [%string "%{port_name p}[%{idx#Int}]"] |> find_pin |> pin_to_xdc)
  in
  let i = map_inputs cores ~f:port_to_xdc |> List.concat in
  let o = map_outputs cores ~f:port_to_xdc |> List.concat in
  let t = map_input_tristates cores ~f:port_to_xdc |> List.concat in
  Rope.concat [ Rope.concat i; Rope.concat o; Rope.concat t ]
;;

let generate ?custom_constraints ?dir ~name ~part ~pins board =
  let subsystems = Board.subsystems board in
  Hashtbl.iteri subsystems ~f:(fun ~key:subsystem ~data ->
    if not data.complete then raise_s [%message "Not completed" (subsystem : string)]);
  let hardcaml_circuit = hardcaml_circuit name subsystems in
  let structural_circuit = structural_circuit name subsystems hardcaml_circuit in
  let xdc_pin_constraints = generate_xdc_pins pins subsystems in
  let xdc_constraints =
    match custom_constraints with
    | None -> xdc_pin_constraints
    | Some custom -> Rope.concat [ xdc_pin_constraints; custom ]
  in
  let tcl = Vivado_scripts.build_tcl ~name ~part in
  let flash_tcl = Vivado_scripts.flash_tcl ~name in
  let run_vivado_remotely_script = Vivado_scripts.run_vivado_remotely_sh ~name in
  let in_dir name =
    match dir with
    | Some dir -> Filename_base.concat dir name
    | None -> name
  in
  let write_rope_to_file ?(set_executable = false) ~filename data =
    let file_path = in_dir filename in
    Stdio.Out_channel.write_all file_path ~data:(data |> Rope.to_string);
    if set_executable then Core_unix.chmod ~perm:0o755 file_path
  in
  write_rope_to_file
    ~filename:(name ^ ".v")
    (Rope.concat
       [ rtl_of_hardcaml_circuit board hardcaml_circuit
       ; Structural.to_verilog structural_circuit
       ]);
  write_rope_to_file ~filename:(name ^ ".xdc") xdc_constraints;
  write_rope_to_file ~filename:(name ^ ".tcl") tcl;
  write_rope_to_file ~filename:"flash.tcl" flash_tcl;
  write_rope_to_file
    ~set_executable:true
    ~filename:"run_vivado_remotely.sh"
    run_vivado_remotely_script
;;

module For_testing = struct
  let rtl_of_hardcaml_circuit board =
    let subsystems = Board.subsystems board in
    let hardcaml_circuit = hardcaml_circuit "for_testing" subsystems in
    rtl_of_hardcaml_circuit board hardcaml_circuit |> Rope.to_string
  ;;
end
