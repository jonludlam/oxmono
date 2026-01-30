open Core
open Hardcaml_hobby_boards_kernel

(* The board files provided tend to split busses into individual bits with a [_0], [_1]
   etc suffix.

   This routine detects this and turns it into a [[0]], [[1]] suffix instead. In our
   hardcaml designs these can then be vectors rather than arrays.
*)
let bussify (pins : Pin.t list) =
  let split_suffix (pin : Pin.t) = String.split pin.name ~on:'_', pin in
  (* Split into groups according to all the parts expect the last one. *)
  let groups =
    List.group (List.map pins ~f:split_suffix) ~break:(fun (a, _) (b, _) ->
      List.compare String.compare (List.rev a |> List.tl_exn) (List.rev b |> List.tl_exn)
      <> 0)
  in
  (* make sure the last part of the group is [0,1,2,3...] - otherwise is isn't a 'bus'.
     Pretty simple *)
  let is_bus (group : (string list * Pin.t) list) =
    try
      let ids = List.map group ~f:(fun (g, _) -> List.last_exn g) in
      let ids : int list = List.map ids ~f:Int.of_string in
      List.equal Int.equal (List.init (List.length ids) ~f:Fn.id) ids
    with
    | _ -> false
  in
  List.map groups ~f:(fun group ->
    if is_bus group
    then (
      let replace_pin_name (name, (pin : Pin.t)) =
        let name =
          match List.rev name with
          | hd :: tl ->
            String.concat [ String.concat ~sep:"_" (List.rev tl); [%string "[%{hd}]"] ]
          | [] -> raise_s [%message "bad name"]
        in
        { pin with name }
      in
      List.map group ~f:replace_pin_name)
    else List.map group ~f:snd)
  |> List.concat
;;

let command_load_and_print =
  Command.basic
    ~summary:""
    [%map_open.Command
      let file = anon ("FILE" %: string)
      and bussify_flag = flag "-bussify" no_arg ~doc:"" in
      fun () ->
        let part_info = Xml_pins.load file in
        let part_info =
          if bussify_flag
          then { part_info with pins = bussify part_info.pins }
          else part_info
        in
        print_s (Xml_pins.Part_and_pins.sexp_of_t part_info)]
;;

let () =
  Command.group ~summary:"" [ "to-sexp", command_load_and_print ] |> Command_unix.run
;;
