open! Core
open Js_of_ocaml
module Path = Command.Private.Path

module Run_output = struct
  type t =
    { stdout : string
    ; stderr : string
    }
end

module Process = struct
  class type process = object
    method pid : Js.number Js.t Js.prop
    method env : Js.Unsafe.top Js.t Js.prop
    method exit : Js.number Js.t Js.optdef -> Nothing.t Js.meth
  end

  type t = process Js.t

  let (t : t Lazy.t) = lazy Js.Unsafe.global##.process
  let unset_env key = Js.Unsafe.delete (force t)##.env (Js.string key)
  let pid () = (force t)##.pid
  let put_env ~key ~data = Js.Unsafe.set (force t)##.env (Js.string key) (Js.string data)
  let exit code = (force t)##exit code
end

module Child_process = struct
  module Exec_sync_input = struct
    class type exec_sync_input = object
      method argv0 : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
      method encoding : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
      method stdio : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
      method env : Js.Unsafe.any Js.readonly_prop
    end

    type t = exec_sync_input Js.t
  end

  module Exec_sync_output = struct
    class type exec_sync_output = object
      method stdout : Js.js_string Js.t Js.opt Js.readonly_prop
      method stderr : Js.js_string Js.t Js.opt Js.readonly_prop
      method status : Js.number Js.t Js.opt Js.readonly_prop
    end

    type t = exec_sync_output Js.t
  end

  open Js_of_ocaml

  class type child_process = object
    method spawnSync :
      Js.js_string Js.t
      -> Js.js_string Js.t Js.js_array Js.t
      -> Exec_sync_input.t
      -> Exec_sync_output.t Js.meth
  end

  type t = child_process Js.t

  let (t : t Lazy.t) =
    lazy
      (Js.Unsafe.js_expr
         {|(function () {
    return require('child_process')
  })()|})
  ;;

  let make_env env =
    let empty_env = Js.Unsafe.obj [||] in
    let node_env_clone =
      Js.Unsafe.pure_js_expr
        {|(function() {
          return Object.assign({}, process.env)
        })()|}
    in
    let set obj ~key ~value = Js.Unsafe.set obj (Js.string key) (Js.string value) in
    match env with
    | `Replace_raw assignments ->
      List.iter assignments ~f:(fun assignment ->
        match String.split ~on:'=' assignment with
        | [ key; value ] -> set empty_env ~key ~value
        | _ -> failwith "Invalid environment variable format");
      empty_env
    | `Replace vars ->
      List.iter vars ~f:(fun (key, value) -> set empty_env ~key ~value);
      empty_env
    | `Extend vars ->
      List.iter vars ~f:(fun (key, value) -> set node_env_clone ~key ~value);
      node_env_clone
    | `Override vars ->
      List.iter vars ~f:(fun (key, value_opt) ->
        match value_opt with
        | None -> Js.Unsafe.delete node_env_clone (Js.string key)
        | Some value -> set node_env_clone ~key ~value);
      node_env_clone
  ;;

  let exec_sync ~stdio ?argv0 ~prog ~args ~env () =
    let command = Js.string prog in
    let args = Array.of_list_map args ~f:Js.string |> Js.array in
    let env =
      Option.value_map env ~default:Js.Optdef.empty ~f:(fun env ->
        Js.Optdef.return (make_env env))
    in
    let stdio =
      match stdio with
      | `Pipe -> "pipe"
      | `Inherit -> "inherit"
    in
    let options : Exec_sync_input.t =
      object%js (_)
        val argv0 = Option.map argv0 ~f:Js.string |> Js.Optdef.option
        val encoding = Js.string "utf8" |> Js.Optdef.return
        val stdio = Js.string stdio |> Js.Optdef.return
        val env = Js.Unsafe.inject env
      end
    in
    let output = (force t)##spawnSync command args options in
    (* If stdio is "inherit", stdout and stderr will be null, since the child process used
       our process's io channels. *)
    let to_string x =
      Js.Opt.to_option x |> Option.value_map ~f:Js.to_string ~default:""
    in
    let stdout = output##.stdout |> to_string in
    let stderr = output##.stderr |> to_string in
    Run_output.{ stdout; stderr }, output##.status
  ;;

  let exec_sync' ~prog ~argv ~env () =
    let argv0, args =
      match argv with
      | hd :: tl -> Some hd, tl
      | [] -> None, []
    in
    exec_sync ?argv0 ~prog ~args ~env ()
  ;;
end

module Command_nodejs = Command.Private.For_unix (struct
    module Pid = struct
      open Js_of_ocaml

      type t = Js.number Js.t

      let to_int x = Js.float_of_number x |> Int.of_float
    end

    module Unix = struct
      let getpid () = Process.pid ()
      let unsetenv = Process.unset_env

      let unsafe_getenv =
        (* The only API node provides for getting env vars is [process.env], which is what
           the [Sys.getenv] runtime implementation uses. *)
        Sys.getenv
      ;;

      let putenv ~key ~data = Process.put_env ~key ~data

      let exec ~prog ~argv ?use_path:(_ : bool option) ?env () =
        let (_ : Run_output.t), status =
          Child_process.exec_sync' ~stdio:`Inherit ~prog ~argv ~env ()
        in
        Process.exit (status |> Js.Opt.to_option |> Js.Optdef.option)
      ;;

      module Run_output = Run_output

      let run ~prog ~args ~env () =
        let output, _status =
          Child_process.exec_sync ~stdio:`Pipe ~prog ~args ~env:(Some env) ()
        in
        output
      ;;
    end

    module Version_util = Version_util_compat
  end)

let run = Command_nodejs.run
let shape = Command_nodejs.shape

module Shape = struct
  let help_text = Command_nodejs.help_for_shape
end
