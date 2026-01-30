open! Core
open! Async

module T = struct
  type t =
    { basename : Filename.t
    ; dir : string
    ; arg_name : string
    ; contents : string
    }
  [@@deriving sexp_of, fields ~getters]

  let arg_name' ~dir ~basename =
    [%string "%{dir}-%{basename}"] |> String.chop_suffix_if_exists ~suffix:".svg"
  ;;

  let all =
    [ "brands", Brands.by_filename
    ; "solid", Solid.by_filename
    ; "regular", Regular.by_filename
    ]
    |> List.concat_map ~f:(fun (dir, by_filename) ->
      List.map by_filename ~f:(fun (basename, contents) ->
        let arg_name = arg_name' ~dir ~basename in
        { basename; dir; arg_name; contents }))
  ;;

  let to_string t = t.arg_name
  let arg_name { dir; basename; _ } = arg_name' ~dir ~basename

  let variable_name t =
    String.to_list [%string "%{t.dir}_%{t.basename}"]
    |> List.mapi ~f:(fun i c ->
      match c with
      | '0' .. '9' -> if i = 0 then sprintf "_%c" c else String.of_char c
      | 'A' .. 'Z' -> String.of_char (Char.lowercase c)
      | 'a' .. 'z' | '_' -> String.of_char c
      | '.' -> "_dot_"
      | '-' -> "_"
      | _ -> sprintf "_0x%x_" (Char.to_int c))
    |> String.concat
  ;;
end

include T

let arg_type = Command.Arg_type.enumerated (module T)
