open! Core
open! Async

module Embed_file = struct
  (* These functions were taken from embed_file_lib, which doesn't have a public_release
     name *)
  let chunk_len = 80

  let chunks str =
    let n = String.length str in
    let q = n / chunk_len in
    let r = n % chunk_len in
    let chunk i ~len = String.sub str ~pos:(chunk_len * i) ~len in
    List.concat
      [ List.init q ~f:(fun i -> chunk i ~len:chunk_len)
      ; (if r = 0 then [] else [ chunk q ~len:r ])
      ]
  ;;

  let replace_CRs : string -> string =
    (* [replace_CRs input] replaces all occurrences in [input] of "CR" with "C\082". *)
    let search_pattern = lazy (String.Search_pattern.create "CR") in
    let replacement = lazy (sprintf "C\\%03d" (Char.to_int 'R')) in
    fun input ->
      String.Search_pattern.replace_all
        (force search_pattern)
        ~in_:input
        ~with_:(force replacement)
  ;;

  let write_ml w ~var ~contents =
    Writer.writef w "let %s =\n  \"" var;
    List.iteri (chunks contents) ~f:(fun i chunk ->
      let escaped_chunk = replace_CRs (String.escaped chunk) in
      if i = 0
      then Writer.writef w "%s" escaped_chunk
      else if String.length chunk > 0 && Char.( = ) chunk.[0] ' '
      then Writer.writef w "\\\n  \\%s" escaped_chunk
      else Writer.writef w "\\\n   %s" escaped_chunk);
    Writer.write w "\"\n;;\n"
  ;;

  let write_mli w ~var = Writer.writef w "val %s : string\n" var
end

let write_arg_name_aarray w ~ext ~paths =
  let aarray_var = "by_arg_name" in
  Writer.newline w;
  match ext with
  | "ml" ->
    Writer.write w [%string "let %{aarray_var} = "];
    List.iteri paths ~f:(fun i path ->
      let arg_name = Svg.arg_name path in
      let variable_name = Svg.variable_name path in
      Writer.newline w;
      Writer.write w (if i = 0 then "[|" else ";");
      Writer.write w [%string {| "%{arg_name}", %{variable_name}|}]);
    Writer.write w {| |];;|}
  | "mli" ->
    Writer.write_line
      w
      [%string
        {|(** [%{aarray_var}] returns a list of arg name and svg content pairs *)|}];
    Writer.write w [%string "val %{aarray_var} : (string * string) array"]
  | _ -> ()
;;

let run ~module_name ~output_directory ~paths ~with_aarray ~only_aarray_in_mli =
  let module_name =
    module_name
    |> String.tr ~target:'-' ~replacement:'_'
    |> String.lowercase
    |> String.capitalize
  in
  let filename ext = output_directory ^/ String.lowercase module_name ^ "." ^ ext in
  let write ext ~write_file_line =
    Writer.with_file_atomic (filename ext) ~f:(fun w ->
      let () =
        List.iter paths ~f:(fun path ->
          let var = Svg.variable_name path in
          write_file_line w ~var ~path)
      in
      if with_aarray then write_arg_name_aarray w ~paths ~ext;
      Deferred.unit)
  in
  let%bind () =
    write "ml" ~write_file_line:(fun w ~var ~path ->
      Embed_file.write_ml w ~var ~contents:(Svg.contents path))
  in
  let%bind () =
    write "mli" ~write_file_line:(fun w ~var ~path:_ ->
      match with_aarray, only_aarray_in_mli with
      | true, true -> ()
      | _ ->
        Writer.newline w;
        Embed_file.write_mli w ~var)
  in
  Deferred.unit
;;

let command =
  Command.async
    ~summary:"Embed FontAwesome SVG icons"
    (let%map_open.Command module_name =
       flag "output" (required string) ~doc:"NAME name of the generated module"
     and output_directory =
       flag
         "output-dir"
         (optional_with_default "." Filename_unix.arg_type)
         ~doc:"PATH where to put the generated module (default = cwd)"
     and svgs =
       let some_icons =
         anon (non_empty_sequence_as_list ("SVG" %: Svg.arg_type))
         |> map ~f:(fun svgs -> Option.some svgs)
       in
       let all_icons =
         flag
           "im-super-duper-sure-i-want-to-embed-hundreds-of-icons"
           (no_arg_some Svg.all)
           ~doc:
             " Embed all available FontAwesome icons. WARNING: You probably really don't \
              want to do this."
       in
       Command.Param.choose_one_non_optional
         [ some_icons; all_icons ]
         ~if_nothing_chosen:Raise
       |> map ~f:(fun svgs -> Option.value_exn svgs)
     and with_aarray =
       flag
         "with-aarray"
         no_arg
         ~doc:
           "BOOL create a variable [ by_arg_name ] that contains an assoc array of the \
            icon command line argument names to svg content"
     and only_aarray_in_mli =
       flag
         "only-aarray-in-mli"
         no_arg
         ~doc:
           "BOOL only expose [ by_arg_name ] in the mli and no individual svgs (must be \
            passed with [-with-aarray])"
     in
     fun () ->
       run ~module_name ~output_directory ~paths:svgs ~with_aarray ~only_aarray_in_mli)
;;
