(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** oxmono CLI - package source management for the OxCaml monorepo *)

open Cmdliner

module Log = (val Logs.src_log (Logs.Src.create "oxmono") : Logs.LOG)

(** Result syntax for cleaner error handling *)
module Result_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r
end

(** Setup logging *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(** Run a result computation, logging errors and returning exit code *)
let run_result r =
  match r with
  | Ok code -> code
  | Error msg ->
    Log.err (fun m -> m "%s" msg);
    1

(** Map process errors to string *)
let process_err ctx e = Oxmono.Process.error_to_string ctx e

(** Strip surrounding quotes from a string *)
let unquote s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    String.sub s 1 (len - 2)
  else s

(** {1 Add Command} *)

(** Parse opam url.src field to extract source info.
    Git URLs look like: git+https://github.com/foo/bar.git#commit
    Archive URLs are just: https://example.com/foo.tar.gz *)
let parse_opam_url url checksum =
  if String.length url > 4 && String.sub url 0 4 = "git+" then
    let url = String.sub url 4 (String.length url - 4) in
    match String.index_opt url '#' with
    | Some i ->
      let base_url = String.sub url 0 i in
      let commit = String.sub url (i + 1) (String.length url - i - 1) in
      Ok (Oxmono.Source.Git { url = base_url; commit })
    | None ->
      Error "Git URL missing commit hash"
  else if checksum <> "" then
    Ok (Oxmono.Source.Archive { url; checksum })
  else
    Error "Archive URL missing checksum"

let add_cmd =
  let package_name =
    let doc = "Name of the package to add from the current opam switch." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () package_name =
    Eio_main.run @@ fun env ->
    let open Result_syntax in
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    run_result @@
    let* sources =
      Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
      |> Result.map_error (Printf.sprintf "Failed to load sources.yaml: %s")
    in
    let* () =
      if Oxmono.Source.find_package package_name sources <> None then
        Error (Printf.sprintf "Package %s already exists in sources.yaml" package_name)
      else Ok ()
    in
    let* url_raw =
      Oxmono.Process.run_with_output ~env ["opam"; "show"; "-f"; "url.src:"; package_name]
      |> Result.map_error (fun e -> Printf.sprintf "Failed to get package info: %s" (process_err ["opam"; "show"] e))
    in
    let url = url_raw |> String.trim |> unquote in
    let* () =
      if url = "" then Error (Printf.sprintf "Package %s has no source URL" package_name)
      else Ok ()
    in
    let checksum =
      Oxmono.Process.run_with_output ~env ["opam"; "show"; "-f"; "url.checksum:"; package_name]
      |> Result.value ~default:""
      |> String.trim |> unquote
    in
    let* source =
      parse_opam_url url checksum
      |> Result.map_error (Printf.sprintf "Failed to parse source URL: %s")
    in
    let* wt_path =
      Oxmono.Worktree.ensure_worktree ~env ~cwd
      |> Result.map_error (fun e -> Printf.sprintf "Failed to setup worktree: %s" (process_err ["worktree"] e))
    in
    let target_dir = Filename.concat wt_path package_name in
    Log.app (fun m -> m "Downloading %s..." package_name);
    let* () =
      Oxmono.Process.run ~env ["opam"; "source"; package_name; Printf.sprintf "--dir=%s" target_dir]
      |> Result.map_error (fun e -> Printf.sprintf "Failed to download source: %s" (process_err ["opam"; "source"] e))
    in
    let sources = Oxmono.Source.add_package package_name source sources in
    Oxmono.Sources_file.save ~fs Oxmono.Sources_file.sources_filename sources;
    let+ () =
      Oxmono.Worktree.commit_package ~env ~cwd ~package_name
      |> Result.map_error (fun e -> Printf.sprintf "Failed to commit: %s" (process_err ["commit"] e))
    in
    Log.app (fun m -> m "Added %s to sources.yaml and sources/" package_name);
    0
  in
  let doc = "Add a package from the current opam switch to sources.yaml" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_t $ package_name)

(** {1 Sync Command} *)

let sync_one_package ~env ~fs ~cwd ~wt_path (name, source) =
  let open Result_syntax in
  Log.app (fun m -> m "Syncing %s..." name);
  let target_dir = Eio.Path.(fs / wt_path / name) in
  let* () = match source with
    | Oxmono.Source.Git { url; commit } ->
      Oxmono.Git.clone_or_update ~env ~url ~target:target_dir ~commit
    | Oxmono.Source.Archive { url; checksum } ->
      Oxmono.Archive.fetch_and_extract ~env ~url ~checksum ~target:target_dir
  in
  Oxmono.Worktree.commit_package ~env ~cwd ~package_name:name

let sync_cmd =
  let run () =
    Eio_main.run @@ fun env ->
    let open Result_syntax in
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    run_result @@
    let* sources =
      Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
      |> Result.map_error (Printf.sprintf "Failed to load sources.yaml: %s")
    in
    let packages = Oxmono.Source.packages sources in
    if packages = [] then begin
      Log.app (fun m -> m "No packages in sources.yaml");
      Ok 0
    end else
      let* wt_path =
        Oxmono.Worktree.ensure_worktree ~env ~cwd
        |> Result.map_error (fun e -> Printf.sprintf "Failed to setup worktree: %s" (process_err ["worktree"] e))
      in
      let errors =
        List.fold_left (fun errors pkg ->
          match sync_one_package ~env ~fs ~cwd ~wt_path pkg with
          | Ok () ->
            Log.app (fun m -> m "Synced %s" (fst pkg));
            errors
          | Error e ->
            Log.err (fun m -> m "Failed to sync %s: %s" (fst pkg) (process_err [fst pkg] e));
            errors + 1
        ) 0 packages
      in
      Ok (if errors > 0 then 1 else 0)
  in
  let doc = "Sync all packages from sources.yaml" in
  let info = Cmd.info "sync" ~doc in
  Cmd.v info Term.(const run $ logging_t)

(** {1 Diff Command} *)

let diff_cmd =
  let package_name =
    let doc = "Name of the package to diff." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () package_name =
    Eio_main.run @@ fun env ->
    let open Result_syntax in
    let cwd = Eio.Stdenv.cwd env in
    let root = Eio.Path.native_exn cwd in
    let wt_path = Oxmono.Worktree.worktree_path ~root in
    let pristine_dir = Filename.concat wt_path package_name in
    let modified_dir = Filename.concat root (Filename.concat "opam" package_name) in
    run_result @@
    let* () =
      if not (Sys.file_exists pristine_dir && Sys.is_directory pristine_dir) then
        Error (Printf.sprintf "Pristine directory %s does not exist. Run 'oxmono sync' first." pristine_dir)
      else Ok ()
    in
    let* () =
      if not (Sys.file_exists modified_dir && Sys.is_directory modified_dir) then
        Error (Printf.sprintf "Modified directory opam/%s does not exist" package_name)
      else Ok ()
    in
    match Oxmono.Process.run ~env ["diff"; "-ruN"; pristine_dir; modified_dir] with
    | Ok () ->
      Log.app (fun m -> m "No differences for %s" package_name);
      Ok 0
    | Error (`Exit_code 1) ->
      Ok 0  (* diff returns 1 when there are differences *)
    | Error e ->
      Error (Printf.sprintf "Diff failed: %s" (process_err ["diff"] e))
  in
  let doc = "Show differences between opam/<pkg> and pristine sources/<pkg>" in
  let info = Cmd.info "diff" ~doc in
  Cmd.v info Term.(const run $ logging_t $ package_name)

(** {1 List Command} *)

let list_cmd =
  let dir_path =
    let doc = "Directory to scan for untracked packages." in
    Arg.(value & opt string "opam" & info ["dir"] ~docv:"DIR" ~doc)
  in
  let run () dir_path =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let sources =
      Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
      |> Result.fold ~ok:Fun.id ~error:(fun e ->
           Log.warn (fun m -> m "Failed to load sources.yaml: %s" e);
           Oxmono.Source.empty)
    in
    let tracked = Oxmono.Source.packages sources |> List.map fst in
    let target_dir = Eio.Path.(fs / dir_path) in
    run_result @@
    let open Result_syntax in
    let* () =
      if not (Eio.Path.is_directory target_dir) then
        Error (Printf.sprintf "Directory %s does not exist" dir_path)
      else Ok ()
    in
    let subdirs =
      Eio.Path.read_dir target_dir
      |> List.filter (fun name -> Eio.Path.is_directory Eio.Path.(target_dir / name))
      |> List.sort String.compare
    in
    let untracked = List.filter (fun name -> not (List.mem name tracked)) subdirs in
    if untracked = [] then
      Log.app (fun m -> m "All packages in %s/ are tracked in sources.yaml" dir_path)
    else begin
      Log.app (fun m -> m "Untracked packages in %s/:" dir_path);
      List.iter (fun name -> Log.app (fun m -> m "  %s" name)) untracked
    end;
    Ok 0
  in
  let doc = "List packages in a directory not tracked in sources.yaml" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_t $ dir_path)

(** {1 Main} *)

let main_cmd =
  let doc = "Package source management for the OxCaml monorepo" in
  let info = Cmd.info "oxmono" ~version:"0.1.0" ~doc in
  Cmd.group info [add_cmd; sync_cmd; diff_cmd; list_cmd]

let () = exit (Cmd.eval' main_cmd)
