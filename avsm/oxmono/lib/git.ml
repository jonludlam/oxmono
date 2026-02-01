(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Git operations for cloning and updating sources *)

module Log = (val Logs.src_log (Logs.Src.create "oxmono.git") : Logs.LOG)

let clone ~env ~url ~target =
  let target_path = Eio.Path.native_exn target in
  Log.info (fun m -> m "Cloning %s to %s" url target_path);
  Process.run ~env ["git"; "clone"; url; target_path]

let fetch ~env ~cwd =
  Log.info (fun m -> m "Fetching in %s" (Eio.Path.native_exn cwd));
  Process.run ~env ~cwd ["git"; "fetch"; "--all"]

let checkout ~env ~cwd ~ref_ =
  Log.info (fun m -> m "Checking out %s in %s" ref_ (Eio.Path.native_exn cwd));
  Process.run ~env ~cwd ["git"; "checkout"; ref_]

let clone_or_update ~env ~url ~target ~commit =
  if Eio.Path.is_directory target then begin
    Log.info (fun m -> m "Repository exists at %s, updating" (Eio.Path.native_exn target));
    match fetch ~env ~cwd:target with
    | Error e -> Error e
    | Ok () -> checkout ~env ~cwd:target ~ref_:commit
  end else begin
    match clone ~env ~url ~target with
    | Error e -> Error e
    | Ok () -> checkout ~env ~cwd:target ~ref_:commit
  end

(** {1 Archive Export} *)

let cache_dir () =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".cache/oxmono/git"

let url_to_cache_name url =
  (* Convert URL to a safe directory name *)
  let s = String.map (function '/' | ':' | '@' -> '_' | c -> c) url in
  (* Remove git+ prefix if present *)
  let s = if String.length s > 4 && String.sub s 0 4 = "git+" then
    String.sub s 4 (String.length s - 4)
  else s in
  (* Remove trailing .git if present *)
  if String.length s > 4 && String.sub s (String.length s - 4) 4 = ".git" then
    String.sub s 0 (String.length s - 4)
  else s

let ensure_cache_clone ~env ~url =
  let cache_base = cache_dir () in
  let cache_name = url_to_cache_name url in
  let cache_path = Filename.concat cache_base cache_name in
  let fs = Eio.Stdenv.fs env in
  let cache_eio = Eio.Path.(fs / cache_path) in
  (* Ensure cache directory exists *)
  let () =
    if not (Sys.file_exists cache_base) then
      ignore (Sys.command (Printf.sprintf "mkdir -p %s" cache_base))
  in
  (* Strip git+ prefix from URL for actual git commands *)
  let git_url =
    if String.length url > 4 && String.sub url 0 4 = "git+" then
      String.sub url 4 (String.length url - 4)
    else url
  in
  (* Also handle fragment (e.g., url#commit) - strip it *)
  let git_url = match String.index_opt git_url '#' with
    | Some i -> String.sub git_url 0 i
    | None -> git_url
  in
  if Sys.file_exists cache_path then begin
    Log.info (fun m -> m "Updating cached clone at %s" cache_path);
    match fetch ~env ~cwd:cache_eio with
    | Error e -> Error e
    | Ok () -> Ok cache_path
  end else begin
    Log.info (fun m -> m "Creating cached clone of %s" git_url);
    match clone ~env ~url:git_url ~target:cache_eio with
    | Error e -> Error e
    | Ok () -> Ok cache_path
  end

let fetch_and_extract ~env ~url ~commit ~target =
  let target_path = Eio.Path.native_exn target in
  Log.info (fun m -> m "Fetching git archive of %s at %s" url commit);
  match ensure_cache_clone ~env ~url with
  | Error e -> Error e
  | Ok cache_path ->
    let fs = Eio.Stdenv.fs env in
    let cache_eio = Eio.Path.(fs / cache_path) in
    (* Fetch to ensure we have the commit *)
    match fetch ~env ~cwd:cache_eio with
    | Error _ as e -> e
    | Ok () ->
      (* Ensure target directory exists using absolute path *)
      let abs_target = if Filename.is_relative target_path then
        Filename.concat (Sys.getcwd ()) target_path
      else target_path in
      let () =
        if not (Sys.file_exists abs_target) then
          ignore (Sys.command (Printf.sprintf "mkdir -p '%s'" abs_target))
      in
      (* Use git archive to export without .git *)
      Log.info (fun m -> m "Extracting to %s" abs_target);
      Process.run ~env ~cwd:cache_eio
        ["sh"; "-c"; Printf.sprintf "git archive %s | tar -xf - -C '%s'" commit abs_target]

let init ~env ~cwd =
  Process.run ~env ~cwd ["git"; "init"]

let add ~env ~cwd paths =
  Process.run ~env ~cwd (["git"; "add"] @ paths)

let commit ~env ~cwd ~message =
  Process.run ~env ~cwd ["git"; "commit"; "-m"; message]

let commit_allow_empty ~env ~cwd ~message =
  Process.run ~env ~cwd ["git"; "commit"; "--allow-empty"; "-m"; message]

let branch_exists ~env ~cwd ~branch =
  match Process.run ~env ~cwd ["git"; "rev-parse"; "--verify"; branch] with
  | Ok () -> true
  | Error _ -> false

let checkout_orphan ~env ~cwd ~branch =
  Process.run ~env ~cwd ["git"; "checkout"; "--orphan"; branch]

let checkout_new ~env ~cwd ~branch =
  Process.run ~env ~cwd ["git"; "checkout"; "-b"; branch]
