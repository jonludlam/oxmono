(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Package change record types and JSON codecs *)

(** Type of changes in a package *)
type change_type =
  | Unchanged
  | Janestreet
  | Dune_port
  | Oxcaml
  | New_feature
  | Bugfix
  | Compatibility
  | Build_fix
  | Mixed

let change_type_to_string = function
  | Unchanged -> "unchanged"
  | Janestreet -> "janestreet"
  | Dune_port -> "dune-port"
  | Oxcaml -> "oxcaml"
  | New_feature -> "new-feature"
  | Bugfix -> "bugfix"
  | Compatibility -> "compatibility"
  | Build_fix -> "build-fix"
  | Mixed -> "mixed"

let change_type_of_string = function
  | "unchanged" -> Ok Unchanged
  | "janestreet" -> Ok Janestreet
  | "dune-port" -> Ok Dune_port
  | "oxcaml" -> Ok Oxcaml
  | "new-feature" -> Ok New_feature
  | "bugfix" -> Ok Bugfix
  | "compatibility" -> Ok Compatibility
  | "build-fix" -> Ok Build_fix
  | "mixed" -> Ok Mixed
  | s -> Error (Printf.sprintf "Unknown change_type: %s" s)

(** Package change record *)
type t = {
  name : string;
  git_commit : string;
  change_type : change_type;
  summary : string option;
  details : string option;
}

let make ~name ~git_commit ~change_type ?summary ?details () =
  { name; git_commit; change_type; summary; details }

let name t = t.name
let git_commit t = t.git_commit
let change_type t = t.change_type
let summary t = t.summary
let details t = t.details

(** {1 JSON Codec} *)

let change_type_jsont : change_type Jsont.t =
  Jsont.enum ~kind:"change_type" [
    ("unchanged", Unchanged);
    ("janestreet", Janestreet);
    ("dune-port", Dune_port);
    ("oxcaml", Oxcaml);
    ("new-feature", New_feature);
    ("bugfix", Bugfix);
    ("compatibility", Compatibility);
    ("build-fix", Build_fix);
    ("mixed", Mixed);
  ]

let jsont : t Jsont.t =
  let make name git_commit change_type summary details =
    { name; git_commit; change_type; summary; details }
  in
  Jsont.Object.map ~kind:"Changes" make
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "git_commit" Jsont.string ~enc:(fun r -> r.git_commit)
  |> Jsont.Object.mem "change_type" change_type_jsont ~enc:(fun r -> r.change_type)
  |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:(fun r -> r.summary)
  |> Jsont.Object.opt_mem "details" Jsont.string ~enc:(fun r -> r.details)
  |> Jsont.Object.finish

(** {1 JSON Schema} *)

let json_schema =
  let open Jsont in
  let meta = Meta.none in
  let json_object fields = Object (fields, meta) in
  let json_string s = String (s, meta) in
  let json_array items = Array (items, meta) in
  let json_field name value = ((name, meta), value) in
  json_object [
    json_field "type" (json_string "object");
    json_field "properties" (json_object [
      json_field "name" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Package name")
      ]);
      json_field "git_commit" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Git commit hash this diff was generated against")
      ]);
      json_field "change_type" (json_object [
        json_field "type" (json_string "string");
        json_field "enum" (json_array [
          json_string "unchanged";
          json_string "janestreet";
          json_string "dune-port";
          json_string "oxcaml";
          json_string "new-feature";
          json_string "bugfix";
          json_string "compatibility";
          json_string "build-fix";
          json_string "mixed"
        ]);
        json_field "description" (json_string "Type of changes: unchanged (no meaningful diff), janestreet (package from github.com/janestreet or github.com/oxcaml), dune-port (adding dune build support), oxcaml (uses oxcaml features like unboxed types), new-feature (adds new features not in upstream), bugfix (bug fixes not yet in upstream), compatibility (compatibility fixes for monorepo), build-fix (build system fixes beyond dune), mixed (multiple types of changes)")
      ]);
      json_field "summary" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Single sentence summary of the changes. Omit if change_type is 'unchanged' or 'janestreet'.")
      ]);
      json_field "details" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Longer markdown description with bullet points detailing specific changes. Omit this field if change_type is 'unchanged' or 'janestreet'.")
      ])
    ]);
    json_field "required" (json_array [
      json_string "name";
      json_string "git_commit";
      json_string "change_type"
    ])
  ]
