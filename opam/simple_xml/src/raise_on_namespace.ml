open Core

type element =
  { tag : string
  ; attributes : (string * string) list [@sexp.list]
  ; children : t list [@sexp.list]
  }

and t =
  | Element of element
  | Text of string
[@@deriving sexp_of]

let unnamespace = function
  | ("", s), x -> s, x
  | (ns, _), _ -> failwithf "Namespaces not supported: %s" ns ()
;;

let parse_input input =
  Xmlm.input_doc_tree
    input
    ~data:(fun s -> Text s)
    ~el:(fun tag_attrs children ->
      let tag, attributes = unnamespace tag_attrs in
      let attributes = List.map ~f:unnamespace attributes in
      Element { tag; attributes; children })
  |> function
  | _dtd, Text _ -> failwith "malformed xml"
  | _dtd, Element n -> n
;;

type inp =
  [ `Channel of In_channel.t
  | `File of string
  | `String of string
  ]

let rec parse = function
  | `String s -> parse_input (Xmlm.make_input (`String (0, s)))
  | `Channel ic -> parse_input (Xmlm.make_input (`Channel ic))
  | `File fn -> In_channel.with_file fn ~f:(fun ic -> parse (`Channel ic :> inp))
;;

let to_string ?decl ?buf ?fmt element =
  let buf =
    match buf with
    | None -> Buffer.create 16
    | Some buf -> buf
  in
  let indent =
    Option.bind fmt ~f:(function
      | `Minified -> None
      | `Newlines_only -> Some 0
      | `Indent n -> Some n)
  in
  let out = Xmlm.make_output ?decl ~indent (`Buffer buf) in
  let rec iter { tag; attributes; children } =
    let tag_name = "", tag in
    let attr_list = List.map attributes ~f:(fun (k, v) -> ("", k), v) in
    Xmlm.output out (`El_start (tag_name, attr_list));
    List.iter children ~f:(function
      | Text str -> Xmlm.output out (`Data str)
      | Element element -> iter element);
    Xmlm.output out `El_end
  in
  Xmlm.output out (`Dtd None);
  iter element;
  Buffer.contents buf
;;
