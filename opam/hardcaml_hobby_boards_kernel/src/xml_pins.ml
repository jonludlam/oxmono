open Base

module Part_and_pins = struct
  type t =
    { part : string
    ; pins : Pin.t list
    }
  [@@deriving sexp, fields ~getters]
end

let find_attribute (attributes : Simple_xml.Attribute.t list) name =
  match
    List.find attributes ~f:(fun { ns = _; key; value = _ } -> String.equal key name)
  with
  | Some t -> t.value
  | None -> raise_s [%message "Failed to find attribute" (name : string)]
;;

let pin_of_attributes attributes =
  let find = find_attribute attributes in
  { Pin.name = find "name"
  ; loc = find "loc"
  ; iostandard = find "iostandard" |> Iostandard.of_string
  }
;;

let load file =
  let xml = Simple_xml.parse (`File file) in
  match xml with
  | { Simple_xml.tag = { ns = _; tag = "part_info" }; attributes; children } ->
    let part = find_attribute attributes "part_name" in
    let pins =
      List.filter_map children ~f:(function
        | Simple_xml.Text _ -> None
        | Simple_xml.Element { tag = { ns = _; tag = "pins" }; attributes = []; children }
          -> Some children
        | Simple_xml.Element { tag; _ } ->
          raise_s [%message "Unknown tag while parsing board file" (tag.tag : string)])
      |> List.concat
    in
    let pins =
      List.filter_map pins ~f:(function
        | Simple_xml.Text _ -> None
        | Simple_xml.Element { tag = { ns = _; tag = "pin" }; attributes; children = [] }
          -> Some (pin_of_attributes attributes)
        | Simple_xml.Element { tag; _ } ->
          raise_s [%message "Unknown tag while parsing pins" (tag.tag : string)])
    in
    { Part_and_pins.part; pins }
  | _ -> raise_s [%message "Expecting part info"]
;;
