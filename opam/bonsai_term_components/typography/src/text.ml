open! Core

type 'attr t =
  { chars : Uchar.t iarray
  ; attr : 'attr
  }
[@@deriving sexp_of]

let of_string ~attr string =
  let utf8_str = String.Utf8.sanitize string in
  let chars = String.Utf8.to_list utf8_str |> Iarray.of_list in
  { chars; attr }
;;

let to_string t =
  let char_list = Iarray.to_list t.chars in
  String.Utf8.of_list char_list |> String.Utf8.to_string
;;
