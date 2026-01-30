open! Core

let string_width s =
  String.Utf8.fold (String.Utf8.of_string s) ~init:0 ~f:(fun acc uchar ->
    let width = Notty.Tty_width_hint.tty_width_hint uchar in
    let width = if width < 0 then 1 else width in
    (* Handle undefined width *)
    acc + width)
;;
