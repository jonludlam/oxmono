open! Base

type code = string

let hex_codes =
  [| "00111111"
   ; "00000110"
   ; "01011011"
   ; "01001111"
   ; "01100110"
   ; "01101101"
   ; "01111101"
   ; "00000111"
   ; "01111111"
   ; "01100111"
   ; "01011111"
   ; "01111100"
   ; "00111001"
   ; "01011110"
   ; "01111001"
   ; "01110001"
  |]
;;

let numeric_codes = Array.subo hex_codes ~len:10
let add_dot = String.mapi ~f:(fun i c -> if i = 0 then '1' else c)
let to_signal = Hardcaml.Signal.of_string

let print s =
  let p = Stdio.print_endline in
  let s = String.rev s in
  p (if Char.equal s.[0] '1' then " _ " else "   ");
  let r a b c =
    String.of_char_list
      [ (if Char.equal a '1' then '|' else ' ')
      ; (if Char.equal b '1' then '_' else ' ')
      ; (if Char.equal c '1' then '|' else ' ')
      ]
  in
  p (r s.[5] s.[6] s.[1]);
  p (r s.[4] s.[3] s.[2] ^ if Char.equal s.[7] '1' then "." else "")
;;
