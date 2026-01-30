open! Base

type t =
  | LVCMOS33
  | LVCMOS18
[@@deriving sexp]

let to_string = function
  | LVCMOS33 -> "LVCMOS33"
  | LVCMOS18 -> "LVCMOS18"
;;

let of_string = function
  | "LVCMOS33" -> LVCMOS33
  | "LVCMOS18" -> LVCMOS18
  | iostandard -> raise_s [%message "Unknown IO standard" (iostandard : string)]
;;
