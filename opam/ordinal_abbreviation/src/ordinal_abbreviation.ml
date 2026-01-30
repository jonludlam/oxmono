open! Base
include Ordinal_abbreviation_intf

module Make (Integer : Int.S_common) = struct
  (* We have to assume [of_int_exn] can handle numbers up to 100. *)
  let ten = Integer.of_int_exn 10
  let twenty = Integer.of_int_exn 20
  let hundred = Integer.of_int_exn 100

  let digit_to_suffix digit =
    (* We have to assume [to_int_exn] can handle numbers up to 9. *)
    match Integer.to_int_exn digit with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | 4 | 5 | 6 | 7 | 8 | 9 | 0 -> "th"
    | _ -> assert false
  ;;

  let to_suffix integer =
    let tens_and_ones = Integer.abs (Integer.rem integer hundred) in
    if Integer.( < ) tens_and_ones ten
    then digit_to_suffix tens_and_ones
    else if Integer.( < ) tens_and_ones twenty
    then "th"
    else digit_to_suffix (Integer.rem tens_and_ones ten)
  ;;

  let to_string integer = Integer.to_string integer ^ to_suffix integer

  let to_string_hum ?delimiter integer =
    Integer.to_string_hum ?delimiter integer ^ to_suffix integer
  ;;
end

include Make (Int)
module Int32 = Make (Int32)
module Int63 = Make (Int63)
module Int64 = Make (Int64)
module Nativeint = Make (Nativeint)
