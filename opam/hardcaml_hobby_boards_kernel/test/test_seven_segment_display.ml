open Core

let%expect_test "hex codes" =
  Array.iter
    Hardcaml_hobby_boards_kernel.Seven_segment_display.hex_codes
    ~f:Hardcaml_hobby_boards_kernel.Seven_segment_display.print;
  [%expect
    {|
     _
    | |
    |_|

      |
      |
     _
     _|
    |_
     _
     _|
     _|

    |_|
      |
     _
    |_
     _|
     _
    |_
    |_|
     _
      |
      |
     _
    |_|
    |_|
     _
    |_|
      |
     _
     _|
    |_|

    |_
    |_|
     _
    |
    |_

     _|
    |_|
     _
    |_
    |_
     _
    |_
    |
    |}]
;;

let%expect_test "add dot" =
  Hardcaml_hobby_boards_kernel.Seven_segment_display.(hex_codes.(5) |> add_dot |> print);
  [%expect
    {|
     _
    |_
     _|.
    |}]
;;
