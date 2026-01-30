open! Core

let%expect_test "SUPER RED" =
  let red_oklab = Oklab.of_rgb ~r:1.0 ~g:0.0 ~b:0.0 () in
  let red_oklch = Oklab.Lch.of_lab red_oklab in
  let super_red_oklch = Oklab.Lch.set_chroma red_oklch 1.0 in
  let super_red_oklab = Oklab.Lch.to_lab super_red_oklch in
  print_endline (Oklab.to_string_css super_red_oklab);
  [%expect {| oklab(0.627955 0.872633 0.488376) |}];
  print_endline (Oklab.to_string_hex super_red_oklab);
  [%expect {| #ff0000ff |}]
;;
