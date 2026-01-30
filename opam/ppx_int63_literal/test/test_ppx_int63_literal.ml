open! Base

let%test_unit "check that various literals are interpreted as expected" =
  let int63_literals_equal_to_42 = [ 42J; 0x2aJ; 0o52J; 0b101010J ] in
  List.iter int63_literals_equal_to_42 ~f:(fun i ->
    [%test_result: int] ~expect:42 (Int63.to_int_exn i));
  let int63_literals_equal_to_one_million =
    [ 1_000_000J; 0xF_42_40J; 0o364_1100J; 0b1111_0100_0010_0100_0000J ]
  in
  List.iter int63_literals_equal_to_one_million ~f:(fun i ->
    [%test_result: int] ~expect:1_000_000 (Int63.to_int_exn i));
  let int63_literals_equal_to_int63_max_int =
    [ 4611686018427387903J
    ; 0x3fffffffffffffffJ
    ; 0o377777777777777777777J
    ; 0b11111111111111111111111111111111111111111111111111111111111111J
    ]
  in
  List.iter int63_literals_equal_to_int63_max_int ~f:(fun i ->
    [%test_result: Int63.t] ~expect:Int63.max_value i);
  let literals_that_overflow_to_negative =
    [ 0x4000000000000000J
    ; 0o400000000000000000000J
    ; 0b100000000000000000000000000000000000000000000000000000000000000J
    ]
  in
  List.iter literals_that_overflow_to_negative ~f:(fun i ->
    [%test_result: Int63.t] ~expect:Int63.min_value i)
;;
