open! Core
open Expectable

let%expect_test "print with homogeneous types" =
  print [ [%sexp { a = "foo"; b = "bar" }]; [%sexp { a = "baz"; b = "qux" }] ];
  [%expect
    {|
    ┌─────┬─────┐
    │ a   │ b   │
    ├─────┼─────┤
    │ foo │ bar │
    │ baz │ qux │
    └─────┴─────┘
    |}]
;;

let%expect_test "print with heterogeneous types" =
  print [ [%sexp { a = "foo"; b = "bar" }]; [%sexp { a = "baz"; c = "qux" }] ];
  [%expect
    {|
    ┌─────┬─────┬─────┐
    │ a   │ b   │ c   │
    ├─────┼─────┼─────┤
    │ foo │ bar │     │
    │ baz │     │ qux │
    └─────┴─────┴─────┘
    |}]
;;

let%expect_test "print works with nested keys" =
  print
    [ [%sexp { a = "foo"; b = { x = 1; y = 2 } }]
    ; [%sexp { a = "baz"; b = { x = 3; y = 4 } }]
    ];
  [%expect
    {|
    ┌─────┬─────┬─────┐
    │ a   │ b.x │ b.y │
    ├─────┼─────┼─────┤
    │ foo │ 1   │ 2   │
    │ baz │ 3   │ 4   │
    └─────┴─────┴─────┘
    |}]
;;

let%expect_test "print max_depth" =
  let records = [ [%sexp { a = "foo"; b = { x = 1; y = 2 } }] ] in
  print records ~max_depth:0;
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐
    ├┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┤
    │ ((a foo) (b ((x 1) (y 2)))) │
    └─────────────────────────────┘
    |}];
  print records ~max_depth:1;
  [%expect
    {|
    ┌─────┬───────────────┐
    │ a   │ b             │
    ├─────┼───────────────┤
    │ foo │ ((x 1) (y 2)) │
    └─────┴───────────────┘
    |}];
  print records ~max_depth:2;
  [%expect
    {|
    ┌─────┬─────┬─────┐
    │ a   │ b.x │ b.y │
    ├─────┼─────┼─────┤
    │ foo │ 1   │ 2   │
    └─────┴─────┴─────┘
    |}]
;;

let%expect_test "max_depth applies to lists as well" =
  let records = [ [%sexp { a = "foo"; b = [ 1; 2 ] }] ] in
  print records ~max_depth:1;
  [%expect
    {|
    ┌─────┬───────┐
    │ a   │ b     │
    ├─────┼───────┤
    │ foo │ (1 2) │
    └─────┴───────┘
    |}];
  print records ~max_depth:2;
  [%expect
    {|
    ┌─────┬───┐
    │ a   │ b │
    ├─────┼───┤
    │ foo │ 1 │
    │     │ 2 │
    └─────┴───┘
    |}]
;;

let%expect_test "print works with nested keys even if they are only sometimes nested" =
  print
    [ [%sexp { a = "foo"; b = { x = 1; y = 2 } }]; [%sexp { a = "foo"; b = "actual" }] ];
  [%expect
    {|
    ┌─────┬────────┬─────┬─────┐
    │ a   │ b      │ b.x │ b.y │
    ├─────┼────────┼─────┼─────┤
    │ foo │        │ 1   │ 2   │
    │ foo │ actual │     │     │
    └─────┴────────┴─────┴─────┘
    |}]
;;

let%expect_test "nested_columns" =
  let custom_example = function
    | "some_long_column_name" :: rest -> String.concat rest ~sep:"-"
    | cols -> Expectable.Column_display.render `auto cols
  in
  let print_table ~(nested_columns : Column_display.t) =
    print
      ~nested_columns
      [ [%sexp { a = "foo"; b = { x = 1; y = 2 } }]
      ; [%sexp { a = "foo"; b = "actual" }]
      ; [%sexp { a = "foo"; some_long_column_name = { x = 1; y = 2 } }]
      ; [%sexp { a = "foo"; some_long_column_name = { nested = { more = 3 } } }]
      ]
  in
  List.iter (Column_display.all ~custom:custom_example) ~f:(fun nested_columns ->
    print_s [%message "" ~_:(nested_columns : Column_display.t)];
    print_table ~nested_columns);
  [%expect
    {|
    (custom <fun>)
    ┌─────┬────────┬─────┬─────┬───┬───┬─────────────┐
    │ a   │ b      │ b.x │ b.y │ x │ y │ nested-more │
    ├─────┼────────┼─────┼─────┼───┼───┼─────────────┤
    │ foo │        │ 1   │ 2   │   │   │             │
    │ foo │ actual │     │     │   │   │             │
    │ foo │        │     │     │ 1 │ 2 │             │
    │ foo │        │     │     │   │   │ 3           │
    └─────┴────────┴─────┴─────┴───┴───┴─────────────┘

    auto
    ┌─────┬────────┬─────┬─────┬───────────────────────┬───────────────────────┬───────────────────────┐
    │     │        │     │     │                       │                       │ some_long_column_name │
    │     │        │     │     │ some_long_column_name │ some_long_column_name │ nested                │
    │ a   │ b      │ b.x │ b.y │ x                     │ y                     │ more                  │
    ├─────┼────────┼─────┼─────┼───────────────────────┼───────────────────────┼───────────────────────┤
    │ foo │        │ 1   │ 2   │                       │                       │                       │
    │ foo │ actual │     │     │                       │                       │                       │
    │ foo │        │     │     │ 1                     │ 2                     │                       │
    │ foo │        │     │     │                       │                       │ 3                     │
    └─────┴────────┴─────┴─────┴───────────────────────┴───────────────────────┴───────────────────────┘

    dotted
    ┌─────┬────────┬─────┬─────┬─────────────────────────┬─────────────────────────┬───────────────────────────────────┐
    │ a   │ b      │ b.x │ b.y │ some_long_column_name.x │ some_long_column_name.y │ some_long_column_name.nested.more │
    ├─────┼────────┼─────┼─────┼─────────────────────────┼─────────────────────────┼───────────────────────────────────┤
    │ foo │        │ 1   │ 2   │                         │                         │                                   │
    │ foo │ actual │     │     │                         │                         │                                   │
    │ foo │        │     │     │ 1                       │ 2                       │                                   │
    │ foo │        │     │     │                         │                         │ 3                                 │
    └─────┴────────┴─────┴─────┴─────────────────────────┴─────────────────────────┴───────────────────────────────────┘

    stacked
    ┌─────┬────────┬───┬───┬───────────────────────┬───────────────────────┬───────────────────────┐
    │     │        │   │   │                       │                       │ some_long_column_name │
    │     │        │ b │ b │ some_long_column_name │ some_long_column_name │ nested                │
    │ a   │ b      │ x │ y │ x                     │ y                     │ more                  │
    ├─────┼────────┼───┼───┼───────────────────────┼───────────────────────┼───────────────────────┤
    │ foo │        │ 1 │ 2 │                       │                       │                       │
    │ foo │ actual │   │   │                       │                       │                       │
    │ foo │        │   │   │ 1                     │ 2                     │                       │
    │ foo │        │   │   │                       │                       │ 3                     │
    └─────┴────────┴───┴───┴───────────────────────┴───────────────────────┴───────────────────────┘

    last
    ┌─────┬────────┬───┬───┬───┬───┬──────┐
    │ a   │ b      │ x │ y │ x │ y │ more │
    ├─────┼────────┼───┼───┼───┼───┼──────┤
    │ foo │        │ 1 │ 2 │   │   │      │
    │ foo │ actual │   │   │   │   │      │
    │ foo │        │   │   │ 1 │ 2 │      │
    │ foo │        │   │   │   │   │ 3    │
    └─────┴────────┴───┴───┴───┴───┴──────┘
    |}]
;;

let%expect_test "print respects [limit_width_to] for tables with many columns" =
  print
    [ [%sexp
        { a = "foo"
        ; some_long_column_a =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam tincidunt \
             augue nisl, tincidunt pellentesque libero sodales ac."
        ; some_long_column_b =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam tincidunt \
             augue nisl, tincidunt pellentesque libero sodales ac."
        }]
    ];
  [%expect
    {|
    ┌─────┬──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ a   │ some_long_column_a                                                                       │ some_long_column_b                                                                       │
    ├─────┼──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ foo │ Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam tincidunt augue nisl,       │ Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam tincidunt augue nisl,       │
    │     │ tincidunt pellentesque libero sodales ac.                                                │ tincidunt pellentesque libero sodales ac.                                                │
    └─────┴──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  print
    ~limit_width_to:90
    [ [%sexp
        { a = "foo"
        ; some_long_column_a =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam tincidunt \
             augue nisl, tincidunt pellentesque libero sodales ac."
        ; some_long_column_b =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam tincidunt \
             augue nisl, tincidunt pellentesque libero sodales ac."
        }]
    ];
  [%expect
    {|
    ┌─────┬────────────────────────────────────────┬─────────────────────────────────────────┐
    │ a   │ some_long_column_a                     │ some_long_column_b                      │
    ├─────┼────────────────────────────────────────┼─────────────────────────────────────────┤
    │ foo │ Lorem ipsum dolor sit amet,            │ Lorem ipsum dolor sit amet,             │
    │     │ consectetur adipiscing elit. Nam       │ consectetur adipiscing elit. Nam        │
    │     │ tincidunt augue nisl, tincidunt        │ tincidunt augue nisl, tincidunt         │
    │     │ pellentesque libero sodales ac.        │ pellentesque libero sodales ac.         │
    └─────┴────────────────────────────────────────┴─────────────────────────────────────────┘
    |}]
;;

let%expect_test "automatic numeric alignment" =
  let records =
    [ [%sexp { first_field = 100; second_field = 10.231; third_field = "+1" }]
    ; [%sexp { first_field = 2.1; second_field = 102.88; third_field = 1 }]
    ; [%sexp { first_field = "100_000"; second_field = -20.1; third_field = -1 }]
    ]
  in
  print records;
  [%expect
    {|
    ┌─────────────┬──────────────┬─────────────┐
    │ first_field │ second_field │ third_field │
    ├─────────────┼──────────────┼─────────────┤
    │     100     │  10.231      │ +1          │
    │       2.1   │ 102.88       │  1          │
    │ 100_000     │ -20.1        │ -1          │
    └─────────────┴──────────────┴─────────────┘
    |}];
  print records ~align:`left;
  [%expect
    {|
    ┌─────────────┬──────────────┬─────────────┐
    │ first_field │ second_field │ third_field │
    ├─────────────┼──────────────┼─────────────┤
    │ 100         │ 10.231       │ +1          │
    │ 2.1         │ 102.88       │ 1           │
    │ 100_000     │ -20.1        │ -1          │
    └─────────────┴──────────────┴─────────────┘
    |}]
;;

let%expect_test "automatic numeric alignment works through lists" =
  print
    ~separate_rows:true
    [ [%sexp { first_field = [ 100; 2.1; 1234 ]; second_field = 10.231 }]
    ; [%sexp { first_field = [ 102; []; 12 ]; second_field = [ 10; 1 ] }]
    ];
  [%expect
    {|
    ┌─────────────┬──────────────┐
    │ first_field │ second_field │
    ├─────────────┼──────────────┤
    │  100        │ 10.231       │
    │    2.1      │              │
    │ 1234        │              │
    ├─────────────┼──────────────┤
    │  102        │ 10           │
    │             │  1           │
    │   12        │              │
    └─────────────┴──────────────┘
    |}]
;;

let%expect_test "no automatic numeric alignment if a column contains non-numbers" =
  print
    ~separate_rows:true
    [ [%sexp { foo = [ 100; -2.1; "label"; 1234 ] }]; [%sexp { foo = [ 102; []; 12 ] }] ];
  [%expect
    {|
    ┌───────┐
    │ foo   │
    ├───────┤
    │ 100   │
    │ -2.1  │
    │ label │
    │ 1234  │
    ├───────┤
    │ 102   │
    │       │
    │ 12    │
    └───────┘
    |}]
;;

let%expect_test "print with list values" =
  print
    [ [%sexp { a = 1; b = [] }]
    ; [%sexp { a = 2; b = [ 1 ] }]
    ; [%sexp { a = 3; b = [ 1; 2 ] }]
    ; [%sexp { a = 4; b = [ 1; 2; 3 ] }]
    ];
  [%expect
    {|
    ┌───┬───┐
    │ a │ b │
    ├───┼───┤
    │ 1 │   │
    │ 2 │ 1 │
    │ 3 │ 1 │
    │   │ 2 │
    │ 4 │ 1 │
    │   │ 2 │
    │   │ 3 │
    └───┴───┘
    |}]
;;

let%expect_test "print with optional list values" =
  let records =
    [ [%sexp { a = 1; b = [] }]
    ; [%sexp { a = 2; b = [ [ 1 ] ] }]
    ; [%sexp { a = 3; b = [ [ 1 ]; [ 2 ] ] }]
    ; [%sexp { a = 4; b = [ [ 1 ]; []; [ 3 ] ] }]
    ; [%sexp { a = 5; b = [ [ 1 ]; []; [ 3 ]; [] ] }]
    ]
  in
  print records;
  [%expect
    {|
    ┌───┬───┐
    │ a │ b │
    ├───┼───┤
    │ 1 │   │
    │ 2 │ 1 │
    │ 3 │ 1 │
    │   │ 2 │
    │ 4 │ 1 │
    │   │   │
    │   │ 3 │
    │ 5 │ 1 │
    │   │   │
    │   │ 3 │
    │   │   │
    └───┴───┘
    |}];
  print records ~separate_rows:true;
  [%expect
    {|
    ┌───┬───┐
    │ a │ b │
    ├───┼───┤
    │ 1 │   │
    ├───┼───┤
    │ 2 │ 1 │
    ├───┼───┤
    │ 3 │ 1 │
    │   │ 2 │
    ├───┼───┤
    │ 4 │ 1 │
    │   │   │
    │   │ 3 │
    ├───┼───┤
    │ 5 │ 1 │
    │   │   │
    │   │ 3 │
    │   │   │
    └───┴───┘
    |}]
;;

let%expect_test "output with nested lists is completely ambiguous" =
  print
    [ [%sexp { a = 1; b = [ 1; 2 ] }]
    ; [%sexp { a = 2; b = [ [ 1; 2 ] ] }]
    ; [%sexp { a = 3; b = [ [ [ [ 1 ] ]; 2 ] ] }]
    ];
  [%expect
    {|
    ┌───┬───┐
    │ a │ b │
    ├───┼───┤
    │ 1 │ 1 │
    │   │ 2 │
    │ 2 │ 1 │
    │   │ 2 │
    │ 3 │ 1 │
    │   │ 2 │
    └───┴───┘
    |}]
;;

let%expect_test "optional records" =
  print
    [ [%sexp { name = "first"; foo = { hi = 10; lo = 11 } }]
    ; [%sexp { name = "second"; foo = { hi = 10; lo = 11 } }]
    ];
  [%expect
    {|
    ┌────────┬────────┬────────┐
    │ name   │ foo.hi │ foo.lo │
    ├────────┼────────┼────────┤
    │ first  │ 10     │ 11     │
    │ second │ 10     │ 11     │
    └────────┴────────┴────────┘
    |}];
  print
    [ [%sexp { name = "first"; foo = [] }]
    ; [%sexp { name = "second"; foo = [ { hi = 10; lo = 11 } ] }]
    ];
  [%expect
    {|
    ┌────────┬────────┬────────┐
    │ name   │ foo.hi │ foo.lo │
    ├────────┼────────┼────────┤
    │ first  │        │        │
    │ second │ 10     │ 11     │
    └────────┴────────┴────────┘
    |}];
  print
    [ [%sexp { name = "first"; foo = [] }]
    ; [%sexp { name = "second"; foo = [ { hi = 10; lo = 11 }; { hi = 9; lo = 12 } ] }]
    ];
  [%expect
    {|
    ┌────────┬────────┬────────┐
    │ name   │ foo.hi │ foo.lo │
    ├────────┼────────┼────────┤
    │ first  │        │        │
    │ second │ 10     │ 11     │
    │        │  9     │ 12     │
    └────────┴────────┴────────┘
    |}]
;;

let%expect_test "list of records with different keys are vertically aligned correctly" =
  print
    [ [%sexp { name = "first"; foo = [] }]
    ; [%sexp { name = "second"; foo = [ { hi = 10 }; { lo = 12 } ] }]
    ];
  [%expect
    {|
    ┌────────┬────────┬────────┐
    │ name   │ foo.hi │ foo.lo │
    ├────────┼────────┼────────┤
    │ first  │        │        │
    │ second │ 10     │        │
    │        │        │ 12     │
    └────────┴────────┴────────┘
    |}]
;;

let%expect_test "print_alist" =
  [ "top left", [%sexp { x = 5; y = 6 }]; "bottom right", [%sexp { x = 0; y = 1.2 }] ]
  |> print_alist Fn.id;
  [%expect
    {|
    ┌──────────────┬───┬─────┐
    │              │ x │ y   │
    ├──────────────┼───┼─────┤
    │ top left     │ 5 │ 6   │
    │ bottom right │ 0 │ 1.2 │
    └──────────────┴───┴─────┘
    |}]
;;

let%expect_test "print_alist with optional values" =
  [ "top left", [%sexp [ { x = 5; y = 6 } ]]; "bottom right", [%sexp []] ]
  |> print_alist Fn.id;
  [%expect
    {|
    ┌──────────────┬───┬───┐
    │              │ x │ y │
    ├──────────────┼───┼───┤
    │ top left     │ 5 │ 6 │
    │ bottom right │   │   │
    └──────────────┴───┴───┘
    |}]
;;

let%expect_test "print_alist with nested optionals" =
  [ "top left", [%sexp [ { x = [ 5 ]; y = [ 6 ] } ]]
  ; "bottom right", [%sexp { x = []; y = [ 1 ] }]
  ]
  |> print_alist Fn.id;
  [%expect
    {|
    ┌──────────────┬───┬───┐
    │              │ x │ y │
    ├──────────────┼───┼───┤
    │ top left     │ 5 │ 6 │
    │ bottom right │   │ 1 │
    └──────────────┴───┴───┘
    |}]
;;

let%expect_test "print_record_transposed" =
  [%sexp { top_left = [ { x = 5; y = 6 } ]; bottom_right = [] }]
  |> print_record_transposed;
  [%expect
    {|
    ┌──────────────┬───┬───┐
    │              │ x │ y │
    ├──────────────┼───┼───┤
    │ top_left     │ 5 │ 6 │
    │ bottom_right │   │   │
    └──────────────┴───┴───┘
    |}]
;;

let%expect_test "print with nested optionals" =
  [ [%sexp { name = "top_left"; value = { x = [ 5 ]; y = [ 6 ] } }]
  ; [%sexp { name = "bottom_right"; value = { x = []; y = [ 1 ] } }]
  ]
  |> print;
  [%expect
    {|
    ┌──────────────┬─────────┬─────────┐
    │ name         │ value.x │ value.y │
    ├──────────────┼─────────┼─────────┤
    │ top_left     │ 5       │ 6       │
    │ bottom_right │         │ 1       │
    └──────────────┴─────────┴─────────┘
    |}]
;;

let%expect_test "align right" =
  [%sexp { top_left = { x = 5; y = 6 }; bottom_right = { x = 1234; y = 600 } }]
  |> print_record_transposed ~align:`right;
  [%expect
    {|
    ┌──────────────┬──────┬─────┐
    │              │    x │   y │
    ├──────────────┼──────┼─────┤
    │     top_left │    5 │   6 │
    │ bottom_right │ 1234 │ 600 │
    └──────────────┴──────┴─────┘
    |}]
;;

module Variant = struct
  type t =
    | Foo
    | Bar of int
  [@@deriving sexp_of]
end

let%expect_test "variants are not incorrectly parsed as records" =
  print
    [ [%sexp { a = (Foo : Variant.t); b = (Bar 1 : Variant.t) }]
    ; [%sexp { a = "baz"; b = "qux" }]
    ];
  [%expect
    {|
    ┌─────┬─────────┐
    │ a   │ b       │
    ├─────┼─────────┤
    │ Foo │ (Bar 1) │
    │ baz │ qux     │
    └─────┴─────────┘
    |}]
;;

let%expect_test "optional variants are not incorrectly parsed as records" =
  let sexp = [%sexp { a = (Foo : Variant.t); b = (Some (Bar 1) : Variant.t option) }] in
  print_s sexp;
  print [ sexp ];
  [%expect
    {|
    ((a Foo) (b ((Bar 1))))
    ┌─────┬─────────┐
    │ a   │ b       │
    ├─────┼─────────┤
    │ Foo │ (Bar 1) │
    └─────┴─────────┘
    |}]
;;

let%expect_test "lists of variants are not incorrectly parsed as records" =
  let sexp = [%sexp { a = (Foo : Variant.t); b = ([ Bar 1; Bar 2 ] : Variant.t list) }] in
  print_s sexp;
  print [ sexp ];
  [%expect
    {|
    ((a Foo) (b ((Bar 1) (Bar 2))))
    ┌─────┬─────────┐
    │ a   │ b       │
    ├─────┼─────────┤
    │ Foo │ (Bar 1) │
    │     │ (Bar 2) │
    └─────┴─────────┘
    |}]
;;

let%expect_test "print_cases" =
  print_cases
    ~sexp_of_input:[%sexp_of: int]
    ~sexp_of_output:[%sexp_of: int]
    ~f:(fun x -> x * x)
    [ 1; 2; 3; 4; 5 ];
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┴┤
    │ 1 │  1 │
    │ 2 │  4 │
    │ 3 │  9 │
    │ 4 │ 16 │
    │ 5 │ 25 │
    └───┴────┘
    |}]
;;

let%expect_test "print_cases with record output" =
  print_cases
    ~sexp_of_input:[%sexp_of: int]
    ~sexp_of_output:(fun (x, x2, x3) -> [%sexp { x : int; x2 : int; x3 : int }])
    ~f:(fun x -> x, x * x, x * x * x)
    [ 1; 2; 3; 4; 5 ];
  [%expect
    {|
    ┌───┬───┬────┬─────┐
    │   │ x │ x2 │ x3  │
    ├───┼───┼────┼─────┤
    │ 1 │ 1 │  1 │   1 │
    │ 2 │ 2 │  4 │   8 │
    │ 3 │ 3 │  9 │  27 │
    │ 4 │ 4 │ 16 │  64 │
    │ 5 │ 5 │ 25 │ 125 │
    └───┴───┴────┴─────┘
    |}]
;;

let%expect_test "print_cases with record input and output" =
  List.iter Bool.all ~f:(fun separate_cols ->
    print_cases
      ~separate_cols
      ~sexp_of_input:(fun x -> [%sexp { x : int; x' = (x + 1 : int) }])
      ~sexp_of_output:(fun (x, x2, x3) -> [%sexp { x : int; x2 : int; x3 : int }])
      ~f:(fun x -> x, x * x, x * x * x)
      [ 1; 2; 3; 4; 5 ]);
  [%expect
    {|
    ┌───┬────┬───┬────┬─────┐
    │ x │ x' │ x │ x2 │ x3  │
    ├───┼────┼───┼────┼─────┤
    │ 1 │ 2  │ 1 │  1 │   1 │
    │ 2 │ 3  │ 2 │  4 │   8 │
    │ 3 │ 4  │ 3 │  9 │  27 │
    │ 4 │ 5  │ 4 │ 16 │  64 │
    │ 5 │ 6  │ 5 │ 25 │ 125 │
    └───┴────┴───┴────┴─────┘

    ┌───┬────┬──┬───┬────┬─────┐
    │ x │ x' │  │ x │ x2 │ x3  │
    ├───┼────┼──┼───┼────┼─────┤
    │ 1 │ 2  │  │ 1 │  1 │   1 │
    │ 2 │ 3  │  │ 2 │  4 │   8 │
    │ 3 │ 4  │  │ 3 │  9 │  27 │
    │ 4 │ 5  │  │ 4 │ 16 │  64 │
    │ 5 │ 6  │  │ 5 │ 25 │ 125 │
    └───┴────┴──┴───┴────┴─────┘
    |}]
;;

let%expect_test "named tuples" =
  (* because this is public-release, we can't acutally put labeled tuples in the test, so
     we're hardcoding the sexp representation *)
  print [ [%sexp [ [ "~foo"; 1 ]; [ "~bar"; 2 ] ]] ];
  [%expect
    {|
    ┌──────┬──────┐
    │ ~foo │ ~bar │
    ├──────┼──────┤
    │ 1    │ 2    │
    └──────┴──────┘
    |}]
;;
