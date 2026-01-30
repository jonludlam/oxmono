open! Core
open! Expectree

(* $MDX part-begin=sample_sexp *)
let my_tree_shaped_sexp : Sexp.t =
  [%sexp
    "lib"
    , ( "bonsai"
      , [ "examples"
        ; "extra"
        ; "src", [ "driver"; "protocol" ]
        ; "test"
        ; "web"
        ; "web_ui"
        ; "web_test"
        ] )]
;;

let%expect_test _ =
  print_s my_tree_shaped_sexp;
  [%expect
    {|
    (lib
     (bonsai (examples extra (src (driver protocol)) test web web_ui web_test)))
    |}]
;;

(* $MDX part-end *)

let my_tree : Expectree.t =
  Branch
    ( "lib"
    , [ Branch
          ( "bonsai"
          , [ Leaf "examples"
            ; Leaf "extra"
            ; Branch ("src", [ Leaf "driver"; Leaf "protocol" ])
            ; Leaf "test"
            ; Leaf "web"
            ; Leaf "web_test"
            ; Leaf "web_ui"
            ] )
      ] )
;;

let%expect_test _ =
  print_endline (Expectree.to_string my_tree);
  [%expect
    {|
    lib
    ╰─╴bonsai
       ├─╴examples
       ├─╴extra
       ├─╴src
       │  ├─╴driver
       │  ╰─╴protocol
       ├─╴test
       ├─╴web
       ├─╴web_test
       ╰─╴web_ui
    |}]
;;

(* $MDX part-begin=sample_expectree *)
let%expect_test _ =
  print_endline (Expectree.sexp_to_string my_tree_shaped_sexp);
  [%expect
    {|
    lib
    ╰─╴bonsai
       ├─╴examples
       ├─╴extra
       ├─╴src
       │  ├─╴driver
       │  ╰─╴protocol
       ├─╴test
       ├─╴web
       ├─╴web_ui
       ╰─╴web_test
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=anon_branch *)
let%expect_test _ =
  {| (a (b (c d e) ())) |} |> Sexp.of_string |> Expectree.sexp_to_string |> print_endline;
  [%expect
    {|
    a
    ├─╴b
    ├──┬─╴c
    │  ├─╴d
    │  ╰─╴e
    ╰─╴•
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=record *)
let%expect_test _ =
  let module T = struct
    type t =
      { a : string
      ; b : int
      ; c : bool
      }
    [@@deriving sexp_of]
  end
  in
  { a = "hi"; b = 5; c = true }
  |> [%sexp_of: T.t]
  |> Expectree.sexp_to_string ~inline_pairs:false
  |> print_endline;
  [%expect
    {|
    ╭──┬─╴a
    │  ╰─╴hi
    ├──┬─╴b
    │  ╰─╴5
    ╰──┬─╴c
       ╰─╴true
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=record_inline *)
let%expect_test _ =
  let module T = struct
    type t =
      { a : string
      ; b : int
      ; c : bool
      }
    [@@deriving sexp_of]
  end
  in
  { a = "hi"; b = 5; c = true }
  |> [%sexp_of: T.t]
  |> Expectree.sexp_to_string
  |> print_endline;
  [%expect
    {|
    ╭─╴a╶╴hi
    ├─╴b╶╴5
    ╰─╴c╶╴true
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=variants *)
let%expect_test _ =
  let module T = struct
    type t =
      | A of
          { a : string
          ; b : int
          }
      | B of string
      | C of string * int
    [@@deriving sexp_of]
  end
  in
  [ A { a = "hi"; b = 5 }; B "hello"; C ("hi", 5) ]
  |> [%sexp_of: T.t list]
  |> Expectree.sexp_to_string
  |> print_endline;
  [%expect
    {|
    ╭──┬─╴A
    │  ├─╴a╶╴hi
    │  ╰─╴b╶╴5
    ├─╴B╶╴hello
    ╰──┬─╴C
       ├─╴hi
       ╰─╴5
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=poly_variants *)
let%expect_test _ =
  let module T = struct
    type t =
      [ `B of string
      | `C of string * int
      ]
    [@@deriving sexp_of]
  end
  in
  `C ("hi", 5) |> [%sexp_of: T.t] |> Expectree.sexp_to_string |> print_endline;
  [%expect
    {|
    C
    ├─╴hi
    ╰─╴5
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=cats *)
let%expect_test _ =
  print_endline
    (Expectree.to_string
       (Branch
          ( "Felidae"
          , [ Leaf "Pantherinae"
            ; Branch
                ( "Felinae"
                , [ Branch
                      ( "Felis"
                      , [ Split
                            [ Leaf "Jungle cat"
                            ; Split
                                [ Leaf "Black-footed cat"
                                ; Split
                                    [ Leaf "Sand cat"
                                    ; Split
                                        [ Split
                                            [ Leaf "Chinese mountain cat"
                                            ; Leaf "African wildcat"
                                            ]
                                        ; Split
                                            [ Leaf "European wildcat"
                                            ; Leaf "Domestic cat"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ] )
                  ; Leaf "(other Felinae lineages)"
                  ] )
            ] )));
  [%expect
    {|
    Felidae
    ├─╴Pantherinae
    ╰─╴Felinae
       ├─╴Felis
       │  ╰──┬─╴Jungle cat
       │     ╰──┬─╴Black-footed cat
       │        ╰──┬─╴Sand cat
       │           ╰──┬──┬─╴Chinese mountain cat
       │              │  ╰─╴African wildcat
       │              ╰──┬─╴European wildcat
       │                 ╰─╴Domestic cat
       ╰─╴(other Felinae lineages)
    |}]
;;
(* $MDX part-end *)
