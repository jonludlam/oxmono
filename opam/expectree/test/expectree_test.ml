open! Core

module%test [@name "t"] _ = struct
  let test tree = Expectree.to_string tree |> print_endline

  let%expect_test _ =
    test (Leaf "hi");
    [%expect {| hi |}]
  ;;

  let%expect_test _ =
    test (Leaf "hello\nworld");
    [%expect
      {|
      hello
      world
      |}]
  ;;

  let%expect_test _ =
    test (Branch ("hello\nworld", [ Leaf "a"; Leaf "b" ]));
    [%expect
      {|
      hello
      world
      ├─╴a
      ╰─╴b
      |}]
  ;;

  let%expect_test _ =
    test (Branch ("a", [ Leaf "hello\nworld"; Leaf "b" ]));
    [%expect
      {|
      a
      ├─╴hello
      │  world
      ╰─╴b
      |}]
  ;;

  let%expect_test _ =
    test
      (Branch
         ( "a"
         , [ Branch ("hello\nworld", [ Leaf "a"; Leaf "another\nmultiline" ]); Leaf "b" ]
         ));
    [%expect
      {|
      a
      ├─╴hello
      │  world
      │  ├─╴a
      │  ╰─╴another
      │     multiline
      ╰─╴b
      |}]
  ;;

  let%expect_test _ =
    test
      (Split
         [ Leaf "a"
         ; Branch
             ( "hello\nworld"
             , [ Leaf "a"
               ; Split
                   [ Leaf "another\nmultiline"
                   ; Split [ Leaf "a\nb\nc" ]
                   ; Leaf "foo\nbar"
                   ]
               ; Split [ Leaf "a\n b\nc" ]
               ] )
         ; Leaf "b"
         ]);
    [%expect
      {|
      ╭─╴a
      ├─╴hello
      │  world
      │  ├─╴a
      │  ├──┬─╴another
      │  │  │  multiline
      │  │  ├─╴a
      │  │  │  b
      │  │  │  c
      │  │  ╰─╴foo
      │  │     bar
      │  ╰─╴a
      │      b
      │     c
      ╰─╴b
      |}]
  ;;

  let%expect_test "newlines before item" =
    test (Branch ("a", [ Leaf "\nb"; Leaf "\nc" ]));
    [%expect
      {|
      a
      │
      ├─╴b
      │
      ╰─╴c
      |}]
  ;;

  let%expect_test "newlines before item" =
    test
      (Branch
         ("a", [ Leaf "\nb"; Leaf "\nc"; Split [ Leaf "\nd"; Leaf "\ne"; Leaf "\nf" ] ]));
    [%expect
      {|
      a
      │
      ├─╴b
      │
      ├─╴c
      │
      ╰──┬─╴d
         │
         ├─╴e
         │
         ╰─╴f
      |}]
  ;;

  let%expect_test "newlines after item" =
    test
      (Branch
         ("a\n", [ Leaf "b\n"; Leaf "c\n"; Split [ Leaf "d\n"; Leaf "e\n"; Leaf "f\n" ] ]));
    [%expect
      {|
      a
      │
      ├─╴b
      │
      ├─╴c
      │
      ╰──┬─╴d
         │
         ├─╴e
         │
         ╰─╴f
      |}]
  ;;

  let%expect_test "newlines before and after item" =
    test
      (Branch
         ( "\na\n\na\n"
         , [ Leaf "\nb\n\nb\n"
           ; Leaf "\nc\n\nc\n"
           ; Split
               [ Leaf "\nd\n\nd\n"
               ; Branch ("\ne\n\ne\n", [ Leaf "\nf\n\nf\n"; Leaf "\ng\n\ng\n" ])
               ; Leaf "\nh\n\nh\n"
               ]
           ] ));
    [%expect
      {|
      a

      a
      │
      │
      ├─╴b
      │
      │  b
      │
      │
      ├─╴c
      │
      │  c
      │
      │
      ╰──┬─╴d
         │
         │  d
         │
         │
         ├─╴e
         │
         │  e
         │  │
         │  │
         │  ├─╴f
         │  │
         │  │  f
         │  │
         │  │
         │  ╰─╴g
         │
         │     g
         │
         │
         ╰─╴h

            h
      |}]
  ;;

  let%expect_test _ =
    test (Split [ Leaf "hi" ]);
    [%expect {| hi |}]
  ;;

  let%expect_test _ =
    test (Branch ("hello", [ Split [ Leaf "world" ] ]));
    [%expect
      {|
      hello
      ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test (Split [ Leaf "hello"; Split [ Leaf "there"; Leaf "world" ] ]);
    [%expect
      {|
      ╭─╴hello
      ╰──┬─╴there
         ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test
      (Split
         [ Split [ Split [ Leaf "a"; Leaf "10" ]; Split [ Leaf "b"; Leaf "20" ] ]
         ; Split [ Split [ Leaf "c"; Leaf "30" ]; Split [ Leaf "d"; Leaf "40" ] ]
         ]);
    [%expect
      {|
      ╭──┬──┬─╴a
      │  │  ╰─╴10
      │  ╰──┬─╴b
      │     ╰─╴20
      ╰──┬──┬─╴c
         │  ╰─╴30
         ╰──┬─╴d
            ╰─╴40
      |}]
  ;;

  let%expect_test _ =
    test
      (Split [ Split [ Split [ Leaf "a"; Leaf "10" ]; Split [ Leaf "b"; Leaf "20" ] ] ]);
    [%expect
      {|
      ╭──┬─╴a
      │  ╰─╴10
      ╰──┬─╴b
         ╰─╴20
      |}]
  ;;

  let%expect_test _ =
    test (Split [ Leaf "hello"; Leaf "there"; Leaf "world" ]);
    [%expect
      {|
      ╭─╴hello
      ├─╴there
      ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test (Branch ("hello", [ Leaf "there"; Leaf "world" ]));
    [%expect
      {|
      hello
      ├─╴there
      ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test (Branch ("lib", [ Leaf "bonsai"; Split [ Leaf "examples"; Leaf "extra" ] ]));
    [%expect
      {|
      lib
      ├─╴bonsai
      ╰──┬─╴examples
         ╰─╴extra
      |}]
  ;;

  let%expect_test _ =
    test
      (Branch
         ( "hello"
         , [ Leaf "there"
           ; Branch ("nice", [ Leaf "world" ])
           ; Branch ("nice", [ Leaf "world" ])
           ] ));
    [%expect
      {|
      hello
      ├─╴there
      ├─╴nice
      │  ╰─╴world
      ╰─╴nice
         ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test
      (Branch
         ( "hello"
         , [ Leaf "there"
           ; Split [ Leaf "nice"; Leaf "world" ]
           ; Branch ("nice", [ Leaf "world" ])
           ] ));
    [%expect
      {|
      hello
      ├─╴there
      ├──┬─╴nice
      │  ╰─╴world
      ╰─╴nice
         ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test (Branch ("hello", [ Leaf "there"; Branch ("nice", [ Leaf "world" ]) ]));
    [%expect
      {|
      hello
      ├─╴there
      ╰─╴nice
         ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test
      (Split
         [ Branch ("a", [ Leaf "aa"; Leaf "ab" ])
         ; Branch ("b", [ Leaf "ba"; Leaf "bb" ])
         ]);
    [%expect
      {|
      ╭─╴a
      │  ├─╴aa
      │  ╰─╴ab
      ╰─╴b
         ├─╴ba
         ╰─╴bb
      |}]
  ;;
end

module%test [@name "sexp_conversion"] _ = struct
  let test ?inline_pairs sexp =
    Expectree.sexp_to_string ?inline_pairs sexp |> print_endline
  ;;

  let%expect_test _ =
    test (List []);
    [%expect {| • |}]
  ;;

  let%expect_test _ =
    test (Atom "hi");
    [%expect {| hi |}]
  ;;

  let%expect_test _ =
    test (List [ Atom "hello"; Atom "world" ]);
    [%expect {| hello╶╴world |}]
  ;;

  let%expect_test _ =
    test (List [ Atom "hello"; List [] ]);
    [%expect {| hello╶╴• |}]
  ;;

  let%expect_test _ =
    test (List [ Atom "hello"; Atom "there"; Atom "world" ]);
    [%expect
      {|
      ╭─╴hello
      ├─╴there
      ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test [%sexp [ "hello"; [ "there"; "you"; [ "absolutely"; [ "great" ] ] ]; "world" ]];
    [%expect
      {|
      ╭─╴hello
      ├──┬─╴there
      │  ├─╴you
      │  ╰─╴absolutely
      │     ╰─╴great
      ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test
      (List
         [ Atom "hello"
         ; List
             [ Atom "there"
             ; List [ Atom ""; Atom "you"; List [ Atom "lovely"; Atom "beautiful" ] ]
             ]
         ; Atom "world"
         ]);
    [%expect
      {|
      ╭─╴hello
      ├─╴there
      │  │
      │  ├─╴
      │  │
      │  ├─╴you
      │  ╰─╴lovely╶╴beautiful
      ╰─╴world
      |}]
  ;;

  let%expect_test _ =
    test (List [ Atom "none"; List [] ]);
    [%expect {| none╶╴• |}]
  ;;

  let%expect_test _ =
    test (List []);
    [%expect {| • |}]
  ;;

  let%expect_test _ =
    let module T = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving sexp_of]
    end
    in
    test ([%sexp_of: T.t] { a = 10; b = 20 });
    [%expect
      {|
      ╭─╴a╶╴10
      ╰─╴b╶╴20
      |}]
  ;;

  let%expect_test _ =
    test ([%sexp_of: string * int * float] ("hi", 5, 3.14));
    [%expect
      {|
      ╭─╴hi
      ├─╴5
      ╰─╴3.14
      |}]
  ;;

  let%expect_test _ =
    let module T = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving sexp_of]
    end
    in
    test ([%sexp_of: T.t list] [ { a = 10; b = 20 }; { a = 30; b = 40 } ]);
    [%expect
      {|
      ╭──┬─╴a╶╴10
      │  ╰─╴b╶╴20
      ╰──┬─╴a╶╴30
         ╰─╴b╶╴40
      |}]
  ;;

  let%expect_test _ =
    let module T = struct
      type t =
        | Pair of
            { a : int
            ; b : int
            }
      [@@deriving sexp_of]
    end
    in
    test ([%sexp_of: T.t] (Pair { a = 10; b = 20 }));
    [%expect
      {|
      ╭─╴Pair
      ├─╴a╶╴10
      ╰─╴b╶╴20
      |}]
  ;;

  let%expect_test _ =
    let module T = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving sexp_of]
    end
    in
    test ~inline_pairs:false ([%sexp_of: T.t] { a = 10; b = 20 });
    [%expect
      {|
      ╭──┬─╴a
      │  ╰─╴10
      ╰──┬─╴b
         ╰─╴20
      |}]
  ;;

  let%expect_test _ =
    let module T = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving sexp_of]
    end
    in
    test
      ([%sexp_of: T.t list]
         [ { a = 10; b = 20 }; { a = 30; b = 40 }; { a = 50; b = 60 } ]);
    [%expect
      {|
      ╭──┬─╴a╶╴10
      │  ╰─╴b╶╴20
      ├──┬─╴a╶╴30
      │  ╰─╴b╶╴40
      ╰──┬─╴a╶╴50
         ╰─╴b╶╴60
      |}]
  ;;

  let%expect_test _ =
    let module T = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving sexp_of]
    end
    in
    test
      ~inline_pairs:false
      ([%sexp_of: T.t list] [ { T.a = 10; b = 20 }; { a = 30; b = 40 } ]);
    [%expect
      {|
      ╭──┬──┬─╴a
      │  │  ╰─╴10
      │  ╰──┬─╴b
      │     ╰─╴20
      ╰──┬──┬─╴a
         │  ╰─╴30
         ╰──┬─╴b
            ╰─╴40
      |}]
  ;;

  module _ = struct
    type t =
      | X of
          { a : int
          ; b : int
          }
      | Y of string
      | Z
    [@@deriving sexp_of]

    let%expect_test _ =
      [ X { a = 10; b = 20 }; Y "hey guys!"; Z ] |> [%sexp_of: t list] |> test;
      [%expect
        {|
        ╭──┬─╴X
        │  ├─╴a╶╴10
        │  ╰─╴b╶╴20
        ├─╴Y╶╴hey guys!
        ╰─╴Z
        |}]
    ;;
  end

  module _ = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving sexp_of]

    let%expect_test _ =
      `my_list [ { a = 10; b = 20 }; { a = 30; b = 40 } ]
      |> [%sexp_of: [ `my_list of t list ]]
      |> test;
      [%expect
        {|
        my_list
        ├──┬─╴a╶╴10
        │  ╰─╴b╶╴20
        ╰──┬─╴a╶╴30
           ╰─╴b╶╴40
        |}]
    ;;
  end
end
