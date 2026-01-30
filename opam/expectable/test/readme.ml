open Core

module Group = struct
  type t =
    | Mammal
    | Fish
    | Amphibean
    | Reptile
    | Bird
    | Invertebrates
  [@@deriving sexp_of]
end

let%expect_test "compare age" =
  (* $MDX part-begin=basic-usage-example *)
  let test animal_name group age ~than =
    [%sexp
      { animal_name : string
      ; group : Group.t
      ; age : int
      ; is =
          (if age = than then "same age" else if age > than then "older" else "younger"
           : string)
      ; than : int
      }]
  in
  [ test "capybara" Mammal 10 ~than:11
  ; test "salmon" Fish 11 ~than:10
  ; test "snail" Invertebrates 11 ~than:10
  ; test "frog" Amphibean 10 ~than:10
  ; test "t-rex" Reptile 83_600_000 ~than:10
  ; test "hummingbird" Bird 10 ~than:10
  ]
  |> Expectable.print;
  [%expect
    {|
    ┌─────────────┬───────────────┬──────────┬──────────┬──────┐
    │ animal_name │ group         │ age      │ is       │ than │
    ├─────────────┼───────────────┼──────────┼──────────┼──────┤
    │ capybara    │ Mammal        │       10 │ younger  │ 11   │
    │ salmon      │ Fish          │       11 │ older    │ 10   │
    │ snail       │ Invertebrates │       11 │ older    │ 10   │
    │ frog        │ Amphibean     │       10 │ same age │ 10   │
    │ t-rex       │ Reptile       │ 83600000 │ older    │ 10   │
    │ hummingbird │ Bird          │       10 │ same age │ 10   │
    └─────────────┴───────────────┴──────────┴──────────┴──────┘
    |}]
;;

(* $MDX part-end *)

module Diet = struct
  type t =
    | Carnivore
    | Omnivore
    | Herbivore
  [@@deriving sexp_of]
end

let animals =
  let module Species_info = struct
    type t =
      { species_name : string
      ; group : Group.t
      ; diet : Diet.t
      }
    [@@deriving sexp_of]
  end
  in
  let module Color = struct
    type t =
      | Tomato
      | Red
      | Blue
      | Green
      | Rgb of
          { r : int
          ; g : int
          ; b : int
          }
    [@@deriving sexp_of]
  end
  in
  let module Date_range = struct
    type t =
      { start_date : Date.t
      ; end_date : Date.t
      }
    [@@deriving sexp_of]
  end
  in
  let module Animal = struct
    type t =
      { species_info : Species_info.t
      ; name : string
      ; age : int
      ; favorite_color : Color.t
      ; observations : Date_range.t list
      }
    [@@deriving sexp_of]
  end
  in
  let animal species_info name age favorite_color date_ranges =
    { Animal.species_info
    ; name
    ; age
    ; favorite_color
    ; observations =
        List.map date_ranges ~f:(fun (start, end_) ->
          { Date_range.start_date = Date.of_string start; end_date = Date.of_string end_ })
    }
  in
  let capy =
    animal { Species_info.diet = Herbivore; species_name = "capybara"; group = Mammal }
  in
  let grasshopper_mouse =
    animal
      { Species_info.diet = Carnivore
      ; species_name = "grasshopper mouse"
      ; group = Mammal
      }
  in
  let squirrel =
    animal { Species_info.diet = Omnivore; species_name = "squirrel"; group = Mammal }
  in
  let animals =
    [ capy "pebble" 1 Tomato []
    ; capy
        "cocoa"
        2
        Red
        [ "2022-01-21", "2022-02-28"
        ; "2023-01-17", "2023-03-01"
        ; "2024-01-20", "2024-02-24"
        ]
    ; capy "squeak" 2 Blue []
    ; capy
        "gizmo"
        1
        (Rgb { r = 255; g = 255; b = 255 })
        [ "2024-02-28", "2025-02-28"
        ; "2025-02-28", "2026-02-28"
        ; "2026-02-28", "2027-02-28"
        ]
    ; grasshopper_mouse
        "chomp"
        1
        (Rgb { r = 0; g = 0; b = 0 })
        [ "2023-05-01", "2023-08-17"; "2023-08-30", "2023-09-22" ]
    ; squirrel
        "gizmo"
        2
        Green
        [ "2022-03-18", "2022-07-11"
        ; "2022-11-10", "2023-06-30"
        ; "2023-09-12", "2024-04-14"
        ]
    ]
  in
  List.map animals ~f:(fun animal -> [%sexp (animal : Animal.t)])
;;

let%expect_test _ =
  (* $MDX part-begin=metadata-sexps *)
  List.iter animals ~f:(fun sexp ->
    print_s sexp;
    print_endline "");
  [%expect
    {|
    ((species_info ((species_name capybara) (group Mammal) (diet Herbivore)))
     (name pebble) (age 1) (favorite_color Tomato) (observations ()))

    ((species_info ((species_name capybara) (group Mammal) (diet Herbivore)))
     (name cocoa) (age 2) (favorite_color Red)
     (observations
      (((start_date 2022-01-21) (end_date 2022-02-28))
       ((start_date 2023-01-17) (end_date 2023-03-01))
       ((start_date 2024-01-20) (end_date 2024-02-24)))))

    ((species_info ((species_name capybara) (group Mammal) (diet Herbivore)))
     (name squeak) (age 2) (favorite_color Blue) (observations ()))

    ((species_info ((species_name capybara) (group Mammal) (diet Herbivore)))
     (name gizmo) (age 1) (favorite_color (Rgb (r 255) (g 255) (b 255)))
     (observations
      (((start_date 2024-02-28) (end_date 2025-02-28))
       ((start_date 2025-02-28) (end_date 2026-02-28))
       ((start_date 2026-02-28) (end_date 2027-02-28)))))

    ((species_info
      ((species_name "grasshopper mouse") (group Mammal) (diet Carnivore)))
     (name chomp) (age 1) (favorite_color (Rgb (r 0) (g 0) (b 0)))
     (observations
      (((start_date 2023-05-01) (end_date 2023-08-17))
       ((start_date 2023-08-30) (end_date 2023-09-22)))))

    ((species_info ((species_name squirrel) (group Mammal) (diet Omnivore)))
     (name gizmo) (age 2) (favorite_color Green)
     (observations
      (((start_date 2022-03-18) (end_date 2022-07-11))
       ((start_date 2022-11-10) (end_date 2023-06-30))
       ((start_date 2023-09-12) (end_date 2024-04-14)))))
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=metadata-table *)
  Expectable.print animals ~separate_rows:true;
  [%expect
    {|
    ┌───────────────────┬──────────────┬──────────────┬────────┬─────┬───────────────────────────────┬──────────────┬──────────────┐
    │ species_info      │ species_info │ species_info │        │     │                               │ observations │ observations │
    │ species_name      │ group        │ diet         │ name   │ age │ favorite_color                │ start_date   │ end_date     │
    ├───────────────────┼──────────────┼──────────────┼────────┼─────┼───────────────────────────────┼──────────────┼──────────────┤
    │ capybara          │ Mammal       │ Herbivore    │ pebble │ 1   │ Tomato                        │              │              │
    ├───────────────────┼──────────────┼──────────────┼────────┼─────┼───────────────────────────────┼──────────────┼──────────────┤
    │ capybara          │ Mammal       │ Herbivore    │ cocoa  │ 2   │ Red                           │ 2022-01-21   │ 2022-02-28   │
    │                   │              │              │        │     │                               │ 2023-01-17   │ 2023-03-01   │
    │                   │              │              │        │     │                               │ 2024-01-20   │ 2024-02-24   │
    ├───────────────────┼──────────────┼──────────────┼────────┼─────┼───────────────────────────────┼──────────────┼──────────────┤
    │ capybara          │ Mammal       │ Herbivore    │ squeak │ 2   │ Blue                          │              │              │
    ├───────────────────┼──────────────┼──────────────┼────────┼─────┼───────────────────────────────┼──────────────┼──────────────┤
    │ capybara          │ Mammal       │ Herbivore    │ gizmo  │ 1   │ (Rgb (r 255) (g 255) (b 255)) │ 2024-02-28   │ 2025-02-28   │
    │                   │              │              │        │     │                               │ 2025-02-28   │ 2026-02-28   │
    │                   │              │              │        │     │                               │ 2026-02-28   │ 2027-02-28   │
    ├───────────────────┼──────────────┼──────────────┼────────┼─────┼───────────────────────────────┼──────────────┼──────────────┤
    │ grasshopper mouse │ Mammal       │ Carnivore    │ chomp  │ 1   │ (Rgb (r 0) (g 0) (b 0))       │ 2023-05-01   │ 2023-08-17   │
    │                   │              │              │        │     │                               │ 2023-08-30   │ 2023-09-22   │
    ├───────────────────┼──────────────┼──────────────┼────────┼─────┼───────────────────────────────┼──────────────┼──────────────┤
    │ squirrel          │ Mammal       │ Omnivore     │ gizmo  │ 2   │ Green                         │ 2022-03-18   │ 2022-07-11   │
    │                   │              │              │        │     │                               │ 2022-11-10   │ 2023-06-30   │
    │                   │              │              │        │     │                               │ 2023-09-12   │ 2024-04-14   │
    └───────────────────┴──────────────┴──────────────┴────────┴─────┴───────────────────────────────┴──────────────┴──────────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=metadata-table-simple *)
  Expectable.print animals ~separate_rows:true ~max_depth:1;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────┬────────┬─────┬───────────────────────────────┬───────────────────────────────────────────────────┐
    │ species_info                                                         │ name   │ age │ favorite_color                │ observations                                      │
    ├──────────────────────────────────────────────────────────────────────┼────────┼─────┼───────────────────────────────┼───────────────────────────────────────────────────┤
    │ ((species_name capybara) (group Mammal) (diet Herbivore))            │ pebble │ 1   │ Tomato                        │ ()                                                │
    ├──────────────────────────────────────────────────────────────────────┼────────┼─────┼───────────────────────────────┼───────────────────────────────────────────────────┤
    │ ((species_name capybara) (group Mammal) (diet Herbivore))            │ cocoa  │ 2   │ Red                           │ (((start_date 2022-01-21) (end_date 2022-02-28))  │
    │                                                                      │        │     │                               │  ((start_date 2023-01-17) (end_date 2023-03-01))  │
    │                                                                      │        │     │                               │  ((start_date 2024-01-20) (end_date 2024-02-24))) │
    ├──────────────────────────────────────────────────────────────────────┼────────┼─────┼───────────────────────────────┼───────────────────────────────────────────────────┤
    │ ((species_name capybara) (group Mammal) (diet Herbivore))            │ squeak │ 2   │ Blue                          │ ()                                                │
    ├──────────────────────────────────────────────────────────────────────┼────────┼─────┼───────────────────────────────┼───────────────────────────────────────────────────┤
    │ ((species_name capybara) (group Mammal) (diet Herbivore))            │ gizmo  │ 1   │ (Rgb (r 255) (g 255) (b 255)) │ (((start_date 2024-02-28) (end_date 2025-02-28))  │
    │                                                                      │        │     │                               │  ((start_date 2025-02-28) (end_date 2026-02-28))  │
    │                                                                      │        │     │                               │  ((start_date 2026-02-28) (end_date 2027-02-28))) │
    ├──────────────────────────────────────────────────────────────────────┼────────┼─────┼───────────────────────────────┼───────────────────────────────────────────────────┤
    │ ((species_name "grasshopper mouse") (group Mammal) (diet Carnivore)) │ chomp  │ 1   │ (Rgb (r 0) (g 0) (b 0))       │ (((start_date 2023-05-01) (end_date 2023-08-17))  │
    │                                                                      │        │     │                               │  ((start_date 2023-08-30) (end_date 2023-09-22))) │
    ├──────────────────────────────────────────────────────────────────────┼────────┼─────┼───────────────────────────────┼───────────────────────────────────────────────────┤
    │ ((species_name squirrel) (group Mammal) (diet Omnivore))             │ gizmo  │ 2   │ Green                         │ (((start_date 2022-03-18) (end_date 2022-07-11))  │
    │                                                                      │        │     │                               │  ((start_date 2022-11-10) (end_date 2023-06-30))  │
    │                                                                      │        │     │                               │  ((start_date 2023-09-12) (end_date 2024-04-14))) │
    └──────────────────────────────────────────────────────────────────────┴────────┴─────┴───────────────────────────────┴───────────────────────────────────────────────────┘
    |}];
  (* $MDX part-end *)
  ()
;;

let%expect_test "nested_columns" =
  (* $MDX part-begin=nested_columns_auto *)
  let records = [ [%sexp { label = { rows = 10; columns = 20 } }] ] in
  Expectable.print records ~nested_columns:`auto;
  [%expect
    {|
    ┌────────────┬─────────┐
    │            │ label   │
    │ label.rows │ columns │
    ├────────────┼─────────┤
    │ 10         │ 20      │
    └────────────┴─────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=nested_columns_dotted *)
  Expectable.print records ~nested_columns:`dotted;
  [%expect
    {|
    ┌────────────┬───────────────┐
    │ label.rows │ label.columns │
    ├────────────┼───────────────┤
    │ 10         │ 20            │
    └────────────┴───────────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=nested_columns_stacked *)
  Expectable.print records ~nested_columns:`stacked;
  [%expect
    {|
    ┌───────┬─────────┐
    │ label │ label   │
    │ rows  │ columns │
    ├───────┼─────────┤
    │ 10    │ 20      │
    └───────┴─────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=nested_columns_last *)
  Expectable.print records ~nested_columns:`last;
  [%expect
    {|
    ┌──────┬─────────┐
    │ rows │ columns │
    ├──────┼─────────┤
    │ 10   │ 20      │
    └──────┴─────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=nested_columns_last_ambiguous *)
  Expectable.print
    [ [%sexp { foo = { rows = 10; columns = 20 }; bar = { rows = 1; columns = 20 } }] ]
    ~nested_columns:`last;
  [%expect
    {|
    ┌──────┬─────────┬──────┬─────────┐
    │ rows │ columns │ rows │ columns │
    ├──────┼─────────┼──────┼─────────┤
    │ 10   │ 20      │ 1    │ 20      │
    └──────┴─────────┴──────┴─────────┘
    |}];
  (* $MDX part-end *)
  ()
;;

let%expect_test "ambiguity" =
  (* $MDX part-begin=ambiguity1 *)
  let records =
    [ [%sexp { foo = 1; bar = (None : string option) }]
    ; [%sexp { foo = 2; bar = (Some "" : string option) }]
    ; [%sexp { foo = 3; bar = (Some "hey" : string option) }]
    ]
  in
  Expectable.print records;
  [%expect
    {|
    ┌─────┬─────┐
    │ foo │ bar │
    ├─────┼─────┤
    │ 1   │     │
    │ 2   │     │
    │ 3   │ hey │
    └─────┴─────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=ambiguity2 *)
  Expectable.print records ~max_depth:1;
  [%expect
    {|
    ┌─────┬───────┐
    │ foo │ bar   │
    ├─────┼───────┤
    │ 1   │ ()    │
    │ 2   │ ("")  │
    │ 3   │ (hey) │
    └─────┴───────┘
    |}];
  (* $MDX part-end *)
  ()
;;

let%expect_test "alignment" =
  let cases =
    [ "-0"
    ; "+0"
    ; "000"
    ; "245e-3"
    ; "7.0"
    ; "9.5"
    ; "0x10"
    ; "0x1e1"
    ; "1_000"
    ; "1.234567e4"
    ; "abc"
    ]
  in
  (* $MDX part-begin=alignment1 *)
  Expectable.print_cases
    ~align:`numbers
    ~sexp_of_input:[%sexp_of: string]
    ~sexp_of_output:[%sexp_of: float option]
    ~f:Float.of_string_opt
    cases;
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐
    ├┴┴┴┴┴┴┴┴┴┴┴┴┼┴┴┴┴┴┴┴┴┴┴┴┤
    │ -0         │    -0     │
    │ +0         │     0     │
    │ 000        │     0     │
    │ 245e-3     │     0.245 │
    │ 7.0        │     7     │
    │ 9.5        │     9.5   │
    │ 0x10       │    16     │
    │ 0x1e1      │   481     │
    │ 1_000      │  1000     │
    │ 1.234567e4 │ 12345.67  │
    │ abc        │           │
    └────────────┴───────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=alignment2 *)
  Expectable.print_cases
    ~align:`left
    ~sexp_of_input:[%sexp_of: string]
    ~sexp_of_output:[%sexp_of: float option]
    ~f:Float.of_string_opt
    cases;
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐
    ├┴┴┴┴┴┴┴┴┴┴┴┴┼┴┴┴┴┴┴┴┴┴┴┤
    │ -0         │ -0       │
    │ +0         │ 0        │
    │ 000        │ 0        │
    │ 245e-3     │ 0.245    │
    │ 7.0        │ 7        │
    │ 9.5        │ 9.5      │
    │ 0x10       │ 16       │
    │ 0x1e1      │ 481      │
    │ 1_000      │ 1000     │
    │ 1.234567e4 │ 12345.67 │
    │ abc        │          │
    └────────────┴──────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=alignment3 *)
  Expectable.print_cases
    ~align:`right
    ~sexp_of_input:[%sexp_of: string]
    ~sexp_of_output:[%sexp_of: float option]
    ~f:Float.of_string_opt
    cases;
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐
    ├┴┴┴┴┴┴┴┴┴┴┴┴┼┴┴┴┴┴┴┴┴┴┴┤
    │         -0 │       -0 │
    │         +0 │        0 │
    │        000 │        0 │
    │     245e-3 │    0.245 │
    │        7.0 │        7 │
    │        9.5 │      9.5 │
    │       0x10 │       16 │
    │      0x1e1 │      481 │
    │      1_000 │     1000 │
    │ 1.234567e4 │ 12345.67 │
    │        abc │          │
    └────────────┴──────────┘
    |}];
  (* $MDX part-end *)
  (* $MDX part-begin=alignment4 *)
  Expectable.print_cases
    ~align:`center
    ~sexp_of_input:[%sexp_of: string]
    ~sexp_of_output:[%sexp_of: float option]
    ~f:Float.of_string_opt
    cases;
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐
    ├┴┴┴┴┴┴┴┴┴┴┴┴┼┴┴┴┴┴┴┴┴┴┴┤
    │     -0     │    -0    │
    │     +0     │    0     │
    │    000     │    0     │
    │   245e-3   │  0.245   │
    │    7.0     │    7     │
    │    9.5     │   9.5    │
    │    0x10    │    16    │
    │   0x1e1    │   481    │
    │   1_000    │   1000   │
    │ 1.234567e4 │ 12345.67 │
    │    abc     │          │
    └────────────┴──────────┘
    |}];
  (* $MDX part-end *)
  ()
;;

let%expect_test "" =
  let module Reading = struct
    type t =
      { elevation : string
      ; temperature : string
      ; pressure : string
      }
    [@@deriving fields ~getters, sexp_of]
  end
  in
  let module Hi_lo_pair = struct
    type 'a t =
      { hi : 'a
      ; lo : 'a
      }
    [@@deriving sexp_of]
  end
  in
  let readings : Reading.t list =
    [ { elevation = "13678ft"; temperature = "4C"; pressure = "612mb" }
    ; { elevation = "33ft"; temperature = "31C"; pressure = "1013.25mb" }
    ]
  in
  let find_extremes data ~by:_ =
    match data with
    | [ hi; lo ] -> { Hi_lo_pair.hi = Some hi; lo = Some lo }
    | _ -> assert false
  in
  (* $MDX part-begin=transposed *)
  find_extremes readings ~by:Reading.elevation
  |> [%sexp_of: Reading.t option Hi_lo_pair.t]
  |> Expectable.print_record_transposed;
  [%expect
    {|
    ┌────┬───────────┬─────────────┬───────────┐
    │    │ elevation │ temperature │ pressure  │
    ├────┼───────────┼─────────────┼───────────┤
    │ hi │ 13678ft   │ 4C          │ 612mb     │
    │ lo │ 33ft      │ 31C         │ 1013.25mb │
    └────┴───────────┴─────────────┴───────────┘
    |}];
  (* $MDX part-end *)
  ()
;;
