"Expectable"
============

`Expectable` is a library that makes it easier to print ASCII tables in tests. All you
need is a sexp, and Expectable will take care of the rest!

It's a wrapper around `Ascii_table_kernel` that infers columns based on sexp shapes. You
can generate lists of any ad-hoc sexps that you want, and `Expectable` will traverse the
sexp representation to figure out what fields are present, and then print a table out of
that.

<!-- $MDX file=test/readme.ml,part=basic-usage-example -->
```ocaml
  let test animal_name group age ~than =
    [%sexp
      { animal_name : string
      ; group       : Group.t
      ; age         : int
      ; is =
          (if age = than then "same age" else if age > than then "older" else "younger"
           : string)
      ; than        : int
      }]
  in
  [ test "capybara"    Mammal                         10 ~than:11
  ; test "salmon"      Fish          11 ~than:        10
  ; test "snail"       Invertebrates 11 ~than:        10
  ; test "frog"        Amphibean                      10 ~than:10
  ; test "t-rex"       Reptile       83_600_000 ~than:10
  ; test "hummingbird" Bird                           10 ~than:10
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
```

Expectable will do its best to infer columns even for nested records, optional fields, or
nested lists of records. For example, here's a list of somewhat complicated records:

<!-- $MDX file=test/readme.ml,part=metadata-sexps -->
```ocaml
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
```

Wow! That's a lot. It might be easier to digest and compare each record if you view them
as a table:

<!-- $MDX file=test/readme.ml,part=metadata-table -->
```ocaml
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
```

# Customizing Expectable's output

All Expectable functions take the same optional arguments:

## `separate_rows`

Whether or not to draw a separator between rows. This is useful when you have a record
that contains lists.

## `limit_width_to`

Maximum width of the printed table. This is useful to restrict the size of the table so
that it fits within the editor window.

## `nested_columns`

How to render the column headers for fields in nested records. Valid options are ``
`dotted ``, `` `stacked ``, `` `last ``, and `` `auto ``. By default, Expectable uses
`` `auto ``, which is a simple heuristic that chooses between `` `dotted `` and
`` `stacked `` rendering based on the length of field names:

<!-- $MDX file=test/readme.ml,part=nested_columns_auto -->
```ocaml
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
```

This can lead to goofy results like the above, where it infers different layouts for
sibling fields. You can force a specific layout instead:

<!-- $MDX file=test/readme.ml,part=nested_columns_dotted -->
```ocaml
  Expectable.print records ~nested_columns:`dotted;
  [%expect
    {|
    ┌────────────┬───────────────┐
    │ label.rows │ label.columns │
    ├────────────┼───────────────┤
    │ 10         │ 20            │
    └────────────┴───────────────┘
    |}];
```
<!-- $MDX file=test/readme.ml,part=nested_columns_stacked -->
```ocaml
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
```

Or you can choose to omit all but the final component of the field's header:
<!-- $MDX file=test/readme.ml,part=nested_columns_last -->
```ocaml
  Expectable.print records ~nested_columns:`last;
  [%expect
    {|
    ┌──────┬─────────┐
    │ rows │ columns │
    ├──────┼─────────┤
    │ 10   │ 20      │
    └──────┴─────────┘
    |}];
```

Although note that this can result in ambiguous column headers:

<!-- $MDX file=test/readme.ml,part=nested_columns_last_ambiguous -->
```ocaml
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
```

## `max_depth`

Expectable does its best to present a reasonable view of whatever data you throw at it,
but its "magic" autoformatting might not always we what you want. For example, optional
fields equal to `None` render as empty cells, and optional fields that are `Some` don't
render with parentheses.

<!-- $MDX file=test/readme.ml,part=ambiguity1 -->
```ocaml
  let records =
    [ [%sexp { foo = 1; bar = (None       : string option) }]
    ; [%sexp { foo = 2; bar = (Some ""    : string option) }]
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
```

You can tell Expectable to be a little less clever by passing the `~max_depth` argument,
which will control how deeply it will traverse the input sexp.

<!-- $MDX file=test/readme.ml,part=ambiguity2 -->
```ocaml
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
```

Note though that this is a global setting, and not something you can control per-column.
If you want more control over the format of your table, you're probably better off
constructing the ASCII table directly.

# `align`

The default alignment is ``~align:`numbers``, which pads purely numeric columns so that
their decimal points line up.

<!-- $MDX file=test/readme.ml,part=alignment1 -->
```ocaml
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
```

But you can provide an explicit alternate alignment if you prefer:

<!-- $MDX file=test/readme.ml,part=alignment2 -->
```ocaml
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
```
<!-- $MDX file=test/readme.ml,part=alignment3 -->
```ocaml
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
```
<!-- $MDX file=test/readme.ml,part=alignment4 -->
```ocaml
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
```

Note that the alignment you pick applies to all columns. If you want more control over the
layout of the final table, it's probably best to construct the ASCII table directly.

# Functions

In order of usefulness, these are:

- `Expectable.print` -- print a list of sexps as a table.
- `Expectable.print_cases` -- given a list of inputs and a function, print a table showing
  the result of calling that function.
- `Expectable.print_alist` -- given a list of named sexps, print them in a table with the
  name on the left-hand side.
- `Expectable.print_record_transposed` -- print a single sexp record as if each of its
  fields were a separate row. This is useful when you have a record with multiple fields
  of the same type, and want to compare them side by side. For example:

<!-- $MDX file=test/readme.ml,part=transposed -->
```ocaml
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
```

See [`expectable.mli`][expectable.mli] for a more detailed description of each function.


# Schema inference issues

Expectable assumes that field names are lowercase and variant tags are uppercase. So it
will interpret `(Foo 123)` as a single value, not a list of values. And it will interpret
`((Foo 123) (Bar 123))` as a list of values, not as a record.

But there's nothing that guarantees that a variant has to be uppercase: polymorphic
variants are often lowercase, and the `versioned_sexp` library produces lower-cased
variants like `v1`. This makes it annoying to use with Expectable, because Expectable will
interpret `(v1 something)` as a list of two values, `v1` and `something`, not as a
constructor.

The only way around this right now is to globally change the `~max_depth` argument to cut
off schema inference before traversing into sexps like that. It would be very reasonable
to allow specifying max-depth on a per-column basis, but Expectable doesn't support that
yet.
