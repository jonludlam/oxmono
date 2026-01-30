ppx_fuelproof
=============

```ocaml
open Base
```

TL;DR: Write `type%fuelproof` to get better error messages around mode-crossing, and
even to get mode-crossing in places where the compiler can't currently
infer it:

```ocaml
type%fuelproof _ t : value mod portable =
  | A : int -> int t
  | B : bool -> bool t
```

`ppx_fuelproof` helps work around several issues when trying to make your
type mode-cross in the way you want:

  1. It generates better error messages when your type doesn't mode-cross.
  2. It helps you avoid running out of fuel. (The mode-crossing check in
     the compiler limits how hard it will try to prove mode-crossing.)
  3. It lets you have some amount of mode-crossing GADTs.

All of these are things that can be fixed, but the OCaml Language team is
busy with lots of things, so a ppx stopgap seems fine for now.

# Examples

## Example of better error message

```ocaml
module type Example_setup = sig
  module type P := sig
    type t : value mod portable
  end
  module Field1 : P
  module Field2 : P
  module Field3 : P
  module Field4 : T
  module Field5 : P
  module Field6 : P
  module Field7 : P
end
```

Confusing message without `fuelproof`:

```ocaml
module type Confusing = sig
  include Example_setup

  type t : value mod portable =
    { field1 : Field1.t;
      field2 : Field2.t;
      field3 : Field3.t;
      field4 : Field4.t;
      field5 : Field5.t;
      field6 : Field6.t;
      field7 : Field7.t;
    }
end
```
```mdx-error
Lines 4-12, characters 5-8:
Error: The kind of type t is
           immutable_data
             with Field1.t

             with Field2.t

             with Field3.t

             with Field4.t

             with Field5.t

             with Field6.t

             with Field7.t
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
```

Good error message with `fuelproof`:

```ocaml
module type Fuelproof = sig
  include Example_setup

  type%fuelproof t : value mod portable =
    { field1 : Field1.t;
      field2 : Field2.t;
      field3 : Field3.t;
      field4 : Field4.t;
      field5 : Field5.t;
      field6 : Field6.t;
      field7 : Field7.t;
    }
end
```
```mdx-error
Line 8, characters 18-26:
Error: Bad layout annotation:
         The kind of Field4/2.t is value.
         But the kind of Field4/2.t must be a subkind of any mod portable
           because of the annotation on the wildcard _ at line 8, characters 18-26.
```

## Example of fuel

`fuelproof` helps the check not run out of fuel by checking each field for mode-crossing
in isolation.

```ocaml
module type Example_setup = sig
  module type P := sig
    type 'a t : value mod portable with 'a
  end

  type t0 : value mod portable
  type 'a t1 = { a : 'a }
  type 'a t2 = 'a t1 t1
  type 'a t3 = 'a t2 t2
  type 'a t4 = 'a t3 t3
end
```

Running out of fuel:
```ocaml
module type Confusing = sig
  include Example_setup

  type t : value mod portable =
    { x1 : [ `a1 ] t2 t2;
      x2 : [ `a2 ] t2 t2;
      x3 : [ `a3 ] t2 t2;
      x4 : [ `a4 ] t2 t2;
      x5 : [ `a5 ] t2 t2;
      x6 : [ `a6 ] t2 t2;
    }
end
```
```mdx-error
Lines 4-11, characters 5-8:
Error: The kind of type t is
           immutable_data
             with [ `a1 ] t2 t2

             with [ `a2 ] t2 t2

             with [ `a3 ] t2 t2

             with [ `a4 ] t2 t2

             with [ `a5 ] t2 t2

             with [ `a6 ] t2 t2
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
```

Refueling:
```ocaml
module type Fuelproof = sig
  include Example_setup

  type%fuelproof t : value mod portable =
    { x1 : [ `a1 ] t2 t2;
      x2 : [ `a2 ] t2 t2;
      x3 : [ `a3 ] t2 t2;
      x4 : [ `a4 ] t2 t2;
      x5 : [ `a5 ] t2 t2;
      x6 : [ `a6 ] t2 t2;
    }
end
```

## Example of GADTs

Currently, mode-crossing inference for GADTs is fairly limited. This
will change very soon! But for now, `fuelproof` lets you make a limited
subset of GADTs cross modes:

Without `fuelproof`:

```ocaml
type _ t : value mod portable =
  | Zero : [ `zero ] t
  | Succ : 'a t -> [ `succ of 'a ] t
```
```mdx-error
Lines 1-3, characters 1-39:
Error: The kind of type t is immutable_data with (type : value) t
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
```

With `fuelproof`:

```ocaml
type%fuelproof _ t : value mod portable =
  | Zero : [ `zero ] t
  | Succ : 'a t -> [ `succ of 'a ] t
```
