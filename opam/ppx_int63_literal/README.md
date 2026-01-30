ppx_int63_literal
=================

Syntax extension for writing int63 literals.

This ppx allows users to write `Int63.t` values as literals, e.g.
`Int63.zero` is `0J`.

The `J` suffix stands for a "Jane Street integer", since `Int63.t` is
defined in Jane Street's `Base` library; or alternatively, a
JavaScript-compatible integer. More importantly, it was chosen to
avoid confusion with digits, like `I` vs. `1`, and confusion with
existing well-known suffixes, like `K` for thousands.
