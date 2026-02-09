When a module has a destructive module substitution (`:=`) and another module
includes it via `module type of struct include ... end`, odoc's `apply_inner_substs`
encounters a `Local` identifier in the substitution's manifest that it cannot resolve.

  $ cat a.mli
  module View : sig type t end
  module Short := View
  type t = Short.t

  $ cat b.mli
  include module type of struct include A end


  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli

  $ odoc compile a.cmti
  $ odoc compile -I . b.cmti
  $ odoc link -I . b.odoc
  $ odoc_print b.odocl --short --show-include-expansions
  include module type of struct include A end
    (sig : module View = A.View type t = View.t end)
