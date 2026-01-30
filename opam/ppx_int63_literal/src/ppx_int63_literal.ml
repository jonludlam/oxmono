open! Base
open Ppxlib
module B = Ast_builder.Default

let () =
  Driver.register_transformation
    ~rules:
      [ Context_free.Rule.constant Integer 'J' (fun loc literal ->
          match Int63.of_string literal with
          | exception exn ->
            let (_ : exn) =
              (* this exception output is not very useful, so we don't include it *) exn
            in
            Location.raise_errorf
              ~loc
              "Integer literal exceeds the range of representable integers of type \
               [Base.Int63.t]"
          | (_ : Int63.t) ->
            [%expr
              Base.Int63.of_int64_trunc
                [%e
                  B.pexp_constant
                    ~loc:{ loc with loc_ghost = true }
                    (Pconst_integer (literal, Some 'L'))]])
      ]
    "int63_literal"
;;
