open Base

type element =
  { tag : string
  ; attributes : (string * string) list
  ; children : t list
  }

and t =
  | Element of element
  | Text of string
[@@deriving sexp_of]

val parse : [ `String of string | `File of string | `Channel of In_channel.t ] -> element

val to_string
  :  ?decl:bool
       (** if [true] the {{:http://www.w3.org/TR/REC-xml/#NT-XMLDecl} XML declaration} is
           output (defaults to [true]). *)
  -> ?buf:
       Buffer.t
       (* Minified = no new lines or indents (default) Newlines_only = new lines for every
          tag Indent of n = include new lines, and also indent each nested level w/ n
          spaces *)
  -> ?fmt:[ `Minified | `Newlines_only | `Indent of int ]
  -> element
  -> string
