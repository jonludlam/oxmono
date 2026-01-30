open! Core
open Bonsai_term

module Kind : sig
  type t =
    | Dot
    | Line
end

val component
  :  ?kind:Kind.t
  -> ?attrs:Attr.t list Bonsai.t
  -> local_ Bonsai.graph
  -> View.t Bonsai.t
