open Base
open! Hardcaml

val generate
  :  ?custom_constraints:Rope.t
  -> ?dir:string
  -> name:string
  -> part:string
  -> pins:Pin.t list
  -> Board.t
  -> unit

module For_testing : sig
  val rtl_of_hardcaml_circuit : Board.t -> String.t
end
