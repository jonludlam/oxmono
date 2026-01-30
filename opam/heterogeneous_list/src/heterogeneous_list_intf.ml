(** [Heterogeneous_list] is a list that can contain arbitrary, mixed types as elements. A
    bit like a Python list.

    We recommend thinking carefully before deciding to use this library, as the list
    syntax can be confusing and compiler error messages difficult to read. *)

open! Base

module type S1 = sig
  module Element : T1

  type 'a t =
    | [] : unit t
    | ( :: ) : 'a Element.t * 'rest t -> ('a -> 'rest) t
end

module type Heterogeneous_list = sig
  module Applicative_or_monad = Applicative_or_monad

  module type S1 = S1

  module Make (X : T1) : S1 with type 'a Element.t = 'a X.t

  module Make_map (X1 : S1) (X2 : S1) : sig
    val map : 'a X1.t -> f:('b. 'b X1.Element.t -> 'b X2.Element.t) -> 'a X2.t
  end

  module Make_applicative_or_monad_map
      (X1 : S1)
      (X2 : S1)
      (A : Applicative_or_monad.S) : sig
    val map : 'a X1.t -> f:('b. 'b X1.Element.t -> 'b X2.Element.t A.t) -> 'a X2.t A.t
  end

  module Ident : S1 with type 'a Element.t = 'a
  module Optional : S1 with type 'a Element.t = 'a Option.t
  include module type of Ident
end
