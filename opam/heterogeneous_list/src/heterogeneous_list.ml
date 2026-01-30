open! Base
include Heterogeneous_list_intf
module Applicative_or_monad = Applicative_or_monad

module Make (X : T1) = struct
  module Element = X

  type 'a t =
    | [] : unit t
    | ( :: ) : 'a Element.t * 'rest t -> ('a -> 'rest) t
end

module Make_map (X1 : S1) (X2 : S1) = struct
  let map t ~(f : 'a. 'a X1.Element.t -> 'a X2.Element.t) =
    let[@tail_mod_cons] rec inner : type a. a X1.t -> a X2.t = function
      | [] -> []
      | x :: xs -> f x :: (inner [@tailcall]) xs
    in
    inner t
  ;;
end

module Make_applicative_or_monad_map (X1 : S1) (X2 : S1) (A : Applicative_or_monad.S) =
struct
  let map t ~(f : 'a. 'a X1.Element.t -> 'a X2.Element.t A.t) =
    let rec inner : type a. a X1.t -> a X2.t A.t = function
      | [] -> A.return X2.[]
      | x :: xs -> A.both (f x) (inner xs) |> A.map ~f:(fun (r, rs) -> X2.(r :: rs))
    in
    inner t
  ;;
end

module Ident = Make (Monad.Ident)
module Optional = Make (Option)
include Ident
