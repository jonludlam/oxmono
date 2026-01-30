open! Base

module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type Applicative_or_monad = sig
  module type S = S
end
