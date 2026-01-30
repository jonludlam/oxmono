open Core
open Async_kernel

(** Many operations have the property that they might return immediately, or they might
    block on some system operation. A ['a Maybe_pushback.t] allows us to chain together
    operations such that we do not jump into the Async scheduler until we hit an operation
    that requires us to. It's the same as [Eager_deferred.t], but with a type that is
    unique from [Deferred.t] so that you have to be more explicit about where you are
    willing to yield to the scheduler. *)
type +'a t = private 'a Deferred.t [@@deriving sexp_of]

include Monad.S with type 'a t := 'a t

val to_deferred : 'a t -> 'a Deferred.t
val of_deferred : 'a Deferred.t -> 'a t
val unit : unit t
val peek : 'a t -> 'a option
val ok : 'a t -> ('a, _) Result.t t

module List : sig
  val all_unit : unit t list -> unit t
  val iter : how:Monad_sequence.how -> 'a list -> f:('a -> unit t) -> unit t
end

module Queue : sig
  val iter : how:Monad_sequence.how -> 'a Queue.t -> f:('a -> unit t) -> unit t
end
