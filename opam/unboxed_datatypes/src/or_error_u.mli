open Base

type 'a t : immediate & value = ('a, Error.t) Result_u.t [@@deriving sexp, globalize]

[%%template:
[@@@mode.default m = (global, local)]

val of_error : Error.t @ m -> _ t @ m [@@zero_alloc]
val to_or_error : 'a t @ m -> 'a Or_error.t @ m
val of_or_error : 'a Or_error.t @ m -> 'a t @ m [@@zero_alloc]
val compare : ('a @ m -> 'a @ m -> int) -> 'a t @ m -> 'a t @ m -> int
val equal : ('a @ m -> 'a @ m -> bool) -> 'a t @ m -> 'a t @ m -> bool
val invariant : ('a @ m -> unit) @ local -> 'a t @ m -> unit
val bind : 'a t @ m -> f:('a @ m -> 'b t @ m) @ local -> 'b t @ m
val map : 'a t @ m -> f:('a @ m -> 'b @ m) @ local -> 'b t @ m
val iter : 'a t @ m -> f:('a @ m -> unit) @ local -> unit
val iter_error : _ t @ m -> f:(Error.t @ m -> unit) @ local -> unit
val return : 'a @ m -> 'a t @ m [@@zero_alloc]
val ignore_m : _ t @ m -> unit t @ m [@@zero_alloc]
val ok : 'a t @ m -> 'a option @ m
val of_option : 'a option @ m -> error:Base.Error.t @ m -> 'a t @ m [@@zero_alloc]
(* template end *)]

val is_ok : _ t @ local -> bool [@@zero_alloc]
val is_error : _ t @ local -> bool [@@zero_alloc]
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) @ local -> 'd t
val both : 'a t -> 'b t -> ('a * 'b) t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val ( <* ) : 'a t -> unit t -> 'a t
val ( *> ) : unit t -> 'a t -> 'a t
val try_with : ?backtrace:bool (** defaults to [false] *) -> local_ (unit -> 'a) -> 'a t
val ok_exn : 'a t -> 'a [@@zero_alloc]
val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> _ t

val of_exn_result
  :  ?backtrace:[ `Get | `This of string ]
  -> ('a, exn) Base.Result.t
  -> 'a t

val of_option_lazy : 'a option -> error:Error.t Lazy.t -> 'a t
val of_option_lazy_sexp : 'a option -> error:Sexp.t Lazy.t -> 'a t
val of_option_lazy_string : 'a option -> error:string Lazy.t -> 'a t

val error
  :  ?here:Stdlib.Lexing.position
  -> ?strict:unit
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _ t

val error_s : Sexp.t -> _ t
val error_string : string -> _ t
val tag : 'a t -> tag:string -> 'a t
val tag_s : 'a t -> tag:Sexp.t -> 'a t
val tag_s_lazy : 'a t -> tag:Sexp.t Lazy.t -> 'a t
val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
val unimplemented : string -> _ t
val hash_fold_t : 'a Ppx_hash_lib.hash_fold -> 'a t Ppx_hash_lib.hash_fold
val ( >>= ) : 'a t -> ('a -> 'b t) @ local -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) @ local -> 'b t

module Let_syntax : sig
  val return : 'a -> 'a t [@@zero_alloc]
  val ( >>= ) : 'a t -> ('a -> 'b t) @ local -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) @ local -> 'b t

  module Let_syntax : sig
    val return : 'a -> 'a t [@@zero_alloc]
    val bind : 'a t -> f:('a -> 'b t) @ local -> 'b t
    val map : 'a t -> f:('a -> 'b) @ local -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end

module Local : sig
  module Let_syntax : sig
    val return : 'a @ local -> 'a t @ local [@@zero_alloc]
    val ( >>= ) : 'a t @ local -> ('a @ local -> 'b t @ local) @ local -> 'b t @ local
    val ( >>| ) : 'a t @ local -> ('a @ local -> 'b @ local) @ local -> 'b t @ local

    module Let_syntax : sig
      val return : 'a @ local -> 'a t @ local [@@zero_alloc]
      val bind : 'a t @ local -> f:('a @ local -> 'b t @ local) @ local -> 'b t @ local
      val map : 'a t @ local -> f:('a @ local -> 'b @ local) @ local -> 'b t @ local
      val both : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

      module Open_on_rhs : sig end
    end
  end
end

module Monad_infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) @ local -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) @ local -> 'b t
end
