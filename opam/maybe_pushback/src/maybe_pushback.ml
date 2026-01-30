open Core
open Async_kernel
include Eager_deferred

type 'a t = 'a Deferred.t

let of_deferred = Fn.id
let to_deferred = Fn.id
let sexp_of_t = Deferred.sexp_of_t
