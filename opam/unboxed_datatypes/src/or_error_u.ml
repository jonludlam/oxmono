open Base

type 'a t = ('a, Error.t) Result_u.t

[%%template
[@@@mode.default m = (global, local)]

let of_error e = (Result_u.fail [@mode m]) e [@exclave_if_local m]
let to_or_error = (Result_u.to_result [@mode m])

let[@inline] of_or_error x = (Result_u.of_result [@mode m]) x [@exclave_if_local m]
[@@zero_alloc]
;;

let[@inline] compare f a b = (Result_u.compare [@mode m]) f (Error.compare [@mode m]) a b
let[@inline] equal f a b = (Result_u.equal [@mode m]) f (Error.equal [@mode m]) a b
let[@inline] invariant f t = (Result_u.iter [@mode m]) ~f t
let[@inline] bind t ~f = (Result_u.bind [@mode m]) t ~f [@exclave_if_local m]
let[@inline] map t ~f = (Result_u.map [@mode m]) t ~f [@exclave_if_local m]
let[@inline] iter t ~f = (Result_u.iter [@mode m]) t ~f
let[@inline] iter_error t ~f = (Result_u.iter_error [@mode m]) t ~f
let[@inline] return x = (Result_u.return [@mode m]) x [@exclave_if_local m]
let[@inline] ignore_m t = (Result_u.ignore_m [@mode m]) t [@exclave_if_local m]
let[@inline] ok t = (Result_u.ok [@mode m]) t [@exclave_if_local m]

let[@inline] of_option x ~error =
  (Result_u.of_option [@mode m]) x ~error [@exclave_if_local m]
;;

(* template end *)]

let[@inline] map2 a b ~f =
  Result_u.combine ~ok:f ~err:(fun e1 e2 -> Error.of_list [ e1; e2 ]) a b
;;

let both a b = map2 a b ~f:(fun a b -> a, b)

let map3 ta tb tc ~f =
  let res = map2 (both ta tb) tc ~f:(fun (a, b) c -> f a b c) in
  res
;;

let apply tf ta = map2 tf ta ~f:(fun f a -> f a)
let ( <*> ) = apply
let ( *> ) u v = map2 u v ~f:(fun () y -> y)
let ( <* ) u v = map2 u v ~f:(fun x () -> x)
let[@inline] sexp_of_t f t = Result_u.sexp_of_t f Error.sexp_of_t t
let[@inline] t_of_sexp f t = Result_u.t_of_sexp f Error.t_of_sexp t
let[@inline] globalize f t = Result_u.globalize f Error.globalize t
let[@inline] hash_fold_t f = Result_u.hash_fold_t f Error.hash_fold_t
let[@inline] is_ok t = Result_u.is_ok t
let[@inline] is_error t = Result_u.is_error t

let try_with ?(backtrace = false) f =
  try return (f ()) with
  | exn ->
    Result_u.fail (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let ok_exn (type a) : a t -> a = function
  | Result_u.T #(Ok, x) -> x
  | Result_u.T #(Error, x) -> Error.raise x
[@@zero_alloc]
;;

let of_exn ?backtrace exn = Result_u.fail (Error.of_exn ?backtrace exn)

let of_exn_result ?backtrace = function
  | Ok x -> return x
  | Error exn -> of_exn ?backtrace exn
;;

let of_option_lazy_string t ~error = of_option t ~error:(Error.of_lazy error)
let of_option_lazy_sexp t ~error = of_option t ~error:(Error.of_lazy_sexp error)
let of_option_lazy t ~error = of_option t ~error:(Error.of_lazy_t error)

let error ?here ?strict message a sexp_of_a =
  Result_u.fail (Error.create ?here ?strict message a sexp_of_a)
;;

let error_s sexp = Result_u.fail (Error.create_s sexp)
let error_string message = Result_u.fail (Error.of_string message)
let tag t ~tag = Result_u.map_error t ~f:(Error.tag ~tag)
let tag_s t ~tag = Result_u.map_error t ~f:(Error.tag_s ~tag)
let tag_s_lazy t ~tag = Result_u.map_error t ~f:(Error.tag_s_lazy ~tag)

let tag_arg t message a sexp_of_a =
  Result_u.map_error t ~f:(fun e -> Error.tag_arg e message a sexp_of_a)
;;

let unimplemented s = error "unimplemented" s sexp_of_string
let[@inline] ( >>| ) a f = map a ~f
let[@inline] ( >>= ) a f = bind a ~f

module Let_syntax = struct
  let return = [%eta1 return] [@@zero_alloc]
  let[@inline] ( >>| ) a f = map a ~f
  let[@inline] ( >>= ) a f = bind a ~f

  module Let_syntax = struct
    let return = [%eta1 return] [@@zero_alloc]
    let bind = bind
    let map = map
    let both = Result_u.combine ~ok:(fun x y -> x, y) ~err:(fun x _ -> x)

    module Open_on_rhs = struct end
  end
end

module Local = struct
  module Let_syntax = struct
    let[@inline] return x = exclave_ (return [@mode local]) x
    let[@inline] ( >>= ) a f = exclave_ (bind [@mode local]) a ~f
    let[@inline] ( >>| ) a f = exclave_ (map [@mode local]) a ~f

    module Let_syntax = struct
      let[@inline] return x = exclave_ (return [@mode local]) x
      let bind = (bind [@mode local])
      let map = (map [@mode local])

      let both =
        (Result_u.combine [@mode local])
          ~ok:(fun x y -> exclave_ x, y)
          ~err:(fun x _ -> x)
      ;;

      module Open_on_rhs = struct end
    end
  end
end

module Monad_infix = struct
  let[@inline] ( >>| ) a f = map a ~f
  let[@inline] ( >>= ) a f = bind a ~f
end
