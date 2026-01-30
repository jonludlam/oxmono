open Base

type ('a, 'b, 'c) tag =
  | Ok : ('a, 'b, 'a) tag
  | Error : ('a, 'b, 'b) tag

type ('a, 'b) t = T : #(('a, 'b, 'c) tag * 'c) -> ('a, 'b) t [@@unboxed]

[%%template
[@@@mode.default m = (global, local)]

let[@inline] match_
  (type a b)
  (T #(tag, x) : (a, b) t)
  ~(ok : (a @ m -> 'c @ m) @ local)
  ~(err : (b @ m -> 'c @ m) @ local)
  =
  match tag with
  | Ok -> ok x [@exclave_if_local m]
  | Error -> err x [@exclave_if_local m]
;;

let[@inline] fail x = T #(Error, x)

let[@inline] to_result (type a b) (T #(tag, x) : (a, b) t) : (a, b) Result.t =
  match tag with
  | Ok -> Ok x [@exclave_if_local m]
  | Error -> Error x [@exclave_if_local m]
;;

let[@inline] of_result = function
  | Result.Ok x -> T #(Ok, x)
  | Result.Error x -> T #(Error, x)
;;

let[@inline] compare
  (type a b)
  (cmp_a : (a @ m -> a @ m -> int) @ local)
  (cmp_b : (b @ m -> b @ m -> int) @ local)
  (t1 : (a, b) t)
  (t2 : (a, b) t)
  =
  match #(t1, t2) with
  | #(T #(Ok, a1), T #(Ok, a2)) -> cmp_a a1 a2
  | #(T #(Ok, _), T #(Error, _)) -> -1
  | #(T #(Error, _), T #(Ok, _)) -> 1
  | #(T #(Error, b1), T #(Error, b2)) -> cmp_b b1 b2
;;

let[@inline] equal
  (type a b)
  (equal_a : (a @ m -> a @ m -> bool) @ local)
  (equal_b : (b @ m -> b @ m -> bool) @ local)
  (t1 : (a, b) t)
  (t2 : (a, b) t)
  =
  match #(t1, t2) with
  | #(T #(Ok, a1), T #(Ok, a2)) -> equal_a a1 a2
  | #(T #(Ok, _), T #(Error, _)) -> false
  | #(T #(Error, _), T #(Ok, _)) -> false
  | #(T #(Error, b1), T #(Error, b2)) -> equal_b b1 b2
;;

let[@inline] bind (type a b c) (t : (a, b) t @ m) ~(f : a @ m -> (c, b) t @ m) : (c, b) t =
  match t with
  | T #(Ok, x) -> f x [@exclave_if_local m]
  | T #(Error, _) as t -> t
;;

let[@inline] return x = T #(Ok, x)

let ignore_m (type a b) (T #(tag, x) : (a, b) t) : (unit, b) t =
  match tag with
  | Ok -> T #(Ok, ())
  | Error -> T #(Error, x)
;;

let invariant
  (type a b)
  (check_ok : a @ m -> unit)
  (check_error : b @ m -> unit)
  (T #(tag, x) : (a, b) t)
  =
  match tag with
  | Ok -> check_ok x
  | Error -> check_error x
;;

let ok (type a b) (T #(tag, x) : (a, b) t) : a option =
  match tag with
  | Ok -> Some x [@exclave_if_local m]
  | Error -> None
;;

let error (type a b) (T #(tag, x) : (a, b) t) : b option =
  match tag with
  | Ok -> None
  | Error -> Some x [@exclave_if_local m]
;;

let of_option a_opt ~error =
  match a_opt with
  | Some a -> T #(Ok, a)
  | None -> T #(Error, error)
;;

let iter (type a b) (T #(tag, x) : (a, b) t) ~(f : a @ m -> unit) =
  match tag with
  | Ok -> f x
  | Error -> ()
;;

let iter_error (type a b) (T #(tag, x) : (a, b) t) ~(f : b @ m -> unit) =
  match tag with
  | Ok -> ()
  | Error -> f x
;;

let to_either (type a b) (T #(tag, x) : (a, b) t) : (a, b) Either.t =
  match tag with
  | Ok -> First x [@exclave_if_local m]
  | Error -> Second x [@exclave_if_local m]
;;

let of_either : _ Either.t @ m -> _ @ m = function
  | First x -> T #(Ok, x)
  | Second x -> T #(Error, x)
;;

let to_either_u (type a b) (T #(tag, x) : (a, b) t) : (a, b) Either_u.t =
  match tag with
  | Ok -> T #(First, x)
  | Error -> T #(Second, x)
;;

let of_either_u (type a b) (T #(tag, x) : (a, b) Either_u.t) : (a, b) t =
  match tag with
  | First -> T #(Ok, x)
  | Second -> T #(Error, x)
;;

let ok_if_true b ~error = if b then T #(Ok, ()) else T #(Error, error)

(* template end *)]

let[@inline] is_ok (type a b) (T #(tag, _) : (a, b) t) =
  match tag with
  | Ok -> true
  | Error -> false
;;

let[@inline] is_error (type a b) (T #(tag, _) : (a, b) t) =
  match tag with
  | Ok -> false
  | Error -> true
;;

let%template[@alloc a @ m = (heap @ global, stack @ local)] sexp_of_t
  (type a b)
  (of_a : a @ m -> Sexp.t @ m)
  (of_b : b @ m -> Sexp.t @ m)
  (T #(tag, x) : (a, b) t)
  =
  match[@exclave_if_stack a] tag with
  | Ok -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Ok"; of_a x ]
  | Error -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Error"; of_b x ]
;;

let globalize
  (type a b)
  (globalize_a : a @ local -> a)
  (globalize_b : b @ local -> b)
  (T #(tag, x) : (a, b) t)
  =
  match tag with
  | Ok -> T #(tag, globalize_a x)
  | Error -> T #(tag, globalize_b x)
;;

let t_of_sexp a_of b_of s = of_result (Result.t_of_sexp a_of b_of s)

let hash_fold_t
  (type a b)
  (hash_a : Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state)
  (hash_b : Ppx_hash_lib.Std.Hash.state -> b -> Ppx_hash_lib.Std.Hash.state)
  (hsv : Ppx_hash_lib.Std.Hash.state)
  (T #(tag, x) : (a, b) t)
  =
  match tag with
  | Ok ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in
    hash_a hsv x
  | Error ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in
    hash_b hsv x
;;

let ok_exn (type a) (T #(tag, x) : (a, exn) t) : a =
  match tag with
  | Ok -> x
  | Error -> raise x
;;

let ok_or_failwith (type a) (T #(tag, x) : (a, string) t) : a =
  match tag with
  | Ok -> x
  | Error -> failwith x
;;

let of_option_or_thunk a_opt ~error =
  match a_opt with
  | Some a -> T #(Ok, a)
  | None -> T #(Error, error ())
;;

let%template[@mode local] of_option_or_thunk a_opt ~error = exclave_
  match a_opt with
  | Some a -> T #(Ok, a)
  | None -> T #(Error, error ())
;;

let map (type a b c) (T #(tag, x) : (a, b) t) ~(f : a -> c) : (c, b) t =
  match tag with
  | Ok -> T #(Ok, f x)
  | Error -> T #(Error, x)
;;

let%template[@mode local] map
  (type a b c)
  (T #(tag, x) : (a, b) t @ local)
  ~(f : a @ local -> c @ local)
  : (c, b) t
  = exclave_
  match tag with
  | Ok -> T #(Ok, f x)
  | Error -> T #(Error, x)
;;

let map_error (type a b c) (T #(tag, x) : (a, b) t) ~(f : b -> c) : (a, c) t =
  match tag with
  | Ok -> T #(Ok, x)
  | Error -> T #(Error, f x)
;;

let%template[@mode local] map_error
  (type a b c)
  (T #(tag, x) : (a, b) t @ local)
  ~(f : b @ local -> c @ local)
  : (a, c) t
  = exclave_
  match tag with
  | Ok -> T #(Ok, x)
  | Error -> T #(Error, f x)
;;

let combine
  (type ok1 ok2 ok3 err)
  (T #(tag1, x1) : (ok1, err) t)
  (T #(tag2, x2) : (ok2, err) t)
  ~(ok : (ok1 -> ok2 -> ok3) @ local)
  ~(err : (err -> err -> err) @ local)
  : (ok3, err) t
  =
  match tag1, tag2 with
  | Ok, Ok -> T #(Ok, ok x1 x2)
  | Error, Ok -> T #(Error, x1)
  | Ok, Error -> T #(Error, x2)
  | Error, Error -> T #(Error, err x1 x2)
;;

let%template[@mode local] combine
  (type ok1 ok2 ok3 err)
  (T #(tag1, x1) : (ok1, err) t)
  (T #(tag2, x2) : (ok2, err) t)
  ~(ok : ok1 @ local -> ok2 @ local -> ok3 @ local)
  ~(err : err @ local -> err @ local -> err @ local)
  : (ok3, err) t
  = exclave_
  match tag1, tag2 with
  | Ok, Ok -> T #(Ok, ok x1 x2)
  | Error, Ok -> T #(Error, x1)
  | Ok, Error -> T #(Error, x2)
  | Error, Error -> T #(Error, err x1 x2)
;;

let try_with f =
  try T #(Ok, f ()) with
  | exn -> T #(Error, exn)
;;

let[@inline] ( >>| ) a f = map a ~f
let[@inline] ( >>= ) a f = bind a ~f

module Let_syntax = struct
  let return = [%eta1 return]
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )

  module Let_syntax = struct
    let return = [%eta1 return]
    let bind = bind
    let map = map
    let both = combine ~ok:(fun x y -> x, y) ~err:(fun x _ -> x)

    module Open_on_rhs = struct end
  end
end

module Local = struct
  module Let_syntax = struct
    let return = [%eta1.exclave return [@mode local]]
    let[@inline] ( >>= ) a f = exclave_ (bind [@mode local]) a ~f
    let[@inline] ( >>| ) a f = exclave_ (map [@mode local]) a ~f

    module Let_syntax = struct
      let return = [%eta1.exclave return [@mode local]]
      let bind = (bind [@mode local])
      let map = (map [@mode local])

      let both =
        (combine [@mode local]) ~ok:(fun x y -> exclave_ x, y) ~err:(fun x _ -> x)
      ;;

      module Open_on_rhs = struct end
    end
  end
end

module Monad_infix = struct
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )
end
