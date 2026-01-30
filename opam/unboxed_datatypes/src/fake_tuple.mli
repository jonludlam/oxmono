(* This interface is based on [lib/core/src/tuple_intf.ml] and added to ad-hoc. It is to
   be deleted once non-values are supported in tuples. *)

module%template T2 : sig
  [@@@kind.default ka = value]
  [@@@kind.default kb = (value, float64, immediate64)]

  type ('a : ka, 'b : kb) t =
    { fst : 'a
    ; snd : 'b
    }
  [@@deriving sexp, equal, compare, globalize]

  val create : ('a : ka) ('b : kb). 'a -> 'b -> (('a, 'b) t[@kind ka kb])
end

module%template T3 : sig
  [@@@kind.default ka = value]
  [@@@kind.default kb = value]
  [@@@kind.default kc = (value, float64, immediate64)]

  type ('a : ka, 'b : kb, 'c : kc) t =
    { fst : 'a
    ; snd : 'b
    ; trd : 'c
    }
  [@@deriving sexp, equal, compare, globalize]

  val create
    : ('a : ka) ('b : kb) ('c : kc).
    'a -> 'b -> 'c -> (('a, 'b, 'c) t[@kind ka kb kc])

  val get1 : ('a : ka) ('b : kb) ('c : kc). (('a, 'b, 'c) t[@kind ka kb kc]) -> 'a
  val get2 : ('a : ka) ('b : kb) ('c : kc). (('a, 'b, 'c) t[@kind ka kb kc]) -> 'b
  val get3 : ('a : ka) ('b : kb) ('c : kc). (('a, 'b, 'c) t[@kind ka kb kc]) -> 'c
end
