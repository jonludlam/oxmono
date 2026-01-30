(** [Unboxed] provides canonical aliases for the fundamental unboxed types and their
    corresponding support libraries. This library is intended to be opened unconditionally
    at the top-level in much the same way that {!Core} is. Any module dealing with unboxed
    types in more than a handful of expressions should open this library instead of using
    the builtin type names directly. *)

include Aliases
module Unboxed_stable = Stable
