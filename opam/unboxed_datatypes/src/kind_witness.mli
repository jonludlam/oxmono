(** Type that can be matched on in order to narrow the kind of its parameter.

    The standard way to reference a kind witness is via templating, e.g.
    [Kind_witness.t[@kind.explicit float64]], though we also define aliases such as
    [Kind_witness.float64] for readability.

    See [lib/unboxed_datatypes/test/kind_witness_example.ml] for an example. *)

type%template (_ : any) t = T : ('a : k). ('a t[@kind.explicit k])
[@@kind.explicit k = (base_or_null_with_imm, value, bits8, bits16)]

type ('a : any) value = ('a t[@kind.explicit value])
type ('a : any) value_or_null = ('a t[@kind.explicit value_or_null])
type ('a : any) float32 = ('a t[@kind.explicit float32])
type ('a : any) float64 = ('a t[@kind.explicit float64])
type ('a : any) bits8 = ('a t[@kind.explicit bits8])
type ('a : any) bits16 = ('a t[@kind.explicit bits16])
type ('a : any) bits32 = ('a t[@kind.explicit bits32])
type ('a : any) bits64 = ('a t[@kind.explicit bits64])
type ('a : any) immediate = ('a t[@kind.explicit immediate])
type ('a : any) immediate64 = ('a t[@kind.explicit immediate64])
type ('a : any) word = ('a t[@kind.explicit word])
