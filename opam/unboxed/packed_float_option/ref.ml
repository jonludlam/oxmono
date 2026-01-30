open! Core

type t = { mutable v : Packed_float_option0.t }

include
  Sexpable.Of_sexpable
    (Packed_float_option0)
    (struct
      type nonrec t = t

      let to_sexpable { v } = v
      let of_sexpable v = { v }
    end)

include
  Binable.Of_binable_without_uuid [@alert "-legacy"]
    (Packed_float_option0)
    (struct
      type nonrec t = t

      let to_binable { v } = v
      let of_binable v = { v }
    end)

let create v = { v }
let get { v } = v
let set t v = t.v <- v
