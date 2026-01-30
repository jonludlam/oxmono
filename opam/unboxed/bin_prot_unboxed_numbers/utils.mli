open! Base

(** A kind-polymorphic version of {{!Bin_prot.Utils.bin_dump} [Bin_prot.Utils.bin_dump]}. *)
val%template bin_dump
  : ('a : k).
  ?header:bool -> 'a Bin_prot.Type_class.writer -> 'a -> Bin_prot.Common.buf
[@@kind k = (value, float32, float64, bits32, bits64, word)]
