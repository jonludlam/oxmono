open! Base

module type S = sig
  type integer

  (** Produces an ordinal abbreviation string, e.g. [to_string 13 = "13th"],
      [to_string 100002 = "100002nd"], etc. *)
  val to_string : integer -> string

  (** Produces an ordinal abbreviation string with delimiters every three digits, e.g.
      [to_string 13 = "13th"], [to_string 100002 = "100_002nd"], etc. *)
  val to_string_hum : ?delimiter:char -> integer -> string

  (** Produces the appropriate ordinal suffix to append to an integer, e.g.
      [to_suffix 3 = "rd"], [to_suffix 100002 = "nd"], etc. *)
  val to_suffix : integer -> string
end

module type Ordinal_abbreviation = sig
  module type S = S

  (** {1 Conversions for Int} *)

  include S with type integer := int

  (** {1 Conversions for Other Built-in Integer Types} *)

  module Int32 : S with type integer := int32
  module Int63 : S with type integer := Int63.t
  module Int64 : S with type integer := int64
  module Nativeint : S with type integer := nativeint

  (** {1 Conversions for Arbitrary Integer Types} *)

  module Make (Integer : Int.S_common) : S with type integer := Integer.t
end
