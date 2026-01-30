open! Base

module type S_any = sig
  type t : any
  type boxed : value

  val name : string

  val%template box : t @ m -> boxed @ m [@@mode m = (global, local)]

  val unbox : boxed -> t
end

module type Binable_any = sig
  module type S_any = S_any

  [%%template:
  [@@@kind.default k = (float32, float64, bits32, bits64, word)]

  module Of_binable
      (Boxed : sig
       @@ portable
         type t

         include Bin_prot.Binable.S_only_functions [@mode local] with type t := t
       end)
      (T : sig
       @@ portable
         type t : k

         include S_any with type t := t and type boxed := Boxed.t
       end) : sig
    @@ portable
    include Bin_prot.Binable.S [@mode local] with type t := T.t
  end]
end
