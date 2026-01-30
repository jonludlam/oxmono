module type S = sig
  module I : sig
    type 'a t =
      { clock_100 : 'a
      ; reset_n : 'a
      }
    [@@deriving hardcaml]
  end

  include Board.M_I(I).S with type board := Board.t
end

module type Clock_and_reset = sig
  module type S = S

  include S
end
