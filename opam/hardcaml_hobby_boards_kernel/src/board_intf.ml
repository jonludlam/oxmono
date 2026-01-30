open Base
open Hardcaml

module M_IOT (I : Interface.S) (O : Interface.S) (T : Interface.S) = struct
  module type S = sig
    type board

    module T_enabled : Interface.S with type 'a t = 'a With_valid.t T.t

    val create : board -> Signal.t I.t * Signal.t T.t
    val complete : board -> Signal.t O.t -> Signal.t T_enabled.t -> unit
  end
end

module M_I (I : Interface.S) = struct
  module type S = sig
    type board

    val create : board -> Signal.t I.t
  end
end

module M_O (O : Interface.S) = struct
  module type S = sig
    type board

    val complete : board -> Signal.t O.t -> unit
  end
end

module M_T (T : Interface.S) = struct
  module type S = sig
    type board

    module T_enabled : Interface.S with type 'a t = 'a With_valid.t T.t

    val create : board -> Signal.t T.t
    val complete : board -> Signal.t T_enabled.t -> unit
  end
end

module M_IO (I : Interface.S) (O : Interface.S) = struct
  module type S = sig
    type board

    val create : board -> Signal.t I.t
    val complete : board -> Signal.t O.t -> unit
  end
end

module M_IT (I : Interface.S) (T : Interface.S) = struct
  module type S = sig
    type board

    module T_enabled : Interface.S with type 'a t = 'a With_valid.t T.t

    val create : board -> Signal.t I.t * Signal.t T.t
    val complete : board -> Signal.t T_enabled.t -> unit
  end
end

module M_OT (O : Interface.S) (T : Interface.S) = struct
  module type S = sig
    type board

    module T_enabled : Interface.S with type 'a t = 'a With_valid.t T.t

    val create : board -> Signal.t T.t
    val complete : board -> Signal.t O.t -> Signal.t T_enabled.t -> unit
  end
end

module type Subsystem_name = sig
  val core : string
end

module type Subsystem = sig
  type t =
    { inputs : Signal.t list
    ; outputs : Signal.t list
    ; input_tristates : Signal.t list
    ; output_tristates : Signal.t list
    ; complete : bool
    }
  [@@deriving sexp_of]
end

module type Board = sig
  module type Subsystem_name = Subsystem_name
  module type Subsystem = Subsystem

  module Subsystem : Subsystem

  type t [@@deriving sexp_of]

  val create : ?flatten_design:bool -> unit -> t
  val scope : t -> Scope.t
  val subsystems : t -> (string, Subsystem.t) Hashtbl.t
  val pins : t -> Pin.t list

  module M_IOT = M_IOT
  module M_I = M_I
  module M_O = M_O
  module M_T = M_T
  module M_IO = M_IO
  module M_IT = M_IT
  module M_OT = M_OT

  module Make_IOT
      (Subsystem_name : Subsystem_name)
      (I : Interface.S)
      (O : Interface.S)
      (T : Interface.S) : M_IOT(I)(O)(T).S with type board := t

  module Make_I (Subsystem_name : Subsystem_name) (I : Interface.S) :
    M_I(I).S with type board := t

  module Make_O (Subsystem_name : Subsystem_name) (O : Interface.S) :
    M_O(O).S with type board := t

  module Make_T (Subsystem_name : Subsystem_name) (T : Interface.S) :
    M_T(T).S with type board := t

  module Make_IO (Subsystem_name : Subsystem_name) (I : Interface.S) (O : Interface.S) :
    M_IO(I)(O).S with type board := t

  module Make_IT (Subsystem_name : Subsystem_name) (I : Interface.S) (T : Interface.S) :
    M_IT(I)(T).S with type board := t

  module Make_OT (Subsystem_name : Subsystem_name) (O : Interface.S) (T : Interface.S) :
    M_OT(O)(T).S with type board := t
end
