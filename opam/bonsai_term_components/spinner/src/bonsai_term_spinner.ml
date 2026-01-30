open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Kind = struct
  type t =
    | Dot
    | Line
end

module Config = struct
  type t =
    { frames : string Array.t
    ; tick_every : Time_ns.Span.t
    }

  let get : Kind.t -> t = function
    | Dot ->
      { frames = [| "⣷"; "⣯"; "⣟"; "⡿"; "⢿"; "⣻"; "⣽"; "⣾" |]
      ; tick_every = Time_ns.Span.of_sec 0.1
      }
    | Line -> { frames = [| "|"; "/"; "-"; "\\" |]; tick_every = Time_ns.Span.of_sec 0.1 }
  ;;
end

let component ?kind ?(attrs = Bonsai.return []) (local_ graph) =
  let kind =
    match kind with
    | Some kind -> kind
    | None ->
      if Bonsai_term_compatibility.is_in_emacs || am_running_test then Kind.Line else Dot
  in
  let%tydi { frames; tick_every } = Config.get kind in
  let current_frame, tick =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ curr () -> (curr + 1) % Array.length frames)
      graph
  in
  let () =
    let tick =
      let%arr tick in
      tick ()
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
      (Bonsai.return tick_every)
      tick
      graph
  in
  let frame =
    let%arr current_frame in
    frames.(current_frame % Array.length frames)
  in
  let%arr frame and attrs in
  View.hcat [ View.text ~attrs frame ]
;;
