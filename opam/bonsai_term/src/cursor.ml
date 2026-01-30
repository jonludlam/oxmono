open! Core

module Kind = struct
  type t =
    | Default
    | Bar
    | Bar_blinking
    | Block
    | Block_blinking
    | Underline
    | Underline_blinking
  [@@deriving sexp_of, equal]

  let to_notty = function
    | Default -> `Default
    | Bar -> `Bar
    | Bar_blinking -> `Bar_blinking
    | Block -> `Block
    | Block_blinking -> `Block_blinking
    | Underline -> `Underline
    | Underline_blinking -> `Underline_blinking
  ;;
end

type t =
  { position : Geom.Position.t
  ; kind : Kind.t
  }
[@@deriving sexp_of, equal]

let variable : (t option -> unit Ui_effect.t) Bonsai.Dynamic_scope.t =
  Bonsai.Dynamic_scope.create
    ~name:"set_cursor"
    ~fallback:(fun cursor ->
      let _ : _ =
        raise_s
          [%message
            "Bug in bonsai_term! Mouse handler not registered! [set_cursor] won't occur"
              (cursor : t option)]
      in
      Ui_effect.Ignore)
    ()
;;

let set_cursor_position = Bonsai.Dynamic_scope.lookup variable

let register term inside =
  let value =
    Bonsai.return
      (Ui_effect_of_deferred.of_deferred_fun (fun cursor ->
         Term.cursor
           term
           (Option.map cursor ~f:(fun { position = { x; y }; kind } ->
              let kind = Kind.to_notty kind in
              x, y, kind))))
  in
  Bonsai.Dynamic_scope.set variable value ~inside
;;

module For_mock_tests = struct
  let register inside =
    let value =
      Bonsai.return (fun cursor ->
        Ui_effect.of_sync_fun (fun () -> print_s [%message (cursor : t option)]) ())
    in
    Bonsai.Dynamic_scope.set variable value ~inside
  ;;
end
