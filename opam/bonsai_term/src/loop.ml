open! Core
open Bonsai
open Async

let start_with_exit driver =
  Driver.compute_first_frame driver;
  let rec go () =
    let%bind (`Frame_painted finish_frame) = Driver.compute_frame driver in
    match%bind finish_frame with
    | `Frame_finished (Exit exit) -> Deferred.Or_error.return exit
    | `Frame_finished Stdin_closed ->
      Deferred.Or_error.error_s
        [%message
          "Bonsai Term app was closed early. The stdin to the app was closed. If this is \
           in prod, and you expect your TUI's stdin's to be closed in practice, please \
           reach out to bonsai-term devs about your use case. If you are writing a test, \
           consider using the bonsai_term_test or bonsai_integration_test libraries \
           instead or alternatively consider mocking out the ?reader, ?writer and \
           ?for_mocking paramters to [Bonsai_term.start]."]
    | `Frame_finished Continue -> go () [@tail]
  in
  go ()
;;

let with_driver
  ~dispose
  ~nosig
  ~mouse
  ~bpaste
  ~reader
  ~writer
  ~time_source
  ~for_mocking
  ~optimize
  ~target_frames_per_second
  (app :
    exit:('exit -> unit Effect.t)
    -> dimensions:Geom.Dimensions.t Bonsai.t
    -> local_ Bonsai.graph
    -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t)
  f
  =
  Deferred.Or_error.try_with_join (fun () ->
    let start_params =
      Start_params.create_exn
        ~dispose
        ~nosig
        ~mouse
        ~bpaste
        ~reader
        ~writer
        ~time_source
        ~for_mocking
        ~optimize
        ~target_frames_per_second
        ~app
    in
    let%bind driver = Driver.create start_params in
    let finally () = Driver.release driver in
    Monitor.protect (fun () -> f driver) ~finally)
;;

let start_with_exit
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  app
  =
  with_driver
    ~dispose
    ~nosig
    ~mouse
    ~bpaste
    ~reader
    ~writer
    ~time_source
    ~optimize
    ~target_frames_per_second
    ~for_mocking
    app
    start_with_exit
;;

let make_app_exit_on_ctrlc app =
  let app ~exit ~dimensions (local_ graph) =
    let ~view, ~handler = app ~dimensions graph in
    let handler =
      let%arr.Bonsai handler in
      fun (event : Event.t) ->
        match event with
        | Key_press { key = ASCII ('C' | 'c'); mods = [ Ctrl ] } -> exit ()
        | Key_press { key = Uchar uchar; mods = [ Ctrl ] }
          when Uchar.equal (Uchar.of_char 'C') uchar
               || Uchar.equal (Uchar.of_char 'c') uchar -> exit ()
        | event -> handler event
    in
    ~view, ~handler
  in
  app
;;

let start
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  app
  =
  start_with_exit
    ?dispose
    ?nosig
    ?mouse
    ?bpaste
    ?reader
    ?writer
    ?time_source
    ?optimize
    ?target_frames_per_second
    ?for_mocking
    (make_app_exit_on_ctrlc app)
;;

module For_testing = struct
  let make_app_exit_on_ctrlc = make_app_exit_on_ctrlc
  let with_driver = with_driver
end
