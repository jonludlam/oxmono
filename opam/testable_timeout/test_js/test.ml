open! Core
open Testable_timeout
open Async_kernel
open Async_js_test

let () = Async_js.init ()

let test ~f =
  let time_source = Time_source.create ~now:Time_ns.epoch () in
  For_running_tests.with_ (Time_source.read_only time_source) ~f:(fun () -> f time_source)
;;

let%expect_test "can schedule and run a callback" =
  test ~f:(fun time_source ->
    let (_ : handle) =
      set_timeout Time_ns.Span.minute ~f:(fun () -> print_endline "hello")
    in
    [%expect {| |}];
    let%bind () = Time_source.advance_by_alarms_by time_source Time_ns.Span.minute in
    [%expect {| hello |}];
    return ())
;;

let%expect_test "can schedule, then cancel a callback" =
  test ~f:(fun time_source ->
    let handle = set_timeout Time_ns.Span.minute ~f:(fun () -> print_endline "hello") in
    [%expect {| |}];
    cancel handle;
    let%bind () = Time_source.advance_by_alarms_by time_source Time_ns.Span.minute in
    [%expect {| |}];
    return ())
;;

let%expect_test "if scheduled for zero time, not resolved immediately." =
  test ~f:(fun time_source ->
    let (_ : handle) =
      set_timeout Time_ns.Span.zero ~f:(fun () -> print_endline "hello")
    in
    [%expect {| |}];
    let%bind () = Time_source.advance_by_alarms_by time_source Time_ns.Span.nanosecond in
    [%expect {| hello |}];
    return ())
;;

let%expect_test "inner [with] is isolated" =
  test ~f:(fun time_source_outer ->
    let (_ : handle) =
      set_timeout Time_ns.Span.minute ~f:(fun () -> print_endline "outer")
    in
    [%expect {| |}];
    let%bind () =
      test ~f:(fun time_source_inner ->
        let (_ : handle) =
          set_timeout Time_ns.Span.day ~f:(fun () -> print_endline "inner")
        in
        [%expect {| |}];
        let%bind () =
          Time_source.advance_by_alarms_by time_source_inner Time_ns.Span.day
        in
        [%expect {| inner |}];
        return ())
    in
    let%bind () =
      Time_source.advance_by_alarms_by time_source_outer Time_ns.Span.minute
    in
    [%expect {| outer |}];
    return ())
;;

let%expect_test "inner exception doesn't affect outer" =
  Backtrace.Exn.set_recording false;
  test ~f:(fun time_source_outer ->
    let (_ : handle) =
      set_timeout Time_ns.Span.minute ~f:(fun () -> print_endline "outer")
    in
    [%expect {| |}];
    let%bind err =
      Deferred.Or_error.try_with (fun () ->
        test ~f:(fun _ ->
          let (_ : handle) =
            set_timeout Time_ns.Span.day ~f:(fun () -> print_endline "inner")
          in
          [%expect {| |}];
          raise_s [%message "something went wrong"]))
    in
    print_s [%message (err : unit Or_error.t)];
    [%expect {| (err (Error (monitor.ml.Error "something went wrong"))) |}];
    let%bind () =
      Time_source.advance_by_alarms_by time_source_outer (Time_ns.Span.of_sec 30.0)
    in
    [%expect {| |}];
    let%bind () =
      Time_source.advance_by_alarms_by time_source_outer (Time_ns.Span.of_sec 30.0)
    in
    [%expect {| outer |}];
    Backtrace.Exn.set_recording true;
    return ())
;;
