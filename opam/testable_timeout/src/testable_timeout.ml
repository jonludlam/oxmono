open! Core
open Async_kernel

let time_source = ref (Time_source.wall_clock ())

type handle = bool ref

let set_timeout span ~f =
  let cancelled = ref false in
  (let%map.Deferred () = Time_source.after !time_source span in
   if not !cancelled then f ())
  |> Deferred.don't_wait_for;
  cancelled
;;

let cancel cancelled = cancelled := true

module For_running_tests = struct
  let with_ override_time_source ~f =
    let old_time_source = !time_source in
    time_source := override_time_source;
    let%map.Deferred result = f () in
    time_source := old_time_source;
    result
  ;;
end
