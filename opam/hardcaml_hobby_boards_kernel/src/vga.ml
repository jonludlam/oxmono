open Base
open Stdio

module Timing = struct
  type t =
    { sync : int
    ; back_porch : int
    ; active : int
    ; front_porch : int
    }
  [@@deriving sexp_of]

  let total { sync; back_porch; active; front_porch } =
    sync + back_porch + active + front_porch
  ;;
end

module Spec = struct
  type t =
    { clock_hz : int
    ; horizontal_timing : Timing.t
    ; vertical_timing : Timing.t
    }
  [@@deriving sexp_of]

  let testing =
    { clock_hz = 100
    ; horizontal_timing = { sync = 2; back_porch = 1; active = 4; front_porch = 2 }
    ; vertical_timing = { sync = 1; back_porch = 2; active = 6; front_porch = 1 }
    }
  ;;

  let testing_no_scaling =
    { clock_hz = 100
    ; horizontal_timing = { sync = 2; back_porch = 1; active = 2; front_porch = 2 }
    ; vertical_timing = { sync = 1; back_porch = 2; active = 3; front_porch = 1 }
    }
  ;;

  let testing_4x4y =
    { clock_hz = 100
    ; horizontal_timing = { sync = 2; back_porch = 1; active = 8; front_porch = 2 }
    ; vertical_timing = { sync = 1; back_porch = 2; active = 12; front_porch = 1 }
    }
  ;;

  let testing_2x4y =
    { clock_hz = 100
    ; horizontal_timing = { sync = 2; back_porch = 1; active = 4; front_porch = 2 }
    ; vertical_timing = { sync = 1; back_porch = 2; active = 12; front_porch = 1 }
    }
  ;;

  let testing_4x2y =
    { clock_hz = 100
    ; horizontal_timing = { sync = 2; back_porch = 1; active = 8; front_porch = 2 }
    ; vertical_timing = { sync = 1; back_porch = 2; active = 6; front_porch = 1 }
    }
  ;;

  let testing_3x5y =
    { clock_hz = 100
    ; horizontal_timing = { sync = 2; back_porch = 1; active = 6; front_porch = 2 }
    ; vertical_timing = { sync = 1; back_porch = 2; active = 15; front_porch = 1 }
    }
  ;;

  let t640x480_60hz =
    { clock_hz = 25_175_000
    ; horizontal_timing = { sync = 96; back_porch = 48; active = 640; front_porch = 16 }
    ; vertical_timing = { sync = 2; back_porch = 33; active = 480; front_porch = 10 }
    }
  ;;

  let timings t =
    let h = Timing.total t.horizontal_timing in
    let v = Timing.total t.vertical_timing in
    let freq = Float.of_int t.clock_hz /. Float.of_int (h * v) in
    print_s [%message (h : int) (v : int) (freq : float)]
  ;;

  let t800x600_60hz =
    { clock_hz = 40_000_000
    ; horizontal_timing = { sync = 128; back_porch = 88; active = 800; front_porch = 40 }
    ; vertical_timing = { sync = 4; back_porch = 23; active = 600; front_porch = 1 }
    }
  ;;

  let t1024x768_60hz =
    { clock_hz = 65_000_000
    ; horizontal_timing =
        { sync = 136; back_porch = 160; active = 1024; front_porch = 24 }
    ; vertical_timing = { sync = 6; back_porch = 29; active = 768; front_porch = 3 }
    }
  ;;

  let t1280x720_60hz =
    { clock_hz = 74_250_000
    ; horizontal_timing =
        { sync = 40; back_porch = 220; active = 1280; front_porch = 110 }
    ; vertical_timing = { sync = 5; back_porch = 20; active = 720; front_porch = 5 }
    }
  ;;

  let t1920x1080_30hz =
    { clock_hz = 74_250_000
    ; horizontal_timing = { sync = 44; back_porch = 148; active = 1920; front_porch = 88 }
    ; vertical_timing = { sync = 5; back_porch = 36; active = 1080; front_porch = 4 }
    }
  ;;

  let t1920x1080_60hz =
    { clock_hz = 148_500_000
    ; horizontal_timing = { sync = 44; back_porch = 148; active = 1920; front_porch = 88 }
    ; vertical_timing = { sync = 5; back_porch = 36; active = 1080; front_porch = 4 }
    }
  ;;

  let%expect_test "check the numbers all work out." =
    timings t640x480_60hz;
    [%expect {| ((h 800) (v 525) (freq 59.94047619047619)) |}];
    timings t800x600_60hz;
    [%expect {| ((h 1056) (v 628) (freq 60.316541208260951)) |}];
    timings t1024x768_60hz;
    [%expect {| ((h 1344) (v 806) (freq 60.003840245775727)) |}];
    timings t1280x720_60hz;
    [%expect {| ((h 1650) (v 750) (freq 60)) |}];
    timings t1920x1080_30hz;
    [%expect {| ((h 2200) (v 1125) (freq 30)) |}];
    timings t1920x1080_60hz;
    [%expect {| ((h 2200) (v 1125) (freq 60)) |}]
  ;;
end

module Scan = struct
  open Hardcaml
  open Signal

  module State = struct
    type t =
      | Sync
      | Back_porch
      | Active
      | Front_porch
    [@@deriving sexp_of, enumerate, compare ~localize]
  end

  module O = struct
    type 'a t =
      { first : 'a
      ; is_sync : 'a
      ; is_back_porch : 'a
      ; is_active : 'a
      ; is_front_porch : 'a
      ; last : 'a
      ; counter : 'a
      }
    [@@deriving hardcaml]
  end

  let create scope spec ~enable (vga_spec : Timing.t) =
    let%hw.Always.State_machine sm = Always.State_machine.create (module State) spec in
    let%hw_var counter =
      Always.Variable.reg spec ~width:(Int.ceil_log2 vga_spec.active)
    in
    let counter_next = counter.value +:. 1 in
    let last = Always.Variable.wire ~default:gnd () in
    let count upto next =
      Always.
        [ counter <-- counter_next
        ; when_
            (counter.value ==:. upto - 1)
            [ counter <--. 0; last <-- vdd; sm.set_next next ]
        ]
    in
    Always.(
      compile
        [ when_
            enable
            [ sm.switch
                [ Sync, count vga_spec.sync Back_porch
                ; Back_porch, count vga_spec.back_porch Active
                ; Active, count vga_spec.active Front_porch
                ; Front_porch, count vga_spec.front_porch Sync
                ]
            ]
        ]);
    let first = counter.value ==:. 0 in
    { O.first
    ; is_sync = sm.is Sync
    ; is_back_porch = sm.is Back_porch
    ; is_active = sm.is Active
    ; is_front_porch = sm.is Front_porch
    ; last = last.value
    ; counter = counter.value
    }
  ;;
end
