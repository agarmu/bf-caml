open Bf_caml
open Control_sim

let%expect_test
    "step completion survives pause and another step under event backpressure" =
  let sim =
    create
      (Rom_control.create
         ~program:[ Instruction.Increment; Increment; Increment ])
  in
  issue sim Step;
  expect_event sim Step_done;
  assert (get sim "count" = 1);
  issue sim Pause;
  issue sim Step;
  advance sim 10;
  expect_event sim Step_done;
  assert (get sim "count" = 1);
  take_event sim;
  expect_event sim Step_done;
  assert (get sim "count" = 2);
  take_event sim;
  issue sim ~argument:8 Step;
  expect_event sim Halted;
  assert (get sim "count" = 3)

let%expect_test "an older step completion is delivered before a later halt" =
  let sim =
    create (Rom_control.create ~program:[ Instruction.Increment; Increment ])
  in
  issue sim Step;
  expect_event sim Step_done;
  issue sim Run;
  wait_for sim "count" 2;
  expect_event sim Step_done;
  take_event sim;
  expect_event sim Halted;
  take_event sim;
  advance sim 5;
  assert (get sim "event_valid" = 0)
