open Bf_caml
open Control_sim

let%expect_test "counts stepped and running instructions and clears on reset" =
  let sim =
    create
      (Rom_control.create
         ~program:[ Instruction.Increment; Increment; Increment ])
  in
  issue sim Step;
  expect_event sim Step_done;
  assert (get sim "count" = 1);
  take_event sim;
  issue sim Run;
  expect_event sim Halted;
  assert (get sim "count" = 3);
  issue sim Reset;
  assert (get sim "count" = 0)
