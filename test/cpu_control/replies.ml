open Bf_caml
open Control_sim

let%expect_test "stalled replies are stable and an upload byte is applied once"
    =
  let sim = create (Ram.create ~program:[]) in
  start sim Begin_load;
  expect_reply sim Accepted;
  let loading_at_reply = get sim "loading" in
  advance sim 6;
  expect_reply sim Accepted;
  assert (get sim "offset" = 0 && get sim "loading" = loading_at_reply);
  take_reply sim;
  assert (get sim "loading" = 1);
  issue sim ~byte:(Char.code '+') Load_byte;
  start sim ~argument:1 ~byte:(Char.code '.') Load_byte;
  expect_reply sim Accepted;
  assert (get sim "offset" = 2);
  advance sim 6;
  expect_reply sim Accepted;
  assert (get sim "offset" = 2);
  take_reply sim;
  issue sim ~argument:2 End_load;
  issue sim Run;
  expect_event sim Output;
  assert (get sim "event_byte" = 1);
  take_event sim;
  expect_event sim Halted;
  assert (get sim "count" = 2)

let%expect_test
    "reset clears execution and upload together before the reply transfers" =
  let sim = create (Ram.create ~program:[]) in
  issue sim Begin_load;
  issue sim ~byte:(Char.code '+') Load_byte;
  issue sim ~argument:1 End_load;
  issue sim Run;
  expect_event sim Halted;
  assert (get sim "count" = 1);
  start sim Reset;
  expect_reply sim Accepted;
  assert (get sim "reset_state" = 1);
  assert (get sim "program_ready" = 0);
  assert (get sim "offset" = 0);
  assert (get sim "count" = 0);
  assert (get sim "cell" = 0);
  assert (get sim "event_valid" = 0);
  advance sim 6;
  expect_reply sim Accepted;
  assert (get sim "reset_state" = 1 && get sim "count" = 0);
  take_reply sim;
  issue sim Begin_load

let%expect_test "a spare binary command encoding is unrecognized" =
  let sim = create (Ram.create ~program:[]) in
  let operation = (1 lsl command_width) - 1 in
  assert (operation >= List.length Cpu_control.Operation.all);
  start_raw sim ~operation ~argument:0 ~byte:0;
  expect_reply sim Unrecognized_command;
  take_reply sim
