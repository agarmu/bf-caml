open Hardcaml
open Bf_caml
open Control_sim

module Slow_tape : Memory.S = struct
  let create ~address_bits ~clear ~clock ~reset ~address ~write_enable
      ~write_data =
    let open Signal in
    let memory =
      Register_memory.create ~address_bits ~clear ~clock ~reset ~address
        ~write_enable ~write_data
    in
    let ready =
      memory.ready &: (address ==:. 0 |: input "other_cells_ready" 1)
    in
    { memory with ready }
end

module Control = Cpu_control.Make_with_rom (Config) (Slow_tape) (Rom)

let%expect_test
    "paused inspection cannot replace an in-flight instruction operand" =
  let sim =
    create
      (Control.create ~program:[ Instruction.Increment; Move_right; Output ])
  in
  issue sim Run;
  wait_for sim "count" 2;
  (* Fetch the output instruction while the second cell is still unavailable. *)
  advance sim 10;
  assert (get sim "count" = 2 && get sim "event_valid" = 0);
  issue sim Pause;
  start sim Read_tape;
  expect_reply sim Accepted;
  assert (get sim "cell" = 1);
  take_reply sim;
  set sim "other_cells_ready" 1;
  issue sim Run;
  expect_event sim Output;
  assert (get sim "event_byte" = 0);
  take_event sim;
  expect_event sim Halted
