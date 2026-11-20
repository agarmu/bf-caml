open Hardcaml
open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 8
  let tape_length = 8
end)

module Ram = Cpu_control.Make (Config) (Register_memory)
module Rom_control = Cpu_control.Make_with_rom (Config) (Register_memory) (Rom)
module Command = Cpu_control.Command
module Result_code = Cpu_control.Result_code
module Event = Cpu_control.Event

let command_width = Command.to_raw Command.port_widths

let set sim name value =
  let port = Cyclesim.in_port sim name in
  port := Bits.of_int ~width:(Bits.width !port) value

let get sim name = Bits.to_int !(Cyclesim.out_port sim name)
let cycle = Cyclesim.cycle

let advance sim cycles =
  for _ = 1 to cycles do
    cycle sim
  done

let wait_for sim name expected =
  let rec loop remaining =
    if get sim name <> expected then (
      if remaining = 0 then
        failwith (Printf.sprintf "timed out waiting for %s = %d" name expected);
      cycle sim;
      loop (remaining - 1))
  in
  loop 100

let create make =
  let open Signal in
  let request : Cpu_control.request =
    {
      valid = input "request_valid" 1;
      operation = Command.Of_signal.of_raw (input "operation" command_width);
      argument = input "argument" 8;
      byte = input "byte" 8;
    }
  in
  let control : Cpu_control.t =
    make ~clock:(input "clock" 1) ~reset:(input "reset" 1) ~input_capacity:2
      ~request ~response_ready:(input "response_ready" 1)
      ~event_ready:(input "event_ready" 1)
  in
  let response = control.response in
  let status = response.status in
  let sim =
    Cyclesim.create
      (Circuit.create_exn ~name:"cpu_control"
         [
           output "response_valid" response.valid;
           output "outcome" (Result_code.to_raw response.outcome);
           output "offset" response.upload_offset;
           output "reset_state" status.reset;
           output "loading" status.loading;
           output "program_ready" status.program_ready;
           output "count" status.instruction_count;
           output "cell" response.cell;
           output "event_valid" control.event.valid;
           output "event_kind" (Event.to_raw control.event.kind);
           output "event_byte" control.event.byte;
         ])
  in
  set sim "reset" 1;
  cycle sim;
  set sim "reset" 0;
  sim

let start_raw sim ~operation ~argument ~byte =
  set sim "operation" operation;
  set sim "argument" argument;
  set sim "byte" byte;
  set sim "request_valid" 1

let start sim ?(argument = 0) ?(byte = 0) operation =
  let operation =
    Bits.to_int (Command.to_raw (Command.Of_bits.of_enum operation))
  in
  start_raw sim ~operation ~argument ~byte

let outcome_code outcome =
  Bits.to_int (Result_code.to_raw (Result_code.Of_bits.of_enum outcome))

let expect_reply sim outcome =
  wait_for sim "response_valid" 1;
  assert (get sim "outcome" = outcome_code outcome)

let take_reply sim =
  set sim "response_ready" 1;
  cycle sim;
  set sim "response_ready" 0;
  set sim "request_valid" 0

let issue sim ?argument ?byte operation =
  start sim ?argument ?byte operation;
  expect_reply sim Cpu_control.Outcome.Accepted;
  take_reply sim

let expect_event sim notification =
  wait_for sim "event_valid" 1;
  let expected =
    Bits.to_int (Event.to_raw (Event.Of_bits.of_enum notification))
  in
  assert (get sim "event_kind" = expected)

let take_event sim =
  set sim "event_ready" 1;
  cycle sim;
  set sim "event_ready" 0
