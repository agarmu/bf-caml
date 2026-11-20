open Hardcaml
open Signal

module Operation = struct
  type t =
    | Unknown
    | Reset
    | Pause
    | Begin_load
    | Load_byte
    | End_load
    | Run
    | Step
    | Input
    | Get_state
    | Get_current_cell
    | Read_tape
  [@@deriving compare, enumerate, sexp_of]
end

module Command = Enum.Make_binary (Operation)
module Outcome = Cpu_command_result.Outcome
module Result_code = Cpu_command_result.Result_code
module Notification = Execution_events.Notification
module Event = Execution_events.Kind
module Stop_reason = Execution_events.Stop_reason
module Stop = Execution_events.Stop

type request = {
  valid : Signal.t;
  operation : Signal.t Command.t;
  argument : Signal.t;
  byte : Signal.t;
}

type status = {
  reset : Signal.t;
  loading : Signal.t;
  program_ready : Signal.t;
  running : Signal.t;
  halted : Signal.t;
  clearing : Signal.t;
  invalid_program : Signal.t;
  program_too_large : Signal.t;
  pc : Signal.t;
  pointer : Signal.t;
  instruction_count : Signal.t;
}

type response = {
  valid : Signal.t;
  outcome : Signal.t Result_code.t;
  upload_offset : Signal.t;
  cell : Signal.t;
  status : status;
}

type event = Execution_events.event
type t = { response : response; event : event }

module Make_with_source
    (C : Config.S)
    (Memory : Memory.S)
    (Program : sig
      module Source : Source.S

      val writable : bool

      val input :
        Signal.t Stream.Byte.Source.t ->
        finish:Signal.t ->
        Signal.t Source.Input.t
    end) =
struct
  module Cpu = Cpu.Make_with_source (C) (Memory) (Program.Source)

  let create ~program ~clock ~reset ~input_capacity ~(request : request)
      ~response_ready ~event_ready =
    let argument_bits = width request.argument in
    if max C.program_size_bits C.pointer_bits > argument_bits then
      invalid_arg
        "CPU command argument is too narrow for the configured addresses";
    let field value = uresize value argument_bits in
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let requested = Command.Of_signal.is request.operation in
    let process = wire 1 in
    let applied = reg spec process in
    let reply_pending =
      reg_fb spec ~width:1 ~f:(fun pending ->
          applied |: (pending &: ~:response_ready))
    in
    let active_request =
      request.valid &: ~:(applied |: reply_pending |: reset)
    in
    let command operation = process &: requested operation in
    let reset_command = active_request &: requested Operation.Reset in
    let cpu_reset = reset |: reset_command in
    let upload =
      Program_upload.create spec ~writable:(of_bool Program.writable)
        ~reset:reset_command
        ~begin_load:(command Operation.Begin_load)
        ~write:(command Operation.Load_byte)
        ~end_load:(command Operation.End_load)
        ~requested_offset:request.argument
    in
    let retired = wire 1 in
    let halted = wire 1 in
    let step_done_reported = wire 1 in
    let execution =
      Execution_control.create spec ~available:upload.ready ~reset:reset_command
        ~pause:(command Operation.Pause) ~run:(command Operation.Run)
        ~step:(command Operation.Step) ~count:request.argument ~retired ~halted
        ~reported:step_done_reported
    in
    let send_input = command Operation.Input &: execution.running in
    let input_ready = wire 1 in
    let input_sink, input =
      Stream.Byte.fifo ~capacity:input_capacity ~clock ~clear:cpu_reset
        { valid = send_input; data = request.byte }
        { ready = input_ready }
    in
    let address = uresize request.argument C.pointer_bits in
    let address_valid = field address ==: request.argument in
    let read_tape =
      active_request
      &: requested Operation.Read_tape
      &: ~:(execution.running) &: address_valid
    in
    let current_cell = active_request &: requested Operation.Get_current_cell in
    let cpu_ready = wire 1 in
    let cpu =
      Cpu.create
        ~inspect:{ valid = read_tape; value = address }
        program
        {
          clock;
          reset = cpu_reset;
          new_program = gnd;
          step = execution.enable;
          source =
            Program.input
              { valid = upload.load_byte; data = request.byte }
              ~finish:upload.finish;
          input;
          output = { ready = cpu_ready };
        }
    in
    input_ready <== cpu.input.ready;
    halted <== cpu.halted;
    retired <== cpu.retired;
    let events =
      Execution_events.create spec ~clear:reset_command ~enabled:~:cpu_reset
        ~ready:event_ready
        ~halted:(upload.ready &: cpu.halted)
        ~invalid_program:cpu.invalid_program
        ~program_too_large:cpu.program_too_large ~step_done:execution.step_done
        ~waiting_for_input:(cpu.input.ready &: ~:(input.valid))
        ~output:cpu.output
    in
    step_done_reported <== events.step_done_reported;
    cpu_ready <== events.output_ready;
    let inspection = requested Operation.Read_tape in
    let reading_state = requested Operation.Get_state |: current_cell in
    let local_outcome =
      Cpu_command_result.choose
        [
          ( reset_command |: command Operation.Pause |: reading_state
            |: read_tape,
            Outcome.Accepted );
          (send_input &: input_sink.ready, Outcome.Accepted);
          (send_input, Outcome.Program_input_fifo_full);
          (requested Operation.Input, Outcome.Program_execution_inactive);
          (inspection &: ~:address_valid, Outcome.Tape_address_out_of_range);
          ( inspection &: execution.running,
            Outcome.Tape_inspection_requires_pause );
        ]
        ~otherwise:Outcome.Unrecognized_command
    in
    let choose flag yes no =
      mux2 flag (Result_code.to_raw yes) (Result_code.to_raw no)
      |> Result_code.Of_signal.of_raw
    in
    let outcome =
      choose upload.requested upload.outcome
        (choose execution.requested execution.outcome local_outcome)
    in
    let debug_wait = read_tape |: current_cell &: ~:(cpu.tape_ready) in
    process <== (active_request &: ~:debug_wait);
    let hold ~enable value =
      mux2 reply_pending (reg spec ~enable value) value
    in
    (* Tape and PC are sampled together. Reset replies sample after the reset
       edge so their status reflects the cleared CPU and upload state. *)
    let after_reset = reg spec reset_command in
    let status_enable = process &: ~:reset_command |: after_reset in
    let status value = hold ~enable:status_enable value in
    let response =
      {
        valid = reply_pending &: ~:reset;
        outcome =
          hold ~enable:process (Result_code.to_raw outcome)
          |> Result_code.Of_signal.of_raw;
        upload_offset = hold ~enable:process upload.next_offset;
        cell = hold ~enable:status_enable cpu.current_cell;
        status =
          {
            reset = status upload.reset_state;
            loading = status upload.loading;
            program_ready = status upload.ready;
            running = status execution.running;
            halted = status cpu.halted;
            clearing = status cpu.clearing;
            invalid_program = status cpu.invalid_program;
            program_too_large = status cpu.program_too_large;
            pc = status cpu.pc;
            pointer = status cpu.pointer;
            instruction_count = status cpu.instruction_count;
          };
      }
    in
    { response; event = events.event }
end

module Make_with_memories
    (C : Config.S)
    (Tape_memory : Memory.S)
    (Program_memory : Memory.S) =
  Make_with_source (C) (Tape_memory)
    (struct
      module Source = Source.Loaded (C) (Program_memory)

      let writable = true
      let input byte ~finish : Signal.t Source.Input.t = { byte; finish }
    end)

module Make (C : Config.S) (Memory : Memory.S) =
  Make_with_memories (C) (Memory) (Memory)

module Make_with_rom
    (C : Config.S)
    (Tape_memory : Memory.S)
    (Rom : Memory.Read_only) =
  Make_with_source (C) (Tape_memory)
    (struct
      module Source = Source.Rom (C) (Rom)

      let writable = false
      let input _ ~finish:_ = Source.Input.None
    end)
