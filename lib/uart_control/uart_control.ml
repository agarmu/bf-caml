open Hardcaml
open Signal
module Kind = Uart_message.Kind
module Wire_error = Uart_message.Error
module Operation = Cpu_control.Operation

let kind = of_int ~width:7
let payload = of_int ~width:8
let field value = uresize value Uart_message.offset_bits

let operation (request : Uart_message.t) =
  let matches opcode = request.kind ==:. opcode in
  let cases =
    [
      (matches Kind.reset, Operation.Reset);
      (matches Kind.pause, Operation.Pause);
      (matches Kind.begin_load, Operation.Begin_load);
      (matches Kind.load_byte, Operation.Load_byte);
      (matches Kind.end_load, Operation.End_load);
      (matches Kind.run, Operation.Run);
      (matches Kind.step, Operation.Step);
      (matches Kind.input, Operation.Input);
      (matches Kind.read_tape, Operation.Read_tape);
      ( matches Kind.get_state &: (request.byte ==:. 1),
        Operation.Get_current_cell );
      (matches Kind.get_state &: (request.byte <=:. 3), Operation.Get_state);
    ]
  in
  let encode operation =
    Cpu_control.Command.to_raw (Cpu_control.Command.Of_signal.of_enum operation)
  in
  Cpu_control.Command.Of_signal.of_raw
    (List.fold_right
       (fun (condition, operation) rest ->
         mux2 condition (encode operation) rest)
       cases (encode Operation.Unknown))

let error_code outcome =
  let open Cpu_control in
  let cases =
    [
      (Outcome.Unrecognized_command, Wire_error.unrecognized_command);
      ( Outcome.Command_not_allowed_in_current_state,
        Wire_error.command_not_allowed_in_current_state );
      (Outcome.Upload_offset_mismatch, Wire_error.upload_offset_mismatch);
      (Outcome.Wrote_to_Readonly, Wire_error.wrote_to_readonly);
      (Outcome.Program_input_fifo_full, Wire_error.program_input_fifo_full);
      (Outcome.Tape_address_out_of_range, Wire_error.tape_address_out_of_range);
      (Outcome.Upload_requires_reset, Wire_error.upload_requires_reset);
      (Outcome.Upload_not_started, Wire_error.upload_not_started);
      (Outcome.Upload_limit_reached, Wire_error.upload_limit_reached);
      (Outcome.Program_execution_inactive, Wire_error.program_execution_inactive);
      ( Outcome.Tape_inspection_requires_pause,
        Wire_error.tape_inspection_requires_pause );
      (Outcome.Program_unavailable, Wire_error.program_unavailable);
      (Outcome.Program_already_halted, Wire_error.program_already_halted);
    ]
  in
  List.fold_right
    (fun (reason, code) rest ->
      mux2 (Result_code.Of_signal.is outcome reason) (payload code) rest)
    cases (zero 8)

let invalid_fields (request : Uart_message.t) =
  let matches opcode = request.kind ==:. opcode in
  let no_arguments =
    matches Kind.reset |: matches Kind.pause |: matches Kind.run
    |: matches Kind.begin_load
  in
  let unused_offset =
    no_arguments |: matches Kind.input |: matches Kind.get_state
  in
  let unused_byte =
    no_arguments |: matches Kind.step |: matches Kind.end_load
    |: matches Kind.read_tape
  in
  unused_offset &: (request.offset <>:. 0)
  |: (unused_byte &: (request.byte <>:. 0))

let reply (request : Uart_message.t) operation (response : Cpu_control.response)
    =
  let requested = Cpu_control.Command.Of_signal.is operation in
  let status = response.status in
  let current_cell = requested Operation.Get_current_cell in
  let read_state = requested Operation.Get_state |: current_cell in
  let read_tape = requested Operation.Read_tape in
  let success =
    Cpu_control.Result_code.Of_signal.is response.outcome
      Cpu_control.Outcome.Accepted
  in
  let count_low = requested Operation.Get_state &: (request.byte ==:. 2) in
  let count_high = requested Operation.Get_state &: (request.byte ==:. 3) in
  let count_low_bits = Uart_message.address_bits + 8 in
  let status_flags =
    concat_lsb
      [
        status.reset;
        status.loading;
        status.program_ready;
        status.running;
        status.halted;
        status.clearing;
        status.invalid_program;
        status.program_too_large;
      ]
  in
  let state_offset =
    mux2 count_low
      (field
         (select status.instruction_count (Uart_message.address_bits - 1) 0))
      (mux2 count_high
         (field (drop_bottom status.instruction_count count_low_bits))
         (mux2 current_cell (field status.pointer) (field status.pc)))
  in
  let state_byte =
    mux2 count_low
      (select status.instruction_count (count_low_bits - 1)
         Uart_message.address_bits)
      (mux2 count_high (zero 8) (mux2 current_cell response.cell status_flags))
  in
  {
    Uart_message.kind =
      mux2 success
        (mux2 read_state (kind Kind.state)
           (mux2 read_tape (kind Kind.tape) (kind Kind.ack)))
        (kind Kind.error);
    id = request.id;
    offset =
      mux2 (success &: read_state) state_offset
        (mux2 (success &: read_tape) request.offset response.upload_offset);
    byte =
      mux2 success
        (mux2 read_state state_byte (mux2 read_tape response.cell (zero 8)))
        (error_code response.outcome);
  }

let event_message ~offset (event : Cpu_control.event) =
  let notification = Cpu_control.Event.Of_signal.is event.kind in
  let stopped = Cpu_control.Stop.Of_signal.is event.stop_reason in
  let halt_reason =
    let open Uart_message.Halt_reason in
    mux2
      (stopped Cpu_control.Stop_reason.Program_too_large)
      (payload program_too_large)
      (mux2
         (stopped Cpu_control.Stop_reason.Invalid_bracket_structure)
         (payload invalid_bracket_structure)
         (payload program_completed))
  in
  {
    Uart_message.kind =
      mux2
        (notification Cpu_control.Notification.Halted)
        (kind Kind.halted)
        (mux2
           (notification Cpu_control.Notification.Step_done)
           (kind Kind.step_done)
           (mux2
              (notification Cpu_control.Notification.Need_input)
              (kind Kind.need_input) (kind Kind.output)));
    id = zero 7;
    offset;
    byte =
      mux2 (notification Cpu_control.Notification.Halted) halt_reason event.byte;
  }

type t = { tx : Signal.t Stream.Byte.Source.t; rx_overrun : Signal.t }

module type S = sig
  val create :
    program:Instruction.t list ->
    clock:Signal.t ->
    reset:Signal.t ->
    rx_capacity:int ->
    rx:Signal.t Stream.Byte.Source.t ->
    tx:Signal.t Stream.Byte.Sink.t ->
    t
end

module Adapt (Package : sig
  val create :
    program:Instruction.t list ->
    clock:Signal.t ->
    reset:Signal.t ->
    input_capacity:int ->
    request:Cpu_control.request ->
    response_ready:Signal.t ->
    event_ready:Signal.t ->
    Cpu_control.t
end) =
struct
  let create ~program ~clock ~reset ~rx_capacity ~rx ~tx =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let read_ready = wire 1 in
    let rx_sink, bytes =
      Stream.Byte.fifo ~capacity:rx_capacity ~clock ~clear:reset rx
        { ready = read_ready }
    in
    let consume = wire 1 in
    let ready, valid, malformed, request =
      Uart_message.receive spec bytes { ready = consume }
    in
    read_ready <== ready;
    let operation = operation request in
    let malformed = malformed |: invalid_fields request in
    let unsupported_selector =
      request.kind ==:. Kind.get_state &: (request.byte >:. 3)
    in
    let invalid_command = malformed |: unsupported_selector in
    let reject = valid &: invalid_command &: ~:reset in
    let can_send = wire 1 in
    let cpu =
      Package.create ~program ~clock ~reset ~input_capacity:rx_capacity
        ~request:
          {
            valid = valid &: ~:invalid_command;
            operation;
            argument = request.offset;
            byte = request.byte;
          }
        ~response_ready:can_send ~event_ready:(can_send &: ~:valid)
    in
    let response_valid = cpu.response.valid |: reject in
    consume <== (can_send &: response_valid);
    let reply = reply request operation cpu.response in
    let reply =
      {
        reply with
        kind = mux2 reject (kind Kind.error) reply.kind;
        offset = mux2 reject cpu.response.upload_offset reply.offset;
        byte =
          mux2 reject
            (mux2 malformed
               (payload Wire_error.malformed_command)
               (payload Wire_error.unsupported_state_selector))
            reply.byte;
      }
    in
    let event = event_message ~offset:cpu.response.upload_offset cpu.event in
    let choose field = mux2 valid (field reply) (field event) in
    let message : Uart_message.t =
      {
        kind = choose (fun message -> message.kind);
        id = choose (fun message -> message.id);
        offset = choose (fun message -> message.offset);
        byte = choose (fun message -> message.byte);
      }
    in
    let tx_ready, tx =
      Uart_message.transmit spec
        ~valid:(response_valid |: (~:valid &: cpu.event.valid))
        message tx
    in
    can_send <== tx_ready;
    { tx; rx_overrun = rx.valid &: ~:(rx_sink.ready |: reset) }
end

module Make_with_memories
    (C : Config.S)
    (Tape_memory : Memory.S)
    (Program_memory : Memory.S) =
  Adapt (Cpu_control.Make_with_memories (C) (Tape_memory) (Program_memory))

module Make (C : Config.S) (Memory : Memory.S) = struct
  include Make_with_memories (C) (Memory) (Memory)

  let create = create ~program:[]
end

module Make_with_rom
    (C : Config.S)
    (Tape_memory : Memory.S)
    (Rom : Memory.Read_only) =
  Adapt (Cpu_control.Make_with_rom (C) (Tape_memory) (Rom))
