open Hardcaml
open Signal

module Input = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    new_program : 'a;
    byte_valid : 'a;
    byte : 'a; [@bits 8]
    finish : 'a;
    step : 'a;
    input_byte : 'a; [@bits 8]
  }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = {
    pc : 'a; [@bits 8]
    opcode : 'a; [@bits 8]
    halted : 'a;
    pointer : 'a; [@bits 4]
    cells : 'a; [@bits 128]
    invalid_program : 'a;
    input_needed : 'a;
    output_valid : 'a;
    output_byte : 'a; [@bits 8]
  }
  [@@deriving hardcaml]
end

let create (inputs : _ Input.t) =
  let program_reset = inputs.reset |: inputs.new_program in
  let loader =
    Loader.create
      {
        Loader.Input.clock = inputs.clock;
        reset = program_reset;
        byte_valid = inputs.byte_valid;
        byte = inputs.byte;
        finish = inputs.finish;
      }
  in
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:program_reset () in
  (* Scan bracket pairs before allowing any instruction to touch the tape. *)
  let phase = wire 2 in
  let loading_phase = of_int ~width:2 0 in
  let preprocess_phase = of_int ~width:2 1 in
  let execute_phase = of_int ~width:2 2 in
  let error_phase = of_int ~width:2 3 in
  let load_finished =
    phase ==: loading_phase &: loader.finished
    &: ~:(reg spec loader.finished)
    &: ~:program_reset
  in
  let preprocessing = phase ==: preprocess_phase in
  let executing = phase ==: execute_phase in
  let invalid_program = phase ==: error_phase in
  let start = wire 1 in
  let scan = wire 1 in
  let execute_step = wire 1 in
  let jump = wire 1 in
  let jump_address = wire 8 in
  let program =
    Program.create
      {
        Program.Input.clock = inputs.clock;
        reset = program_reset;
        load = loader.load;
        load_address = loader.load_address;
        load_opcode = loader.load_opcode;
        program_size = loader.program_size;
        start;
        step = scan |: execute_step;
        jump;
        jump_address;
      }
  in
  scan <== (preprocessing &: ~:(program.halted) &: ~:program_reset);
  let brackets =
    Brackets.create
      {
        Brackets.Input.clock = inputs.clock;
        reset = program_reset;
        scan;
        pc = program.pc;
        opcode = program.opcode;
      }
  in
  let preprocessing_done = preprocessing &: program.halted &: ~:program_reset in
  let invalid_brackets = brackets.invalid |: ~:(brackets.balanced) in
  let start_execution = preprocessing_done &: ~:invalid_brackets in
  (* [start] rewinds the program counter for both passes. *)
  start <== (load_finished |: start_execution);
  phase
  <== reg spec
        ~enable:(load_finished |: preprocessing_done)
        (mux2 load_finished preprocess_phase
           (mux2 invalid_brackets error_phase execute_phase));
  execute_step
  <== (inputs.step &: executing &: ~:(program.halted) &: ~:program_reset);
  let tape =
    Tape.create
      {
        Tape.Input.clock = inputs.clock;
        reset = inputs.reset;
        enable = execute_step;
        opcode = program.opcode;
        input_byte = inputs.input_byte;
        pointer_load = gnd;
        pointer_next = zero 4;
      }
  in
  let opening = Instruction.matches program.opcode Instruction.Loop_start in
  let closing = Instruction.matches program.opcode Instruction.Loop_end in
  jump
  <== (execute_step
      &: (opening &: (tape.cell ==:. 0) |: (closing &: (tape.cell <>:. 0))));
  (* The table stores bracket positions; execution resumes after the match. *)
  jump_address <== brackets.match_address +:. 1;
  {
    Output.pc = program.pc;
    opcode = program.opcode;
    halted = executing &: program.halted |: invalid_program;
    pointer = tape.pointer;
    cells = tape.cells;
    invalid_program;
    input_needed =
      execute_step &: Instruction.matches program.opcode Instruction.Input;
    output_valid = reg spec tape.output_valid;
    (* [.] leaves the selected cell unchanged across the clock edge. *)
    output_byte = tape.cell;
  }
