open Hardcaml
open Signal

module Make_with_source (C : Config.S) (Memory : Memory.S) (Source : Source.S) =
struct
  module Program = Program.Make (C)
  module Brackets = Brackets.Make (C)
  module Tape = Tape.Make_with_memory (C) (Memory)
  module Pipeline = Instruction_pipeline.Make (C)

  let program_bits = C.program_bits
  let pointer_bits = C.pointer_bits

  module Input = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      new_program : 'a;
      source : 'a Source.Input.t;
      step : 'a;
      input_byte : 'a; [@bits 8]
      input_ready : 'a;
      output_ready : 'a;
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pc : 'a; [@bits program_bits]
      opcode : 'a; [@bits 8]
      halted : 'a;
      pointer : 'a; [@bits pointer_bits]
      current_cell : 'a; [@bits 8]
      invalid_program : 'a;
      input_needed : 'a;
      output_valid : 'a;
      output_byte : 'a; [@bits 8]
    }
    [@@deriving hardcaml]
  end

  let fetch ~clock ~reset (source : Source.t) ~step ~jump =
    let start = wire 1 in
    let scan = wire 1 in
    let jump_address = wire program_bits in
    let program =
      Program.create ~clock ~reset ~read:source.read
        {
          Program.Input.load = source.load;
          program_size = source.program_size;
          start;
          step = scan |: step;
          jump;
          jump_address;
        }
    in
    let brackets =
      Brackets.create ~clock ~reset
        {
          Brackets.Input.load_finished = source.finished;
          program_halted = program.halted;
          program_ready = program.ready;
          pc = program.pc;
          opcode = program.opcode;
        }
    in
    start <== brackets.start;
    scan <== brackets.scan;
    jump_address <== brackets.match_address +:. 1;
    (program, brackets)

  let io_ready (inputs : _ Input.t) opcode =
    let blocked kind ready = Instruction.matches opcode kind &: ~:ready in
    ~:(blocked Input inputs.input_ready |: blocked Output inputs.output_ready)

  let execute (inputs : _ Input.t) ~reset ~running
      (program : _ Program.Output.t) =
    let cell = wire 8 in
    let cell_ready = wire 1 in
    let ready = wire 1 in
    let instruction =
      Pipeline.create ~clock:inputs.clock ~reset
        ~enable:(running &: program.ready) ~pc:program.pc ~opcode:program.opcode
        ~cell ~cell_ready ~ready
    in
    let opcode = instruction.value.opcode in
    let step = instruction.valid &: ready in
    let tape =
      Tape.create ~clock:inputs.clock ~reset:inputs.reset
        { Tape.Input.enable = step; opcode; input_byte = inputs.input_byte }
    in
    cell <== tape.cell;
    cell_ready <== tape.ready;
    ready <== (running &: io_ready inputs opcode);
    (instruction, tape, step)

  let jumps ~opcode ~cell =
    let is = Instruction.matches opcode in
    is Loop_start &: (cell ==:. 0) |: (is Loop_end &: (cell <>:. 0))

  let create instructions (inputs : _ Input.t) =
    let clock = inputs.clock in
    let reset = inputs.reset |: inputs.new_program in
    let source = Source.create instructions ~clock ~reset inputs.source in
    let step = wire 1 in
    let jump = wire 1 in
    let program, brackets = fetch ~clock ~reset source ~step ~jump in
    let running =
      inputs.step &: brackets.ready &: ~:(program.halted) &: ~:reset
    in
    let instruction, tape, executed = execute inputs ~reset ~running program in
    let { Pipeline.Execute_data.pc; opcode; cell } = instruction.value in
    step <== executed;
    jump <== (executed &: jumps ~opcode ~cell);
    let issued kind =
      running &: instruction.valid &: Instruction.matches opcode kind
    in
    {
      Output.pc = mux2 instruction.valid pc program.pc;
      opcode = mux2 instruction.valid opcode program.opcode;
      halted = brackets.ready &: program.halted |: brackets.invalid_program;
      pointer = tape.pointer;
      current_cell = tape.cell;
      invalid_program = brackets.invalid_program;
      input_needed = issued Input;
      output_valid = issued Output;
      output_byte = cell;
    }
end

module Make_with_memory (C : Config.S) (Memory : Memory.S) =
  Make_with_source (C) (Memory) (Source.Loaded (C) (Memory))

module Make (C : Config.S) = Make_with_memory (C) (Register_memory)
