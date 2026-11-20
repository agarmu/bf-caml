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
  let instruction_count_bits = 32

  module Input = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      new_program : 'a;
      source : 'a Source.Input.t;
      step : 'a;
      input : 'a Stream.Byte.Source.t; [@rtlprefix "input_"]
      output : 'a Stream.Byte.Sink.t; [@rtlprefix "output_"]
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pc : 'a; [@bits program_bits]
      opcode : 'a Instruction.Opcode.t; [@rtlprefix "opcode_"]
      halted : 'a;
      pointer : 'a; [@bits pointer_bits]
      current_cell : 'a; [@bits 8]
      clearing : 'a;
      tape_ready : 'a;
      retired : 'a;
      instruction_count : 'a; [@bits instruction_count_bits]
      invalid_program : 'a;
      program_too_large : 'a;
      input : 'a Stream.Byte.Sink.t; [@rtlprefix "input_"]
      output : 'a Stream.Byte.Source.t; [@rtlprefix "output_"]
      source : 'a Source.Output.t;
    }
    [@@deriving hardcaml]
  end

  let fetch ~clock ~reset (source : Source.t) ~step ~jump =
    let start = wire 1 in
    let scan = wire 1 in
    let jump_address = wire C.program_size_bits in
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
    jump_address <== uresize brackets.match_address C.program_size_bits +:. 1;
    (program, brackets)

  let io_ready (inputs : _ Input.t) opcode =
    let blocked kind ready = Instruction.is opcode kind &: ~:ready in
    ~:(blocked Input inputs.input.valid |: blocked Output inputs.output.ready)

  let execute ?inspect (inputs : _ Input.t) ~reset ~running ~inspecting
      (program : _ Program.Output.t) =
    let cell = wire 8 in
    let cell_ready = wire 1 in
    let ready = wire 1 in
    let instruction =
      Pipeline.create ~clock:inputs.clock ~reset
        ~enable:(running &: program.ready) ~pc:program.pc ~opcode:program.opcode
        ~cell ~cell_ready ~ready
    in
    let opcode = instruction.value.fetched.opcode in
    let step = instruction.valid &: ready in
    let tape =
      Tape.create ?inspect ~clock:inputs.clock ~reset:inputs.reset
        { Tape.Input.enable = step; opcode; input_byte = inputs.input.data }
    in
    cell <== tape.cell;
    cell_ready <== (tape.ready &: ~:inspecting);
    ready <== (running &: tape.ready &: io_ready inputs opcode);
    (instruction, tape, step)

  let jumps ~opcode ~cell =
    let is = Instruction.is opcode in
    is Loop_start &: (cell ==:. 0) |: (is Loop_end &: (cell <>:. 0))

  let create ?inspect instructions (inputs : _ Input.t) =
    let clock = inputs.clock in
    let reset = inputs.reset |: inputs.new_program in
    let source, source_outputs =
      Source.create instructions ~clock ~reset inputs.source
    in
    let step = wire 1 in
    let jump = wire 1 in
    let program, brackets = fetch ~clock ~reset source ~step ~jump in
    let inspecting =
      match inspect with
      | None -> gnd
      | Some (read : _ With_valid.t) -> read.valid
    in
    let running =
      inputs.step &: brackets.ready &: ~:(program.halted)
      &: ~:(reset |: inspecting)
    in
    let instruction, tape, executed =
      execute ?inspect inputs ~reset ~running ~inspecting program
    in
    let { Pipeline.Execute_data.fetched = { pc; opcode }; cell } =
      instruction.value
    in
    let instruction_count =
      reg_fb (Reg_spec.create ~clock ~clear:reset ())
        ~width:instruction_count_bits ~enable:executed ~f:(fun count ->
          count +:. 1)
    in
    step <== executed;
    jump <== (executed &: jumps ~opcode ~cell);
    let issued kind =
      running &: tape.ready &: instruction.valid &: Instruction.is opcode kind
    in
    {
      Output.pc = mux2 instruction.valid pc program.pc;
      opcode =
        Instruction.Opcode.Of_signal.mux2 instruction.valid opcode
          program.opcode;
      halted =
        brackets.ready &: program.halted |: brackets.invalid_program
        |: source.too_large;
      pointer = tape.pointer;
      current_cell = tape.cell;
      clearing = tape.clearing;
      tape_ready = tape.ready;
      retired = executed;
      instruction_count;
      invalid_program = brackets.invalid_program;
      program_too_large = source.too_large;
      input = { ready = issued Input };
      output = { valid = issued Output; data = cell };
      source = source_outputs;
    }
end

module Make_with_memory (C : Config.S) (Memory : Memory.S) =
  Make_with_source (C) (Memory) (Source.Loaded (C) (Memory))

module Make (C : Config.S) = Make_with_memory (C) (Register_memory)
