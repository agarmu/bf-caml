open Hardcaml
open Signal

module State = struct
  type t = Halted | Running [@@deriving compare, enumerate, sexp_of]
end

module Make (C : Config.S) = struct
  let program_bits = C.program_bits
  let capacity = C.program_capacity

  (* [program_size] is the number of loaded opcodes, from 0 to [capacity].
   [start] begins a run; [step] advances the counter. *)
  module Input = struct
    type 'a t = {
      load : 'a;
      program_size : 'a; [@bits program_bits]
      start : 'a;
      step : 'a;
      jump : 'a;
      jump_address : 'a; [@bits program_bits]
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pc : 'a; [@bits program_bits]
      opcode : 'a Instruction.Opcode.t;
      halted : 'a;
      ready : 'a;
    }
    [@@deriving hardcaml]
  end

  let create ~clock ~reset ~(read : Memory.read) (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let state = Always.State_machine.create (module State) spec in
    let pc = Always.Variable.reg spec ~width:program_bits in
    let memory = read ~address:pc.value in
    let advance = inputs.step &: state.is Running &: ~:(inputs.load) in
    (* A jump to [program_size] halts; otherwise the final sequential step halts. *)
    let last = pc.value ==: inputs.program_size -:. 1 in
    let continue =
      mux2 inputs.jump (inputs.jump_address <: inputs.program_size) ~:last
    in
    let next_pc = mux2 inputs.jump inputs.jump_address (pc.value +:. 1) in
    Always.(
      compile
        [
          if_ inputs.start
            [
              pc <-- zero program_bits;
              if_
                (inputs.program_size <>:. 0)
                [ state.set_next Running ]
                [ state.set_next Halted ];
            ]
            [
              when_ advance
                [ if_ continue [ pc <-- next_pc ] [ state.set_next Halted ] ];
            ];
        ]);
    {
      Output.pc = pc.value;
      opcode = Instruction.Opcode.Of_signal.of_raw memory.data;
      halted = state.is Halted;
      ready = memory.ready;
    }
end
