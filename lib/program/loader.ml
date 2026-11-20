open Hardcaml
open Signal

module State = struct
  type t = Loading | Finished | Too_large
  [@@deriving compare, enumerate, sexp_of]
end

module Make (C : Config.S) = struct
  let program_bits = C.program_bits
  let capacity = C.program_capacity

  module Input = struct
    type 'a t = { byte_valid : 'a; byte : 'a; [@bits 8] finish : 'a }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      byte_ready : 'a;
      load : 'a;
      opcode : 'a; [@bits Instruction.bits]
      program_size : 'a; [@bits program_bits]
      finished : 'a;
      too_large : 'a;
    }
    [@@deriving hardcaml]
  end

  let create ~clock ~reset (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let state = Always.State_machine.create (module State) spec in
    let program_size = Always.Variable.reg spec ~width:program_bits in
    let loading = state.is Loading in
    let finished = state.is Finished in
    let opcode = Instruction.decode inputs.byte in
    let byte_ready = ~:reset &: ~:finished in
    let instruction =
      loading &: inputs.byte_valid &: byte_ready &: opcode.valid
      &: ~:(inputs.finish)
    in
    let full = program_size.value ==:. capacity in
    let load = instruction &: ~:full in
    Always.(
      compile
        [
          when_ load [ program_size <-- program_size.value +:. 1 ];
          when_ (instruction &: full) [ state.set_next Too_large ];
          when_ (loading &: inputs.finish) [ state.set_next Finished ];
        ]);
    {
      Output.byte_ready;
      load;
      opcode = opcode.value;
      program_size = program_size.value;
      finished;
      too_large = state.is Too_large;
    }
end
