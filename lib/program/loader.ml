open Hardcaml
open Signal

module State = struct
  type t = Loading | Finished | Too_large
  [@@deriving compare, enumerate, sexp_of]
end

module Make (C : Config.S) = struct
  let program_size_bits = C.program_size_bits
  let capacity = C.program_capacity

  module Input = struct
    type 'a t = {
      byte : 'a Stream.Byte.Source.t; [@rtlprefix "byte_"]
      finish : 'a;
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      byte : 'a Stream.Byte.Sink.t; [@rtlprefix "byte_"]
      load : 'a;
      opcode : 'a; [@bits Instruction.bits]
      program_size : 'a; [@bits program_size_bits]
      finished : 'a;
      too_large : 'a;
    }
    [@@deriving hardcaml]
  end

  let create ~clock ~reset (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let state = Always.State_machine.create (module State) spec in
    let program_size = Always.Variable.reg spec ~width:program_size_bits in
    let loading = state.is Loading in
    let finished = state.is Finished in
    let opcode = Instruction.decode inputs.byte.data in
    let byte = { Stream.Byte.Sink.ready = ~:reset &: ~:finished } in
    let instruction =
      loading
      &: Stream.Byte.transfer inputs.byte byte
      &: opcode.valid &: ~:(inputs.finish)
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
      Output.byte;
      load;
      opcode = opcode.value;
      program_size = program_size.value;
      finished;
      too_large = state.is Too_large;
    }
end
