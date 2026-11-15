open Hardcaml
open Signal

module State = struct
  type t = Loading | Finished [@@deriving compare, enumerate, sexp_of]
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
      load : 'a;
      load_address : 'a; [@bits program_bits]
      load_opcode : 'a; [@bits 8]
      program_size : 'a; [@bits program_bits]
      finished : 'a;
    }
    [@@deriving hardcaml]
  end

  let create ~clock ~reset (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let state = Always.State_machine.create (module State) spec in
    let program_size = Always.Variable.reg spec ~width:program_bits in
    let finished = state.is Finished in
    let valid_opcode =
      List.map
        (fun (opcode, _) -> Instruction.matches inputs.byte opcode)
        Instruction.opcode_chars
      |> reduce ~f:( |: )
    in
    let load =
      inputs.byte_valid &: valid_opcode &: ~:reset &: ~:(inputs.finish)
      &: ~:finished
    in
    Always.(
      compile
        [
          when_ load [ program_size <-- program_size.value +:. 1 ];
          when_
            (inputs.finish |: (load &: (program_size.value ==:. capacity - 1)))
            [ state.set_next Finished ];
        ]);
    {
      Output.load;
      load_address = program_size.value;
      load_opcode = inputs.byte;
      program_size = program_size.value;
      finished;
    }
end
