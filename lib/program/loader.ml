open Hardcaml
open Signal

module Make (C : Config.S) = struct
  let program_bits = C.program_bits
  let capacity = Config.entries program_bits - 1

  module Input = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      byte_valid : 'a;
      byte : 'a; [@bits 8]
      finish : 'a;
    }
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

  let create (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
    (* The count is also the address for the next accepted opcode. *)
    let program_size = wire program_bits in
    let finished = wire 1 in
    let valid_opcode =
      List.fold_left
        (fun valid (opcode, _) ->
          valid |: Instruction.matches inputs.byte opcode)
        gnd Instruction.opcode_chars
    in
    let load =
      inputs.byte_valid &: valid_opcode &: ~:(inputs.reset) &: ~:(inputs.finish)
      &: ~:finished
    in
    program_size
    <== reg_fb spec ~enable:load ~width:program_bits ~f:(fun size -> size +:. 1);
    (* Finishing or filling the store makes later bytes inert until reset. *)
    finished
    <== reg_fb spec ~width:1 ~f:(fun was_finished ->
        was_finished |: inputs.finish
        |: (load &: (program_size ==:. capacity - 1)));
    {
      Output.load;
      load_address = program_size;
      load_opcode = inputs.byte;
      program_size;
      finished;
    }
end
