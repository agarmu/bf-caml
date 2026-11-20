open Hardcaml
open Signal

module Make_with_memory (C : Config.S) (Memory : Memory.S) = struct
  let pointer_bits = C.pointer_bits

  module Input = struct
    type 'a t = {
      enable : 'a;
      opcode : 'a Instruction.Opcode.t;
      input_byte : 'a; [@bits 8]
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pointer : 'a; [@bits pointer_bits]
      cell : 'a; [@bits 8]
      ready : 'a;
      clearing : 'a;
      output_valid : 'a;
    }
    [@@deriving hardcaml]
  end

  let step ~up value = mux2 up (value +:. 1) (value -:. 1)

  let create ?inspect ~clock ~reset (inputs : _ Input.t) =
    let is = Instruction.is inputs.opcode in
    let ready = wire 1 in
    let enable = inputs.enable &: ready in
    let move_right = enable &: is Move_right in
    let move_left = enable &: is Move_left in
    let pointer =
      reg_fb
        (Reg_spec.create ~clock ~clear:reset ())
        ~enable:(move_right |: move_left) ~width:pointer_bits
        ~f:(step ~up:move_right)
    in
    let cell = wire 8 in
    let increment = is Increment in
    let decrement = is Decrement in
    let input = is Input in
    let write_data = mux2 input inputs.input_byte (step ~up:increment cell) in
    let address =
      match inspect with
      | None -> pointer
      | Some (inspect : _ With_valid.t) ->
          mux2 inspect.valid inspect.value pointer
    in
    let memory =
      Memory.create ~address_bits:pointer_bits ~clear:reset ~clock ~reset
        ~address
        ~write_enable:(enable &: (increment |: decrement |: input))
        ~write_data
    in
    cell <== memory.data;
    ready <== memory.ready;
    {
      Output.pointer;
      cell;
      ready;
      clearing = memory.clearing;
      output_valid = enable &: ~:reset &: is Output;
    }
end

module Make (C : Config.S) = Make_with_memory (C) (Register_memory)
