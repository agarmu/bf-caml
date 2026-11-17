open Hardcaml
open Signal

module Make (C : Config.S) = struct
  let program_bits = C.program_bits

  module Fetch_data = struct
    type 'a t = {
      pc : 'a; [@bits program_bits]
      opcode : 'a Instruction.Opcode.t;
    }
    [@@deriving hardcaml]
  end

  module Execute_data = struct
    type 'a t = { fetched : 'a Fetch_data.t; cell : 'a [@bits 8] }
    [@@deriving hardcaml]
  end

  module Fetch = Hardcaml_circuits.Datapath_register.Make (Fetch_data)
  module Execute = Hardcaml_circuits.Datapath_register.Make (Execute_data)

  let create ~clock ~reset ~enable ~pc ~opcode ~cell ~cell_ready ~ready =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let busy = wire 1 in
    let operand_ready = wire 1 in
    let fetch_valid = enable &: ~:busy &: ~:reset in
    let fetched =
      Fetch.create_io spec
        {
          Fetch.IO.data = { Fetch_data.pc; opcode };
          valid = fetch_valid;
          ready = cell_ready &: operand_ready;
        }
    in
    let execution =
      Execute.create_io spec
        {
          Execute.IO.data = { Execute_data.fetched = fetched.data; cell };
          valid = fetched.valid &: cell_ready &: ~:reset;
          ready;
        }
    in
    operand_ready <== execution.ready;
    busy
    <== reg spec
          (mux2 (execution.valid &: ready) gnd
             (busy |: (fetch_valid &: fetched.ready)));
    { With_valid.valid = execution.valid; value = execution.data }
end
