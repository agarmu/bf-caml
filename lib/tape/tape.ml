open Hardcaml
open Signal

let cell_count = 16

module Input = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    enable : 'a;
    opcode : 'a; [@bits 8]
    pointer_load : 'a;
    pointer_next : 'a; [@bits 4]
  }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = {
    pointer : 'a; [@bits 4]
    cell : 'a; [@bits 8]
    cells : 'a; [@bits 128]
  }
  [@@deriving hardcaml]
end

let create (inputs : _ Input.t) =
  (* Cell writes use the current pointer. A pointer load takes effect after the
     same clock edge, so simultaneous load and write target the old address. *)
  let pointer =
    Pointer.create
      {
        Pointer.Input.clock = inputs.clock;
        reset = inputs.reset;
        enable = inputs.enable;
        opcode = inputs.opcode;
        load = inputs.pointer_load;
        next_address = inputs.pointer_next;
      }
  in
  let cells =
    List.init cell_count (fun index ->
        let selected = pointer.address ==: of_int ~width:4 index in
        (Cell.create
           {
             Cell.Input.clock = inputs.clock;
             reset = inputs.reset;
             enable = inputs.enable &: selected;
             opcode = inputs.opcode;
           })
          .cell)
  in
  {
    Output.pointer = pointer.address;
    cell = mux pointer.address cells;
    cells = concat_lsb cells;
  }
