open Hardcaml
open Signal

let create ~clock ~reset ~contents ~address =
  let padding = Config.entries (width address) - List.length contents in
  let cells = contents @ List.init padding (fun _ -> zero 8) in
  let read_address = reg (Reg_spec.create ~clock ~clear:reset ()) address in
  {
    Memory.data = mux read_address cells;
    ready = address ==: read_address &: ~:reset;
  }
