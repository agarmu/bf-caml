open Hardcaml
open Signal

let create ~address_bits ~clear ~clock ~reset ~address ~write_enable ~write_data
    =
  let spec = Reg_spec.create ~clock ~clear () in
  let cells =
    List.init (Config.entries address_bits) (fun index ->
        let selected = address ==: of_int ~width:address_bits index in
        reg spec ~enable:(write_enable &: selected) write_data)
  in
  { Memory.data = mux address cells; ready = ~:reset }
