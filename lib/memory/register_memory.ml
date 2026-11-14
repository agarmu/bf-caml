open Hardcaml
open Signal

module Make (C : Config.S) : Memory.S = struct
  let address_bits = C.pointer_bits

  let create ~clock ~reset ~address ~write_enable ~write_data =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let cells =
      List.init (Config.entries address_bits) (fun index ->
          let selected = address ==: of_int ~width:address_bits index in
          reg spec ~enable:(write_enable &: selected) write_data)
    in
    mux address cells
end
