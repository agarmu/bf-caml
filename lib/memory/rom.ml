open Hardcaml
open Signal

let create ~contents ~address =
  let padding = Config.entries (width address) - List.length contents in
  let cells = contents @ List.init padding (fun _ -> zero 8) in
  { Memory.data = mux address cells; ready = vdd }
