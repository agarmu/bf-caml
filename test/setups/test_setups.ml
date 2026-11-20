open Bf_caml

module Four_lanes = struct
  let lanes = 4
end

module Banked = Banked_memory.Make (Ram_memory) (Four_lanes)
module Banked_rom = Banked_memory.Make_read_only (Rom) (Four_lanes)

module Make (C : Config.S) = struct
  let tapes =
    [ ("ram", (module Ram_memory : Memory.S)); ("banked", (module Banked)) ]

  let programs =
    [
      ("ram", (module Source.Loaded (C) (Ram_memory) : Source.S));
      ("banked ram", (module Source.Loaded (C) (Banked)));
      ("rom", (module Source.Rom (C) (Rom)));
      ("banked rom", (module Source.Rom (C) (Banked_rom)));
    ]

  let all =
    List.concat_map
      (fun (tape, (module Tape : Memory.S)) ->
        List.map
          (fun (program, (module Program : Source.S)) ->
            let module S = Simulation.Make (C) (Tape) (Program) in
            (Printf.sprintf "%s tape, %s program" tape program, S.run))
          programs)
      tapes
end
