module Config = Bf_caml.Config.Make (struct
  let max_program_size = 256
  let tape_length = 16
end)

module Memory = Bf_caml.Ram_memory
module Simulation = Bf_caml.Simulation.Make (Config) (Memory)
module Loaded = Simulation (Bf_caml.Source.Loaded (Config) (Memory))
module Rom = Simulation (Bf_caml.Source.Rom (Config))

let () =
  match Sys.argv with
  | [| _; path |] | [| _; "--rom"; path |] -> (
      let source =
        try In_channel.with_open_bin path In_channel.input_all
        with Sys_error message ->
          prerr_endline message;
          exit 1
      in
      let write byte =
        output_char stdout byte;
        flush stdout
      in
      match
        (if Array.length Sys.argv = 3 then Rom.run else Loaded.run)
          ~vcd_path:"cell.vcd"
          ~read:(fun () -> In_channel.input_char stdin)
          ~write source
      with
      | Ok () -> ()
      | Error (Bf_caml.Simulation.Program_too_large capacity) ->
          Printf.eprintf "error: program exceeds %d instructions\n%!" capacity;
          exit 1
      | Error Bf_caml.Simulation.Unmatched_brackets ->
          prerr_endline "error: unmatched brackets";
          exit 1)
  | _ ->
      prerr_endline "usage: bf_caml [--rom] PROGRAM.bf";
      exit 2
