module Config = Bf_caml.Config.Make (struct
  let max_program_size = 32768
  let tape_length = 32768
end)

module Memory = Bf_caml.Ram_memory
module Simulation = Bf_caml.Simulation.Make (Config) (Memory)
module Loaded = Simulation (Bf_caml.Source.Loaded (Config) (Memory))
module Rom = Simulation (Bf_caml.Source.Rom (Config))

let read_source path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error message ->
    prerr_endline message;
    exit 1

let write byte =
  output_char stdout byte;
  flush stdout

let read =
  let ended = ref false in
  fun () ->
    match In_channel.input_char stdin with
    | Some byte -> Some byte
    | None when !ended -> None
    | None ->
        ended := true;
        Some '\004'

let run ~rom path =
  match
    (if rom then Rom.run else Loaded.run)
      ~vcd_path:"cell.vcd" ~read ~write (read_source path)
  with
  | Ok stats -> prerr_endline (Bf_caml.Simulation.Stats.to_string stats)
  | Error (Bf_caml.Simulation.Program_too_large capacity) ->
      Printf.eprintf "error: program exceeds %d instructions\n%!" capacity;
      exit 1
  | Error Bf_caml.Simulation.Unmatched_brackets ->
      prerr_endline "error: unmatched brackets";
      exit 1
  | Error (Bf_caml.Simulation.Interrupted stats) ->
      prerr_endline
        ("\ninterrupted\n" ^ Bf_caml.Simulation.Stats.to_string stats);
      exit 130
  | Error (Bf_caml.Simulation.End_of_input stats) ->
      prerr_endline
        ("\nend of input\n" ^ Bf_caml.Simulation.Stats.to_string stats);
      exit 1

let () =
  Sys.catch_break true;
  match Sys.argv with
  | [| _; path |] -> run ~rom:false path
  | [| _; "--rom"; path |] -> run ~rom:true path
  | _ ->
      prerr_endline "usage: bf_caml [--rom] PROGRAM.bf";
      exit 2
