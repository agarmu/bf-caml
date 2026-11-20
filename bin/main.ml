open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 32768
  let tape_length = 32768
end)

module Eight_lanes = struct
  let lanes = 8
end

module Banked = Banked_memory.Make (Ram_memory) (Eight_lanes)
module Banked_rom = Banked_memory.Make_read_only (Rom) (Eight_lanes)

type memory = Ram | Rom
type bank = Tape | Program | All | None

let tape_memory bank : (module Memory.S) =
  match bank with
  | Tape | All -> (module Banked)
  | Program | None -> (module Ram_memory)

let program_source memory bank : (module Source.S) =
  let banked = match bank with Program | All -> true | Tape | None -> false in
  match (memory, banked) with
  | Ram, false -> (module Source.Loaded (Config) (Ram_memory))
  | Ram, true -> (module Source.Loaded (Config) (Banked))
  | Rom, false -> (module Source.Rom (Config) (Rom))
  | Rom, true -> (module Source.Rom (Config) (Banked_rom))

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

let run ~memory ~bank path =
  let module Tape = (val tape_memory bank) in
  let module Program = (val program_source memory bank) in
  let module S = Simulation.Make (Config) (Tape) (Program) in
  match S.run ~vcd_path:"cell.vcd" ~read ~write (read_source path) with
  | Ok stats -> prerr_endline (Simulation.Stats.to_string stats)
  | Error (Simulation.Program_too_large capacity) ->
      Printf.eprintf "error: program exceeds %d instructions\n%!" capacity;
      exit 1
  | Error Unmatched_brackets ->
      prerr_endline "error: unmatched brackets";
      exit 1
  | Error (Interrupted stats) ->
      prerr_endline ("\ninterrupted\n" ^ Simulation.Stats.to_string stats);
      exit 130
  | Error (Cycle_limit stats) ->
      prerr_endline
        ("\ncycle limit reached\n" ^ Simulation.Stats.to_string stats);
      exit 1
  | Error (End_of_input stats) ->
      prerr_endline ("\nend of input\n" ^ Simulation.Stats.to_string stats);
      exit 1

let usage =
  "usage: bf_caml [--memory ram|rom] [--bank tape|program|all|none] PROGRAM.bf"

let () =
  Sys.catch_break true;
  let memory = ref Ram in
  let bank = ref None in
  let paths = ref [] in
  let choose options target =
    Arg.Symbol
      (List.map fst options, fun name -> target := List.assoc name options)
  in
  Arg.parse
    [
      ( "--memory",
        choose [ ("ram", Ram); ("rom", Rom) ] memory,
        " program memory: loaded into ram, or baked into rom (default ram)" );
      ( "--bank",
        choose
          [ ("tape", Tape); ("program", Program); ("all", All); ("none", None) ]
          bank,
        " which memories to bank (default none)" );
    ]
    (fun path -> paths := path :: !paths)
    usage;
  match !paths with
  | [ path ] -> run ~memory:!memory ~bank:!bank path
  | _ ->
      prerr_endline usage;
      exit 2
