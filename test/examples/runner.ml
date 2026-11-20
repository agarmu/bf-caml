open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 4096
  let tape_length = 1024
end)

module Sim =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

let reader input =
  let pending = ref (String.to_seq input) in
  fun () ->
    Option.map
      (fun (byte, rest) ->
        pending := rest;
        byte)
      (Seq.uncons !pending)

let simulate ?(input = "") ?max_cycles source =
  let buffer = Buffer.create 64 in
  let result =
    Sim.run ?max_cycles ~read:(reader input) ~write:(Buffer.add_char buffer)
      source
  in
  (Buffer.contents buffer, result)

let output ?input ?max_cycles source =
  match simulate ?input ?max_cycles source with
  | text, Ok _ -> text
  | text, Error (Cycle_limit _) -> text ^ "\n[cycle limit]"
  | _, Error _ -> "error"

let run ?input ?max_cycles source =
  print_string (output ?input ?max_cycles source)

let example name =
  In_channel.with_open_bin
    ("../../examples/" ^ name ^ ".bf")
    In_channel.input_all

let check ?(count = 10) ~generate ~input ~expected source =
  let state = Random.State.make [| 0 |] in
  for _ = 1 to count do
    let case = generate state in
    let actual = output ~input:(input case) source in
    if actual <> expected case then
      Printf.printf "input %S: got %S, expected %S\n" (input case) actual
        (expected case)
  done

let check_prefix ~max_cycles ~expected source =
  match simulate ~max_cycles source with
  | text, Error (Cycle_limit _) when String.starts_with ~prefix:text expected ->
      Printf.printf "%d characters match" (String.length text)
  | text, _ -> Printf.printf "mismatch: %S" text
