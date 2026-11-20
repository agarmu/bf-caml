open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 4096
  let tape_length = 1024
end)

module Setups = Test_setups.Make (Config)

let extremes =
  List.filter
    (fun (name, _) ->
      List.mem name
        [ "ram tape, ram program"; "banked tape, banked rom program" ])
    Setups.all

let reader input =
  let pending = ref (String.to_seq input) in
  fun () ->
    Option.map
      (fun (byte, rest) ->
        pending := rest;
        byte)
      (Seq.uncons !pending)

let simulate_on run ~input ?max_cycles source =
  let buffer = Buffer.create 64 in
  let result =
    run ?vcd_path:None ?max_cycles ~read:(reader input)
      ~write:(Buffer.add_char buffer) source
  in
  (Buffer.contents buffer, result)

let render : _ * (_, Simulation.error) result -> string = function
  | text, Ok _ -> text
  | text, Error (Cycle_limit _) -> text ^ "\n[cycle limit]"
  | _, Error _ -> "error"

let consistent ((text, result) as a) ((text', result') as b) =
  match (result, result') with
  | Error (Simulation.Cycle_limit _), Error (Simulation.Cycle_limit _) ->
      String.starts_with ~prefix:text text'
      || String.starts_with ~prefix:text' text
  | _ -> render a = render b

let simulate ?(setups = Setups.all) ?(input = "") ?max_cycles source =
  let results =
    List.map
      (fun (name, run) -> (name, simulate_on run ~input ?max_cycles source))
      setups
  in
  let shortest =
    List.map snd results
    |> List.fold_left
         (fun a b ->
           if String.length (fst b) < String.length (fst a) then b else a)
         (snd (List.hd results))
  in
  List.iter
    (fun (name, result) ->
      if not (consistent result shortest) then
        Printf.printf "%s differs: %S\n" name (render result))
    results;
  shortest

let output ?setups ?input ?max_cycles source =
  render (simulate ?setups ?input ?max_cycles source)

let run ?setups ?input ?max_cycles source =
  print_string (output ?setups ?input ?max_cycles source)

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

let check_prefix ?(setups = Setups.all) ~max_cycles ~expected source =
  let matched (name, run) =
    match simulate_on run ~input:"" ~max_cycles source with
    | text, Error (Simulation.Cycle_limit _)
      when String.starts_with ~prefix:text expected ->
        String.length text
    | text, _ ->
        Printf.printf "%s mismatch: %S\n" name text;
        0
  in
  Printf.printf "%d characters match"
    (List.fold_left min max_int (List.map matched setups))
