open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 16
end)

module Sim =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

module Interpreter = Reference.Make (Config)

let reader input =
  let pending = ref (String.to_seq input) in
  fun () ->
    Option.map
      (fun (byte, rest) ->
        pending := rest;
        byte)
      (Seq.uncons !pending)

let with_io ~input run =
  let output = Buffer.create 16 in
  let result = run ~read:(reader input) ~write:(Buffer.add_char output) in
  (result, Buffer.contents output)

let outcome : (_, Simulation.error) result -> Reference.outcome = function
  | Ok _ -> Halted
  | Error (End_of_input _) -> End_of_input
  | Error Unmatched_brackets -> Unmatched_brackets
  | Error (Program_too_large _) -> Program_too_large
  | Error (Cycle_limit _ | Interrupted _) -> failwith "simulation did not halt"

let simulate ~input source =
  let result, output =
    with_io ~input (fun ~read ~write ->
        Sim.run ~max_cycles:100_000 ~read ~write source)
  in
  (outcome result, output)

let interpret ~input source =
  match
    with_io ~input (fun ~read ~write ->
        Interpreter.run ~max_steps:2_000 ~read ~write (Sim.parse source))
  with
  | Some outcome, output -> Some (outcome, output)
  | None, _ -> None

let program ~balanced state =
  let buffer = Buffer.create 32 in
  let add = Buffer.add_char buffer in
  let rec go remaining depth =
    if remaining = 0 then (
      if balanced then Buffer.add_string buffer (String.make depth ']'))
    else
      match Random.State.int state 10 with
      | 0 ->
          add '[';
          go (remaining - 1) (depth + 1)
      | 1 when depth > 0 || not balanced ->
          add ']';
          go (remaining - 1) (depth - 1)
      | _ ->
          add "+-<>.,".[Random.State.int state 6];
          go (remaining - 1) depth
  in
  go (Random.State.int state 30) 0;
  Buffer.contents buffer

let input state =
  String.init (Random.State.int state 6) (fun _ ->
      Char.chr (Random.State.int state 256))

let show (outcome, output) =
  Printf.sprintf "%s %S"
    (Sexplib0.Sexp.to_string (Reference.sexp_of_outcome outcome))
    output

let agrees (outcome, output) (outcome', output') =
  Reference.equal_outcome outcome outcome' && String.equal output output'

let compare_with_reference ~balanced =
  let state = Random.State.make [| 0 |] in
  let checked = ref 0 in
  for _ = 1 to 100 do
    let source = program ~balanced state in
    let input = input state in
    match interpret ~input source with
    | None -> ()
    | Some expected ->
        incr checked;
        let actual = simulate ~input source in
        if not (agrees actual expected) then
          Printf.printf "%S with input %S: got %s, expected %s\n" source input
            (show actual) (show expected)
  done;
  Printf.printf "checked %d programs" !checked

let%expect_test "random programs match the reference" =
  compare_with_reference ~balanced:true;
  [%expect {| checked 88 programs |}]

let%expect_test "random unbalanced programs are rejected like the reference" =
  compare_with_reference ~balanced:false;
  [%expect {| checked 100 programs |}]
