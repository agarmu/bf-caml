open Hardcaml
open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 16
end)

module Tape = Tape.Make_with_memory (Config) (Ram_memory)

let tape_length = Config.tape_length

type sim = {
  sim : Cyclesim.t_port_list;
  reset : Bits.t ref;
  inputs : Bits.t ref Tape.Input.t;
  outputs : Bits.t ref Tape.Output.t;
}

let create () =
  let tape =
    Tape.create ~clock:(Signal.input "clock" 1) ~reset:(Signal.input "reset" 1)
      (Tape.Input.Of_signal.inputs ())
  in
  let sim =
    Cyclesim.create
      (Circuit.create_exn ~name:"tape"
         (Tape.Output.to_list (Tape.Output.Of_signal.outputs tape)))
  in
  {
    sim;
    reset = Cyclesim.in_port sim "reset";
    inputs = Tape.Input.map Tape.Input.port_names ~f:(Cyclesim.in_port sim);
    outputs = Tape.Output.map Tape.Output.port_names ~f:(Cyclesim.out_port sim);
  }

let high port = Bits.to_bool !port

let rec wait_until ?(limit = 1_000) t condition =
  if limit = 0 then failwith "timed out"
  else if not (condition ()) then (
    Cyclesim.cycle t.sim;
    wait_until ~limit:(limit - 1) t condition)

let settle t =
  Cyclesim.cycle t.sim;
  wait_until t (fun () -> high t.outputs.ready)

let reset t =
  t.reset := Bits.vdd;
  Cyclesim.cycle t.sim;
  t.reset := Bits.gnd;
  wait_until t (fun () -> not (high t.outputs.clearing));
  settle t

let apply t ~input op =
  Instruction.Opcode.iter2 t.inputs.opcode
    (Instruction.Opcode.Of_bits.of_enum op)
    ~f:( := );
  t.inputs.input_byte := Bits.of_int ~width:8 input;
  settle t;
  t.inputs.enable := Bits.vdd;
  Cyclesim.cycle t.sim;
  t.inputs.enable := Bits.gnd;
  settle t

let%expect_test "random operations match a model" =
  let t = create () in
  let state = Random.State.make [| 0 |] in
  let cells = Array.make tape_length 0 in
  let pointer = ref 0 in
  let mismatches = ref 0 in
  let move delta = pointer := (!pointer + delta) land (tape_length - 1) in
  let update f = cells.(!pointer) <- f cells.(!pointer) land 0xff in
  reset t;
  for _ = 1 to 1_000 do
    let input = Random.State.int state 256 in
    (match Random.State.int state 50 with
    | 0 ->
        reset t;
        Array.fill cells 0 tape_length 0;
        pointer := 0
    | _ -> (
        let op = List.nth Instruction.all (Random.State.int state 8) in
        apply t ~input op;
        match op with
        | Move_right -> move 1
        | Move_left -> move (-1)
        | Increment -> update succ
        | Decrement -> update pred
        | Input -> update (Fun.const input)
        | Output | Loop_start | Loop_end -> ()));
    if
      Bits.to_int !(t.outputs.pointer) <> !pointer
      || Bits.to_int !(t.outputs.cell) <> cells.(!pointer)
    then incr mismatches
  done;
  Printf.printf "mismatches: %d" !mismatches;
  [%expect {| mismatches: 0 |}]
