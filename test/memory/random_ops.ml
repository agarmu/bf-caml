open Hardcaml
open Bf_caml

let address_bits = 5
let entries = 1 lsl address_bits

let backends =
  [
    ("ram", (module Ram_memory : Memory.S));
    ("register", (module Register_memory : Memory.S));
    ( "banked ram x4",
      (module Banked_memory.Make
                (Ram_memory)
                (struct
                  let lanes = 4
                end) : Memory.S) );
  ]

let create (module M : Memory.S) =
  let reset = Signal.input "reset" 1 in
  let memory =
    M.create ~address_bits ~clear:reset ~clock:(Signal.input "clock" 1) ~reset
      ~address:(Signal.input "address" address_bits)
      ~write_enable:(Signal.input "write_enable" 1)
      ~write_data:(Signal.input "write_data" 8)
  in
  Cyclesim.create
    (Circuit.create_exn ~name:"memory"
       [
         Signal.output "data" memory.data;
         Signal.output "ready" memory.ready;
         Signal.output "clearing" memory.clearing;
       ])

let set sim name value =
  let port = Cyclesim.in_port sim name in
  port := Bits.of_int ~width:(Bits.width !port) value

let get sim name = Bits.to_int !(Cyclesim.out_port sim name)

let rec wait_until ?(limit = 1_000) sim condition =
  if limit = 0 then failwith "timed out"
  else if not (condition ()) then (
    Cyclesim.cycle sim;
    wait_until ~limit:(limit - 1) sim condition)

let settle sim address =
  set sim "address" address;
  Cyclesim.cycle sim;
  wait_until sim (fun () -> get sim "ready" = 1)

let pulse sim name =
  set sim name 1;
  Cyclesim.cycle sim;
  set sim name 0

let reset sim =
  pulse sim "reset";
  wait_until sim (fun () -> get sim "clearing" = 0)

let read sim address =
  settle sim address;
  get sim "data"

let write sim address value =
  settle sim address;
  set sim "write_data" value;
  pulse sim "write_enable"

let random_ops backend =
  let sim = create backend in
  let state = Random.State.make [| 0 |] in
  let model = Array.make entries 0 in
  let mismatches = ref 0 in
  reset sim;
  for _ = 1 to 1_000 do
    let address = Random.State.int state entries in
    match Random.State.int state 40 with
    | 0 ->
        reset sim;
        Array.fill model 0 entries 0
    | n when n < 20 ->
        let value = Random.State.int state 256 in
        write sim address value;
        model.(address) <- value
    | _ -> if read sim address <> model.(address) then incr mismatches
  done;
  !mismatches

let%expect_test "random reads, writes and resets match a model" =
  List.iter
    (fun (name, backend) ->
      Printf.printf "%-14s mismatches: %d\n" name (random_ops backend))
    backends;
  [%expect
    {|
    ram            mismatches: 0
    register       mismatches: 0
    banked ram x4  mismatches: 0
    |}]
