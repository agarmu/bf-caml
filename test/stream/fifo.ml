open Hardcaml
open Bf_caml

let capacity = 4

let create () =
  let upstream, downstream =
    Stream.Byte.fifo ~capacity ~clock:(Signal.input "clock" 1)
      ~clear:(Signal.input "clear" 1)
      { valid = Signal.input "in_valid" 1; data = Signal.input "in_data" 8 }
      { ready = Signal.input "out_ready" 1 }
  in
  Cyclesim.create
    (Circuit.create_exn ~name:"fifo"
       [
         Signal.output "in_ready" upstream.ready;
         Signal.output "out_valid" downstream.valid;
         Signal.output "out_data" downstream.data;
       ])

let set sim name value =
  let port = Cyclesim.in_port sim name in
  port := Bits.of_int ~width:(Bits.width !port) value

let get sim name = Bits.to_int !(Cyclesim.out_port ~clock_edge:Before sim name)

let%expect_test "bytes come out complete and in order under random backpressure"
    =
  let sim = create () in
  let state = Random.State.make [| 0 |] in
  let sent = String.init 200 (fun _ -> Char.chr (Random.State.int state 256)) in
  let next = ref 0 in
  let received = Buffer.create 200 in
  while Buffer.length received < String.length sent do
    let offering = !next < String.length sent && Random.State.bool state in
    set sim "in_valid" (Bool.to_int offering);
    if offering then set sim "in_data" (Char.code sent.[!next]);
    set sim "out_ready" (Bool.to_int (Random.State.bool state));
    Cyclesim.cycle sim;
    if offering && get sim "in_ready" = 1 then incr next;
    if
      get sim "out_valid" = 1
      && Bits.to_bool !(Cyclesim.in_port sim "out_ready")
    then Buffer.add_char received (Char.chr (get sim "out_data"))
  done;
  Printf.printf "intact: %b" (String.equal sent (Buffer.contents received));
  [%expect {| intact: true |}]

let%expect_test "accepts capacity bytes when nothing reads" =
  let sim = create () in
  let accepted = ref 0 in
  set sim "in_valid" 1;
  for byte = 1 to 10 do
    set sim "in_data" byte;
    Cyclesim.cycle sim;
    if get sim "in_ready" = 1 then incr accepted
  done;
  Printf.printf "accepted %d of 10, drained:" !accepted;
  set sim "in_valid" 0;
  set sim "out_ready" 1;
  for _ = 1 to 10 do
    Cyclesim.cycle sim;
    if get sim "out_valid" = 1 then Printf.printf " %d" (get sim "out_data")
  done;
  [%expect {| accepted 4 of 10, drained: 1 2 3 4 |}]
