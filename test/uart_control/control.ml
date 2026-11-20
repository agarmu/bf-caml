open Bf_caml
open Serial_sim
open Host

module Config = Config.Make (struct
  let max_program_size = 16
  let tape_length = 8
end)

module Control = Uart_control.Make (Config) (Ram_memory)
module Rom_control = Uart_control.Make_with_rom (Config) (Register_memory) (Rom)

let%expect_test "RAM upload, exact stepping, paused tape reads, and runtime I/O"
    =
  let host =
    connect (fun rx tx ->
        let control = Control.create ~clock ~reset ~rx_capacity:4 ~rx ~tx in
        (control.tx, control.rx_overrun))
  in
  let request kind id offset byte = host.send (packet kind id offset byte) in
  let ack id offset = host.expect (packet 16 id offset 0) in
  host.send [ 0x82; 0x00 ];
  request 15 30 0 0;
  host.expect (packet 17 30 0 1);
  request 9 31 0 4;
  host.expect (packet 17 31 0 7);
  request 5 32 0 1;
  host.expect (packet 17 32 0 8);
  host.send [ 0x85; 33; 0; 0; 0; 4 ];
  host.expect (packet 17 33 0 8);
  request 2 34 0 0;
  host.expect (packet 17 34 0 10);
  request 8 35 0 0;
  host.expect (packet 17 35 0 14);
  request 1 1 0 0;
  ack 1 0;
  request 2 2 1 (Char.code '+');
  host.expect (packet 17 2 0 3);
  let program = "+>++>,." in
  String.iteri
    (fun offset byte ->
      request 2 (3 + offset) offset (Char.code byte);
      ack (3 + offset) (offset + 1))
    program;
  let size = String.length program in
  request 3 10 size 0;
  ack 10 size;
  request 1 11 0 0;
  host.expect (packet 17 11 size 9);
  request 7 12 0 0;
  host.expect (packet 16 12 size 0 @ packet 21 0 size 0);
  request 9 26 0 2;
  host.expect (packet 22 26 1 0);
  request 9 13 0 0;
  host.expect (packet 22 13 1 4);
  request 9 14 0 1;
  host.expect (packet 22 14 0 1);
  request 7 15 4 0;
  host.expect (packet 16 15 size 0 @ packet 21 0 size 0);
  request 10 16 1 0;
  host.expect (packet 23 16 1 2);
  request 10 17 8 0;
  host.expect (packet 17 17 size 6);
  request 10 37 16384 0;
  host.expect (packet 17 37 size 6);
  request 7 18 1 0;
  host.expect (packet 16 18 size 0 @ packet 20 0 size 0);
  request 6 19 0 0;
  ack 19 size;
  request 4 20 0 0x81;
  host.expect (packet 17 20 size 12);
  request 8 21 0 0;
  host.expect (packet 16 21 size 0 @ packet 20 0 size 0);
  request 4 22 0 0x81;
  host.expect (packet 16 22 size 0 @ packet 18 0 size 0x81 @ packet 19 0 size 0);
  request 7 36 1 0;
  host.expect (packet 17 36 size 15);
  request 9 27 0 2;
  host.expect (packet 22 27 7 0);
  request 9 28 0 3;
  host.expect (packet 22 28 0 0);
  request 5 23 0 0;
  ack 23 0;
  request 9 29 0 2;
  host.expect (packet 22 29 0 0);
  request 10 24 1 0;
  host.expect (packet 23 24 1 0);
  request 1 25 0 0;
  ack 25 0

let%expect_test
    "ROM rejects uploads, halts before a step budget, and reports full input" =
  let host =
    connect (fun rx tx ->
        let program = [ Instruction.Input; Output; Loop_start; Loop_end ] in
        let control =
          Rom_control.create ~program ~clock ~reset ~rx_capacity:4 ~rx ~tx
        in
        (control.tx, control.rx_overrun))
  in
  let request kind id offset byte = host.send (packet kind id offset byte) in
  let ack id = host.expect (packet 16 id 0 0) in
  request 1 1 0 0;
  host.expect (packet 17 1 0 4);
  request 7 2 100 0;
  host.expect (packet 16 2 0 0 @ packet 20 0 0 0);
  request 4 3 0 0;
  host.expect (packet 16 3 0 0 @ packet 18 0 0 0 @ packet 19 0 0 0);
  request 5 4 0 0;
  ack 4;
  request 8 5 0 0;
  host.expect (packet 16 5 0 0 @ packet 20 0 0 0);
  request 4 6 0 1;
  host.expect (packet 16 6 0 0 @ packet 18 0 0 1);
  for id = 7 to 10 do
    request 4 id 0 0;
    ack id
  done;
  request 4 11 0 0;
  host.expect (packet 17 11 0 5);
  request 10 12 0 0;
  host.expect (packet 17 12 0 13);
  request 5 13 0 0;
  ack 13;
  request 10 14 0 0;
  host.expect (packet 23 14 0 0)

let%expect_test
    "halt reports invalid brackets separately from an oversized program" =
  List.iter
    (fun (program, reason) ->
      let host =
        connect (fun rx tx ->
            let control =
              Rom_control.create ~program ~clock ~reset ~rx_capacity:4 ~rx ~tx
            in
            (control.tx, control.rx_overrun))
      in
      host.expect (packet 19 0 0 reason))
    [
      ([ Instruction.Loop_start ], 1);
      (List.init 17 (fun _ -> Instruction.Increment), 2);
    ]
