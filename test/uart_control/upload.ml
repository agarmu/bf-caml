open Bf_caml
open Host

module Config = Config.Make (struct
  let max_program_size = 8
  let tape_length = 8
end)

module Control = Uart_control.Make (Config) (Register_memory)

let connect () =
  Host.connect (fun rx tx ->
      let control =
        Control.create ~clock:Serial_sim.clock ~reset:Serial_sim.reset
          ~rx_capacity:4 ~rx ~tx
      in
      (control.tx, control.rx_overrun))

let upload host source =
  host.send (packet 1 1 0 0);
  host.expect (packet 16 1 0 0);
  String.iteri
    (fun offset byte ->
      let id = offset + 2 in
      host.send (packet 2 id offset (Char.code byte));
      host.expect (packet 16 id (offset + 1) 0))
    source;
  let offset = String.length source in
  let id = offset + 2 in
  host.send (packet 3 id offset 0);
  host.expect (packet 16 id offset 0)

let%expect_test
    "upload offsets include comments and malformed reset preserves the loaded \
     program" =
  let host = connect () in
  upload host "+text\004.";
  host.send (packet 5 10 0 1);
  host.expect (packet 17 10 7 8);
  host.send (packet 8 11 0 0);
  host.expect (packet 16 11 7 0 @ packet 18 0 7 1 @ packet 19 0 7 0);
  host.send (packet 5 12 0 0);
  host.expect (packet 16 12 0 0);
  upload host "";
  host.expect (packet 19 0 0 0)

let%expect_test
    "upload offset acceptance is distinct from program storage capacity" =
  let host = connect () in
  upload host "+++++++++";
  host.expect (packet 19 0 9 2);
  host.send (packet 8 11 0 0);
  host.expect (packet 17 11 9 15)
