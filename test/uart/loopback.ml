open Hardcaml
open Bf_caml
open Serial_sim

let check_loopback clocks_per_bit =
  let sink, tx = transmitter ~clocks_per_bit in
  let rx = Uart_rx.create ~clocks_per_bit ~clock ~reset ~rx:tx in
  let sim =
    simulate
      [
        output "ready" sink.ready;
        output "received" rx.byte.valid;
        output "data_out" rx.byte.data;
        output "framing_error" rx.framing_error;
      ]
  in
  let received = ref [] in
  let cycle () =
    Cyclesim.cycle sim;
    assert (get sim "framing_error" = 0);
    if get sim "received" = 1 then received := get sim "data_out" :: !received
  in
  set sim "reset" 1;
  cycle ();
  set sim "reset" 0;
  let bytes = List.init 256 Fun.id in
  List.iter
    (fun byte ->
      offer sim ~cycle byte;
      for _ = 1 to Uart_frame.width * clocks_per_bit do
        cycle ()
      done)
    bytes;
  for _ = 1 to clocks_per_bit do
    cycle ()
  done;
  assert (List.rev !received = bytes)

let%expect_test "all bytes survive transmit and receive loopback" =
  List.iter check_loopback [ 4; 9; 10 ]
