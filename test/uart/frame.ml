open Hardcaml
open Bf_caml
open Serial_sim

let%expect_test "8N1 encoding and framing checks" =
  let encoded = Uart_frame.encode (Signal.input "byte" 8) in
  let decoded = Uart_frame.decode (Signal.input "frame" Uart_frame.width) in
  let sim =
    simulate
      [
        output "encoded" encoded;
        output "data" decoded.data;
        output "framing_error" decoded.framing_error;
      ]
  in
  set sim "byte" 0xa5;
  let check frame ~framing_error =
    set sim "frame" frame;
    Cyclesim.cycle sim;
    assert (get sim "encoded" = 0x34a);
    assert (get sim "data" = 0xa5);
    assert (get sim "framing_error" = framing_error)
  in
  check 0x34a ~framing_error:0;
  check 0x34b ~framing_error:1;
  check 0x14a ~framing_error:1
