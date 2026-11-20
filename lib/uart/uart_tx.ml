open Hardcaml
open Signal
open Always
open Uart_timing

let create ~clocks_per_bit ~clock ~reset (source : _ Stream.Byte.Source.t) =
  let spec, timer = Bit_timer.create ~clock ~reset ~clocks_per_bit in
  let frame_bits = Uart_frame.width in
  let remaining =
    Variable.reg spec ~width:(Bits.num_bits_to_represent frame_bits)
  in
  let frame = Variable.reg spec ~width:frame_bits in
  let sending = remaining.value <>:. 0 in
  let sink = { Stream.Byte.Sink.ready = ~:sending &: ~:reset } in
  compile
    [
      when_
        (Stream.Byte.transfer source sink)
        [
          frame <-- Uart_frame.encode source.data;
          remaining <--. frame_bits;
          Bit_timer.restart timer;
        ];
      when_ sending [ Bit_timer.advance timer ];
      when_
        (sending &: Bit_timer.elapsed timer)
        [
          frame <-- shift_in vdd frame.value;
          remaining <-- remaining.value -:. 1;
        ];
    ];
  (sink, reset |: ~:sending |: bit frame.value 0)
