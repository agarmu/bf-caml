open Hardcaml
open Signal

let data_bits = Stream.Byte.Source.port_widths.data
let width = 1 + data_bits + 1

type decoded = { data : Signal.t; framing_error : Signal.t }

let encode byte = concat_msb [ vdd; byte; gnd ]

let decode frame =
  {
    data = select frame data_bits 1;
    framing_error = bit frame 0 |: ~:(bit frame (width - 1));
  }
