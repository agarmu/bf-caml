open Hardcaml
open Signal
open Always
open Uart_timing

module Sampling = struct
  type t = { sample : Signal.t; complete : Signal.t }

  let create spec timer ~frame_bits rx =
    let idle = 0 in
    let start = 1 in
    let wait_for_idle = frame_bits + 1 in
    let phase =
      Variable.reg spec ~width:(Bits.num_bits_to_represent wait_for_idle)
    in
    let at index = phase.value ==:. index in
    let receiving = ~:(at idle |: at wait_for_idle) in
    let sample = receiving &: Bit_timer.elapsed timer in
    let last_bit = at frame_bits in
    compile
      [
        when_
          (at idle &: ~:rx)
          [ phase <--. start; Bit_timer.start_half_bit timer ];
        when_ (at wait_for_idle &: rx) [ phase <--. idle ];
        when_ receiving [ Bit_timer.advance timer ];
        when_ sample
          [
            if_
              (rx &: (at start |: last_bit))
              [ phase <--. idle ]
              [ phase <-- phase.value +:. 1 ];
          ];
      ];
    { sample; complete = sample &: last_bit }
end

type t = { byte : Signal.t Stream.Byte.Source.t; framing_error : Signal.t }

let create ~clocks_per_bit ~clock ~reset ~rx =
  let spec, timer = Bit_timer.create ~clock ~reset ~clocks_per_bit in
  (* Synchronize the inverted pin so both stages reset to the idle level. *)
  let rx = ~:(reg spec (reg spec ~:rx)) in
  let frame_bits = Uart_frame.width in
  let samples = Sampling.create spec timer ~frame_bits rx in
  let previous_bits =
    reg_fb spec ~enable:samples.sample ~width:(frame_bits - 1) ~f:(shift_in rx)
  in
  let decoded = Uart_frame.decode (rx @: previous_bits) in
  let complete = samples.complete &: ~:reset in
  {
    byte =
      { valid = complete &: ~:(decoded.framing_error); data = decoded.data };
    framing_error = complete &: decoded.framing_error;
  }
