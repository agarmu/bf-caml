open Hardcaml

let shift_in incoming value = Signal.(incoming @: drop_bottom value 1)

module Bit_timer = struct
  open Signal
  open Always

  type t = { counter : Variable.t; clocks_per_bit : int }

  let create ~clock ~reset ~clocks_per_bit =
    if clocks_per_bit < 4 then
      invalid_arg "UART requires at least four clocks per bit";
    let width = Bits.num_bits_to_represent (clocks_per_bit - 1) in
    let spec = Reg_spec.create ~clock ~clear:reset () in
    (spec, { counter = Variable.reg spec ~width; clocks_per_bit })

  let elapsed t = t.counter.value ==:. 0
  let restart t = t.counter <--. t.clocks_per_bit - 1

  let advance t =
    if_ (elapsed t) [ restart t ] [ t.counter <-- t.counter.value -:. 1 ]

  let start_half_bit t = t.counter <--. (t.clocks_per_bit / 2) - 1
end
