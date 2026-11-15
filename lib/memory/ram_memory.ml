open Hardcaml
open Signal

let sweep ~clock ~reset ~clear ~address_bits =
  let width = address_bits + 1 in
  let capacity = of_int ~width (Config.entries address_bits) in
  let spec =
    Reg_spec.override (Reg_spec.create ~clock ~clear ()) ~clear_to:capacity
  in
  let remaining = wire width in
  remaining
  <== reg spec
        ~enable:(reset |: (remaining <>:. 0))
        (mux2 reset (zero width) (remaining -:. 1));
  (remaining ==:. 0, uresize (remaining -:. 1) address_bits)

let read_ready spec ~initialized ~address ~write_enable ~reset ~clear =
  let read_address = reg spec address in
  let read_valid = reg spec (initialized &: ~:write_enable) in
  read_valid &: initialized &: (address ==: read_address) &: ~:(reset |: clear)

let create ~address_bits ~clear ~clock ~reset ~address ~write_enable ~write_data
    =
  let capacity = Config.entries address_bits in
  let spec = Reg_spec.create ~clock ~clear:reset () in
  let initialized, clear_address = sweep ~clock ~reset ~clear ~address_bits in
  let data =
    ram_rbw capacity
      ~write_port:
        {
          Write_port.write_clock = clock;
          write_enable = ~:reset &: (~:initialized |: write_enable);
          write_address = mux2 initialized address clear_address;
          write_data = mux2 initialized write_data (zero 8);
        }
      ~read_port:
        {
          Read_port.read_clock = clock;
          read_enable = vdd;
          read_address = address;
        }
  in
  let ready =
    read_ready spec ~initialized ~address ~write_enable ~reset ~clear
  in
  { Memory.data; ready }
