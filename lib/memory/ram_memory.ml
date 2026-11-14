open Hardcaml
open Signal

module Make (C : Config.S) : Memory.S = struct
  let address_bits = C.pointer_bits
  let capacity = Config.entries address_bits

  let clear spec =
    let cleared_cells =
      reg_fb spec ~width:(address_bits + 1) ~f:(fun count ->
          mux2 (count ==:. capacity) count (count +:. 1))
    in
    let initialized = cleared_cells ==:. capacity in
    let address = uresize cleared_cells address_bits in
    (initialized, address)

  let read_ready spec ~initialized ~address ~write_enable ~reset =
    let read_address = reg spec address in
    let read_valid = reg spec (initialized &: ~:write_enable) in
    read_valid &: (address ==: read_address) &: ~:reset

  let create ~clock ~reset ~address ~write_enable ~write_data =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let initialized, clear_address = clear spec in
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
    let ready = read_ready spec ~initialized ~address ~write_enable ~reset in
    { Memory.data; ready }
end
