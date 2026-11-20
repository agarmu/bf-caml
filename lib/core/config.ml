module type S = sig
  val program_bits : int
  val program_size_bits : int
  val pointer_bits : int
  val program_capacity : int
end

module type Sizes = sig
  val max_program_size : int
  val tape_length : int
end

let entries bits =
  if bits < 1 || bits >= Sys.int_size - 1 then
    invalid_arg "bit width must be positive and fit in an int";
  1 lsl bits

let bits_for_power_of_two name size =
  if size < 2 || size land (size - 1) <> 0 then
    invalid_arg (name ^ " must be a power of two of at least 2");
  let rec log2 bits size =
    if size = 1 then bits else log2 (bits + 1) (size lsr 1)
  in
  log2 0 size

module Make (C : Sizes) = struct
  include C

  let program_bits = bits_for_power_of_two "max_program_size" max_program_size
  let pointer_bits = bits_for_power_of_two "tape_length" tape_length
  let program_size_bits = program_bits + 1
  let program_capacity = max_program_size
end
