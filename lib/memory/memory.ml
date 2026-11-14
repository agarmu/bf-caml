open Hardcaml

module type S = sig
  val address_bits : int

  val create :
    clock:Signal.t ->
    reset:Signal.t ->
    address:Signal.t ->
    write_enable:Signal.t ->
    write_data:Signal.t ->
    Signal.t
end
