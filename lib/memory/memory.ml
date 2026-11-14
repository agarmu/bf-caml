open Hardcaml

type output = { data : Signal.t; ready : Signal.t }

module type S = sig
  val address_bits : int

  val create :
    clock:Signal.t ->
    reset:Signal.t ->
    address:Signal.t ->
    write_enable:Signal.t ->
    write_data:Signal.t ->
    output
end
