open Hardcaml

type output = { data : Signal.t; ready : Signal.t }
type read = address:Signal.t -> output

module type S = sig
  val create :
    address_bits:int ->
    clear:Signal.t ->
    clock:Signal.t ->
    reset:Signal.t ->
    address:Signal.t ->
    write_enable:Signal.t ->
    write_data:Signal.t ->
    output
end
