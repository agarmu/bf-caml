open Hardcaml

type output = { data : Signal.t; ready : Signal.t; clearing : Signal.t }
[@@deriving fields ~getters]

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

module type Read_only = sig
  val create :
    clock:Signal.t ->
    reset:Signal.t ->
    data_bits:int ->
    contents:Signal.t list ->
    address:Signal.t ->
    output
end
