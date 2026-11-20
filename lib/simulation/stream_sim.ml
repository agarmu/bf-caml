open Hardcaml

module type Stream = sig
  module Source : sig
    type 'a t = { valid : 'a; data : 'a }
  end

  module Sink : sig
    type 'a t = { ready : 'a }
  end
end

module Make (S : Stream) = struct
  let offer (source : Bits.t ref S.Source.t) data =
    source.valid := Bits.vdd;
    source.data := data

  let withdraw (source : Bits.t ref S.Source.t) = source.valid := Bits.gnd
  let offered (source : Bits.t ref S.Source.t) = Bits.to_bool !(source.valid)

  let transferred source (sink : Bits.t ref S.Sink.t) =
    offered source && Bits.to_bool !(sink.ready)

  let send ~cycle source sink data =
    offer source data;
    let rec wait () =
      cycle ();
      if not (transferred source sink) then wait ()
    in
    wait ();
    withdraw source
end

module Byte = Make (Stream.Byte)
