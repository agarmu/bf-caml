open Hardcaml
open Signal

module type Lanes = sig
  val lanes : int
end

module Lanes (L : Lanes) = struct
  let lane_bits = Config.bits_for_power_of_two "lanes" L.lanes

  let split address =
    if width address <= lane_bits then
      invalid_arg "lanes must be fewer than memory entries";
    (sel_bottom address lane_bits, drop_bottom address lane_bits)

  let combine ~lane outputs =
    let get f = List.map f outputs in
    {
      Memory.data = get Memory.data |> mux lane;
      ready = get Memory.ready |> reduce ~f:( &: );
      clearing = get Memory.clearing |> reduce ~f:( |: );
    }
end

module Make (Lane : Memory.S) (L : Lanes) : Memory.S = struct
  include Lanes (L)

  let create ~address_bits ~clear ~clock ~reset ~address ~write_enable
      ~write_data =
    let lane, row = split address in
    List.init L.lanes (fun index ->
        Lane.create ~address_bits:(address_bits - lane_bits) ~clear ~clock
          ~reset ~address:row
          ~write_enable:(write_enable &: (lane ==:. index))
          ~write_data)
    |> combine ~lane
end

module Make_read_only (Lane : Memory.Read_only) (L : Lanes) : Memory.Read_only =
struct
  include Lanes (L)

  let create ~clock ~reset ~data_bits ~contents ~address =
    let lane, row = split address in
    let lane_contents index =
      List.filteri (fun i _ -> i land (L.lanes - 1) = index) contents
    in
    List.init L.lanes (fun index ->
        Lane.create ~clock ~reset ~data_bits ~contents:(lane_contents index)
          ~address:row)
    |> combine ~lane
end
