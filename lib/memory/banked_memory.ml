open Hardcaml
open Signal

module type Lanes = sig
  val lanes : int
end

module Make (Lane : Memory.S) (L : Lanes) : Memory.S = struct
  let lane_bits = Config.bits_for_power_of_two "lanes" L.lanes

  let row_bits address_bits =
    if address_bits <= lane_bits then
      invalid_arg "lanes must be fewer than memory entries";
    address_bits - lane_bits

  let create ~address_bits ~clear ~clock ~reset ~address ~write_enable
      ~write_data =
    let lane = sel_bottom address lane_bits in
    let addr_within_lane = drop_bottom address lane_bits in
    let lanes =
      List.init L.lanes (fun index ->
          Lane.create ~address_bits:(row_bits address_bits) ~clear ~clock ~reset
            ~address:addr_within_lane
            ~write_enable:(write_enable &: (lane ==:. index))
            ~write_data)
    in
    let get f = lanes |> List.map f in
    {
      Memory.data = get Memory.data |> mux lane;
      ready = get Memory.ready |> reduce ~f:( &: );
      clearing = get Memory.clearing |> reduce ~f:( |: );
    }
end
