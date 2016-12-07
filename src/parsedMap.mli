type info 

type t =
{
  points: (float * float) Cont.intmap;
  waypoints : Cont.intset;
  highways: (int list * info) list;
  bounds : float * float * float * float;
}

type transport = Foot | Bike | Car
type priority = Fast | Short | Suitable

type search = (transport * priority)

val empty: t

val create_info : unit -> info

val oneway : info -> info

val oneway_check : info -> bool

val pedestrian_usable : info -> info

val make_pedestrian_only : info -> info

val bike_usable : info -> info

val car_usable: info -> info

val check: info -> transport -> bool

val set_bike_suitability: info -> int -> info

val get_bike_suitability: info -> int

val set_max_speed: info -> int -> info

val get_max_speed: info -> int

val nearest: t -> float * float -> int

val in_bounds: t -> float * float -> bool

val highlayer_parsed: t -> t 
