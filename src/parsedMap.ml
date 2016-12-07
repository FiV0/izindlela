open Cont

type info = {
  transport_mode: int;
  max_speed: int;
  bike_suitable: int;
}

type t =
{
  points: (float * float) Cont.intmap;
  waypoints : Cont.intset;
  highways: (int list * info ) list;
  bounds : float * float * float * float;
}

let empty = 
{
  points = IntMap.empty;
  waypoints = IntSet.empty;
  highways = [];
  (* France *)
  bounds = 51.234, 41.256, 9.844, -5.823;
}

type transport = Foot | Bike | Car
type priority = Fast | Short | Suitable

type search = (transport * priority)

let create_info () = { transport_mode = 0; bike_suitable=0; max_speed=0}

let oneway info =
  { info with transport_mode = (info.transport_mode lor 1)}

let oneway_check info = if (info.transport_mode land 1) != 0 then true else false

let pedestrian_usable info = 
  { info with transport_mode = (info.transport_mode lor 2) }

let pedestrian_check info = if (info.transport_mode land 2) != 0 then true 
  else false

let make_pedestrian_only info = 
  { info with 
    transport_mode = (pedestrian_usable (create_info ())).transport_mode }

let bike_usable info = 
  { info with transport_mode = (info.transport_mode lor 4)}

let bike_check info = if (info.transport_mode land 4) != 0 then true else false
  
let car_usable info = 
  { info with transport_mode = (info.transport_mode lor 8)}

let car_check info = if (info.transport_mode land 8) != 0 then true else false

let set_bike_suitability info bs = { info with bike_suitable = bs}

let get_bike_suitability info = info.bike_suitable

let check info = function
  | Foot -> pedestrian_check info
  | Bike -> bike_check info
  | Car -> car_check info

let set_max_speed info max = {info with max_speed = max}

let get_max_speed info = info.max_speed

let in_bounds map (lat, lon) = 
  let maxlat, minlat, maxlon, minlon = map.bounds in
  (minlat <= lat) && (lat <= maxlat) && (minlon <= lon) && (lon <= maxlon)

let min (i1, v1) (i2, v2) = 
  let m = min v1 v2 in
  if m = v1 then i1, v1 else i2, v2

let nearest map (lat, lon) = 
  let nst_id, _ = IntSet.fold (fun id nst -> 
      let lat1, lon1 = IntMap.find id map.points in
      let dlat = abs_float (lat -. lat1) in
      let dlon = abs_float (lon -. lon1) in
      min (id, dlat +. dlon) nst) map.waypoints (0, infinity)
  in
  nst_id   

let exists_in_rest ele rest =
  List.fold_left (fun b (ls,_) -> if b then b else (List.mem ele ls)) false rest


let remove_from_current (ls,info) rest =
  let rec remove_from_current_aux acc = function 
    | [] -> acc
    | a::ending -> 
        (if (exists_in_rest a rest) 
        then remove_from_current_aux (a::acc) ending
        else remove_from_current_aux acc ending) in
  let newls = (List.rev (remove_from_current_aux [] ls)) in
  (newls,info)

let create_highlayer_highways highways = 
  let rec create_highlayer_highways_aux acc front highways =
    match highways with
    | [] -> acc
    | current::rest -> 
        let newcurrent = remove_from_current current (front @ rest) in
        create_highlayer_highways_aux (newcurrent::acc) (current::front) rest in
  create_highlayer_highways_aux [] [] highways

let flatten_highways highways = 
  List.fold_left (fun acc (ids,_) -> (ids @ acc)) [] highways

let highlayer_parsed parsed = 
  let highways = parsed.highways in
  let new_highways = create_highlayer_highways highways in
  let new_highways_flattened = flatten_highways new_highways in
  let new_waypoints=
    List.fold_left (fun s a -> IntSet.add a s) IntSet.empty
    new_highways_flattened in
  let new_points =
    IntMap.fold (fun k im map -> 
      if (IntSet.mem k new_waypoints)
      then IntMap.add k im map 
      else map) parsed.points IntMap.empty in
  { parsed with highways = new_highways;
                waypoints = new_waypoints; 
                points = new_points} 
