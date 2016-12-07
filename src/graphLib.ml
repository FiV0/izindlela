(* --- graph library --- *)

open Cont

type edge_info = {
  dist : float;
  information : ParsedMap.info
}

(* maybe replace by hashes *)
type graph = {
  mutable id_to_int : int Cont.intmap;
  mutable int_to_id : int Cont.intmap;
  mutable size : int ;
  adj : ((int * edge_info) DynArray.dynArray) array 
}

type lalons = {
  lalo : (float * float) array
}

type landmarks = {
  landmark_coordinates: (int * float * float) array;
  distances : (float array) array
}

type lowerbound_type = 
  | Simple 
  | Euclidean of lalons
  | Landmark of landmarks

(* formula based on 
 * http://andrew.hedges.name/experiments/haversine/
 *)

let empty = {
  id_to_int = IntMap.empty;
  int_to_id = IntMap.empty;
  size = 0;
  adj = Array.make 0 (DynArray.create ());
}

let empty_lalons = Euclidean { lalo = Array.make 0 (0.,0.) }
let empty_landmarks = Landmark {
    landmark_coordinates = Array.make 0 (0,0.,0.);
    distances = Array.make 0 (Array.make 0 (0.))
  }

let distance (la1,lo1) (la2,lo2) = 
  let pi = 4.0 *. atan 1.0 in 
  let dlat = (la2 -. la1) *. pi /. 180. in
  let dlon = (lo2 -. lo1) *. pi /. 180.  in
  let a = sin(dlat /. 2.) ** 2. +. 
          sin(dlon /. 2.) ** 2. *. 
          cos(la1 *. pi /. 180. ) *. cos(la2 *. pi /. 180. ) in
  let c = 2. *. atan2 (sqrt a) (sqrt(1.-.a)) in
  6373.0 *. c

exception LatitudeLongtitudeMissing

(* all this can be further optimized and maybe even integrated into 
 * the parser*)

let insert_edge g i e =
  DynArray.add g.adj.(i) e; 
  g 

let add_new_edge g nodes id1 id2 info =
  let latlon1 = IntMap.find id1 nodes in
  let latlon2 = IntMap.find id2 nodes in
  let dis = distance latlon1 latlon2 in 
  let i1 = IntMap.find id1 g.id_to_int in 
  let i2 = IntMap.find id2 g.id_to_int in 
  let edge_info = {dist = dis; information = info } in 
  let g = if ParsedMap.oneway_check info then
      let g = insert_edge g i1 (i2,edge_info) in
      let info = ParsedMap.make_pedestrian_only info in 
      let edge_info = { edge_info with information = info } in
      insert_edge g i2 (i1,edge_info) 
  else
    let g = insert_edge g i1 (i2,edge_info) in
    insert_edge g i2 (i1,edge_info) 
  in
  g

let create_graph parsed =
  let nodes = parsed.ParsedMap.points in
  let ways, infos = List.split parsed.ParsedMap.highways in
  let n = IntMap.cardinal nodes in
  let g = { 
    id_to_int = IntMap.empty;
    int_to_id = IntMap.empty;
    size = 0;
    adj = Array.init n (fun _ -> DynArray.create ())
  }
  in
  let g = IntMap.fold (fun id lalo g -> 
      g.id_to_int <- (IntMap.add id g.size g.id_to_int);
      g.int_to_id <- (IntMap.add g.size id g.int_to_id) ;
      g.size <- g.size + 1;
      g) nodes g 
  in 
  let rec create_edges g info = function
    | id1::id2::ids -> 
      let g = add_new_edge g nodes id1 id2 info in
      create_edges g info (id2::ids)
    | _ -> g 
  in
  List.fold_left2 create_edges g infos ways

let inverse_graph g =
  let inv_g = {
    g with adj = Array.init g.size (fun _ -> DynArray.create ())
  } in
  let inv_g,_ = 
    Array.fold_left
    (fun (inv_g,n) dyArr ->
      let inv_g = DynArray.fold_left
        (fun inv_g (id,edge_info) ->
          (DynArray.add (inv_g.adj.(id)) (n,edge_info));inv_g) 
        inv_g dyArr in
        (inv_g,n+1)) (inv_g,0) g.adj in
  inv_g

let default_speed_foot () = 5
let default_speed_bike () = 20
let default_speed_car () = 50

let time_of_dis edge_info transport = 
  let maxspeed = match transport with
    | ParsedMap.Foot -> default_speed_foot ()
    | ParsedMap.Bike -> default_speed_bike ()
    | ParsedMap.Car -> ParsedMap.get_max_speed edge_info.information 
  in
  edge_info.dist /. (float_of_int maxspeed)

let create_dijkstra = Simple

let create_lalons parsed g = 
 let nodes = parsed.ParsedMap.points in
 let lalo = Array.make g.size 0 in
 let lalons =  { 
   lalo = Array.mapi
    (fun i _ -> let id = IntMap.find i g.int_to_id in
    IntMap.find id nodes) lalo } in
 Euclidean(lalons)

let choose_from_distance dis (transport, priority) =
  let maxspeed = match transport with 
    | ParsedMap.Foot -> default_speed_foot ()
    | ParsedMap.Bike -> default_speed_bike ()
    | ParsedMap.Car -> default_speed_car ()
  in
  let time = dis /. (float_of_int maxspeed) in
  match priority with
  | ParsedMap.Short -> dis 
  | ParsedMap.Fast -> time
  | ParsedMap.Suitable -> assert false

let lowerbound_lalo mode lalons i1 i2 = 
  let dis = distance lalons.lalo.(i1) lalons.lalo.(i2) in
  choose_from_distance dis mode
                         
let lowerbound_landmarks mode landmarks i1 i2 = 
  let dis = Array.fold_left
      (fun max arr -> 
         let max = Pervasives.max max (abs_float (arr.(i1) -. arr.(i2))) in
         max ) 0.0 landmarks.distances
  in
  choose_from_distance dis mode

let lowerbound mode lowerbound_type i1 i2 =
  match lowerbound_type with
  | Simple -> 0.0
  | Euclidean lalons -> lowerbound_lalo mode lalons i1 i2 
  | Landmark landmarks -> lowerbound_landmarks mode landmarks i1 i2

let connected g id1 id2 =
  let i1 = IntMap.find id1 g.id_to_int in
  let i2 = IntMap.find id2 g.id_to_int in
  let b = DynArray.fold_left (fun b (i,_) -> 
      if i=i2 then true else b) false g.adj.(i1) 
  in
  b

let nbLandmarks = 4

let create_landmarks parsed g =
  let landmarks = {
    landmark_coordinates = Array.make nbLandmarks (0,0.0,0.0);
    distances = Array.make_matrix nbLandmarks g.size 0.0;
  } in
  let nodes = parsed.ParsedMap.points in
  let maxlat, minlat, maxlon, minlon = parsed.ParsedMap.bounds in
  let bounds = [maxlat, minlon; 
                maxlat, maxlon; 
                minlat, maxlon; 
                minlat, minlon] 
  in
  let bounds = List.map (fun latlon -> 
      let id = ParsedMap.nearest parsed latlon in
      let lat, lon = IntMap.find id nodes in
      id, lat, lon) bounds
  in 
  let landmarks = { landmarks with 
                    landmark_coordinates = Array.of_list bounds} 
  in
  let ids = Array.of_list (List.map (fun (id,_,_) -> id) bounds) in    
  let landmarks = {
    landmarks with distances =
    Array.mapi
      (fun i arr -> let lalo1 = IntMap.find ids.(i) nodes in
        Array.mapi
          (fun j _ ->
            let id = IntMap.find j g.int_to_id in
            let lalo2 = IntMap.find id nodes in
            distance lalo1 lalo2) arr ) landmarks.distances } 
  in
  Landmark landmarks

let lookup_ids g ids = 
  let rec lookup_ids_aux acc = function 
    | [] -> acc
    | n::r -> 
      let id = IntMap.find n g.int_to_id in
      lookup_ids_aux (id::acc) r 
  in
  lookup_ids_aux [] ids 

(*let rec search_next_aux prev adj current visited =
  visited.(current) <- true;
  match (DynArray.size adj.(current)) with 
  | n when n > 2 -> Some current
  | 2 ->
    let pos1 = (fst (DynArray.get adj.(current) 0)) in 
    let pos2 = (fst (DynArray.get adj.(current) 1)) in 
    if pos1 = prev then
      if visited.(pos2) then None
      else search_next_aux current adj pos2 visited
    else 
      if visited.(pos1) then None
      else search_next_aux current adj pos1 visited 
  | 1 ->
    let pos1 = (fst (DynArray.get adj.(current) 0)) in 
    if pos1 = prev then
      None 
    else  
      if visited.(pos1) then None
      else search_next_aux current adj pos1 visited
  | _ -> None

let search_next prev adj current visited =
  Array.fill visited 0 (Array.length visited) false;
  search_next_aux prev adj current visited

exception ExpectsLatitudeLongituds

let create_highlayer g lalons =
  let lalons = match lalons with 
   | Euclidean lalons -> lalons.lalo
   | _ -> raise ExpectsLatitudeLongituds in
  let g_high = { 
    id_to_int = IntMap.empty;
    int_to_id = IntMap.empty;
    size = 0;
    adj = Array.init 0 (fun _ -> DynArray.create ())
  } in
  let g_high,_ = Array.fold_left 
    (fun (g_high,n) arr -> 
      if DynArray.size arr > 2 then 
        let id = IntMap.find n g.int_to_id in
        g_high.int_to_id <- IntMap.add g_high.size id g_high.int_to_id;     
        g_high.id_to_int <- IntMap.add id g_high.size g_high.id_to_int; 
        g_high.size <- g_high.size + 1;
        (g_high,n+1)
        else (g_high,n+1)) (g_high,0) g.adj in
  let g_high = 
    { g_high with adj = Array.init g_high.size (fun _ -> DynArray.create()) } in
  let visited = Array.make g.size false in
  let g_high = 
    IntMap.fold
    (fun id1 new_index1 g_high ->
      try
       let old_index1 = IntMap.find id1 g.id_to_int in
       DynArray.fold_left 
        (fun g_high (old_index_next,edge) -> 
          let old_index2 = search_next old_index1 g.adj old_index_next visited in
          begin match old_index2 with 
          | None -> g_high
          | Some old_index2 ->
              let id2 = IntMap.find old_index2 g.int_to_id in
              let new_index2 = IntMap.find id2 g_high.id_to_int in
              let new_edge = (new_index2, 
              { edge with dist = 
                (distance lalons.(old_index1) lalons.(old_index2))
              }) in
            DynArray.add g_high.adj.(new_index1) new_edge;
            g_high end) 
        g_high g.adj.(old_index1)
        with Not_found -> failwith "Error in higherlayer creation")
    g_high.id_to_int g_high in
  g_high 

  
let dfs_connected g id1 id2 =
let seen = Array.make g.size 0 in
let i1 = Maps.find_strmap id1 g.id_to_int in
let i2 = Maps.find_strmap id2 g.id_to_int in
let rec dfs_aux g cur i2 seen =
if seen.(cur) = 1 then (0,seen)
else if cur = i2 then (1,seen)
else begin
seen.(cur) <- 1;
let check (x,seen) (i,_) =
let (x',seen) = dfs_aux g i i2 seen in
(x lor x',seen) in
DynArray.fold_left check (0,seen) g.adj.(cur)
end in
let (res,_) = dfs_aux g i1 i2 seen in
res*)
