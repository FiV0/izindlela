open Cont

type algo = Dijkstra | A_Star_E | A_Star_L

let data = ref Data.({
    map = ParsedMap.empty;
    graph = GraphLib.empty;
    highlayer_map = ParsedMap.empty; 
    highlayer_graph = GraphLib.empty;
    reverse_graph = GraphLib.empty;
    lalons = GraphLib.empty_lalons;
    landmarks = GraphLib.empty_landmarks;
    highlayer_lalons = GraphLib.empty_lalons;
    highlayer_landmarks = GraphLib.empty_landmarks
  })

let dijkstra_typ = GraphLib.Simple

let lat_from = ref 0.0
let lon_from = ref 0.0
let lat_to = ref 0.0
let lon_to = ref 0.0

let transport = ref ParsedMap.Car
let priority = ref ParsedMap.Fast

let algo = ref Dijkstra

let use_highlayer = ref false

let load_map = function
  | None -> ()
  | Some s ->
    data := IO.parse s 

let get_point = GetHttp.get_osm_data

let calculate () = 
  let data = !data in
  if not (ParsedMap.in_bounds data.Data.map (!lat_from, !lon_from)) ||
     not (ParsedMap.in_bounds data.Data.map (!lat_to, !lon_to)) 
  then None 
  else
    begin
      let id1 = ParsedMap.nearest data.Data.map (!lat_from, !lon_from) in
      let id2 = ParsedMap.nearest data.Data.map (!lat_to, !lon_to) in
      let mode = !transport, !priority in
      let t = Sys.time () in
      let algo_typ = match !algo with
        | Dijkstra -> dijkstra_typ
        | A_Star_E -> 
          if !use_highlayer 
          then data.Data.highlayer_lalons 
          else data.Data.lalons
        | A_Star_L -> 
          if !use_highlayer 
          then data.Data.highlayer_landmarks 
          else data.Data.landmarks
      in
      let algo = if !use_highlayer 
        then  Highlayer.highlayer_algo data.Data.graph data.Data.reverse_graph 
            data.Data.highlayer_graph 
        else Dijkstra2.dijkstra data.Data.graph 
      in
      let time, dis, path, visited = algo algo_typ id1 id2 mode in
      Some (Sys.time() -. t, time, dis, path, visited)
    end
  
let get_bounds () = !data.Data.map.ParsedMap.bounds

let find_point id = IntMap.find id !data.Data.map.ParsedMap.points

let update_endpoint (lat, lon) = function
  | `FROM -> 
    lat_from := lat;
    lon_from := lon
  | `TO ->  
    lat_to := lat;
    lon_to := lon 

let update_transport = (:=) transport
let update_priority = (:=) priority
let update_algo = (:=) algo
let update_use_highlayer = (:=) use_highlayer
