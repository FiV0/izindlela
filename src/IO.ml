(* --- xml_treatment --- *)
open Cont
 
let read_map f =  
  try 
    Xml.parse_file f
  with
  | Xml.Error e -> print_endline (Xml.error e); raise Exit
  | Xml.File_not_found f -> Printf.printf "File '%s' not found.\n" f; 
    raise Exit
 
let right_attributes = function
  | ("id" | "visible" | "lat" | "lon"),_ -> true
  | _ -> false

let clean_attributes = function
  | Xml.Element (_, attrs, _) -> 
    let id = int_of_string (List.assoc "id" attrs) in
    let lon = float_of_string (List.assoc "lon" attrs) in
    let lat = float_of_string (List.assoc "lat" attrs) in
    id, (lat,lon)
  | _ -> assert false

let clean_attributes_2 = function
  | Xml.Element ("nd", attrs, _) -> 
    let id = int_of_string (List.assoc "ref" attrs) in
    id
  | _ -> assert false

let rec search_tag = function
  | Xml.Element ("tag", attrs, _) :: xxs -> 
      if List.mem ("k","highway") attrs then true
      else search_tag xxs
  | _ :: xxs -> search_tag xxs 
  | [] -> false

let get_tags cs = 
  List.filter (function Xml.Element("tag",_,_) -> true | _-> false ) cs

let rec find_value = function
  | ("v",value)::res -> value
  | _::res -> find_value res
  | _ -> raise Not_found

let find_value_of_key_in_tags tags key = 
  try 
    begin
      let xml_elt = 
        List.find (function 
            | Xml.Element(_,attri,_) -> List.mem ("k",key) attri
            | _ -> assert false
          ) tags 
      in
      match xml_elt with
      | Xml.Element(_,attri,_) -> find_value attri 
      | _ -> assert false
    end
  with Not_found -> raise Not_found

let oneway_check tags = 
  try
   let direction = find_value_of_key_in_tags tags "oneway" in
   match direction with
    | "yes" -> (true,false)
    | "-1" -> (true,true)
    | _ -> (false,false)
  with Not_found -> (false,false)

let highway_typ tags =
  find_value_of_key_in_tags tags "highway"

let cycleway_typ tags =
  find_value_of_key_in_tags tags "cycleway"

let maxspeed tags =
  try
    find_value_of_key_in_tags tags "maxspeed"
  with
    Not_found -> "50" 

let car_tags = 
  "motorway"::"trunk"::"primary"::"secondary"::"tertiary"::"unclassified"::
  "residential"::"service"::"track"::"motorway_link"::"trunk_link"::
    "primary_link"::"secondary_link"::"tertiary_link"::[]

(* TODO This should be changed to a map 
 *  motorway -> 130 
 *  trunk -> 130 etc
 *)
let get_default_speed () = 50

let create_information cs = 
  let info = ParsedMap.create_info () in
  let tags = get_tags cs in
  (* check for oneway and correct oneway*)
  let (onew,wrong_order) = oneway_check tags in
  let info = if onew then (ParsedMap.oneway info) else info in
  let typ = highway_typ tags in 
  (* check if bikes or pedestrians are allowed *)
  let info = if typ != "motorway" && typ != "trunk" then
    let info = ParsedMap.pedestrian_usable info in
    ParsedMap.bike_usable info 
  else 
    info 
  in
  (* check if cars are allowed *)
  let info = if (List.mem typ car_tags) then (ParsedMap.car_usable info) else info in
  let contains_cycleway =  
    try 
      let _ = cycleway_typ tags in
      true
    with Not_found -> false
  in 
  (* TODO 
   * THis could be more fine`
   *
   * only rudimentary distinction*)
  let info = if typ = "cycleway" || contains_cycleway then
    ParsedMap.set_bike_suitability info 10 
  else 
    ParsedMap.set_bike_suitability info (-10)
  in
  let info = 
    try
      let ma_sp = int_of_string (maxspeed tags) in
      ParsedMap.set_max_speed info ma_sp
    with
      Not_found | Failure "int_of_string" -> 
      ParsedMap.set_max_speed info (get_default_speed ())
  in
  (wrong_order,info)


(** Select nodes and highways form specific *)
let rec select_nodes xmls = 
  let rec select_aux xs acc_nodes acc_waypoints acc_highways = 
    match xs with  
    | Xml.Element ("node", _, _) as n :: xs -> 
      let id, coord = clean_attributes n in
      select_aux xs (IntMap.add id coord acc_nodes) acc_waypoints acc_highways
    | Xml.Element ("way", _, cs) :: xs -> 
      if search_tag cs 
      then 
        let wrong_order,info = create_information cs in
        let cs = List.filter (function 
            | Xml.Element ("nd", _, _) -> true | _ -> false) cs in
        let hw = List.map clean_attributes_2 cs in
        let hw = if wrong_order then List.rev hw else hw in
        let hw_set = 
          List.fold_left (fun s e -> IntSet.add e s) IntSet.empty hw in
        select_aux xs acc_nodes (IntSet.union hw_set acc_waypoints)
        ((hw,info)::acc_highways)
      else select_aux xs acc_nodes acc_waypoints acc_highways
    | _ :: xs -> select_aux xs acc_nodes acc_waypoints acc_highways 
    | _ -> 
      ParsedMap.({ 
          points = acc_nodes; 
          waypoints = acc_waypoints; 
          highways = acc_highways; 
          bounds = 0.,0.,0.,0. })
  in 
  select_aux xmls IntMap.empty IntSet.empty []

let rec extract_bounds = function
  | Xml.Element ("bounds", attrs, _) :: _ ->
    let maxlat = float_of_string (List.assoc "maxlat" attrs) in 
    let minlat = float_of_string (List.assoc "minlat" attrs) in 
    let maxlon = float_of_string (List.assoc "maxlon" attrs) in 
    let minlon = float_of_string (List.assoc "minlon" attrs) in 
    maxlat, minlat, maxlon, minlon
  | _ :: xs -> extract_bounds xs
  | _ -> failwith "NOTFOUND"

let parsed_data_from_xml s =
  let x = read_map s in
  let xs = Xml.children x in
  let map = select_nodes xs in
  let map = {map with ParsedMap.bounds = extract_bounds xs} in
  let graph = GraphLib.create_graph map in
  let lalons = GraphLib.create_lalons map graph in 
  let landmarks = GraphLib.create_landmarks map graph in
  let highlayer_map = ParsedMap.highlayer_parsed map in
  let highlayer_graph = GraphLib.create_graph highlayer_map in
  let reverse_graph = GraphLib.inverse_graph graph in
  let highlayer_lalons = GraphLib.create_lalons highlayer_map highlayer_graph in
  let highlayer_landmarks = 
    GraphLib.create_landmarks highlayer_map highlayer_graph 
  in
  Data.({
      map = map;
      graph = graph;
      highlayer_map = highlayer_map;
      highlayer_graph = highlayer_graph;
      reverse_graph = reverse_graph;
      lalons = lalons;
      landmarks = landmarks;
      highlayer_lalons = highlayer_lalons;
      highlayer_landmarks = highlayer_landmarks;
    })

let reduce_data s =
  let prefix = Str.split (Str.regexp "\\.") s in
  let new_s = (List.hd prefix) ^ "_reduced.marshal"  in
  let ostream = open_out_bin new_s in
  let data = parsed_data_from_xml s in
  Marshal.to_channel ostream data [];
  close_out ostream;
  data

let parsed_data_from_reduced s =
  let istream = open_in_bin s in
  let data = (Marshal.from_channel istream : Data.t) in
  close_in istream; 
  data

let parse s =
  let prefix = Str.split (Str.regexp "\\.") s in
  let new_s = (List.hd prefix) ^ "_reduced.marshal" in
  if Sys.file_exists new_s 
  then parsed_data_from_reduced new_s
  else reduce_data s
