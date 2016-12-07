let dbg = ref true 
let print_dbg s = if !dbg then print_endline s else ()

let init s =
  print_dbg "Parsing...";  
  let data = IO.parse s in
  print_dbg "Parsing finished.";

  DrawMap.init data.Data.map (800, 600);

  print_dbg "Drawing nodes...";
  DrawMap.draw_nodes data.Data.map;
  print_dbg "Drawing nodes finished...";

  print_dbg "Drawing ways...";
  DrawMap.draw_ways data.Data.map data.Data.graph;
  print_dbg "Drawing ways finished...";
  
  print_string "Highlayer map constains ";
  print_int (Cont.IntSet.cardinal data.Data.highlayer_map.ParsedMap.waypoints);
  print_endline " points";
  print_string "Highlayer map constains ";
  print_int (Cont.IntMap.cardinal data.Data.highlayer_map.ParsedMap.points);
  print_endline " points";
  print_dbg "Highlayer_map finished";

  let dijkstra_typ = GraphLib.create_dijkstra in

  Printf.printf "Normal graph contains %d nodes \n" 
    data.Data.graph.GraphLib.size; 
  Printf.printf "Higherlayer graph contains %d nodes \n" 
    data.Data.highlayer_graph.GraphLib.size;

  print_endline "Please specify start adress:";
  let start = read_line () in
  let latlon1 = GetHttp.get_osm_data start in
  let latlon1 = match latlon1 with
  | Some latlon -> latlon
  | None -> failwith "Address not found"
  in
  print_endline "Please specify end adress:";
  let endp = read_line () in
  let latlon2 = GetHttp.get_osm_data endp in
  let latlon2 = match latlon2 with
  | Some latlon -> latlon
  | None -> failwith "Address not found"
  in
  if not (ParsedMap.in_bounds data.Data.map latlon1) && 
     not (ParsedMap.in_bounds data.Data.map latlon2)
  then begin print_endline "Out of bounds, retry."; raise Exit; end;

  let id1 = ParsedMap.nearest data.Data.map latlon1 in
  let id2 = ParsedMap.nearest data.Data.map latlon2 in

    (*print_string "Please specify the type of transport\n"; *)
  let mode = ParsedMap.Car, ParsedMap.Short in

  print_newline ();
  print_endline "Calculating with Dijkstra ...";
  let t = Sys.time() in
  let time, dis, path_dij, visited_dij = 
    Dijkstra2.dijkstra data.Data.graph dijkstra_typ id1 id2 mode 
  in
  Printf.printf "Execution time Dijkstra: %fs\n" (Sys.time() -. t); 
  Printf.printf "The distance between %i and %i is %f\n%!" id1 id2 dis;
  Printf.printf "The journey will approximatly take %f hours\n" time;
  Printf.printf "Number of nodes explored %d\n" (List.length visited_dij);
  print_endline "Finished calculating with Dijkstra";

  print_newline ();
  print_endline "Calculating with A* Euclidean";
  let t = Sys.time() in
  let time, dis, path_aStar, visited_aStar_euclid = 
    Dijkstra2.dijkstra data.Data.graph data.Data.lalons id1 id2 mode 
  in
  Printf.printf "Execution time A*: %fs\n" (Sys.time() -. t); 
  Printf.printf "The distance between %i and %i is %f\n%!" id1 id2 dis;
  Printf.printf "The journey will approximatly take %f hours\n" time;
  Printf.printf "Number of nodes explored %d\n" 
    (List.length visited_aStar_euclid);
  print_endline "Finished calculating with A* Euclidean";  

  print_newline ();
  print_endline "Calculating with A* Landmarks";
  let t = Sys.time() in
  let time, dis, path_aStar, visited_aStar_landmarks = 
    Dijkstra2.dijkstra data.Data.graph data.Data.landmarks id1 id2 mode 
  in
  Printf.printf "Execution time A*: %fs\n" (Sys.time() -. t); 
  Printf.printf "The distance between %i and %i is %f\n%!" id1 id2 dis;
  Printf.printf "The journey will approximatly take %f hours\n" time;
  Printf.printf "Number of nodes explored %d\n" 
    (List.length visited_aStar_landmarks);
  print_endline "Finished calculating with A* Landmarks";  

  print_newline ();
  print_endline "Highlayer with Dijkstra";
  let t = Sys.time() in
  let time, dis, path_higherlayer, visited_highlayer_dijkstra = 
    Highlayer.highlayer_algo data.Data.graph data.Data.reverse_graph 
      data.Data.highlayer_graph dijkstra_typ id1 id2 mode 
  in
  Printf.printf "Execution time:  %fs\n" (Sys.time() -. t); 
  Printf.printf "The distance between %i and %i is %f\n%!" id1 id2 dis;
  Printf.printf "The journey will approximatly take %f hours\n" time;
  Printf.printf "Number of nodes explored %d\n" 
    (List.length visited_highlayer_dijkstra);
  print_endline "Finished Highlayer with Dijkstra";  

  (*print_endline "Highlayer with A* Euclidean";
  let t = Sys.time() in
  let (time,dis,path_higherlayer,visited_highlayer_dijkstra) 
    = Highlayer.highlayer_algo graph reverse_graph highlayer_graph
    highlayer_lalons id1 id2 (Car,Short) in
  Printf.printf "Execution time:  %fs\n" (Sys.time() -. t); 
  Printf.printf "The distance between %i and %i is %f\n%!" id1 id2 dis;
  Printf.printf "The journey will approximatly take %f hours\n" time;
  Printf.printf "Number of nodes explored %d\n" (List.length
  visited_highlayer_dijkstra);
  print_endline "Finished Highlayer A* Euclidean";  
  

  print_endline "Highlayer with A* landmarks";
  let t = Sys.time() in
  let (time,dis,path_higherlayer,visited_highlayer_landmarks) 
    = Highlayer.highlayer_algo graph reverse_graph highlayer_graph
    highlayer_landmarks id1 id2 (Car,Short) in
  Printf.printf "Execution time:  %fs\n" (Sys.time() -. t); 
  Printf.printf "The distance between %i and %i is %f\n%!" id1 id2 dis;
  Printf.printf "The journey will approximatly take %f hours\n" time;
  Printf.printf "Number of nodes explored %d\n" (List.length
  visited_highlayer_dijkstra);
  print_endline "Finished Highlayer with A* landmarks";  *)

  print_newline ();
  print_endline "Finished highlayer calculations";
  print_newline ();

  print_dbg "Drawing path...";
  DrawMap.draw_path data.Data.map path_higherlayer;
  print_dbg "Drawing path finished.";

  print_dbg "Drawing visited...";
  DrawMap.draw_visited data.Data.map visited_highlayer_dijkstra;
  print_dbg "Drawing visited finished.";

  DrawMap.idle ();
