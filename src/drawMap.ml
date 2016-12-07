open Graphics
open ParsedMap
open GraphLib
open Cont

let minlat = ref 0.
let minlon = ref 0.

let mulh = ref 0.
let mulw = ref 0.

let nodes_col = rgb 232 224 216
let ways_col = rgb 188 165 141
let path_col = rgb 255 70 70
let visited_col = rgb 70 70 255

let init map (w, h) = 
  let maxlat, minlatn, maxlon, minlonn = map.bounds in
  minlat := minlatn;
  minlon := minlonn;
  mulh := (float_of_int h) /. (maxlat -. !minlat);
  mulw := (float_of_int w) /. (maxlon -. !minlon);
  open_graph "";
  set_window_title "Izindlela - Simple UI";
  resize_window w h

let transform (lat, lon) = 
  let x = int_of_float (ceil (!mulw *. (lon -. !minlon))) in
  let y = int_of_float (ceil (!mulh *. (lat -. !minlat))) in
  x, y

let draw_nodes map = 
  set_color nodes_col;
  IntMap.iter (fun _ latlon ->
      let x, y = transform latlon in
      plot x y;) map.points

let draw_ways map graph =
  set_color ways_col;
  Array.iteri (fun i1 da -> 
      DynArray.iter (fun (i2,_) ->
          let id1 = IntMap.find i1 graph.int_to_id in
          let id2 = IntMap.find i2 graph.int_to_id in
          let latlon = IntMap.find id1 map.points in
          let x, y = transform latlon in
          moveto x y;
          let latlon = IntMap.find id2 map.points in
          let x, y = transform latlon in
          lineto x y;) da) graph.adj

let draw_path map path = 
  set_line_width 2;
  set_color path_col;
  let path = List.map (fun id -> 
      let latlon = IntMap.find id map.points in
      transform latlon) path 
  in
  draw_poly_line (Array.of_list path) 

let draw_visited map pts = 
  set_line_width 2;
  set_color visited_col;
  let pts = List.map (fun id -> 
      let latlon = IntMap.find id map.points in
      transform latlon) pts 
  in
  (*print_endline "ok";*)
  (*plots (Array.of_list pts)*)
    List.iter (fun (x,y) -> draw_circle x y 1) pts

(*let draw_landmarks = function
  | GraphLib.Landmark lmks ->
    set_line_width 10;
    set_color green;
    let pts = Array.(to_list (map (fun (i,lat,lon) -> 
      transform (lat,lon)) lmks.GraphLib.landmark_coordinates))
    in
    List.iter (fun (x,y) -> draw_circle x y 1) pts
  | _ -> assert false*)

let idle () =
  let st = wait_next_event [Key_pressed] in
  if st.key = 'q' then begin close_graph (); exit 0 end
