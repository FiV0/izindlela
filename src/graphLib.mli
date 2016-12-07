(* --- graph-library --- *)

type edge_info = {
  dist : float;
  information : ParsedMap.info
}

type graph = {
  mutable id_to_int : int Cont.intmap;
  mutable int_to_id : int Cont.intmap;
  mutable size : int ; 
  adj : ((int * edge_info) DynArray.dynArray) array
}

type lalons
type landmarks

type lowerbound_type = 
  | Simple 
  | Euclidean of lalons
  | Landmark of landmarks

val empty: graph
val empty_lalons:lowerbound_type  
val empty_landmarks:lowerbound_type 

val time_of_dis: edge_info -> ParsedMap.transport -> float

(* creates a graph from a parsed_map *)
val create_graph: ParsedMap.t ->  graph

val create_dijkstra: lowerbound_type

val create_lalons: ParsedMap.t -> graph -> lowerbound_type 

val create_landmarks : ParsedMap.t -> graph -> lowerbound_type 

val inverse_graph: graph -> graph

(* maps graph nodes to xml nodes *)
val lookup_ids: graph -> int list -> int list

(* expects Lalon not Landmark *)

val lowerbound: ParsedMap.search -> lowerbound_type -> int -> int -> float

val default_speed_foot : unit -> int 
val default_speed_bike : unit -> int

val connected : graph -> int -> int -> bool

(* function not used in final version *)

(*val create_highlayer : graph -> lowerbound_type -> graph
 
(*dfs to check for connectedness of two nodes *)
val dfs_connected : graph -> string -> string -> int*)
