val init: ParsedMap.t -> int * int -> unit

val draw_nodes: ParsedMap.t -> unit 

val draw_ways: ParsedMap.t -> GraphLib.graph -> unit

val draw_path: ParsedMap.t -> int list -> unit

val draw_visited: ParsedMap.t -> int list -> unit

(*val draw_landmarks: GraphLib.lowerbound_type -> unit*)

val idle: unit -> unit

