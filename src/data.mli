type t = {
  map: ParsedMap.t; 
  graph: GraphLib.graph;
  highlayer_map: ParsedMap.t; 
  highlayer_graph: GraphLib.graph;
  reverse_graph: GraphLib.graph;
  lalons: GraphLib.lowerbound_type;
  landmarks: GraphLib.lowerbound_type;
  highlayer_lalons: GraphLib.lowerbound_type;
  highlayer_landmarks: GraphLib.lowerbound_type;
}
