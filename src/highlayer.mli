(* uses Dijkstra2 
 * takes advantages of highlayer graphs *)

open ParsedMap
open GraphLib

(* first graph is the normal graph 
 * second graph is the reversed normal graph
 * third graph is the graph created by create_highlayer
 * the lowerbound_type indicates which algorithm will be used
 *)

val highlayer_algo: graph -> graph -> graph -> lowerbound_type -> 
    int -> int -> search -> (float * float * int list * int list )
