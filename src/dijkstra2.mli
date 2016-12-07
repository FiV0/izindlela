(* --- library that implements dijstras algorithm --- *)
(* only for debug version *)

open GraphLib
open ParsedMap

(* returns estimated time  * distance * path*)
val dijkstra : graph -> lowerbound_type -> int -> int -> 
                  search -> (float * float * int list * int list) 
