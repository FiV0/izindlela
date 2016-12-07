type algo = Dijkstra | A_Star_E | A_Star_L

val load_map: string option -> unit
val get_point: string -> (float * float) option
val calculate: unit -> (float * float * float * int list * int list ) option
val get_bounds: unit -> float * float * float * float
val find_point: int -> float * float
val update_endpoint: float * float -> [< `FROM | `TO ] -> unit
val update_transport: ParsedMap.transport -> unit
val update_priority: ParsedMap.priority -> unit
val update_algo: algo -> unit
val update_use_highlayer: bool -> unit
