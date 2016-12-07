val html_code: string
val js_init: float * float * float * float -> string
val js_move: float * float * float * float -> string
val js_set_marker: float * float -> [< `FROM | `TO ] -> string
val js_draw_path: string
val js_clear_path: string
val js_add_waypoint: float * float -> string
val js_clear_visited: string
val js_toggle_visited: bool -> string
val js_add_visited_point: float * float -> string
