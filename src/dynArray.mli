(* small dynamic array library, can of course be extended *)

(* is raise if an index out of bound is specified or 
 * the array is empty *)
exception IndexOutOfBounds

type 'a dynArray 

val create : unit -> 'a dynArray

val empty : 'a dynArray -> bool

val size : 'a dynArray -> int

val set : 'a dynArray -> int -> 'a -> unit 

val get : 'a dynArray -> int -> 'a 

val add : 'a dynArray -> 'a -> unit

val iter : ('a -> unit) -> 'a dynArray -> unit

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b dynArray -> 'a
