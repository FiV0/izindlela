module IntMap : Map.S with type key = int
module IntSet : Set.S with type elt = int
module PriorityQueue : Set.S with type elt = (int * float)

type 'a intmap = 'a IntMap.t
type intset = IntSet.t
