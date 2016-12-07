module IntOrder = struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make(IntOrder)
module IntSet = Set.Make(IntOrder)

type 'a intmap = 'a IntMap.t
type intset = IntSet.t

module PairCompare = struct 
  type t = int * float
  let compare (x,dis1) (y,dis2) =
    let res = compare dis1 dis2 in
    if res = 0 then compare x y else res
end

module PriorityQueue = Set.Make(PairCompare)

