open Cont

(* maybe stop if we visit n2 for the first time as we have 
 * triangle inquality *)

exception NoSuchID
exception GraphNotConnected 
exception SearchNotSupported

let bind_to_pq (pq,i) e = 
  let pq = PriorityQueue.add (i,e) pq in
  pq, i+1

type advancement = {
  previous: int array;
  visited: bool array;
  distances: float array;
  estimates: float array;
  times: float array;
}

let initial n n1 = 
  let adv = {
    previous = Array.make n (-1); 
    visited = Array.make n false; 
    distances = Array.make n infinity; 
    estimates = Array.make n infinity;
    times = Array.make n infinity
  } 
  in
  Array.set adv.distances n1 0.0; 
  Array.set adv.estimates n1 0.0;
  Array.set adv.times n1 0.0;
  adv

let update_neighbours ncurrent target lowerbound_type adv 
                    (transport,priority) pq (n1,edge_info) =
  let dis = edge_info.GraphLib.dist in 
  let time = GraphLib.time_of_dis edge_info transport in
  let allowed = ParsedMap.check edge_info.GraphLib.information transport in
  if  (not adv.visited.(n1)) && allowed 
  then
    let pq = PriorityQueue.remove (n1, adv.estimates.(n1)) pq in 
    adv.distances.(n1) <- adv.distances.(ncurrent) +. dis;
    adv.previous.(n1) <- ncurrent;
    adv.times.(n1) <- adv.times.(ncurrent) +. time;
    adv.estimates.(n1) <- 
      begin match priority with 
        | ParsedMap.Short -> adv.distances.(n1) 
        | ParsedMap.Fast -> adv.times.(n1) 
        (* by dijkstra_aux *)
        | ParsedMap.Suitable -> assert false 
      end 
      +. (GraphLib.lowerbound (transport, priority) lowerbound_type n1 target);
    PriorityQueue.add (n1,adv.estimates.(n1)) pq 
  else pq

let dijkstra_aux g lowerbound_type n1 n2 search = 
  let n = g.GraphLib.size in 
  let adv = initial n n1 in
  let pq = PriorityQueue.empty in
  let pq,_ = match search with 
    | _, ParsedMap.Short -> Array.fold_left bind_to_pq (pq,0) adv.distances 
    | _, ParsedMap.Fast -> Array.fold_left bind_to_pq (pq,0) adv.times
    | _, ParsedMap.Suitable -> raise SearchNotSupported       
  in 
  let ncurrent = ref 0 in
  let pq = ref pq in
  begin 
    try 
      while true do
        let ncurrent', dis as e = PriorityQueue.min_elt !pq in
        pq := PriorityQueue.remove e !pq;
        ncurrent := ncurrent';
        (* when we are exploring from target node we are finished *)
        if !ncurrent = n2 then raise Exit
        else 
          begin
            adv.visited.(!ncurrent) <- true;
            let update_neighbours' = 
              update_neighbours !ncurrent n2 lowerbound_type adv search 
            in
            pq := DynArray.fold_left update_neighbours' !pq 
                (Array.get g.GraphLib.adj !ncurrent) 
          end
      done 
    with 
    | Exit -> ()
    | Not_found -> raise GraphNotConnected 
  end;
(*let rec looping pq = 
    try 
      let ncurrent, dis as e = PriorityQueue.min_elt pq in
      let pq = PriorityQueue.remove e pq in
      (* when we are exploring from target node we are finished *)
      if ncurrent = n2 then () 
      else 
         begin 
          adv.visited.(ncurrent) <- true;
          let update_neighbours' = 
            update_neighbours ncurrent n2 lowerbound_type adv search 
          in
          let pq = DynArray.fold_left update_neighbours' pq 
              (Array.get g.GraphLib.adj ncurrent) 
          in
          looping pq
        end
    with
      Not_found -> raise GraphNotConnected 
                     
  in
  looping pq; *) 
  let rec reconstruct_path cur acc = 
    if cur = n1 then n1::acc
    else reconstruct_path adv.previous.(cur) (cur::acc) 
  in
  (*  path n1 ... n2 *)
  let path = reconstruct_path n2 [] in
  adv.times.(n2), adv.distances.(n2), path, adv.visited


let dijkstra g lowerbound_type id1 id2 search =
  try 
    let n1 = Cont.IntMap.find id1 g.GraphLib.id_to_int in
    let n2 = Cont.IntMap.find id2 g.GraphLib.id_to_int in
    let time, dis, path, visited = dijkstra_aux g lowerbound_type n1 n2 search in 
    (* path n2 ... n1 *)
    let id_path = GraphLib.lookup_ids g path in
    let _, visited = Array.fold_left (fun (index, acc) b -> 
        if b 
        then index + 1, (Cont.IntMap.find index g.GraphLib.int_to_id)::acc
        else index + 1, acc) 
        (0,[]) visited 
    in
    time, dis, id_path, visited
  with Not_found -> raise NoSuchID 
