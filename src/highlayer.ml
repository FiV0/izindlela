open GraphLib
open ParsedMap
open Cont

(* acc = [time,dist,visited = path] *)

let rec next_highlayer_node_aux (time,dist,path) prev current g
    transport =
  let adj = g.adj in
  if DynArray.size adj.(current) > 2 then Some(time,dist,current::path)
  else match DynArray.size adj.(current) with
    | 2 -> 
      let next1,edge_info1 = DynArray.get adj.(current) 0 in
      let next2,edge_info2 = DynArray.get adj.(current) 1 in
      let dist1 = dist +. edge_info1.dist in
      let info1 = edge_info1.information in
      let dist2 = dist +. edge_info2.dist in
      let info2 = edge_info2.information in
      begin 
        match (next1 != prev), 
              (check info1 transport),
              (check info2 transport) 
        with
        | true, true, _ ->
          let time = time +. (time_of_dis edge_info1 transport) in
          next_highlayer_node_aux (time,dist1,current::path) current next1 g
            transport 
        | _, _, true -> 
          let time = time +. (time_of_dis edge_info2 transport) in
          next_highlayer_node_aux (time,dist2,current::path) current next2 g
            transport 
        | _ -> None (* dead end *) 
      end

    | 1 -> 
      let next,edge_info = DynArray.get adj.(current) 0 in
      let dist = dist +. edge_info.dist in
      let info = edge_info.information in
      begin 
        match (next = prev), (check info transport) with
        | true, _ | _, false -> None (* dead end *)
        | _ ->
          let time = time +. (time_of_dis edge_info transport) in
          next_highlayer_node_aux (time,dist,current::path) current next g
            transport 
      end

    | _ -> assert false (* this would be really weird *)

(* could add lowerbound calculation to this function *)

let next_highlayer_node start goal g (transport,priority) =
  let init = (0.0,0.0,[]) in
  begin match DynArray.size g.adj.(start) with
  | 1 -> 
      let res = (next_highlayer_node_aux init (-1) start g transport) in
      begin match res with 
      | Some(time,dist,path) -> (time,dist,path,path)
      | _ -> assert false end

  | 2 -> 
      let n1,_ = DynArray.get g.adj.(start) 0 in
      let n2,_ = DynArray.get g.adj.(start) 1 in
      let res1 =
        (next_highlayer_node_aux init start n1 g transport) in
      let res2 =
        next_highlayer_node_aux init start n2 g transport in
      begin match res1,res2 with
      | None,Some(time1,dist1,path1) -> (time1,dist1,path1,path1)
      | Some(time2,dist2,path2),None -> (time2,dist2,path2,path2) 
      | Some(time1,dist1,path1),Some(time2,dist2,path2) -> 
        begin match priority with
        | Fast | Suitable ->  
          if dist1 < dist2 then (time1,dist1,path1,path1 @ path2)  
          else (time2,dist2,path1,path1 @ path2 )
        | _ -> if time1 < time2 then (time1,dist1,path2,path1 @ path2)  
          else (time2,dist2,path2,path1 @ path2) end
      | _ -> assert false end

  | _ -> (0.0,0.0,[start],[]) end

let highlayer_algo normal_graph inverse_graph 
                      highlayer_graph lowerbounds id1 id2 search =
  let start = IntMap.find id1 normal_graph.id_to_int in
  let goal = IntMap.find id2 normal_graph.id_to_int in
  let time1, dist1, path1, visited1 =
    next_highlayer_node start goal normal_graph search 
  in
  let low1 = List.hd path1 in
  let path1 = List.tl path1 in
  let time2, dist2, path2, visited2 =
    next_highlayer_node goal start inverse_graph search in
  let low2 = List.hd path2 in
  let path2 = List.tl path2 in
  let high_id1 = IntMap.find low1 normal_graph.int_to_id in
  let high_id2 = IntMap.find low2 normal_graph.int_to_id in
  let (time,dist,path_ids,visited_ids) =
    Dijkstra2.dijkstra highlayer_graph lowerbounds high_id1 high_id2 search in
  try
  let time = (time1 +. time +. time2) in
  let dist = (dist1 +. dist +. dist2) in
  let path1_ids = lookup_ids normal_graph path1 in
  let path2_ids = lookup_ids normal_graph path2 in
  let visited1_ids = lookup_ids normal_graph visited1 in
  let visited2_ids = lookup_ids normal_graph visited2 in
  (* path n2 ... n1 *)
  let path = (path2_ids @ path_ids @ (List.rev path1_ids)) in
  let visited = (visited1_ids @ visited_ids @ visited2_ids) in 
  (time,dist,path,visited)
  with Invalid_argument "index out of bounds"  -> failwith "lol"
