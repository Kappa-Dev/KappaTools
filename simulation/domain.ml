module Root = struct
  type t = int
  let allocate _ _ = ()
end

module RootHeap = Heap.Make(Root)

type t =
    {
      roots_of_ccs: RootHeap.t Connected_component.Map.t;
      edges: Edges.t;
      free_id: int;
    }

let update_roots is_add map cc root =
  let va = try Connected_component.Map.find cc map
	   with Not_found -> RootHeap.create 13 in
  Connected_component.Map.add
    cc ((if is_add then RootHeap.alloc else RootHeap.remove) root va) map

let from_place (inj_nodes,inj_fresh,free_id as inj2graph) = function
  | Transformations.Existing n ->
     (Connected_component.Node.get_sort n,
      Connected_component.Matching.get n inj_nodes,
      inj2graph)
  | Transformations.Fresh (ty,id) ->
     try (ty,Mods.IntMap.find id inj_fresh,inj2graph)
     with Not_found ->
       ty,free_id,(inj_nodes,Mods.IntMap.add id free_id inj_fresh,succ free_id)

let deal_transformation is_add domain inj2graph edges roots transf =
  let inj,graph,obs =
    match transf with
    | Transformations.Freed (n,s) ->
       let ty, id, inj2graph' = from_place inj2graph n in
       let edges' = Edges.add_free id s edges in
       let new_obs =
	 Connected_component.Matching.observables_from_free
	   domain (if is_add then edges' else edges) ty id s in
       (inj2graph',edges',new_obs)
    | Transformations.Linked ((n,s),(n',s')) ->
       let ty, id, inj2graph' = from_place inj2graph n in
       let ty', id', inj2graph'' = from_place inj2graph' n' in
       let edges' = Edges.add_link ty id s ty' id' s' edges in
       let new_obs =
	 Connected_component.Matching.observables_from_link
	   domain (if is_add then edges' else edges) ty id s ty' id' s' in
       (inj2graph'',edges',new_obs)
    | Transformations.Internalized (n,s,i) ->
       let ty, id, inj2graph' = from_place inj2graph n in
       let edges' = Edges.add_internal id s i edges in
       let new_obs =
	 Connected_component.Matching.observables_from_internal
	   domain (if is_add then edges' else edges) ty id i s in
       (inj2graph',edges',new_obs)
  in
  let roots' =
    List.fold_left
      (fun r' (cc,root) -> update_roots is_add r' cc root) roots obs in
  (inj,graph,roots')

let update_edges domain inj_nodes state removed added =
  let aux =
    List.fold_left
      (fun (inj2graph,edges,roots) transf ->
       deal_transformation false domain inj2graph edges roots transf)
      ((inj_nodes,Mods.IntMap.empty,state.free_id),
       state.edges,state.roots_of_ccs)
      removed in
  let ((_,_,free_id'),edges',roots') =
    List.fold_left
      (fun (inj2graph,edges,roots) transf ->
       deal_transformation true domain inj2graph edges roots transf)
      aux added in
  { roots_of_ccs = roots'; edges = edges'; free_id = free_id'; }

let apply_rule domain state (ccs_rule,removed_edges,added_edges) =
  let inj =
    List.fold_left
      (fun inj cc ->
       let root =
	 RootHeap.random (Connected_component.Map.find cc state.roots_of_ccs) in
       Connected_component.Matching.reconstruct domain state.edges inj cc root)
    Connected_component.Matching.empty in
  update_edges domain () state removed_edges added_edges
