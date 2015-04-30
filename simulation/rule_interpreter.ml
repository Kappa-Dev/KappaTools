module Root = struct
  type t = int
  let allocate _ _ = ()
end

module RootHeap = Heap.Make(Root)

type t = {
  roots_of_ccs: RootHeap.t Connected_component.Map.t;
  edges: Edges.t;
  tokens: Nbr.t array;
  free_id: int;
}

let empty env = {
  roots_of_ccs = Connected_component.Map.empty;
  edges = Edges.empty;
  tokens = Array.make (NamedDecls.size env.Environment.tokens) Nbr.zero;
  free_id = 1;
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
  { roots_of_ccs = roots'; edges = edges';
    tokens = state.tokens; free_id = free_id'; }

let instance_number state ccs_l =
  let size cc =
    try
      Nbr.I
	(RootHeap.size (Connected_component.Map.find cc state.roots_of_ccs))
    with Not_found -> Nbr.zero in
  let rect_approx ccs =
    List.fold_left (fun acc cc -> Nbr.mult acc (size cc)) (Nbr.I 1) ccs in
  List.fold_left (fun acc ccs -> Nbr.add acc (rect_approx ccs)) Nbr.zero ccs_l

let value_bool ~get_alg counter state expr =
  Expr_interpreter.value_bool
    counter ~get_alg
    ~get_mix:(fun (_,ccs) -> instance_number state ccs)
    ~get_tok:(fun i -> state.tokens.(i))
    expr
let value_alg ~get_alg counter state alg =
  Expr_interpreter.value_alg
    counter ~get_alg
    ~get_mix:(fun (_,ccs) -> instance_number state ccs)
    ~get_tok:(fun i -> state.tokens.(i))
    alg

let update_tokens ~get_alg counter state consumed injected =
  let do_op op l =
    List.iter
      (fun (expr,i) ->
       state.tokens.(i) <-
	 op state.tokens.(i) (value_alg ~get_alg counter state expr))
      l in
  let () = do_op Nbr.sub consumed in do_op Nbr.add injected

let apply_rule ~get_alg domain counter state rule =
  let inj =
    List.fold_left
      (fun inj cc ->
       let root =
	 RootHeap.random (Connected_component.Map.find cc state.roots_of_ccs) in
       match inj with
       | Some inj ->
	  Connected_component.Matching.reconstruct state.edges inj cc root
       | None -> None)
      (Some Connected_component.Matching.empty)
      rule.Primitives.connected_components in
  match inj with
  | Some inj ->
     let () =
       update_tokens
	 ~get_alg counter state rule.Primitives.consumed_tokens
	 rule.Primitives.injected_tokens in
     update_edges domain inj state rule.Primitives.removed rule.Primitives.inserted
  | None -> state
