type t = {
  roots_of_ccs: ValMap.tree Connected_component.Map.t;
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

let print_heap f h =
  let () = Format.pp_open_box f 0 in
  let () = ValMap.iter
	     (fun c -> Format.fprintf f "%i%t" c Pp.comma) h in
  Format.pp_close_box f ()

let update_roots is_add map cc root =
  let va = try Connected_component.Map.find cc map
	   with Not_found -> ValMap.empty in
  Connected_component.Map.add
    cc ((if is_add then ValMap.add else ValMap.remove) root va) map

let from_place (inj_nodes,inj_fresh,free_id as inj2graph) = function
  | Transformations.Existing (n,id) ->
     (Connected_component.ContentAgent.get_sort n,
      Connected_component.Matching.get (n,id) inj_nodes,
      inj2graph)
  | Transformations.Fresh (ty,id) ->
     try (ty,Mods.IntMap.find id inj_fresh,inj2graph)
     with Not_found ->
       ty,free_id,(inj_nodes,Mods.IntMap.add id free_id inj_fresh,succ free_id)

let deal_transformation is_add domain inj2graph edges roots transf = (*transf: abstract edge to be added or removed*)
  let inj,graph,obs = (*inj: inj2graph', graph: edges', obs: delta_roots -NB inj should not change if [is_add] is false*)
    match transf with
    | Transformations.Freed (n,s) -> (*(n,s)-bottom*)
       let ty, id, inj2graph' = from_place inj2graph n in (*(A,23,phi)*)
       let edges' =
	 if is_add then Edges.add_free ty id s edges
	 else Edges.remove_free id s edges in
       let new_obs =
	 Connected_component.Matching.observables_from_free
	   domain (if is_add then edges' else edges) ty id s in (*this hack should disappear when chekcing O\H only*)
       (inj2graph',edges',new_obs)
    | Transformations.Linked ((n,s),(n',s')) ->
       let ty, id, inj2graph' = from_place inj2graph n in
       let ty', id', inj2graph'' = from_place inj2graph' n' in
       let edges' =
	 if is_add then Edges.add_link ty id s ty' id' s' edges
	 else Edges.remove_link id s id' s' edges in
       let new_obs =
	 Connected_component.Matching.observables_from_link
	   domain (if is_add then edges' else edges) ty id s ty' id' s' in
       (inj2graph'',edges',new_obs)
    | Transformations.Internalized (n,s,i) ->
       let ty, id, inj2graph' = from_place inj2graph n in
       let edges' =
	 if is_add then Edges.add_internal id s i edges
	 else Edges.remove_internal id s i edges in
       let new_obs =
	 Connected_component.Matching.observables_from_internal
	   domain (if is_add then edges' else edges) ty id s i in
       (inj2graph',edges',new_obs)
  in
  let roots' =
    List.fold_left
      (fun r' (cc,root) ->
       (* let () = *)
       (* 	 Format.eprintf *)
       (* 	   "@[add:%b %a in %i@]@." is_add *)
       (* 	   (Connected_component.print true !Debug.global_sigs) cc root in *)
       update_roots is_add r' cc root) roots obs in
  (inj,graph,roots')

let update_edges domain inj_nodes state removed added =
  (* let () = Format.printf "@[%a@]@." (Edges.debug_print) state.edges in *)

(*Negative update*)
  let aux =
    List.fold_left
      (fun (inj2graph,edges,roots) transf -> (*inj2graph: abs -> conc, roots define the injection that is used*)
       deal_transformation false domain inj2graph edges roots transf)
      ((inj_nodes,Mods.IntMap.empty,state.free_id), (*initial inj2graph: (existing,new,fresh_id) *)
       state.edges,state.roots_of_ccs)
      removed (*removed: statically defined edges*)
  in
(*Positive update*)
  let ((_,_,free_id'),edges',roots') =
    List.fold_left
      (fun (inj2graph,edges,roots) transf ->
       deal_transformation true domain inj2graph edges roots transf)
      aux 
      added (*statically defined edges*) 
  in
  { roots_of_ccs = roots'; edges = edges';
    tokens = state.tokens; free_id = free_id'; }

let instance_number state ccs_l =
  let size cc =
    try
      ValMap.total (Connected_component.Map.find cc state.roots_of_ccs)
    with Not_found -> 0 in
  let rect_approx ccs =
    Array.fold_left (fun acc cc ->  acc * (size cc)) 1 ccs in
  Nbr.I (List.fold_left (fun acc ccs -> acc + (rect_approx ccs)) 0 ccs_l)

let value_bool ~get_alg counter state expr =
  Expr_interpreter.value_bool
    counter ~get_alg
    ~get_mix:(fun ccs -> instance_number state ccs)
    ~get_tok:(fun i -> state.tokens.(i))
    expr
let value_alg ~get_alg counter state alg =
  Expr_interpreter.value_alg
    counter ~get_alg
    ~get_mix:(fun ccs -> instance_number state ccs)
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

let transform_by_a_rule ~get_alg domain counter state rule inj =
  let () =
    update_tokens
      ~get_alg counter state rule.Primitives.consumed_tokens
      rule.Primitives.injected_tokens in
  update_edges domain inj state rule.Primitives.removed rule.Primitives.inserted

let apply_rule ~get_alg domain counter state rule =
  let inj =
    Tools.array_fold_lefti
      (fun id inj cc ->
       let root =
	 ValMap.random_val
	   (Connected_component.Map.find cc state.roots_of_ccs) in
       match inj with
       | Some inj ->
	  Connected_component.Matching.reconstruct state.edges inj id cc root
       | None -> None)
      (Some Connected_component.Matching.empty)
      rule.Primitives.connected_components in
  match inj with
  | Some inj ->
     Some (transform_by_a_rule ~get_alg domain counter state rule inj)
  | None -> None

let all_injections state rule =
  Tools.array_fold_lefti
    (fun id inj_list cc ->
     ValMap.fold
       (fun root new_injs ->
	List.fold_left
	  (fun corrects inj ->
	   match Connected_component.Matching.reconstruct
		   state.edges inj id cc root with
	   | None -> corrects
	   | Some new_inj -> new_inj :: corrects)
	new_injs inj_list)
       (Connected_component.Map.find cc state.roots_of_ccs) [])
    [] rule.Primitives.connected_components

let force_rule ~get_alg domain counter state rule =
  match apply_rule ~get_alg domain counter state rule with
  | Some state -> state,None
  | None ->
     match all_injections state rule with
     | [] -> state,Some []
     | h :: t ->
	transform_by_a_rule ~get_alg domain counter state rule h, Some t

let print_injections ?sigs f roots_of_ccs =
  Format.fprintf
    f "@[<v>%a@]"
    (Pp.set Connected_component.Map.bindings Pp.space
	    (fun f (cc,roots) ->
	     Format.fprintf
	       f "@[# @[%a@] ==> %a@]"
	       (Connected_component.print ?sigs true) cc print_heap roots
	    )
    ) roots_of_ccs

let print env f state =
  Format.fprintf f "@[<v>%a@,%a@,%a@]"
		 (Edges.print env.Environment.signatures) state.edges
		 (Pp.array Pp.space (fun i f el ->
				     Format.fprintf f "%%init: %s <- %a"
						    (NamedDecls.elt_name
						       env.Environment.tokens i)
						    Nbr.print el))
		 state.tokens
		 (print_injections ~sigs:env.Environment.signatures) state.roots_of_ccs

let debug_print f state =
  Format.fprintf f "@[<v>%a@,%a@,%a@]"
		 Edges.debug_print state.edges
		 (Pp.array Pp.space (fun i f el ->
				     Format.fprintf f "token_%i <- %a"
						    i Nbr.print el))
		 state.tokens
		 (print_injections ?sigs:None) state.roots_of_ccs
