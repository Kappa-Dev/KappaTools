type jf_data = Compression_main.secret_log_info * Compression_main.secret_step list

type t = {
  roots_of_ccs: ValMap.tree Connected_component.Map.t;
  edges: Edges.t;
  tokens: Nbr.t array;
  outdated_elements: Operator.DepSet.t;
  free_id: int;
  story_machinery :
    ((Causal.event_kind *
	Instantiation.abstract Instantiation.test list)
       Connected_component.Map.t (*currently tracked ccs *)
     * jf_data) option;
}

let empty ~has_tracking env = {
  roots_of_ccs = Connected_component.Map.empty;
  edges = Edges.empty;
  tokens = Array.make (Environment.nb_tokens env) Nbr.zero;
  outdated_elements = Operator.DepSet.empty;
  free_id = 1;
  story_machinery =
    if has_tracking
    then Some (Connected_component.Map.empty,
	       (Compression_main.init_secret_log_info (), []))
    else None;
}

let print_heap f h =
  ValMap.iter (fun c -> Format.fprintf f "%i%t" c Pp.comma) h

let update_roots is_add map cc root =
  let va = try Connected_component.Map.find cc map
	   with Not_found -> ValMap.empty in
  Connected_component.Map.add
    cc ((if is_add then ValMap.add else ValMap.remove) root va) map

let from_place (inj_nodes,inj_fresh,free_id as inj2graph) = function
  | Place.Existing (n,id) ->
     (Connected_component.ContentAgent.get_sort n,
      Connected_component.Matching.get (n,id) inj_nodes,
      inj2graph)
  | Place.Fresh (ty,id) ->
     try (ty,Mods.IntMap.find id inj_fresh,inj2graph)
     with Not_found ->
       ty,free_id,(inj_nodes,Mods.IntMap.add id free_id inj_fresh,succ free_id)

let deal_transformation is_add domain inj2graph edges roots transf = (*transf: abstract edge to be added or removed*)
  let inj,graph,(obs,deps) = (*inj: inj2graph', graph: edges', obs: delta_roots -NB inj should not change if [is_add] is false*)
    match transf with
    | Primitives.Transformation.Freed (n,s) -> (*(n,s)-bottom*)
       let ty, id, inj2graph' = from_place inj2graph n in (*(A,23,phi)*)
       let edges' =
	 if is_add then Edges.add_free ty id s edges
	 else Edges.remove_free id s edges in
       let new_obs =
	 Connected_component.Matching.observables_from_free
	   domain (if is_add then edges' else edges) ty id s in (*this hack should disappear when chekcing O\H only*)
       (inj2graph',edges',new_obs)
    | Primitives.Transformation.Linked ((n,s),(n',s')) ->
       let ty, id, inj2graph' = from_place inj2graph n in
       let ty', id', inj2graph'' = from_place inj2graph' n' in
       let edges' =
	 if is_add then Edges.add_link ty id s ty' id' s' edges
	 else Edges.remove_link id s id' s' edges in
       let new_obs =
	 Connected_component.Matching.observables_from_link
	   domain (if is_add then edges' else edges) ty id s ty' id' s' in
       (inj2graph'',edges',new_obs)
    | Primitives.Transformation.Internalized (n,s,i) ->
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
  ((inj,graph,roots',deps),obs)

let store_event
      counter inj2graph new_tracked_obs_instances event_kind rule = function
  | None as x -> x
  | Some (x,(info,steps)) ->
(*     let quarks_obs =
       List.map
	 (Connected_component.Matching.quark_lists_of_cc_instance edges)
	 new_tracked_obs_instances in
     let quark_event =
        quark_list_from_event inj2graph rule.Primitives.instantiations in*)
     let concrete_event =
       Instantiation.concretize_event
	 (fun p -> let (_,x,_) = from_place inj2graph p in x)
	 rule.Primitives.instantiations in
     let infos',steps' =
       Compression_main.secret_store_event
	 info (event_kind,concrete_event) steps in
     let infos'',steps'' =
       List.fold_left
	 (fun (infos,steps) (ev,obs_tests) ->
	  let obs =
	    (ev,
	     obs_tests,
	    Mods.Counter.next_story counter) in
	  Compression_main.secret_store_obs infos obs steps)
	 (infos',steps')
	 new_tracked_obs_instances
     in
       Some (x,(infos'',steps''))
(*Compression_main.store rule final_inj2graph counter quarks_obs quark_event*)

let store_obs edges obs acc = function
  | None -> acc
  | Some (tracked,_) ->
     List.fold_left
       (fun acc (cc,root) ->
	try
	  let ev,tests = Connected_component.Map.find cc tracked in
	  match Connected_component.Matching.reconstruct
		  edges Connected_component.Matching.empty 0 cc root with
	  | None ->
	     raise (ExceptionDefn.Internal_Error
		      (Location.dummy_annot
			 "Problem with Rule_interpreter.store_obs"))
	  | Some inj ->
	     let tests' =
	       List.map (Instantiation.concretize_test
			   (fun p ->
			    let (_,x,_) =
			      from_place (inj,Mods.IntMap.empty,0) p in x))
			tests in
	      (ev,tests') :: acc
	with Not_found -> acc)
       acc obs

let update_edges counter domain inj_nodes state event_kind rule =
  (*Negative update*)
  let aux =
    List.fold_left
      (fun (inj2graph,edges,roots,deps) transf -> (*inj2graph: abs -> conc, roots define the injection that is used*)
       let ((a,b,c,new_deps),_) =
	 deal_transformation false domain inj2graph edges roots transf in
       (a,b,c,Operator.DepSet.union new_deps deps))
      ((inj_nodes,Mods.IntMap.empty,state.free_id), (*initial inj2graph: (existing,new,fresh_id) *)
       state.edges,state.roots_of_ccs,state.outdated_elements)
      rule.Primitives.removed (*removed: statically defined edges*)
  in
  (*Positive update*)
  let (((_,_,free_id' as final_inj2graph),edges',roots',rev_deps'),new_tracked_obs_instances) =
    List.fold_left
      (fun ((inj2graph,edges,roots,deps),tracked_inst) transf ->
       let (a,b,c,new_deps),new_obs =
	 deal_transformation true domain inj2graph edges roots transf in
       ((a,b,c,Operator.DepSet.union deps new_deps),
	store_obs b new_obs tracked_inst state.story_machinery))
      (aux,[])
      rule.Primitives.inserted (*statically defined edges*)
  in
  (*Store event*)
  let story_machinery' =
    store_event
      counter final_inj2graph new_tracked_obs_instances event_kind rule
      state.story_machinery in

  { roots_of_ccs = roots'; edges = edges';
    tokens = state.tokens; outdated_elements = rev_deps';
    free_id = free_id'; story_machinery = story_machinery'; }

let raw_instance_number state ccs_l =
  let size cc =
    try
      ValMap.total (Connected_component.Map.find cc state.roots_of_ccs)
    with Not_found -> 0 in
  let rect_approx ccs =
    Array.fold_left (fun acc cc ->  acc * (size cc)) 1 ccs in
  List.fold_left (fun acc ccs -> acc + (rect_approx ccs)) 0 ccs_l
let instance_number state ccs_l =
  Nbr.I (raw_instance_number state @@ List.map fst ccs_l)

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

let update_outdated_activities ~get_alg store env counter state activities =
  (* I don't know if this is more efficient than computing the
  transitive closure of what should be updated and then updating them
  only once or not *)
  let rec aux deps =
    Operator.DepSet.iter
      (function
	| Operator.RULE i ->
	   let rule = Environment.get_rule env i in
	   let rate =
	     Nbr.to_float @@
	       value_alg
		 counter state ~get_alg rule.Primitives.rate in
	   let cc_va =
	     raw_instance_number state [rule.Primitives.connected_components] in
	   let () =
	     if !Parameter.debugModeOn then
	       Format.printf "@[Rule %a has now %i instances.@]@."
			     (Environment.print_rule ~env) i cc_va in
	   let act =
	     if cc_va = 0 then 0. else rate *. float_of_int cc_va in
	   store i act activities
	| Operator.ALG j -> aux (Environment.get_reverse_dependencies env j)
	| Operator.PERT _ -> assert false) deps in
  let () = aux (Environment.get_always_outdated env) in
  let () = aux state.outdated_elements in
  {state with outdated_elements = Operator.DepSet.empty }

let update_tokens ~get_alg counter state consumed injected =
  let do_op op l =
    List.iter
      (fun (expr,i) ->
       state.tokens.(i) <-
	 op state.tokens.(i) (value_alg ~get_alg counter state expr))
      l in
  let () = do_op Nbr.sub consumed in do_op Nbr.add injected

let transform_by_a_rule ~get_alg domain counter state event_kind rule inj =
  let () =
    update_tokens
      ~get_alg counter state rule.Primitives.consumed_tokens
      rule.Primitives.injected_tokens in
  update_edges counter domain inj state event_kind rule

let apply_rule ~get_alg domain counter state event_kind rule =
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
     Some
       (transform_by_a_rule ~get_alg domain counter state event_kind rule inj)
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

let force_rule ~get_alg domain counter state event_kind rule =
  match apply_rule ~get_alg domain counter state event_kind rule with
  | Some state -> state,None
  | None ->
     match all_injections state rule with
     | [] -> state,Some []
     | h :: t ->
	transform_by_a_rule ~get_alg domain counter state event_kind rule h,
	Some t

let print_injections ?sigs f roots_of_ccs =
  Format.fprintf
    f "@[<v>%a@]"
    (Pp.set Connected_component.Map.bindings Pp.space
	    (fun f (cc,roots) ->
	     Format.fprintf
	       f "@[# @[%a@] ==>@ @[%a@]@]"
	       (Connected_component.print ?sigs true) cc print_heap roots
	    )
    ) roots_of_ccs

let print env f state =
  Format.fprintf f "@[<v>%a@,%a@,%a@]"
		 (Edges.print (Environment.signatures env)) state.edges
		 (Pp.array Pp.space (fun i f el ->
				     Format.fprintf
				       f "%%init: %a <- %a"
				       (Environment.print_token ~env) i
				       Nbr.print el))
		 state.tokens
		 (print_injections ~sigs:(Environment.signatures env))
		 state.roots_of_ccs

let debug_print f state =
  Format.fprintf f "@[<v>%a@,%a@,%a@]"
		 Edges.debug_print state.edges
		 (Pp.array Pp.space (fun i f el ->
				     Format.fprintf f "token_%i <- %a"
						    i Nbr.print el))
		 state.tokens
		 (print_injections ?sigs:None) state.roots_of_ccs

let add_tracked cc event_kind tests state =
  match state.story_machinery with
  | None ->
     raise (ExceptionDefn.Internal_Error
	      (Location.dummy_annot "TRACK in non tracking mode"))
  | Some (tcc,x) ->
     { state with
       story_machinery =
	 Some (Connected_component.Map.add cc (event_kind,tests) tcc,x) }

let remove_tracked cc state =
  match state.story_machinery with
  | None ->
     raise (ExceptionDefn.Internal_Error
	      (Location.dummy_annot "TRACK in non tracking mode"))
  | Some (tcc,x) ->
     { state with
       story_machinery = Some (Connected_component.Map.remove cc tcc,x) }

let generate_stories logger env state =
  match state.story_machinery with
  | None -> ()
  | Some (_,(infos,steps)) ->
     Compression_main.compress_and_print logger env infos steps
