type jf_data =
  Compression_main.secret_log_info * Compression_main.secret_step list

type t = {
  roots_of_ccs: int ValMap.tree Connected_component.Map.t;
  unary_candidates: (int * int) ValMap.tree Mods.IntMap.t;
  unary_pathes: Edges.path Mods.Int2Map.t;
  edges: Edges.t;
  tokens: Nbr.t array;
  outdated_elements:
    Operator.DepSet.t * (Connected_component.t * int) list list;
  free_id: int;
  story_machinery :
    ((Causal.event_kind * Connected_component.t array *
	Instantiation.abstract Instantiation.test list) list
       Connected_component.Map.t (*currently tracked ccs *)
     * jf_data) option;
}

type result = Clash | Success of t | Corrected of t

let empty ~has_tracking env = {
    roots_of_ccs = Connected_component.Map.empty;
    unary_candidates = Mods.IntMap.empty;
    unary_pathes = Mods.Int2Map.empty;
    edges = Edges.empty;
    tokens = Array.make (Environment.nb_tokens env) Nbr.zero;
    outdated_elements = Operator.DepSet.empty,[];
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

let add_candidate cands pathes rule_id x y p =
  let a = min x y in
  let b = max x y in
  let va = try Mods.IntMap.find rule_id cands with Not_found -> ValMap.empty in
  (Mods.IntMap.add rule_id (ValMap.add (a,b) va) cands,
   Mods.Int2Map.add (a,b) p pathes)
let remove_candidate cands pathes rule_id x y =
  let a = min x y in
  let b = max x y in
  let va = ValMap.remove (a,b) (Mods.IntMap.find rule_id cands) in
  ((if ValMap.is_empty va then Mods.IntMap.remove rule_id cands
    else Mods.IntMap.add rule_id va cands),
   Mods.Int2Map.remove (a,b) pathes)

let from_place (inj_nodes,inj_fresh,free_id as inj2graph) = function
  | Place.Existing (n,id) ->
     (Connected_component.ContentAgent.get_sort n,
      Connected_component.Matching.get (n,id) inj_nodes,
      inj2graph)
  | Place.Fresh (ty,id) ->
     try (ty,Mods.IntMap.find id inj_fresh,inj2graph)
     with Not_found ->
       ty,free_id,(inj_nodes,Mods.IntMap.add id free_id inj_fresh,succ free_id)

let all_injections ?excp edges roots cca =
  Tools.array_fold_lefti
    (fun id inj_list cc ->
     ValMap.fold
       (fun root new_injs ->
	List.fold_left
	  (fun corrects inj ->
	   match Connected_component.Matching.reconstruct
		   edges inj id cc root with
	   | None -> corrects
	   | Some new_inj -> new_inj :: corrects)
	new_injs inj_list)
       (match excp with
	| Some (cc',root)
	     when Connected_component.is_equal_canonicals cc cc' ->
	   ValMap.add root ValMap.empty
	| (Some _ | None) ->
	   Connected_component.Map.find cc roots) [])
    [Connected_component.Matching.empty] cca

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

let store_obs edges roots obs acc = function
  | None -> acc
  | Some (tracked,_) ->
     List.fold_left
       (fun acc (cc,root) ->
	try
	  List.fold_left
	    (fun acc (ev,ccs,tests) ->
	     List.fold_left
	       (fun acc inj ->
		let tests' =
		  List.map (Instantiation.concretize_test
			      (fun p ->
			       let (_,x,_) =
				 from_place (inj,Mods.IntMap.empty,0) p in x))
			   tests in
		(ev,tests') :: acc)
	       acc (all_injections ~excp:(cc,root) edges roots ccs))
	    acc (Connected_component.Map.find cc tracked)
	with Not_found -> acc)
       acc obs

let update_edges counter domain inj_nodes state event_kind rule =
  (*Negative update*)
  let aux =
    List.fold_left
      (fun (inj2graph,edges,roots,(deps,created_obs)) transf ->
       (*inj2graph: abs -> conc, roots define the injection that is used*)
       let ((a,b,c,new_deps),_) =
	 deal_transformation false domain inj2graph edges roots transf in
       (a,b,c,(Operator.DepSet.union new_deps deps,created_obs)))
      ((inj_nodes,Mods.IntMap.empty,state.free_id), (*initial inj2graph: (existing,new,fresh_id) *)
       state.edges,state.roots_of_ccs,state.outdated_elements)
      rule.Primitives.removed (*removed: statically defined edges*)
  in
  (*Positive update*)
  let (((_,_,free_id' as final_inj2graph),edges',roots',rev_deps'),new_tracked_obs_instances) =
    List.fold_left
      (fun ((inj2graph,edges,roots,(deps,created_obs)),tracked_inst) transf ->
       let (a,b,c,new_deps),new_obs =
	 deal_transformation true domain inj2graph edges roots transf in
       let created_obs' =
	 match new_obs with [] -> created_obs | l -> l::created_obs in
       ((a,b,c,(Operator.DepSet.union deps new_deps,created_obs')),
	store_obs b c new_obs tracked_inst state.story_machinery))
      (aux,[])
      rule.Primitives.inserted (*statically defined edges*)
  in
  (*Store event*)
  let story_machinery' =
    store_event
      counter final_inj2graph new_tracked_obs_instances event_kind rule
      state.story_machinery in

  { roots_of_ccs = roots';
    unary_candidates = state.unary_candidates; unary_pathes = state.unary_pathes;
    edges = edges'; tokens = state.tokens; outdated_elements = rev_deps';
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
  Nbr.I (raw_instance_number state ccs_l)

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

let extra_outdated_var i state =
  let deps,created_obs = state.outdated_elements in
  {state with outdated_elements =
		(Operator.DepSet.add (Operator.ALG i) deps,created_obs)}

let new_unary_instances rule_id cc1 cc2 created_obs state =
  let (unary_candidates,unary_pathes) =
    List.fold_left
      (List.fold_left
	 (fun (cands,pathes as acc) (cc,id) ->
	  if Connected_component.is_equal_canonicals cc cc1 then
	    try
	      let goals = Connected_component.Map.find cc2 state.roots_of_ccs in
	      List.fold_left
		(fun (cands,pathes) (d,p) ->
		 add_candidate cands pathes rule_id id d p)
		acc
		(Edges.pathes_of_interrest
		   false (fun x -> ValMap.mem x goals) state.edges id)
	    with Not_found -> acc
	  else if Connected_component.is_equal_canonicals cc cc1 then
	    (cands,pathes)
	 else acc)
      ) (state.unary_candidates,state.unary_pathes) created_obs in
  {state with unary_candidates = unary_candidates;
	      unary_pathes = unary_pathes }

let update_outdated_activities ~get_alg store env counter state =
  let deps,created_obs = state.outdated_elements in
  let rec aux deps state =
    Operator.DepSet.fold
      (fun dep state ->
       match dep with
	| Operator.ALG j ->
	   aux (Environment.get_reverse_dependencies env j) state
	| Operator.PERT (-1) -> state (* TODO *)
	| Operator.PERT _ -> assert false
	| Operator.RULE i ->
	   let rule = Environment.get_rule env i in
	   let store_activity id rate cc_va =
	     let rate =
	       Nbr.to_float @@ value_alg counter state ~get_alg rate in
	     let () =
	       if !Parameter.debugModeOn then
		 Format.printf "@[%sule %a has now %i instances.@]@."
			       (if id mod 2 = 1 then "Unary r" else "R")
			       (Environment.print_rule ~env) (id/2) cc_va in
	     let act =
	       if cc_va = 0 then 0. else rate *. float_of_int cc_va in
	     store id rule.Primitives.syntactic_rule act in
	   let cc_va =
	     raw_instance_number state [rule.Primitives.connected_components] in
	   let () = store_activity (2*i) rule.Primitives.rate cc_va in
	   match rule.Primitives.unary_rate with
	   | None -> state
	   | Some unrate ->
	      let state' =
		new_unary_instances
		  i rule.Primitives.connected_components.(0)
		  rule.Primitives.connected_components.(1) created_obs state in
	      let va =
		try
		  ValMap.total (Mods.IntMap.find i state'.unary_candidates)
		with Not_found -> 0 in
	      let () = store_activity (2*i+1) unrate va in
	      state') deps state in
  let state' = aux (Environment.get_always_outdated env) state in
  let state'' = aux deps state' in
  {state'' with outdated_elements = (Operator.DepSet.empty,[]) }

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

let apply_unary_rule ~rule_id ~get_alg domain counter state event_kind rule =
  let root1,root2 =
    ValMap.random_val (Mods.IntMap.find rule_id state.unary_candidates) in
  let pair = (min root1 root2,max root1 root2) in
  let candidate = Mods.Int2Map.find pair state.unary_pathes in
  let cands,pathes = remove_candidate state.unary_candidates state.unary_pathes
				      rule_id root1 root2 in
  match Edges.are_connected ~candidate state.edges root1 root2 with
  | None ->
     let deps,created_obs = state.outdated_elements in
     let state' =
       {state with
	 unary_candidates = cands; unary_pathes = pathes;
	 outdated_elements =
	   (Operator.DepSet.add (Operator.RULE rule_id) deps,created_obs)} in
     Corrected state'
  | Some _path' ->
     let inj1 =
       Connected_component.Matching.reconstruct
	 state.edges Connected_component.Matching.empty 0
	 rule.Primitives.connected_components.(0) root1 in
     let inj =
       match inj1 with
       | None -> None
       | Some inj ->
	  Connected_component.Matching.reconstruct
	    state.edges inj 1 rule.Primitives.connected_components.(1) root2 in
     match inj with
     | None -> Clash
     | Some inj ->
	Success
	  (transform_by_a_rule ~get_alg domain counter state event_kind rule inj)

let apply_rule ?rule_id ~get_alg domain counter state event_kind rule =
  let inj,roots =
    Tools.array_fold_left_mapi
      (fun id inj cc ->
       let root =
	 ValMap.random_val
	   (Connected_component.Map.find cc state.roots_of_ccs) in
       (match inj with
       | Some inj ->
	  Connected_component.Matching.reconstruct state.edges inj id cc root
       | None -> None),root)
      (Some Connected_component.Matching.empty)
      rule.Primitives.connected_components in
  match inj with
  | None -> Clash
  | Some inj ->
     let out =
       transform_by_a_rule ~get_alg domain counter state event_kind rule inj
     in
     match rule.Primitives.unary_rate with
     | None -> Success out
     | Some _ ->
	try
	  let point = (min roots.(0) roots.(1), max roots.(0) roots.(1)) in
	  let candidate = Mods.Int2Map.find point state.unary_pathes in
	  match
	    Edges.are_connected ~candidate state.edges roots.(0) roots.(1) with
	  | None ->
	     let rid =
	       match rule_id with None -> assert false | Some rid -> rid in
	     let cands,pathes =
	       remove_candidate state.unary_candidates state.unary_pathes rid
				roots.(0) roots.(1) in
	     let state' =
	       {state with unary_candidates = cands; unary_pathes = pathes} in
	     Success (transform_by_a_rule
			~get_alg domain counter state' event_kind rule inj)
	  | Some p ->
	     let state' =
	       if p == candidate then state
	       else {state with unary_pathes =
				  Mods.Int2Map.add point p state.unary_pathes}
	     in Corrected state'
	with Not_found -> Success out

let force_rule ~get_alg domain counter state event_kind rule =
  match apply_rule ~get_alg domain counter state event_kind rule with
  | (Success out | Corrected out) -> out,None
  | Clash ->
     match all_injections
	     state.edges state.roots_of_ccs rule.Primitives.connected_components
     with
     | [] -> state,Some []
     | h :: t ->
	  (transform_by_a_rule ~get_alg domain counter state event_kind rule h),
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
  Format.fprintf
    f "@[<v>%a@,%a@]"
    (Edges.print (Environment.signatures env)) state.edges
    (Pp.array Pp.space (fun i f el ->
			Format.fprintf
			  f "%%init: %a <- %a"
			  (Environment.print_token ~env) i Nbr.print el))
    state.tokens

let debug_print f state =
  Format.fprintf f "@[<v>%a@,%a@,%a@]"
		 Edges.debug_print state.edges
		 (Pp.array Pp.space (fun i f el ->
				     Format.fprintf f "token_%i <- %a"
						    i Nbr.print el))
		 state.tokens
		 (print_injections ?sigs:None) state.roots_of_ccs

let add_tracked ccs event_kind tests state =
  match state.story_machinery with
  | None ->
     raise (ExceptionDefn.Internal_Error
	      (Location.dummy_annot "TRACK in non tracking mode"))
  | Some (tcc,x) ->
     let tcc' =
     Array.fold_left
       (fun tcc cc ->
	let acc =
	  try Connected_component.Map.find cc tcc
	  with Not_found -> [] in
	Connected_component.Map.add cc ((event_kind,ccs,tests)::acc) tcc)
       tcc ccs in
     { state with story_machinery = Some (tcc',x) }

let remove_tracked ccs state =
  match state.story_machinery with
  | None ->
     raise (ExceptionDefn.Internal_Error
	      (Location.dummy_annot "TRACK in non tracking mode"))
  | Some (tcc,x) ->
     let tester (_,el,_) =
       not @@
	 Tools.array_fold_lefti
	   (fun i b x -> b && Connected_component.is_equal_canonicals x el.(i))
	   true ccs in
     let tcc' =
     Array.fold_left
       (fun tcc cc ->
	let acc = Connected_component.Map.find cc tcc in
	match List.filter tester acc with
	| [] -> Connected_component.Map.remove cc tcc
	| l -> Connected_component.Map.add cc l tcc)
       tcc ccs in
     { state with story_machinery = Some (tcc',x) }

let generate_stories logger env state =
  match state.story_machinery with
  | None -> ()
  | Some (_,(infos,steps)) ->
     Compression_main.compress_and_print logger env infos (List.rev steps)
