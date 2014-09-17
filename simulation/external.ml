open Dynamics
open State
open Tools
open ExceptionDefn
open Graph
open Mods
open LargeArray

let eval_pre_pert pert state counter env =
  match pert.stopping_time with
  | Some num ->
     let t = (Mods.Num.float_of_num num) in
     if t <= (Mods.Counter.time counter) then (Some t,true) else (None,false)
  | _ -> (None, State.value state counter env pert.precondition)

let eval_abort_pert just_applied pert state counter env =
  match pert.abort with
  | None -> just_applied
  | Some var -> State.value state counter env var

let eval_pexpr pexpr state counter env =
	let l =
		List.fold_left
		  (fun cont ast ->
		    match ast with
		    | Ast.Str_pexpr (str,p) -> str::cont
		    | Ast.Alg_pexpr alg -> 
		      let (x, is_constant, opt_v, dep, str) = Eval.partial_eval_alg env alg in
		      let v =
			if is_constant
			then (match opt_v with Some v -> Dynamics.CONST v | None -> invalid_arg "Eval.effects_of_modif")
			else Dynamics.VAR x 
		      in
		      let n = State.value state counter env v in
		      (Num.to_string n)::cont
		  ) [] pexpr
	in
	String.concat "" (List.rev l)

let dump_print_expr desc pexpr state counter env =
	List.iter
	(fun ast ->
		match ast with
			| Ast.Str_pexpr (str,p) -> Printf.fprintf desc "%s" str 
			| Ast.Alg_pexpr alg -> 
				let (x, is_constant, opt_v, dep, str) = Eval.partial_eval_alg env alg in
				let v =
						if is_constant
						then (match opt_v with Some v -> Dynamics.CONST v | None -> invalid_arg "Eval.effects_of_modif")
						else Dynamics.VAR x 
				in
				Num.print desc (State.value state counter env v)
	) pexpr ;
	Printf.fprintf desc "\n"

let apply_n_time x r state env counter pert_ids pert_events tracked =
  Num.iteri
    (fun n (env,state,pert_ids,with_tracked,pert_events as pack) ->
     try
       (*FIXME: highly unefficient to compute new injection at each loop*)
       let embedding_t =
	 try State.select_injection (infinity,None) (0.,None)
				    state r.lhs counter env
	 with Null_event _ ->
	      let mix_id = Mixture.get_id r.lhs in
	      if !Parameter.debugModeOn then
		Debug.tag "Clashing instance detected: building matrix";
	      match State.instances_of_square mix_id (-1) state env with
	      (*JK: un peu bete de generer la matrice pour ne prendre que la premiere injection*)
	      | (embedding,_,_)::_ -> Embedding.DISJOINT
					{Embedding.map=embedding;
					 Embedding.roots = IntSet.empty ;
					 Embedding.components = None ;
					 Embedding.depth_map = None}
	      | [] -> raise Not_found
       in (*empty embedding, cannot raise null-event*)
       let (env, state, side_effects, embedding_t, psi, pert_ids_neg) =
	 State.apply state r embedding_t counter env in
       let phi = State.Embedding.map_of embedding_t in
       let env,state,pert_ids_pos,new_injs,tracked' =
	 State.positive_update ~with_tracked state r (phi,psi)
			       (side_effects,Int2Set.empty) counter env
       in
       let pert_ids =
	 if Num.is_equal n x then (*only the first time*)
	   IntSet.union pert_ids (IntSet.union pert_ids_neg pert_ids_pos)
	 else pert_ids in
       (env,state,pert_ids,tracked',(r,phi,psi,side_effects)::pert_events)
     with Not_found ->
       let () =
	 Debug.tag_if_debug "No more non clashing instances were found!"
       in pack)
    (env,state,pert_ids,tracked,pert_events) x

let trigger_effect state env pert_ids tracked pert_events pert p_id eff snapshot counter =
  match eff with
  | (Some r,INTRO (v,mix)) ->
     let x = State.value state counter env v in
    if x = Num.F infinity then
      let p_str = pert.flag in
      invalid_arg
	("Perturbation "^p_str^" would introduce an infinite number of agents, aborting...")
    else
      let () =
	Debug.tag_if_debug "Introducing %a instances of %a"
			   Num.print x (Mixture.print false env) mix
      in apply_n_time x r state env counter pert_ids pert_events tracked
  | (Some r,DELETE (v,mix)) ->
     let mix_id = Mixture.get_id r.lhs in
     let instance_num = State.instance_number mix_id state env in
     let x = (Num.min (State.value state counter env v) instance_num) in
     apply_n_time x r state env counter pert_ids pert_events tracked
  | (None,UPDATE_RULE (id,v)) ->
     let () =
       Debug.tag_if_debug "Updating rate of rule '%a'"
			 (Environment.print_rule env) id
     in
     State.update_dep_value state counter env v (RULE id);
     let env,pert_ids =
       State.update_dep state ~cause:p_id (RULE id) pert_ids counter env in
     (env,state ,pert_ids,tracked,pert_events)
  | (None,UPDATE_VAR (id,v)) ->
     let () =
       Debug.tag_if_debug "Updating variable '%a'"
			  (Environment.print_alg env) id
     in
     State.update_dep_value state counter env v (ALG id);
     let env,pert_ids = State.update_dep state (ALG id) pert_ids counter env in
     (env,state,pert_ids,tracked,pert_events)
  | (None,UPDATE_TOK (tk_id,v)) ->
     let _ = Debug.tag_if_debug "Updating token '%a'"
				(Environment.print_token env) tk_id
    in
    (*Change here if one wants to have address passing style of assignation*)
    begin
      try
	State.update_dep_value state counter env v (TOK tk_id);
	let env,pert_ids =
	  State.update_dep state (TOK tk_id) pert_ids counter env in
	(env,state,pert_ids,tracked,pert_events)
      with Invalid_argument _ ->
	failwith "External.apply_effect: invalid token id"
    end
  | (None,SNAPSHOT pexpr) ->
      let str = eval_pexpr pexpr state counter env in
      snapshot str;
      (env, state ,pert_ids,tracked,pert_events)
  | (None,PRINT (pexpr_file,pexpr)) ->
    let str = eval_pexpr pexpr_file state counter env in
    let desc =
      match str with "" -> stdout | _ -> Environment.get_desc str env
    in
    dump_print_expr desc pexpr state counter env ;
    flush desc ;
    (env,state,pert_ids,tracked,pert_events)
  | (None,CFLOW id) ->
    Debug.tag_if_debug "Tracking causality" ;
    Parameter.causalModeOn := true;
    let env =
      if Environment.is_tracked id env then env
      else Environment.inc_active_cflows env in
    let env = Environment.track id env in
    (env, state, pert_ids,tracked,pert_events)
  | (None,CFLOWOFF id) ->
    begin
      let env = Environment.dec_active_cflows env in
      let env = Environment.untrack id env in
      if Environment.active_cflows env = 0 then Parameter.causalModeOn := false;
      (env,state,pert_ids,tracked,pert_events)
    end
  | (None,FLUXOFF pexpr) ->
    begin
      let str = eval_pexpr pexpr state counter env in
      let desc =
	match str with "" -> open_out !Parameter.fluxFileName | _ -> open_out str in
      Parameter.add_out_desc desc ;
      State.dot_of_flux desc state env ;
      close_out desc ;
      Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors) ;
      Parameter.fluxModeOn := false ;
      (env,state,pert_ids,tracked,pert_events)
    end
  | (None,STOP pexpr) ->
     Debug.tag_if_debug "Interrupting simulation now!" ;
     let str = eval_pexpr pexpr state counter env in
     snapshot str ;
     raise (ExceptionDefn.StopReached
	      (Printf.sprintf "STOP instruction was satisfied at (%d e,%f t.u)"
			      (Counter.event counter) (Counter.time counter)))
  | (None,FLUX pexpr) ->
    begin
      if !Parameter.fluxModeOn
      then ExceptionDefn.warning "Flux modes are overlapping" ;
      Parameter.fluxModeOn := true ;
      let nme = eval_pexpr pexpr state counter env in
      let _ =
	match nme with
	| "" -> Parameter.fluxFileName := "flux"^"_"^(string_of_int (Counter.event counter))
	| _ -> Parameter.fluxFileName := nme
      in
      Parameter.set Parameter.fluxFileName (Some "dot");
      (env, state, pert_ids,tracked,pert_events)
    end
  | _ -> invalid_arg "External.trigger_effect"

let apply_effect p_id pert tracked pert_events state counter env =
  let snapshot str =
    Debug.tag_if_debug "Taking a snapshot of current state (%s)" str;
    let ext = if !Parameter.dotOutput then "dot" else "ka" in
    let filename =
      if str = ""
      then !Parameter.snapshotFileName^"_"^(string_of_int (Counter.event counter))
      else str in
    let desc = open_out (Tools.find_available_name filename ext) in
    let hr = !Parameter.snapshotHighres in
    Parameter.openOutDescriptors := desc::(!Parameter.openOutDescriptors) ;
    State.snapshot state counter desc hr env; (*could use a dedicated thread here*)
    close_out desc ;
    Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors)
  in
  List.fold_left
    (fun (env, state, pert_ids,tracked,pert_events) effect ->
     try
       trigger_effect state env pert_ids tracked pert_events pert p_id effect
		      snapshot counter
     with ExceptionDefn.StopReached msg ->
       counter.Counter.stop <- true;
       Debug.tag msg;
       (env, state, pert_ids,tracked,pert_events)
    )
    (env,state,IntSet.empty,tracked,pert_events) pert.effect

let try_perturbate tracked state pert_ids pert_events counter env =
  let rec iter state pert_ids tracked pert_events env =
    let state,env,pert_ids',tracked,pert_events,stopping_time =
      IntSet.fold
	(fun pert_id (state,env,pert_ids,tracked,pert_events,stopping_time) ->
	 let opt_pert = State.maybe_find_perturbation pert_id state
	 in
	 match opt_pert with
	 | None -> (state,env,pert_ids,tracked,pert_events,None)
	 | Some pert ->
	    let state,pert_ids,tracked,pert_events,env,stopping_time' =
	      let stopping_time',trigger_pert = eval_pre_pert pert state counter env in
	      let () = match stopping_time' with
		| Some t ->
		   (Debug.tag_if_debug
		      "Next event time is beyond perturbation time, applying null event and resetting clock to %f" t ;
		    counter.Counter.time <- t)
		| None -> () in
	      if trigger_pert then
		begin
		  Debug.tag_if_debug
		    "\n*************Applying perturbation %d***************" pert_id;
		  let env,state,pert_ids,tracked,pert_events =
		    apply_effect pert_id pert tracked pert_events state counter env in
		  Debug.tag_if_debug "************End perturbation*************" ;
		  let state,env =
		    if eval_abort_pert true pert state counter env then
		      (Debug.tag_if_debug "***Aborting pert[%d]***" pert_id;
		    (State.remove_perturbation pert_id state,env))
		    else
		      begin
			Debug.tag_if_debug "************Maintaining perturbation*************" ; 
			(state,env)
		      end
		  in
		  (state,pert_ids,tracked,pert_events,env,stopping_time')
		end
	      else
		(state,pert_ids,tracked,pert_events,env,stopping_time')
	    in

	    let stopping_time = match stopping_time' with Some _ -> stopping_time' | None -> stopping_time
	    in
	    if eval_abort_pert false pert state counter env then
	      (Debug.tag_if_debug "***Aborting pert[%d]***" pert_id;
	       (State.remove_perturbation pert_id state,env,IntSet.remove pert_id pert_ids,tracked,pert_events,stopping_time))
	    else
	      (state,env,pert_ids,tracked,pert_events,stopping_time)
	)
	pert_ids (state,env,IntSet.empty,tracked,pert_events,None)
    in
    Debug.tag_if_debug "Should now try perturbations %a"
		       (Pp.set IntSet.elements Pp.colon Pp.int)
		       pert_ids';
    if IntSet.is_empty pert_ids' then
      (state,env,tracked,pert_events,stopping_time)
    else
      (*Chance of looping perturbation if user was not careful*)
      iter state pert_ids' tracked pert_events env
  in
  iter state pert_ids tracked pert_events env
