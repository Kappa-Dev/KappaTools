open Dynamics
open State
open Tools
open ExceptionDefn
open Graph
open Mods
open LargeArray

let eval_pre_pert pert state counter env =
	match pert.stopping_time with
		| Some num -> let t = (Mods.Num.float_of_num num) in if t <= (Mods.Counter.time counter) then (Some t,true) else (None,false)
		| _ -> 
			match pert.precondition with
			| BCONST b -> (None,b)
			| BVAR b_fun -> 
				let act_of_id = (fun id -> (instance_number id state env)) (*act_of_id:functional argument*)
				and v_of_id = (fun id -> State.value state id counter env)
				and v_of_token id = 
					let x = try state.token_vector.(id) with _ -> failwith "External.eval_pre: Invalid token id"
					in Num.F x
				in
					(None,b_fun act_of_id v_of_id (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token)

let eval_abort_pert just_applied pert state counter env = 
	match pert.abort with
		| None -> just_applied
		| Some (BCONST b) -> b
		| Some (BVAR b_fun) -> 
			let act_of_id = (fun id -> (instance_number id state env)) (*act_of_id:functional argument*)
			and v_of_id = (fun id -> State.value state id counter env)
			and v_of_token id = 
				let x = try state.token_vector.(id) with _ -> failwith "External.eval_abort: Invalid token id"
				in Num.F x
			in
				b_fun act_of_id v_of_id (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token

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
					let n = State.value state ~var:v (-1) counter env in
					match n with
						| Num.I x -> (Printf.sprintf "%d" x)::cont
						| Num.F x -> (Printf.sprintf "%E" x)::cont
						| Num.I64 x -> (Printf.sprintf "%Ld" x)::cont
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
				let n = State.value state ~var:v (-1) counter env in
				match n with
					| Num.I x -> Printf.fprintf desc "%d" x
					| Num.F x -> Printf.fprintf desc "%E" x
					| Num.I64 x -> Printf.fprintf desc "%Ld" x
	) pexpr ;
	Printf.fprintf desc "\n"


let trigger_effect state env pert_ids tracked pert_events pert p_id eff eval_var snapshot counter =
		match eff with
			| (Some r,INTRO (v,mix)) -> 
				let x = eval_var v in
				if x = Num.F infinity then 
					let p_str = pert.flag in 
						invalid_arg ("Perturbation "^p_str^" would introduce an infinite number of agents, aborting...")
				else
					if !Parameter.debugModeOn then 
						Debug.tag (Printf.sprintf "Introducing %d instances of %s" (Num.int_of_num x) (Mixture.to_kappa false mix env)) ;
					let n = ref (Num.int_of_num x)
					and st = ref state
					and pert_ids = ref pert_ids
					and envr = ref env 
					and tracked = ref tracked
					and pert_events = ref pert_events
					in
						while !n > 0 do (*FIXME: highly unefficient to compute new injection at each loop*)
							let embedding_t = State.select_injection (infinity,None) (0.,None) state r.lhs counter env in (*empty embedding, cannot raise null-event*)
							let (env, state, side_effects, embedding_t, psi, pert_ids_neg) = State.apply !st r embedding_t counter env in
							let phi = State.map_of embedding_t in
							let env,state,pert_ids_pos,new_injs,tracked' = State.positive_update ~with_tracked:!tracked state r (phi,psi) (side_effects,Int2Set.empty) counter env
							in
							pert_events := (r,phi,psi,side_effects)::!pert_events ;
							if !n = (Num.int_of_num x) then pert_ids := IntSet.union !pert_ids (IntSet.union pert_ids_neg pert_ids_pos) ; (*only the first time*)
							st := state ;
							envr := env ;
							n := !n-1 ;
							tracked := tracked'
						done ;
						(!envr,!st,!pert_ids,!tracked,!pert_events)
			| (Some r,DELETE (v,mix)) ->
				let mix_id = Mixture.get_id mix in
				let instance_num = State.instance_number mix_id state env in
				let x = 
					let t = eval_var v in
					if t = (Num.F infinity) then instance_num else (Num.min t instance_num) 
				in
				let x= Num.int_of_num x in
				let cpt = ref 0 
				and st = ref state
				and pert_ids = ref pert_ids
				and envr = ref env
				and tracked = ref tracked
				and pert_events = ref pert_events
				in
					while !cpt < x do
						let opt = 
							try Some (State.select_injection (infinity,None) (0.,None) state mix counter env) with 
								| Not_found -> None (*Not found is raised if there is no more injection to draw in instances of mix*)
								| Null_event _ -> 
									if !Parameter.debugModeOn then Debug.tag "Clashing instance detected: building matrix";
									let matrix = State.instances_of_square mix_id (-1) state env in
										match matrix with
											| (embedding,_,_)::_ -> Some (CONNEX {map=embedding; roots = IntSet.empty ; components = None ; depth_map = None}) 
											| [] -> None
						in
							match opt with
								| None -> (if !Parameter.debugModeOn then Debug.tag "No more non clashing instances were found!" ; cpt:=x)
								| Some embedding_t ->
									let (env, state, side_effects, phi, psi, pert_ids_neg) = State.apply state r embedding_t counter env in
									let phi = State.map_of phi in
									let env,state,pert_ids_pos,new_injs,tracked' = State.positive_update ~with_tracked:!tracked state r (phi,psi) (side_effects,Int2Set.empty) counter env
									in
									pert_events := (r,phi,psi,side_effects)::!pert_events ;
									if !cpt=0 then pert_ids := IntSet.union !pert_ids (IntSet.union pert_ids_neg pert_ids_pos) ; (*only the first time*)
									st := state ;
									cpt := !cpt+1 ;
									envr := env ;
									tracked := tracked'
					done ;
					(!envr,!st,!pert_ids,!tracked,!pert_events)
			| (None,UPDATE_RULE (id,v)) -> 
				let _ =
					if !Parameter.debugModeOn then 
						(Debug.tag (Printf.sprintf "Updating rate of rule '%s'" (Environment.rule_of_num id env)) 
						)
				in
				let value = State.value state ~var:v (-1) counter env in (*Change here if one wants to have address passing style of assignation*)
				let r = State.rule_of_id id state in
  			Hashtbl.replace state.rules id {r with k_def = Dynamics.CONST value} ;
  			State.update_activity state p_id id counter env ;		
  			let env,pert_ids = State.update_dep state (-1) (RULE id) pert_ids counter env in
  			(env,state ,pert_ids,tracked,pert_events)
			| (None,UPDATE_VAR (id,v)) ->
				let _ =
					if !Parameter.debugModeOn then 
						(Debug.tag (Printf.sprintf "Updating variable '%s'" ((fun (x,_)->x) (Environment.alg_of_num id env)) ))
				in
				let value = State.value state ~var:v (-1) counter env in (*Change here if one wants to have address passing style of assignation*)
				State.set_variable id value state ;
				let env,pert_ids = State.update_dep state (-1) (ALG id) pert_ids counter env in
				(env,state,pert_ids,tracked,pert_events) 
			| (None,UPDATE_TOK (tk_id,v)) -> 
				let _ =
					if !Parameter.debugModeOn then 
						(Debug.tag (Printf.sprintf "Updating token '%s'" (Environment.token_of_num tk_id env)))
				in
				let value = State.value state ~var:v (-1) counter env in (*Change here if one wants to have address passing style of assignation*)
					begin
						try
							state.State.token_vector.(tk_id) <- (Num.float_of_num value) ;
							let env,pert_ids = State.update_dep state (-1) (TOK tk_id) pert_ids counter env in
							(env,state,pert_ids,tracked,pert_events) 
						with Invalid_argument _ -> failwith "External.apply_effect: invalid token id"
					end
			| (None,SNAPSHOT pexpr) -> 
				(
				let str = eval_pexpr pexpr state counter env in
				snapshot str ; (env, state ,pert_ids,tracked,pert_events)
				)
			| (None,PRINT (pexpr_file,pexpr)) ->
				let str = eval_pexpr pexpr_file state counter env in
				let desc = 
					match str with "" -> stdout | _ -> Environment.get_desc str env
				in
				dump_print_expr desc pexpr state counter env ;
				flush desc ;
				(env,state,pert_ids,tracked,pert_events)
			| (None,CFLOW id) -> 
				if !Parameter.debugModeOn then Debug.tag "Tracking causality" ;
				Parameter.causalModeOn := true ; 
				let env = if Environment.is_tracked id env then env else Environment.inc_active_cflows env in 
				let env = Environment.track id env in
				(env, state, pert_ids,tracked,pert_events)
			| (None,CFLOWOFF id) ->
				begin
					let env = Environment.dec_active_cflows env in
					let env = Environment.untrack id env in
					if Environment.active_cflows env = 0 then Parameter.causalModeOn := false ;
					(env,state,pert_ids,tracked,pert_events)
				end
			| (None,FLUXOFF pexpr) ->
				begin
					let str = eval_pexpr pexpr state counter env in
					let desc = match str with "" -> open_out !Parameter.fluxFileName | _ -> open_out str in
					Parameter.add_out_desc desc ;
					State.dot_of_flux desc state env ;
					close_out desc ;
					Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors) ;
					Parameter.fluxModeOn := false ;
					(env,state,pert_ids,tracked,pert_events)
				end
			| (None,STOP pexpr) ->
				(if !Parameter.debugModeOn then Debug.tag "Interrupting simulation now!" ;
				let str = eval_pexpr pexpr state counter env in
				snapshot str ;
				raise (ExceptionDefn.StopReached 
				(Printf.sprintf "STOP instruction was satisfied at (%d e,%f t.u)" (Counter.event counter) (Counter.time counter)))
				)
			| (None,FLUX pexpr) ->
				begin
					if !Parameter.fluxModeOn then ExceptionDefn.warning "Flux modes are overlapping" ;
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
		if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Taking a snapshot of current state (%s)" str) ;
		let ext = if !Parameter.dotOutput then "dot" else "ka" in
						
		let filename = 
			match str with 
				| "" -> 
					begin
						let name =
  						if (Filename.check_suffix (!Parameter.snapshotFileName) ext) then Filename.chop_extension (!Parameter.snapshotFileName)
  						else 
  							 !Parameter.snapshotFileName
						in
						name^"_"^(string_of_int (Counter.event counter))
					end
				| _ -> 
					let name = 
						if (Filename.check_suffix str ext) then Filename.chop_extension str
						else 
							 str
					in
					Filename.concat !Parameter.outputDirName name 
		in
		let file_exists = ref true in
		let cpt = ref 1 in
		let suffix = if !Parameter.dotOutput then ".dot" else ".ka" in
		let tmp_name = ref (filename^suffix) in
		while !file_exists do
			if Sys.file_exists !tmp_name then
				begin
					tmp_name := filename^"~"^(string_of_int !cpt)^suffix ;
					cpt := !cpt+1 
				end
			else
				file_exists := false
		done ;
		let desc = open_out !tmp_name
		in
		let hr = !Parameter.snapshotHighres in
		Parameter.openOutDescriptors := desc::(!Parameter.openOutDescriptors) ;
		State.snapshot state counter desc hr env ; (*could use a dedicated thread here*) 
		close_out desc ;
		Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors)
	in
	let act_of_id = (fun id -> (instance_number id state env))  (*act_of_id:functional argument*) 
	and v_of_id = (fun id -> State.value state id counter env)
	and v_of_token id = 
		let x = try state.token_vector.(id) with _ -> failwith "External.apply_effect: Invalid token id"
		in
		 Num.F x
	in
	let eval_var v =
		match v with
			| CONST f -> f
			| VAR v_fun -> v_fun act_of_id v_of_id (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token
	in
	let env, state, pert_ids,tracked,pert_events =
  	List.fold_left 
  	(fun (env, state, pert_ids,tracked,pert_events) effect -> 
  		let (env, state, pert_ids,tracked,pert_events) = 
  			try
  				trigger_effect state env pert_ids tracked pert_events pert p_id effect eval_var snapshot counter 
  			with ExceptionDefn.StopReached msg -> (counter.Counter.stop <- true ; Debug.tag msg ; (env, state, pert_ids,tracked,pert_events))
  		in
  		(env, state, pert_ids,tracked,pert_events)
  	) 
  	(env,state,IntSet.empty,tracked,pert_events) pert.effect
	in
	(env, state, pert_ids,tracked,pert_events)
					

let try_perturbate tracked state pert_ids pert_events counter env = 
	
	let rec iter state pert_ids tracked pert_events env = 
		let state,env,pert_ids',tracked,pert_events,stopping_time = 
			IntSet.fold 
			(fun pert_id (state,env,pert_ids,tracked,pert_events,stopping_time) ->
				let opt_pert = try Some (IntMap.find pert_id state.perturbations) with 
						| Not_found -> None
				in
				match opt_pert with
					| None -> (state,env,pert_ids,tracked,pert_events,None)
					| Some pert ->
						let state,pert_ids,tracked,pert_events,env,stopping_time' = 
							let stopping_time',trigger_pert = eval_pre_pert pert state counter env in
							let _ = match stopping_time' with 
								| Some t -> 
									(if !Parameter.debugModeOn then print_string 
									("Next event time is beyond perturbation time, applying null event and resetting clock to "^(string_of_float t)) ;
									 counter.Counter.time <- t) 
								| None -> () in
							if trigger_pert then
								begin
									if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "\n*************Applying perturbation %d***************" pert_id) ; 
									let env,state,pert_ids,tracked,pert_events = apply_effect pert_id pert tracked pert_events state counter env in
									if !Parameter.debugModeOn then Debug.tag "************End perturbation*************" ;
									let state,env = 
										if eval_abort_pert true pert state counter env then 
											(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "***Aborting pert[%d]***" pert_id) ;
											({state with perturbations = IntMap.remove pert_id state.perturbations},env) )
										else
											begin
												if !Parameter.debugModeOn then Debug.tag "************Maintaining perturbation*************" ; 
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
							(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "***Aborting pert[%d]***" pert_id) ;
							({state with perturbations = IntMap.remove pert_id state.perturbations},env,IntSet.remove pert_id pert_ids,tracked,pert_events,stopping_time))
						else 
							(state,env,pert_ids,tracked,pert_events,stopping_time)
			) 
			pert_ids (state,env,IntSet.empty,tracked,pert_events,None)
		in
			if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Should now try perturbations %s" (string_of_set string_of_int IntSet.fold pert_ids')) ;
			if IntSet.is_empty pert_ids' then 
				(state,env,tracked,pert_events,stopping_time)
			else
				iter state pert_ids' tracked pert_events env (*Chance of looping perturbation if user was not careful*)
  in
	iter state pert_ids tracked pert_events env 
