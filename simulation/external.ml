open Dynamics
open State
open Tools
open ExceptionDefn
open Graph
open Mods
open LargeArray

let eval_pre_pert pert state counter env = 
	match pert.precondition with
		| BCONST b -> b
		| BVAR b_fun -> 
			let act_of_id = (fun id -> (instance_number id state env)) (*act_of_id:functional argument*)
			and v_of_id = (fun id -> State.value state id counter env)
			and v_of_token id = try state.token_vector.(id) with _ -> failwith "External.eval_pre: Invalid token id"
			in
				b_fun act_of_id v_of_id (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token

let eval_abort_pert just_applied pert state counter env = 
	match pert.abort with
		| None -> just_applied
		| Some (BCONST b) -> b
		| Some (BVAR b_fun) -> 
			let act_of_id = (fun id -> (instance_number id state env)) (*act_of_id:functional argument*)
			and v_of_id = (fun id -> State.value state id counter env)
			and v_of_token id = try state.token_vector.(id) with _ -> failwith "External.eval_abort: Invalid token id"
			in
				b_fun act_of_id v_of_id (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token

let apply_effect p_id pert state counter env =
	let snapshot opt =
		if !Parameter.debugModeOn then Debug.tag "Taking a snapshot of current state" ;
		let filename = 
			match opt with 
				| None -> !Parameter.snapshotFileName^"_"^(string_of_int (Counter.event counter)) 
				| Some s -> (Filename.concat !Parameter.outputDirName s)^"_"^(string_of_int (Counter.event counter)^"_"^(string_of_int (Counter.null_event counter)))
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
	and v_of_token id = try state.token_vector.(id) with _ -> failwith "External.apply_effect: Invalid token id"
	in
	let eval_var v =
		match v with
			| CONST f -> f
			| VAR v_fun -> v_fun act_of_id v_of_id (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token
	in
		match pert.effect with
			| INTRO (v,mix) -> 
				let x = eval_var v in
				if x = infinity then 
					let p_str = pert.flag in 
						invalid_arg ("Perturbation "^p_str^" would introduce an infinite number of agents, aborting...")
				else
					if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Introducing %d instances of %s" (int_of_float x) (Mixture.to_kappa false mix env)) ;
					let r = 
						match Environment.rule_of_pert p_id env with 
							| None -> invalid_arg "External.apply_effect"
							| Some r_id -> try State.rule_of_id r_id state with Not_found -> invalid_arg "External.apply_effect"
					in
					let n = ref (int_of_float x)
					and st = ref state
					and pert_ids = ref IntSet.empty
					and envr = ref env 
					and tracked = ref []
					in
						while !n > 0 do (*FIXME: highly unefficient to compute new injection at each loop*)
							let embedding_t = State.select_injection (infinity,0.) state r.lhs counter env in (*empty embedding, cannot raise null-event*)
							let (env, state, side_effects, embedding_t, psi, pert_ids_neg) = State.apply !st r embedding_t counter env in
							let env,state,pert_ids_pos,new_injs,tracked' = State.positive_update state r (State.map_of embedding_t,psi) (side_effects,Int2Set.empty) counter env
							in
							if !n = (int_of_float x) then pert_ids := IntSet.union !pert_ids (IntSet.union pert_ids_neg pert_ids_pos) ; (*only the first time*)
							st := state ;
							envr := env ;
							n := !n-1 ;
							tracked := tracked'@(!tracked)
						done ;
						(!envr,!st,!pert_ids,!tracked)
			| DELETE (v,mix) ->
				let mix_id = Mixture.get_id mix in
				let instance_num = State.instance_number mix_id state env in
				let r = 
					match Environment.rule_of_pert p_id env with 
						| None -> invalid_arg "External.apply_effect"
						| Some r_id -> try State.rule_of_id r_id state with Not_found -> invalid_arg "External.apply_effect"
				in
				let x = 
					let t = eval_var v in
					if t=infinity then int_of_float instance_num else int_of_float (min t instance_num) 
				in
				let cpt = ref 0 
				and st = ref state
				and pert_ids = ref IntSet.empty
				and envr = ref env
				and tracked = ref []
				in
					while !cpt<x do
						let opt = 
							try Some (State.select_injection (infinity,0.) state mix counter env) with 
								| Not_found -> None (*Not found is raised if there is no more injection to draw in instances of mix*)
								| Null_event _ -> 
									if !Parameter.debugModeOn then Debug.tag "Clashing instance detected: building matrix";
									let matrix = State.instances_of_square mix_id state env in
										match matrix with
											| (embedding,_,_)::_ -> Some (CONNEX {map=embedding; roots = IntSet.empty ; components = None ; depth_map = None}) 
											| [] -> None
						in
							match opt with
								| None -> (if !Parameter.debugModeOn then Debug.tag "No more non clashing instances were found!" ; cpt:=x)
								| Some embedding_t ->
									let (env, state, side_effects, phi, psi, pert_ids_neg) = State.apply state r embedding_t counter env in
									let env,state,pert_ids_pos,new_injs,tracked' = State.positive_update state r (State.map_of phi,psi) (side_effects,Int2Set.empty) counter env
									in
									if !cpt=0 then pert_ids := IntSet.union !pert_ids (IntSet.union pert_ids_neg pert_ids_pos) ; (*only the first time*)
									st := state ;
									cpt := !cpt+1 ;
									envr := env ;
									tracked := tracked'@(!tracked)
					done ;
					(!envr,!st,!pert_ids,!tracked)
			| UPDATE (id,v) -> 
				let _ =
					if !Parameter.debugModeOn then 
						(if Environment.is_rule id env then 
							Debug.tag (Printf.sprintf "Updating rate of rule '%s'" (Environment.rule_of_num id env)) 
						else Debug.tag (Printf.sprintf "Updating variable '%s'" ((fun (x,_)->x) (Environment.alg_of_num id env)) ))
				in
				let value = State.value state ~var:v (-1) counter env in (*Change here if one wants to have address passing style of assignation*)
				if Environment.is_rule id env then
					begin
						let r = State.rule_of_id id state in
							Hashtbl.replace state.rules id {r with k_def = Dynamics.CONST value} ;
							State.update_activity state p_id id counter env ;		
							let env,pert_ids = State.update_dep state (RULE id) IntSet.empty counter env in
							(env,state ,pert_ids,[])
					end
				else (*updating a variable*)
					begin
						State.set_variable id value state ;
						let env,pert_ids = State.update_dep state (ALG id) IntSet.empty counter env in
						(env,state,pert_ids,[]) 
					end
			| UPDATE_TOK (tk_id,v) -> 
				let _ =
					if !Parameter.debugModeOn then 
						(Debug.tag (Printf.sprintf "Updating token '%s'" (Environment.token_of_num tk_id env)))
				in
				let value = State.value state ~var:v (-1) counter env in (*Change here if one wants to have address passing style of assignation*)
					begin
						try
							state.State.token_vector.(tk_id) <- value ;
							let env,pert_ids = State.update_dep state (TOK tk_id) IntSet.empty counter env in
							(env,state,pert_ids,[]) 
						with Invalid_argument _ -> failwith "External.apply_effect: invalid token id"
					end
			| SNAPSHOT opt -> (snapshot opt ; (env, state ,IntSet.empty,[]))
			| CFLOW id -> 
				if !Parameter.debugModeOn then Debug.tag "Tracking causality" ;
				Parameter.causalModeOn := true ; 
				let env = if Environment.is_tracked id env then env else Environment.inc_active_cflows env in 
				let env = Environment.track id env in
				(env, state, IntSet.empty,[])
			| CFLOWOFF id ->
				begin
					let env = Environment.dec_active_cflows env in
					let env = Environment.untrack id env in
					if Environment.active_cflows env = 0 then Parameter.causalModeOn := false ;
					(env,state,IntSet.empty,[])
				end
			| FLUXOFF opt ->
				begin
					let desc = match opt with None -> open_out !Parameter.fluxFileName | Some nme -> open_out nme in
					Parameter.add_out_desc desc ;
					State.dot_of_flux desc state env ;
					close_out desc ;
					Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors) ;
					Parameter.fluxModeOn := false ;
					(env,state,IntSet.empty,[])
				end
			| STOP opt ->
				(if !Parameter.debugModeOn then Debug.tag "Interrupting simulation now!" ;
				snapshot opt ;
				raise (ExceptionDefn.StopReached (Printf.sprintf "STOP instruction was satisfied at event %d" (Counter.event counter)))
				)
			| FLUX opt ->
				begin
					if !Parameter.fluxModeOn then ExceptionDefn.warning "Flux modes are overlapping" ;
					Parameter.fluxModeOn := true ;
					begin
						match opt with
							| None -> Parameter.fluxFileName := "flux"^"_"^(string_of_int (Counter.event counter))^".dot"
							| Some nme -> Parameter.fluxFileName := nme
					end ;
					(env, state, IntSet.empty,[])
				end
				

let rec try_perturbate state pert_ids counter env = 
	let state,env,pert_ids',tracked = 
		IntSet.fold 
		(fun pert_id (state,env,pert_ids,tracked) ->
			let opt_pert = try Some (IntMap.find pert_id state.perturbations) with 
					| Not_found -> None
			in
			match opt_pert with
				| None -> (state,env,pert_ids,[])
				| Some pert ->
					let state,pert_ids,tracked,env = 
						if eval_pre_pert pert state counter env then
							begin
								if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "\n*************Applying perturbation %d***************" pert_id) ; 
								let env,state,pert_ids,tracked = apply_effect pert_id pert state counter env in
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
								(state,pert_ids,tracked,env)
							end
						else
							(state,pert_ids,tracked,env)
					in				
					if eval_abort_pert false pert state counter env then
						(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "***Aborting pert[%d]***" pert_id) ;
						({state with perturbations = IntMap.remove pert_id state.perturbations},env,IntSet.remove pert_id pert_ids,tracked))
					else (state,env,pert_ids,tracked)
		) 
		pert_ids (state,env,IntSet.empty,[])
	in
		if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Should now try perturbations %s" (string_of_set string_of_int IntSet.fold pert_ids')) ;
		if IntSet.is_empty pert_ids' then (state,env,[])
		else
			try_perturbate state pert_ids' counter env (*Chance of looping perturbation if user was not careful*)
	
