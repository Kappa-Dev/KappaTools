open Mods
open Tools
open ExceptionDefn
open Random_tree

let event state (*grid*) event_list counter plot env =
	(*1. Time advance*)
	let dt,activity = 
		let rd = Random.float 1.0 
		and activity = (*Activity.total*) Random_tree.total state.State.activity_tree 
		in
		if activity < 0. then invalid_arg "Activity invariant violation" ;
			let dt = -. (log rd /. activity) in 
			if dt = infinity or activity <= 0. then
				let depset = Environment.get_dependencies Mods.TIME env in
				DepSet.fold
				(fun dep (dt,activity) ->
					match dep with
						| Mods.PERT p_id ->
							begin
								let pert_opt = try Some (IntMap.find p_id state.State.perturbations) with Not_found -> None
								in
								match pert_opt with
									| None -> (dt,activity)
									| Some pert -> 
										(match Mods.Counter.dT counter with Some dt -> (dt,activity) | None -> (Mods.Counter.last_increment counter,activity)) (*find_dt state pert counter env*) (*recherche dicho. pour connaitre la bonne valeur de t?*)
							end
						| _ -> (dt,activity)
				) depset (infinity,0.)
			else (dt,activity) 
	in 
	if dt = infinity || activity = 0. then 
		begin
			if !Parameter.dumpIfDeadlocked then	
				let desc = if !Parameter.dotOutput then open_out "deadlock.dot" else open_out "deadlock.ka" in
				State.snapshot state counter desc true env
			else () ;
			raise Deadlock
		end ; 
	Plot.fill state counter plot env dt ; 
	Counter.inc_time counter dt ;
	
	(*updating activity of rule whose rate depends on time or event number*)
	(*let env,pert_ids = State.update_dep state Mods.EVENT IntSet.empty counter env in*)
	let env,pert_ids_time = State.update_dep state Mods.TIME IntSet.empty counter env in
	
	State.dump state counter env ;
	
	(*2. Draw rule*)
	if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Drawing a rule... (activity=%f) " (Random_tree.total state.State.activity_tree));
	(*let t_draw = Profiling.start_chrono () in*)
	let opt_instance,state = try State.draw_rule state counter env with 
		| Null_event _ -> (None,state)
	in			
	
	(*3. Apply rule & negative update*)
	let opt_new_state =
		match opt_instance with
			| None -> None
			| Some (r,embedding_t) ->
				(**********************************************)
				if !Parameter.debugModeOn then 
				begin
					let version,embedding = match embedding_t with State.DISJOINT emb -> ("binary",emb.State.map) | State.CONNEX emb -> ("unary",emb.State.map) | State.AMBIGUOUS emb -> ("ambig.",emb.State.map)
					in 
					Debug.tag
					(Printf.sprintf "Applying %s version of '%s' with embedding:" version 
						(try Environment.rule_of_num r.Dynamics.r_id env with Not_found -> r.Dynamics.kappa)
					); 
					Debug.tag (Printf.sprintf "%s" (string_of_map string_of_int string_of_int IntMap.fold embedding)) 
				end
				else () ;
				(********************************************)
				try Some (State.apply state r embedding_t counter env,r) with Null_event _ -> None
	
	in
	
	(*4. Positive update*)
	
	let env,state,pert_ids,(*grid,*)event_list = 
		match opt_new_state with
			| Some ((env,state,side_effect,embedding_t,psi,pert_ids_rule),r) ->

				Counter.inc_events counter ;
				counter.Counter.cons_null_events <- 0 ; (*resetting consecutive null event counter since a real rule was applied*)  
				let env,pert_ids = State.update_dep state Mods.EVENT (IntSet.union pert_ids_rule pert_ids_time) counter env in
				
				
				
				(*Local positive update: adding new partial injection*)
				let env,state,pert_ids',new_injs,obs_from_rule_app = 
					State.positive_update state r (State.map_of embedding_t,psi) (side_effect,Int2Set.empty) counter env
				in
				
				(*Non local positive update: adding new possible intras*)
				let state = if env.Environment.has_intra then NonLocal.positive_update r embedding_t new_injs state counter env else state 
				in
				
				(****************END POSITIVE UPDATE*****************)
				let phi = State.map_of embedding_t in
				
				(*let event_to_record = (r,side_effect,phi,psi,Counter.event counter) in*)
				 
				let (*grid,*)event_list = 
					if !Parameter.causalModeOn or !Parameter.weakcompressionModeOn 
	        then
					  begin
                                            let event_list = Compression_main.S.PH.B.PB.K.store_event (Compression_main.S.PH.B.PB.K.import_event ((r,phi,psi),(obs_from_rule_app,r,Counter.event counter,side_effect))) event_list in 
                                            let event_list = 
                                              List.fold_left 
                                                (fun event_list (obs,phi) -> 
                                                  
                                                  let lhs = State.kappa_of_id obs state in 
                                                  Compression_main.S.PH.B.PB.K.store_obs (obs,lhs,phi) event_list)
                                                event_list obs_from_rule_app
                                            in 
					 (*   (Causal.record ~decorate_with:obs_from_rule_app r side_effect (phi,psi) (Counter.event counter) grid env, (*to be removed*)*)
					     event_list
					  end
					else event_list
				in
				(env,state,IntSet.union pert_ids pert_ids',event_list)
			| None ->
				begin
					if !Parameter.debugModeOn then Debug.tag "Null (clash or doesn't satisfy constraints)"; 
					Counter.inc_null_events counter ; 
					Counter.inc_consecutive_null_events counter ;
					(*if counter.Counter.cons_null_events > !Parameter.maxConsecutiveClash then 
						raise Deadlock
					else*) 
					(env,state,IntSet.empty,(*grid,*)event_list)
				end
	in
	
	(*Applying perturbation if any*)
	(*Printf.printf "Applying %s perturbations \n" (Tools.string_of_set string_of_int IntSet.fold pert_ids) ;*)
	let state,env,obs_from_perturbation = External.try_perturbate state pert_ids counter env (*shoudl add obs_from_pert in the causal flow at some point...*)
	in
	(state(*,grid*),event_list,env)
					
let loop state grid event_list counter plot env =
	(*Before entering the loop*)
	
	Counter.tick counter counter.Counter.time counter.Counter.events ;
	Plot.output state counter.Counter.time counter.Counter.events plot env counter ;
	
	(*Checking whether some perturbation should be applied before starting the event loop*)
	let env,pert_ids = State.update_dep state Mods.EVENT IntSet.empty counter env in
	let env,pert_ids = State.update_dep state Mods.TIME pert_ids counter env in
	let state,env,_ = External.try_perturbate state pert_ids counter env 
	in
	
	let rec iter state event_list counter plot env =
		if !Parameter.debugModeOn then 
			Debug.tag (Printf.sprintf "[**Event %d (Activity %f)**]" counter.Counter.events (Random_tree.total state.State.activity_tree));
		if (Counter.check_time counter) && (Counter.check_events counter) then
			let state,event_list,env = event state event_list counter plot env 
			in
			iter state event_list counter plot env
		else (*exiting the loop*)
		  begin
      	let _ = 
		      Plot.fill state counter plot env 0.0; (*Plotting last measures*)
		      Plot.flush_ticks counter ;
		      Plot.close plot
      	in 
        if Environment.tracking_enabled env then
					begin
				    let _ = 
            	if !Parameter.weakcompressionModeOn or !Parameter.causalModeOn 
                then Compression_main.weak_compression env state event_list 
	          in
            ()
				  end
		  end
	in
	iter state event_list counter plot env
