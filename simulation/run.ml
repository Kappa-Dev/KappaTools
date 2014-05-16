open Mods
open Tools
open ExceptionDefn
open Random_tree

let event state (*grid*) story_profiling event_list counter plot env =
	(*1. Time advance*)
	let dt,activity = 
		let rd = Random.float 1.0 
		and activity = (*Activity.total*) Random_tree.total state.State.activity_tree 
		in
		if activity < 0. then invalid_arg "Activity invariant violation" ;
			let dt = -. (log rd /. activity) in 
			if dt = infinity || activity <= 0. then
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
	let env,pert_ids_time = State.update_dep state (-1) Mods.TIME IntSet.empty counter env in
	
	State.dump state counter env ;
	
	(*Applying time dependant perturbation if any*)
	let state,env,obs_from_perturbation,pert_events,stopping_time = 
		External.try_perturbate [] state pert_ids_time [] counter env 
	in
	
	let restart = 
		match stopping_time with 
			| Some t -> (Counter.stat_null 5 counter ; true)
			| None -> false
	in
		
	(*2. Draw rule*)
	if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Drawing a rule... (activity=%f) " (Random_tree.total state.State.activity_tree));
	(*let t_draw = Profiling.start_chrono () in*)
	let opt_instance,state = 
		if restart then (None,state)
		else
			try State.draw_rule state counter env with 
				| Null_event i -> (Counter.stat_null i counter ; (None,state))
	in			
	
	(*3. Apply rule & negative update*)
	let opt_new_state =
		match opt_instance with
			| None -> None
			| Some (r,embedding_t) ->
				(**********************************************)
				if !Parameter.debugModeOn then 
				begin
					let version,embedding = match embedding_t with 
						| State.DISJOINT emb -> ("binary",emb.State.map) 
						| State.CONNEX emb -> ("unary",emb.State.map) 
						| State.AMBIGUOUS emb -> ("ambig.",emb.State.map)
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
	
	let env,state,pert_ids,story_profiling,event_list = 
		match opt_new_state with
			| Some ((env,state,side_effect,embedding_t,psi,pert_ids_rule),r) ->

				Counter.inc_events counter ;
				counter.Counter.cons_null_events <- 0 ; (*resetting consecutive null event counter since a real rule was applied*)  
				let env,pert_ids = State.update_dep state (-1) Mods.EVENT (IntSet.union pert_ids_rule pert_ids_time) counter env in
				
				(*Local positive update: adding new partial injection*)
				let env,state,pert_ids',new_injs,obs_from_rule_app = 
					State.positive_update state r (State.map_of embedding_t,psi) (side_effect,Int2Set.empty) counter env
				in
				
				(*Non local positive update: adding new possible intras*)
				let state = 
					if env.Environment.has_intra then 
						NonLocal.positive_update r embedding_t new_injs state counter env 
					else state 
				in

				if !Parameter.safeModeOn then Safe.check_invariants (Safe.check 4) state counter env ; 
												
				(****************END POSITIVE UPDATE*****************)
				
				(****************CFLOW PRODUCTION********************)
				let phi = State.map_of embedding_t in
				 
				let story_profiling,event_list = 
					if Environment.tracking_enabled env then (*if logging events is required*) 
					  begin
            let story_profiling,event_list = 
					  	Compression_main.D.S.PH.B.PB.CI.Po.K.store_event story_profiling (Compression_main.D.S.PH.B.PB.CI.Po.K.import_event ((r,phi,psi),(obs_from_rule_app,r,Counter.event counter,side_effect))) event_list 
	          in 
            (story_profiling,event_list) 
            end
          else
						(story_profiling,event_list)
        in 
        let story_profiling,event_list =
        	if Environment.tracking_enabled env && !Parameter.causalModeOn then (*if tracking the observable is required*) 
          	begin 
						let simulation_info = 
			        {Mods.story_id=  0 ;
			         Mods.story_time= counter.Mods.Counter.time ;
			         Mods.story_event= counter.Mods.Counter.events ;
			         Mods.profiling_info = ()}
			      in 
			      let story_profiling,event_list = 
			      	List.fold_left 
			       	(fun (story_profiling,event_list) (obs,phi) -> 
			        	let lhs = State.kappa_of_id obs state in 
			          Compression_main.D.S.PH.B.PB.CI.Po.K.store_obs story_profiling (obs,lhs,phi,simulation_info) event_list
							)
							(story_profiling,event_list) obs_from_rule_app
      			in 
  					(story_profiling,event_list)
						end
					else 
						(story_profiling,event_list)
				in
				(env,state,IntSet.union pert_ids pert_ids',story_profiling,event_list)
			| None ->
				begin
					if !Parameter.debugModeOn then Debug.tag "Null (clash or doesn't satisfy constraints)"; 
					Counter.inc_null_events counter ; 
					Counter.inc_consecutive_null_events counter ; 
					let env,pert_ids = State.update_dep state (-1) Mods.EVENT pert_ids_time counter env in
					(env,state,pert_ids,story_profiling,event_list)
				end
			(**************END CFLOW PRODUCTION********************)
				
	in
	
	
	(*Applying perturbation if any*)
	let state,env,obs_from_perturbation,pert_events,_ = 
		External.try_perturbate obs_from_perturbation state pert_ids pert_events counter env 
	in
	
	(*Adding perturbation event to story -if any*)
	let story_profiling,event_list,cpt = 
		if Environment.tracking_enabled env then (*if logging events is required*) 
		begin
			
			let story_profiling,event_list,cpt = 
				List.fold_left 
				(fun (story_prof,event_list,cpt) (r,phi,psi,side_effects) ->
					let sp,el =
						Compression_main.D.S.PH.B.PB.CI.Po.K.store_event story_prof
						(Compression_main.D.S.PH.B.PB.CI.Po.K.import_event 
						((r,phi,psi),(obs_from_perturbation,r,cpt+1,side_effects))) event_list (*we are adding several events with the same id in the grid!*)
						in
						(sp,el,cpt+1)
					) (story_profiling,event_list,Counter.event counter) pert_events
			  in 
		    (story_profiling,event_list,cpt) 
		  end
		  else
				(story_profiling,event_list,Counter.event counter)
		in 
		counter.Counter.perturbation_events <- cpt ;
		(state,story_profiling,event_list,env)
 						
let loop state story_profiling event_list counter plot env =
	(*Before entering the loop*)
	
	Counter.tick counter counter.Counter.time counter.Counter.events ;
	Plot.output state counter.Counter.time counter.Counter.events plot env counter ;
	
	(*Checking whether some perturbation should be applied before starting the event loop*)
	let env,pert_ids = State.update_dep state (-1) Mods.EVENT IntSet.empty counter env in
	let env,pert_ids = State.update_dep state (-1) Mods.TIME pert_ids counter env in
	let state,env,_,_,_ = External.try_perturbate [] state pert_ids [] counter env 
	in
	
	let rec iter state story_profiling event_list counter plot env =
		if !Parameter.debugModeOn then 
			Debug.tag (Printf.sprintf "[**Event %d (Activity %f)**]" counter.Counter.events (Random_tree.total state.State.activity_tree));
		if (Counter.check_time counter) && (Counter.check_events counter) && not (Counter.stop counter) then
			let state,story_profiling,event_list,env = 
				event state story_profiling event_list counter plot env 
			in
			iter state story_profiling event_list counter plot env
		else (*exiting the loop*)
		  begin
      	let _ = 
		      Plot.fill state counter plot env 0.0; (*Plotting last measures*)
		      Plot.flush_ticks counter ;
		      Plot.close plot
      	in 
        if Environment.tracking_enabled env then
					begin
	          let causal,weak,strong = (*compressed_flows:[(key_i,list_i)] et list_i:[(grid,_,sim_info option)...] et sim_info:{with story_id:int story_time: float ; story_event: int}*)
        	    if !Parameter.weakCompression || !Parameter.mazCompression || !Parameter.strongCompression (*if a compression is required*)
              then Compression_main.compress env state story_profiling event_list
              else None,None,None
	          in
	          let g prefix label x = 
	            match x with 
	              | None -> ()
	              | Some flows -> 
	                Causal.pretty_print Graph_closure.config_std prefix label flows state env
	          in 
	          let _ = g "" "" causal in 
	          let _ = g "Weakly" "weakly " weak in 
	          let _ = g "Strongly" "strongly " strong in 
	          ()
	        end
		  end
	in
	iter state story_profiling event_list counter plot env
	
