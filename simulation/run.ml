open Mods
open Tools
open ExceptionDefn
open Random_tree

let event state grid counter plot env =
	(*1. Time advance*)
	let dt = 
		let rd = Random.float 1.0 
		and activity = (*Activity.total*) Random_tree.total state.State.activity_tree 
		in
		if activity < 0. then invalid_arg "Activity invariant violation" ;
			let dt = -. (log rd /. activity) in 
			if dt = infinity then 
				let depset = Environment.get_dependencies Mods.TIME env in
				DepSet.fold
				(fun dep dt ->
					match dep with
						| Mods.PERT p_id ->
							begin
								let pert_opt = try Some (IntMap.find p_id state.State.perturbations) with Not_found -> None
								in
								match pert_opt with
									| None -> dt
									| Some pert -> (match Mods.Counter.dT counter with Some dt -> dt | None -> Mods.Counter.last_increment counter) (*find_dt state pert counter env*) (*recherche dicho. pour connaitre la bonne valeur de t?*)
							end
						| _ -> dt
				) depset infinity
			else dt 
	in 
	if dt = infinity then raise Deadlock ; 
	
	Plot.fill state counter plot env dt ; 
	Counter.inc_time counter dt ;
	
	(*updating activity of rule whose rate depends on time or event number*)
	let env,pert_ids = State.update_dep state Mods.EVENT IntSet.empty counter env in
	let env,pert_ids = State.update_dep state Mods.TIME pert_ids counter env in
	
	State.dump state counter env ;
	
	(*2. Draw rule*)
	if !Parameter.debugModeOn then Debug.tag "Drawing a rule...";
	(*let t_draw = Profiling.start_chrono () in*)
	let opt_instance,state = try State.draw_rule state counter env with 
		| Null_event _ -> (None,state)
	in			
	(*3. Apply rule & negative update*)
	(*let t_apply = Profiling.start_chrono () in*)
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
				try Some (State.apply state r embedding_t counter env,r.Dynamics.r_id) with Null_event _ -> None
	
	in
	
	(*4. Positive update*)
	Counter.inc_events counter ;
	
	let env,state,pert_ids',grid = 
		match opt_new_state with
			| Some ((env,state,side_effect,phi,psi,pert_ids),r_id) ->
				
				counter.Counter.cons_null_events <- 0 ; (*resetting consecutive null event counter since a real rule was applied*)  
				let r = State.rule_of_id r_id state in
				let env,state,pert_ids' = 
					State.positive_update state r (phi,psi) (side_effect,Int2Set.empty) counter env
				in
					let grid = 
						if !Parameter.causalModeOn then
							Causal.record r.Dynamics.lhs (Some (r.Dynamics.pre_causal,side_effect,psi,false,r.Dynamics.r_id)) phi state counter false grid env
						else grid
					in
					(env,state,IntSet.union pert_ids pert_ids',grid)
			| None ->
				begin
					if !Parameter.debugModeOn then Debug.tag "Null (clash or doesn't satisfy constraints)"; 
					Counter.inc_null_events counter ; 
					Counter.inc_consecutive_null_events counter ;
					(env,state,IntSet.empty,grid)
				end
	in
	
	(*Applying perturbation if any*)
	let state,env = External.try_perturbate state (IntSet.union pert_ids pert_ids') counter env 
	in
	(*Profiling.add_chrono "Pert" Parameter.profiling t_pert ;*) 
	(state,grid,env)
					
let rec loop state grid counter plot env =
	if !Parameter.debugModeOn then 
		Debug.tag (Printf.sprintf "[**Event %d (Activity %f)**]" counter.Counter.events (Random_tree.total state.State.activity_tree));
	if Counter.is_initial counter then
		begin (*Plotting first measure*)
			Counter.tick counter counter.Counter.time counter.Counter.events ;
			Plot.output state counter.Counter.time counter.Counter.events plot env counter
		end ;
	if (Counter.check_time counter) && (Counter.check_events counter) then
		let state,grid,env = event state grid counter plot env 
		in
		loop state grid counter plot env
	else
		begin
			if !Parameter.causalModeOn then Causal.dump grid state env ;
			Plot.fill state counter plot env 0.0; (*Plotting last measures*)
			Plot.flush_ticks counter ;
			Plot.close plot
		end			