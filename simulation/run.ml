open Mods
open Tools
open ExceptionDefn
open Random_tree

let event state counter plot env =
	(*1. Time advance*)
	let dt = 
		let rd = Random.float 1.0 
		and activity = (*Activity.total*) Random_tree.total state.State.activity_tree 
		in
			-. (log rd /. activity) 
	in 
	if dt = infinity then raise Deadlock ; (*not so good, should check here that no more perturbation applies*)
	
	(*let t_plot = Profiling.start_chrono () in*)
	Plot.fill state counter plot env dt ; 
	Counter.inc_time counter dt ;
	
	(*updating activity of rule whose rate depends on time or event number*)
	let env,pert_ids = State.update_dep state Mods.EVENT IntSet.empty counter env in
	let env,pert_ids' = State.update_dep state Mods.TIME IntSet.empty counter env in
	let pert_ids = IntSet.union pert_ids' pert_ids in
	
	State.dump state counter env ;
	
	(*2. Draw rule*)
	if !Parameter.debugModeOn then Debug.tag "Drawing a rule...";
	(*let t_draw = Profiling.start_chrono () in*)
	let opt_instance,state = try State.draw_rule state counter env with 
		| Null_event -> (None,state)
	in			
	(*3. Apply rule & negative update*)
	(*let t_apply = Profiling.start_chrono () in*)
	let opt_new_state =
		match opt_instance with
			| None -> None
			| Some (r,embedding) ->
				(**********************************************)
				if !Parameter.debugModeOn then 
				begin
					Debug.tag
					(Printf.sprintf "Applying '%s' with embedding:" 
						(try Environment.rule_of_num r.Dynamics.r_id env with Not_found -> r.Dynamics.kappa)
					); 
					Array.iteri 
					(fun i inj_opt ->
						match inj_opt with
							| Some inj -> 
								if !Parameter.debugModeOn then Debug.tag 
								(Printf.sprintf "[%d,%d,%d]: %s" (Mixture.get_id r.Dynamics.lhs) i (Injection.get_address inj) (Injection.to_string inj))
							| None -> invalid_arg "Run.event"
					) embedding 
				end
				else () ;
				(********************************************)
				try Some (State.apply state r embedding counter env,r.Dynamics.r_id) with Null_event -> None
	
	in
	
	(*4. Positive update*)
	Counter.inc_events counter ;
	
	let env,state,pert_ids' = 
		match opt_new_state with
			| Some ((env,state,side_effect,phi,psi,pert_ids),r_id) -> 
				let phi' = Array.init (Array.length phi) (fun i -> match phi.(i) with Some inj -> Some (Injection.copy inj) | None -> None) in (*Not optimal but avoids bug caused by reusing embedding when a rule is activating oneself*)
				let env,state,pert_ids' = 
					State.positive_update state (State.rule_of_id r_id state) (phi',psi) (side_effect,Int2Set.empty) counter env
				in
					(env,state,IntSet.union pert_ids pert_ids')
			| None ->
				begin
					if !Parameter.debugModeOn then Debug.tag "Null (clash or doesn't satisfy constraints)"; 
					Counter.inc_null_events counter ; 
					(env,state,IntSet.empty)
				end
	in
	
	(*Applying perturbation if any*)
	let state,env = External.try_perturbate state (IntSet.union pert_ids pert_ids') counter env 
	in
	(*Profiling.add_chrono "Pert" Parameter.profiling t_pert ;*) 
	(state,env)
					
let rec loop state counter plot env =
	if !Parameter.debugModeOn then 
		Debug.tag (Printf.sprintf "[**Event %d (Activity %f)**]" counter.Counter.events (Random_tree.total state.State.activity_tree));
	if Counter.is_initial counter then
		begin (*Plotting first measure*)
			Counter.tick counter counter.Counter.time counter.Counter.events ;
			Plot.output state counter.Counter.time counter.Counter.events plot env counter
		end ;
	if (Counter.check_time counter) && (Counter.check_events counter) then
		let state,env = event state counter plot env 
		in
		loop state counter plot env
	else
		begin
			Plot.fill state counter plot env 0.0; (*Plotting last measures*)
			Plot.flush_ticks counter ;
			Plot.close plot
		end			