open State
open Random_tree
open Graph
open Mods

(*type implicit_state =
	{ graph : SiteGraph.t;
	 	injections : (component_injections option) array;
		nl_injections : (InjProdHeap.t option) array ;
		rules : (int, rule) Hashtbl.t; 
		perturbations : perturbation IntMap.t;
		kappa_variables : (Mixture.t option) array;
		token_vector : float array ; 
		alg_variables : (Dynamics.variable option) array;
		observables : obs list; 
		influence_map : (int, (int IntMap.t list) IntMap.t) Hashtbl.t ;
		mutable activity_tree : Random_tree.tree; 
		wake_up : Precondition.t ;
		flux : (int,float IntMap.t) Hashtbl.t ;
		mutable silenced : IntSet.t (*Set of rule ids such that eval-activity was overestimated and whose activity was manually set to a lower value*) 
	}
and component_injections = (InjectionHeap.t option) array
and obs = { label : string; expr : Dynamics.variable }
*)

exception Invariant_violation of string

let check_invariants state counter env =
	Hashtbl.iter
	(fun r_id rule ->
		let x = Random_tree.find r_id state.activity_tree in
		let a2,a1 = State.eval_activity rule state counter env in
		 	if x < (a2 +. a1) then 
				let msg = Printf.sprintf "Activity of rule %s is underapproximated" (Environment.rule_of_num r_id env) in
				raise (Invariant_violation msg)
			else
				()
	) state.rules ; 
	SiteGraph.fold 
	(fun i u_i _ ->
		let _ = 
  		Node.fold_dep 
  		(fun j (int_j,lnk_j) lifts_base ->
  			if j=0 then
  				begin
    				let str = Environment.site_of_id (Node.name u_i) 0 env in
    				if str <> "_" then raise (Invariant_violation "Site 0 should be '_'") ;
    				lnk_j
  				end
  			else
					begin
    				LiftSet.fold
    				(fun inj _ ->
							if Injection.is_trashed inj then
								raise (Invariant_violation "Injection is thrashed but is still pointed at") ; 
    					if not (LiftSet.mem inj lifts_base) then
								(State.dump state counter env ; 
								raise 
								(Invariant_violation 
									(
										Printf.sprintf "Injection (%d,%d) is missing in site '_' of node %d" ((fun (x,y)->x) (Injection.get_coordinate inj)) ((fun (x,y)->y) (Injection.get_coordinate inj)) j
									)
								)
								)
    				) lnk_j () ;
						LiftSet.fold
    				(fun inj _ ->
    					if Injection.is_trashed inj then
								raise (Invariant_violation "Injection is thrashed but is still pointed at") ; 
							if not (LiftSet.mem inj lifts_base) then
								(State.dump state counter env ; 
								raise 
								(Invariant_violation 
									(
										Printf.sprintf "Injection (%d,%d) is missing in site '_' of node %d" ((fun (x,y)->x) (Injection.get_coordinate inj)) ((fun (x,y)->y) (Injection.get_coordinate inj)) j
									)
								)
								)
    				) int_j () ;
  					lifts_base
					end
  		) u_i (LiftSet.empty())
		in
		()
	) state.graph ()