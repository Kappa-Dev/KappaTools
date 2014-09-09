open State
open Random_tree
open Graph
open Mods

exception Invariant_violation of string

type check_options = {rule_act : bool ; lifts : bool ; unary : bool}

let check n =
	let check_options = {rule_act = false ; lifts = false ; unary = false} in
	let check_options =
		if (n land 1) = 1 then {check_options with rule_act = true}
		else check_options
	in 
	let check_options =
		if (n land 2) = 2 then {check_options with lifts = true}
		else check_options
	in
	let check_options =
		if (n land 4) = 4 then {check_options with unary = true}
		else check_options
	in
	check_options

let check_invariants check_opt state counter env =
	try
		if check_opt.rule_act then
  	begin
  		Hashtbl.iter (*checking rule activities are OK*)
    	(fun r_id rule ->
    		let x = Random_tree.find r_id state.activity_tree in
    		let a2,a1 = State.eval_activity rule state counter env in
  			let alpha = Num.float_of_num (Num.add a2 a1) in
    		 	if x < alpha then 
    				if (IntSet.mem r_id state.silenced || Random_tree.is_infinite r_id state.activity_tree) then ()
    				else
      				let msg = Printf.sprintf "Activity of rule %s is underapproximated (%f < %f)" (Environment.rule_of_num r_id env) x alpha in
      				raise (Invariant_violation msg)
    			else
    				()
    	) state.rules
		end ;
		
		if check_opt.lifts then
			begin 
    	SiteGraph.fold 
    	(fun i u_i _ -> (*checking graph lifts are OK*)
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
  									let (r_id,cc_id) = Injection.get_coordinate inj in
      								if IntSet.mem r_id state.silenced then ()
      								else
    	  								raise (Invariant_violation "Injection is thrashed but is still pointed at") ; 
        					if not (LiftSet.mem inj lifts_base) then
    								raise 
    								(Invariant_violation 
    									(
    										Printf.sprintf "Injection (%d,%d) is missing in site '_' of node %d" ((fun (x,y)->x) (Injection.get_coordinate inj)) ((fun (x,y)->y) (Injection.get_coordinate inj)) j
    									)
    								)
        				) lnk_j () ;
    						LiftSet.fold
        				(fun inj _ ->
        					if Injection.is_trashed inj then
    								let (r_id,cc_id) = Injection.get_coordinate inj in
    								if IntSet.mem r_id state.silenced then ()
    								else
    									raise (Invariant_violation "Injection is thrashed but is still pointed at") ; 
    							if not (LiftSet.mem inj lifts_base) then
    								raise 
    								(Invariant_violation 
    									(
    										Printf.sprintf "Injection (%d,%d) is missing in site '_' of node %d" ((fun (x,y)->x) (Injection.get_coordinate inj)) ((fun (x,y)->y) (Injection.get_coordinate inj)) j
    									)
    								)
        				) int_j () ;
      					lifts_base
    					end
      		) u_i (LiftSet.empty())
    		in
    		()
    	) state.graph ()
		end ;
	
	if check_opt.unary then
		begin
			let ar = Array.init (Array.length state.nl_injections) (fun i -> 0) in
  		Array.iteri
  		(fun r_id injprod_hp_opt ->
  			(*let _ = try Environment.unary_rule_of_num r_id env with Not_found -> raise (Invariant_violation (Printf.sprintf "Rule %d is not unary" r_id)) 
  			in*)
  			match injprod_hp_opt with
  				| None -> ()
  				| Some injprod_hp -> 
  					let n = InjProdHeap.size injprod_hp in
  					ar.(r_id) <- n  
  		) state.nl_injections ;
			Printf.fprintf stderr "%E " (Counter.time counter) ;
			Array.iteri (fun _ n -> Printf.fprintf stderr "%d " n ) ar ;
			Printf.fprintf stderr "\n" ;	
		end 
		
	with
		| Invariant_violation msg -> (Parameter.debugModeOn := true; State.dump state counter env ; Printf.fprintf stderr "%s\n" msg ; exit (-1))
