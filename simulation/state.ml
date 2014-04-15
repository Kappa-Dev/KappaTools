open Mods
open Tools
open ExceptionDefn
open Dynamics
open Graph
open ValMap
open Random_tree


type implicit_state =
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


let silence rule_id state = state.silenced <- (IntSet.add rule_id state.silenced)
			
let kappa_of_id id state =
	try
		match state.kappa_variables.(id) with
		| None -> raise Not_found
		| Some mix -> mix
	with | Invalid_argument msg -> invalid_arg ("State.kappa_of_id: " ^ msg)

let rule_of_id id state = Hashtbl.find state.rules id

let alg_of_id id state =
	try
		match state.alg_variables.(id) with
		| None -> raise Not_found
		| Some var_f -> var_f
	with | Invalid_argument msg -> invalid_arg ("State.kappa_of_id: " ^ msg)

(*should use one representative of each cc of mixture in [set] in order to be more efficient*)
let connex ?(d_map = IntMap.empty) ?(filter = false) ?start_with (roots,codomain) (radius:int) with_full_components state env =  
	let start_root = match start_with with Some r -> r | None -> IntSet.choose roots in
	let (is_connex,d_map,component,remaining_roots) = 
		if filter then
			let predicate env = fun node -> Environment.is_nl_root (Node.name node) env
			in  
			SiteGraph.neighborhood ~check_connex:(roots,codomain) ~complete_construction:with_full_components ~d_map:d_map ~filter_elements:(predicate env) state.graph start_root radius 
		else
			SiteGraph.neighborhood ~check_connex:(roots,codomain) ~complete_construction:with_full_components ~d_map:d_map state.graph start_root radius
	in 
	(is_connex,d_map,component,remaining_roots)
	

let update_flux state id1 id2 w = 
	let flux = state.flux in
	let map = try Hashtbl.find flux id1 with Not_found -> IntMap.empty
	in
	let w' = try IntMap.find id2 map with Not_found -> 0.0
	in
	Hashtbl.replace flux id1 (IntMap.add id2 (w+.w') map)
	
let is_complete mix_id state =
	match state.injections.(mix_id) with
		| None -> false
		| Some comp_injs ->
			Array.fold_left 
			(fun is_complete opt -> 
				match opt with 
					| Some injs -> ((InjectionHeap.size injs) <> 0) && is_complete
					| None -> false
			) true comp_injs

(**[instance_number mix_id state] returns the number of instances of mixture [mix_id] in implicit state [state]*)
let instance_number mix_id state env =
	if Environment.is_empty_lhs mix_id env then (Num.I64 Int64.one) 
	else
		match state.injections.(mix_id) with
		| None -> (Num.I64 Int64.zero)
		| Some component_injections ->
			let act =
				Array.fold_left
					(fun act opt ->
								match opt with
								| Some injs -> let n = InjectionHeap.size injs in if n=0 then Int64.zero else (Int64.mul act (Int64.of_int n))
								| None -> Int64.zero
					)
					Int64.one component_injections
			in
			(Num.I64 act)

(**[nl_instance_number mix_id state] returns the number of instances of non local mixture [mix_id] in implicit state [state]*)
let nl_instance_number mix_id state env =
	if Environment.is_empty_lhs mix_id env then (Num.I 1) 
	else
		match state.nl_injections.(mix_id) with
		| None -> (Num.I 0)
		| Some inj_prod_hp -> Num.I (InjProdHeap.size inj_prod_hp)


(**[instances_of_square mix_id state] returns [(inj_0,codom_0,prod_0);...] the list of full and valid embeddings inj_i, their codomains codom_j and the explicit product prod_i=[phi_cc0;phi_cc1;...]*)
let instances_of_square ?(disjoint=false) mix_id radius_def state env =
	let extend (inj, codom) phi =
		try
			Some
			(Injection.fold
					(fun i j (inj, codom) ->
								if IntSet.mem j codom
								then raise False
								else ((IntMap.add i j inj), (IntSet.add j codom))
					)
					phi (inj, codom))
		with | False -> None
	in
	let embeddings = 
		match state.injections.(mix_id) with
		| None -> [] (*mix_id has no cc injection*)
		| Some comp_injs ->
			try
				Array.fold_left (*fold comp_injs*)
					(fun m opt ->
								match opt with
								| None -> raise Not_found (*this cc injection is missing*)
								| Some injhp ->
										List.fold_left
										(fun cont (part_inj, part_codom, part_injs) ->
													let ext_injhp =
														InjectionHeap.fold
														(fun _ phi cont' ->
																	let opt = extend (part_inj, part_codom) phi
																	in
																	match opt with
																	| None -> cont'
																	| Some (ext_inj, ext_codom) ->
																			(ext_inj, ext_codom, phi::part_injs) :: cont'
														)	injhp []
													in 
													ext_injhp@cont
										) [] m
					)	[ (IntMap.empty, IntSet.empty, []) ] comp_injs
		with Not_found -> []
	in
	(*let embeddings = List.fold_left (fun cont l -> l@cont) [] embeddings in *)
	if not disjoint then embeddings (*doesn't need to check the embeddings are binary*)
	else
		let mix = kappa_of_id mix_id state in
		List.fold_left 
		(fun cont (embedding,codomain,inj_list) ->
			let roots = 
				List.fold_left 
				(fun set inj -> 
					let _,cc_id = Injection.get_coordinate inj in 
					let u_i = match Mixture.root_of_cc mix cc_id with None -> invalid_arg "State.instances_of_square" | Some a_i -> Injection.find a_i inj
					in
					IntSet.add u_i set
					(*match Injection.root_image inj with None -> invalid_arg "State.instances_of_square" | Some (_,u_i) -> IntSet.add u_i set*)
				) IntSet.empty inj_list 
			in
			let (is_connex,_,_,_) = connex (roots,codomain) radius_def false state env in
			if is_connex  then cont else (embedding,codomain,inj_list)::cont
		) [] embeddings 

let rec value state ?var var_id counter env =
	let var_opt = match var with Some v -> Some v | None -> (try Some (alg_of_id var_id state) with | Not_found -> None)
	in
	match var_opt with
	| None ->
			invalid_arg (Printf.sprintf "v[%d] is not a valid variable" var_id)
	| Some var ->
			(match var with
				| Dynamics.CONST f -> f
				| Dynamics.VAR v_fun ->
						let act_of_id id = instance_number id state env
						and v_of_var id = value state id counter env
						and v_of_token id =
							let x = try state.token_vector.(id) with _ -> failwith "State.value: Invalid token id"
							in
							(Num.F x)
						in
						v_fun act_of_id v_of_var (Counter.time counter)
							(Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token
			)
			
(*missing recomputation of dependencies*)
let set_variable id v state =
	try state.alg_variables.(id) <- Some (Dynamics.CONST v) with Invalid_argument msg -> invalid_arg ("State.set_variable: "^msg)


(**[eval_activity rule state] returns the evaluation of the overestimated activity of rule [rule] in implicit state [state]*)
let eval_activity ?using rule state counter env =
	let mix_id = Mixture.get_id rule.lhs
	and k_def = rule.k_def
	in
	let a_2 = (*overestimated activity of binary instances of the rule*)
 		(match k_def with
			| Dynamics.CONST f -> 
				let n = (match using with None -> instance_number mix_id state env | Some x -> Num.I x) in 
				if Num.is_zero n then (Num.I 0)
				else
					(Num.mult f n)  (*Issue #65*)
			| Dynamics.VAR k_fun ->
					let act_of_id id = instance_number id state env
					and v_of_var id = value state id counter env 
					and v_of_token id = 
						let x = try state.token_vector.(id) with _ -> failwith "State.value: Invalid token id"
						in Num.F x
					in
					let k =
						k_fun act_of_id v_of_var (Counter.time counter)
							(Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token
					in
					let n = (match using with None -> instance_number mix_id state env | Some x -> Num.I x) in
					if Num.is_zero n then (Num.I 0)
					else
						Num.mult k n
						
		)
	in
	(*overestimated activity of unary instances of the rule*)
	let a_1 =
		match rule.k_alt with
			| (None,_) -> (Num.I 0)
			| (Some x ,_)->
				begin
					match x with
						| Dynamics.CONST f -> 
							let n = nl_instance_number mix_id state env in 
							if Mods.Num.is_zero n then (Num.I 0) (*whatever the rate is, if 0 instance then 0 activity*)
							else Num.mult f n
						| Dynamics.VAR k_fun ->
							let act_of_id id = nl_instance_number id state env 
							and v_of_var id = value state id counter env 
							and v_of_token id = 
								let x = try state.token_vector.(id) with _ -> failwith "State.value: Invalid token id"
								in Num.F x
							in
							let k =	k_fun act_of_id v_of_var (Counter.time counter) (Counter.event counter) (Counter.null_event counter) (Sys.time()) v_of_token 
							in
							let n = nl_instance_number mix_id state env in
							if Num.is_zero n then (Num.I 0)
							else
								Num.mult k n 
				end
	in
	(a_2,a_1)

let pert_of_id state id = IntMap.find id state.perturbations

let update_activity state cause var_id counter env =
	if not (Environment.is_rule var_id env) then ()
	else
		let rule = rule_of_id var_id state in
		let a2,a1 = eval_activity rule state counter env in
		let alpha = Num.float_of_num (Num.add a2 a1) in (*a1 is zero if rule doesn't have ambiguous molarity*)
		
		if !Parameter.fluxModeOn && cause > 0 then
			begin
				try
					let alpha_old = Random_tree.find var_id state.activity_tree in
					Random_tree.add var_id alpha state.activity_tree ;
					let w = (alpha -. alpha_old) in
					update_flux state cause var_id w		
				with Invalid_argument msg -> invalid_arg ("State.update_activity: "^msg)
			end
		else
			Random_tree.add var_id alpha state.activity_tree 

(* compute complete embedding of mix into sg at given root --for           *)
(* initialization phase                                                    *)
let generate_embeddings sg u_i mix comp_injs env =
	let mix_id = Mixture.get_id mix in
	
	let rec iter cc_id sg comp_injs  =
		if cc_id = (Mixture.arity mix)
		then (sg, comp_injs)
		else
			(
			let id_opt = Mixture.root_of_cc mix cc_id 
			in
				match id_opt with
				| None -> iter (cc_id + 1) sg comp_injs
				| Some id_root ->
						let opt_inj =
							Matching.component
								(Injection.empty (Mixture.size_of_cc cc_id mix) (mix_id,cc_id)) id_root
								(sg, u_i) mix
						in
						(match opt_inj with
							| None -> iter (cc_id + 1) sg comp_injs 
							| Some (injection, port_map) ->
							(* port_map: u_i -> [(p_k,0/1);...] if port k of node i is   *)
							(* int/lnk-tested by map                                     *)
								let opt =
									(try comp_injs.(cc_id)
									with
									| Invalid_argument msg ->
											invalid_arg ("State.generate_embeddings: " ^ msg)) in
								let cc_id_injections =
									(match opt with
										| Some injections -> injections
										| None ->
												InjectionHeap.create
													!Parameter.defaultHeapSize) in
								let cc_id_injections =
									InjectionHeap.alloc injection cc_id_injections
								in
								(comp_injs.(cc_id) <- Some cc_id_injections;
									let sg =
										SiteGraph.add_lift sg injection port_map env
									in 
									iter (cc_id + 1) sg comp_injs 
								)
						)
			)
	in 
	iter 0 sg comp_injs 

(**[initialize_embeddings state mix_list] *) (*mix list is the list of kappa observables one wishes to track during simulation*)
let initialize_embeddings state mix_list counter env =
	SiteGraph.fold 
	(fun i node_i state ->
		List.fold_left
		(fun state mix ->
			let injs = state.injections in
			let opt = try injs.(Mixture.get_id mix) with exn -> (print_string ("caught: "^(Printexc.to_string exn)) ; raise exn) in 
			let comp_injs =
				match opt with
				| None -> Array.create (Mixture.arity mix) None
				| Some comp_injs -> comp_injs in
			(* complement the embeddings of mix in sg using node i  as anchor for matching *)
			let (sg, comp_injs) =	generate_embeddings state.graph i mix comp_injs env
			in
				(* adding injections.(mix_id) = injs(mix) to injections array*)
				injs.(Mixture.get_id mix) <- Some comp_injs;
				{state with graph = sg}
		)
		state mix_list
	)
	state.graph state

let build_influence_map rules patterns env =
	let add_influence im i j glueings = 
		let map = try Hashtbl.find im i with Not_found -> IntMap.empty in
		Hashtbl.replace im i (IntMap.add j glueings map)
	in
	let influence_map = Hashtbl.create (Hashtbl.length rules) in
	Hashtbl.iter
	(fun i r -> 
		match r.refines with
			| Some _ -> () 
			| None ->
				Array.iteri 
				(fun j opt ->
					match opt with
						| None -> () (*empty pattern*)
						| Some mix ->
							if !Parameter.debugModeOn then 
								(Printf.printf "%s -+-> %s?\n" (Dynamics.to_kappa r env) (Mixture.to_kappa false mix env) ; flush stdout) ;
							let glueings = Dynamics.enable r mix env in (*glueings: [phi_0;...;phi_n] partial embeddings list*)
							match glueings with
								| [] -> if !Parameter.debugModeOn then (Printf.printf "No\n") ; ()
								| _ ->
									if !Parameter.debugModeOn then (Printf.printf "Yes\n") ; 
							 		add_influence influence_map i j glueings	
				) patterns
	) rules ;
	influence_map

let dot_of_influence_map desc state env =
	Printf.fprintf desc "digraph G{ node [shape=box, style=filled, fillcolor=lightskyblue]; \n " ;
	Hashtbl.iter
	(fun r_id rule ->
		let opt = if rule.Dynamics.is_pert then "[shape=invhouse,fillcolor=lightsalmon]" else "" in 
		Printf.fprintf desc "\"%d:%s\" %s;\n" r_id (Dynamics.to_kappa rule env) opt
	) state.rules ;
	Array.iteri
	(fun mix_id mix_opt ->
		if Environment.is_rule mix_id env then ()
		else
			match mix_opt with
				| None -> ()
				| Some mix -> Printf.fprintf desc "\"%d:%s\" [shape=ellipse,fillcolor=palegreen3] ;\n" mix_id (Mixture.to_kappa false mix env)
	) state.kappa_variables ;
	Hashtbl.iter 
	(fun r_id act_map ->
		let rule = rule_of_id r_id state in
		let n_label = Dynamics.to_kappa rule env in
		IntMap.iter
		(fun mix_id glueings ->
			let n_label' = 
				if Environment.is_rule mix_id env then
					let rule'=rule_of_id mix_id state in
					Dynamics.to_kappa rule' env
				else
					let mix = kappa_of_id mix_id state in 
					Mixture.to_kappa false mix env
			in
			let arrow_label = 
				let ls = 
					List.fold_left 
					(fun label glueing ->
						LongString.concat ~sep:';' (Tools.string_of_map ~swap:true string_of_int string_of_int IntMap.fold glueing) label
					) LongString.empty glueings
				in
				LongString.to_string ls
			in
			Printf.fprintf desc "\"%d:%s\" -> \"%d:%s\" [label=\"%s\"];\n" r_id n_label mix_id n_label' arrow_label 
		) act_map
	) state.influence_map ;
	Printf.fprintf desc "}\n"

let initialize sg token_vector rules kappa_vars alg_vars obs (pert,rule_pert) counter env =
	let dim_pure_rule = max (List.length rules) 1
	in
	let dim_rule = dim_pure_rule + (List.length rule_pert) 
	and dim_kappa = (List.length kappa_vars) + 1
	and dim_var = List.length alg_vars 
	in
	
	let injection_table = Array.make (dim_rule + dim_kappa) None
	and kappa_var_table = Array.make (dim_rule + dim_kappa) None (*list of rule left hand sides and kappa variables*)
	and alg_table = Array.make dim_var None (*list of algebraic values*)
	and rule_table = Hashtbl.create dim_rule (*list of rules*)
	and perturbation_table = IntMap.empty (*list of perturbations*)
	and wake_up_table = Precondition.empty () (*wake up table for side effects*)
	and influence_table = Hashtbl.create dim_rule (*influence map*)
	in
	
	let _ = (*adding observables in the kappa table*) 
		List.iter
		(fun mix -> kappa_var_table.(Mixture.get_id mix) <- Some mix) kappa_vars
	in
	let kappa_variables =
		(* forming kappa variable list by merging rule (and perturbation) lhs with kappa variables *)
		List.fold_left
		(fun patterns r ->
			let i = r.r_id
			in
				let patterns = 
					if Mixture.is_empty r.lhs then patterns (*nothing to track if left hand side is empty*)
					else (kappa_var_table.(Mixture.get_id r.lhs) <- Some r.lhs ; r.lhs :: patterns)
				in
				(Hashtbl.replace rule_table i r; patterns)
		)
		kappa_vars (rule_pert@rules) 
	in
	let state_init =
		{
			graph = sg;
			injections = injection_table ;
			nl_injections = Array.make (dim_rule+dim_kappa) None ; (*this table is too big, one should restrict to unary rules only*)
			rules = rule_table ;
			perturbations =
				begin
					let perturbation_table, _ =
						List.fold_left
						(fun (pt, i) pert -> ((IntMap.add i pert pt), (i + 1))
						)
						(perturbation_table, 0) pert
					in 
					perturbation_table
				end ;
			kappa_variables = kappa_var_table;
			alg_variables = alg_table;
			token_vector = token_vector ; 
			observables = 
				begin
					List.fold_left
					(fun cont (plot_v, const, opt_v, dep, lbl) ->
								let expr = 
									if const then (match opt_v with Some v -> CONST v | None -> invalid_arg "State.initialize") 
									else (VAR plot_v)
								in
									{
										expr = expr ;
										label = replace_space lbl;
									} :: cont
					)	[] obs
				end ;
			activity_tree = Random_tree.create dim_pure_rule ; (*put only true rules in the activity tree*)
			influence_map = influence_table ;
			wake_up = wake_up_table;
			flux = if !Parameter.fluxModeOn then Hashtbl.create 5 else Hashtbl.create 0 ;
			silenced = IntSet.empty
		}
	in
	
	if !Parameter.debugModeOn then Debug.tag "\t * Initializing injections...";
	let state = (*initializing injections*)
		initialize_embeddings state_init kappa_variables counter env
	in
	
	if !Parameter.debugModeOn then Debug.tag "\t * Initializing variables...";
	let env =
		List.fold_left
		(fun env (v, deps, var_id) ->
			try
				let env =
					DepSet.fold
						(fun dep env ->
									Environment.add_dependencies dep (Mods.ALG var_id) env
						)
						deps env
				in (state.alg_variables.(var_id) <- Some v; env)
			with
			| Invalid_argument msg ->
					invalid_arg ("State.initialize: " ^ msg)
		)
		env alg_vars
	in
	
	if !Parameter.debugModeOn then Debug.tag	"\t * Initializing wake up map for side effects...";
	let state =
		(* initializing preconditions on pattern list for wake up map *)
		List.fold_left
		(fun state mix ->
					{state with wake_up = Precondition.add mix state.wake_up}
		)
		state kappa_variables
	in
	
	if !Parameter.debugModeOn then Debug.tag "\t * Initializing activity tree...";
	let act_tree = (*initializing activity tree*)
		Hashtbl.fold
		(fun id rule act_tree ->
			(*rule could be a perturbation*)
			if not (Environment.is_rule id env) then act_tree
			else
				let a2,a1 = eval_activity rule state counter env in
				let alpha_rule = Num.float_of_num (Num.add a1 a2) in
				(Random_tree.add id alpha_rule act_tree ; act_tree)
		)
			state.rules state.activity_tree	
	in
	if !Parameter.debugModeOn then Debug.tag "\t * Computing influence map...";
	let im = build_influence_map state.rules state.kappa_variables env 
	in
	({state with activity_tree = act_tree; influence_map = im}, env)
	
let clean_injprod injprod state counter env = 
	let mix_id = InjProduct.get_coordinate injprod
	and injprod_id = InjProduct.get_address injprod
	in
	let inj_prod_hp = 
		match state.nl_injections.(mix_id) with 
			| None -> invalid_arg "State.clean_injprod" 
			| Some h -> h
	in 
	let hp = InjProdHeap.remove injprod_id inj_prod_hp in (*removing injection product from the heap*)
	(*let hp = 
		if (float_of_int (Counter.null_event counter))/. (float_of_int (Counter.event counter)) > 0.8 then
			InjProdHeap.gc inj_prod_hp (fun c -> InjProduct.is_trashed c) 
		else hp
	in*)
	
	state.nl_injections.(mix_id) <- (Some hp) ;
	update_activity state (-1) mix_id counter env 

type embedding_t = DISJOINT of embedding_info | CONNEX of embedding_info | AMBIGUOUS of embedding_info
and embedding_info = {map: int IntMap.t ; roots : IntSet.t ; components : IntSet.t IntMap.t option; depth_map : int IntMap.t option}	

let empty_embedding = CONNEX {map=IntMap.empty ; roots = IntSet.empty ; components = None ; depth_map = None}
let map_of embedding = match embedding with CONNEX e | DISJOINT e | AMBIGUOUS e -> e.map 
				
(**returns either valid embedding or raises Null_event if injection is no longer valid --function also cleans inj_hp and nodes as a side effect*)
let check_validity injprod with_full_components radius state counter env =
	try
		let embedding,roots,codomain = 
			InjProduct.fold_left
			(fun (embedding,roots,codom) inj_i ->
				if Injection.is_trashed inj_i then (*injection product is no longer valid because one of its element is trashed*) 
					(if !Parameter.debugModeOn then Debug.tag "Clashing because one of the component of injection product is no longer valid" ;
					raise (Null_event 4))
				else
				(*injection product might be invalid because co-domains are no longer connected*)
					let map,codom = 
						Injection.fold 
						(fun i j (map,codom) -> 
							let map = IntMap.add i j map
							and codom = 
								if IntSet.mem j codom then raise (Null_event 2) (*clashing instance*) 
								else IntSet.add j codom
							in
							(map,codom)
						) inj_i (embedding,codom) in
					let roots =
						let mix_id,cc_id = Injection.get_coordinate inj_i in
						let mix = kappa_of_id mix_id state in
						let u_i = match Mixture.root_of_cc mix cc_id with None -> invalid_arg "State.check_validity" | Some a_i -> Injection.find a_i inj_i in
						IntSet.add u_i roots
						(*match (Injection.root_image inj_i) with None -> invalid_arg "State.check_validity" | Some (_,u_i) -> IntSet.add u_i roots in*)
					in
					(map,roots,codom)
			) (IntMap.empty,IntSet.empty,IntSet.empty) injprod
		in
		let (is_connex,d_map,components,_) = connex (roots,codomain) with_full_components radius state env in
		if is_connex then 
			(
			{map = embedding ; components = Some (IntMap.add 0 components IntMap.empty) ; depth_map = Some d_map ; roots = roots}
			)
		else 
			(if !Parameter.debugModeOn then Debug.tag "Clashing because injection product's codomain is no longer connex" ;
			raise (Null_event 0))
	with
		| Null_event i -> (*correcting over approximation*)
			begin
				clean_injprod injprod state counter env ;
				raise (Null_event i)
			end
	
let select_injection (a2,radius_def) (a1,radius_alt) state mix counter env =
	if Mixture.is_empty mix then empty_embedding 
  else
  	let mix_id = Mixture.get_id mix in
  		
  	let select_unary () = 
  		let opt =
  			try state.nl_injections.(mix_id)
  			with
  			| Invalid_argument msg -> invalid_arg ("State.select_injection: " ^ msg)
  		in
  		match opt with
  		| None ->
  				invalid_arg
  					("State.select_injection: variable " ^
  						((string_of_int mix_id) ^
  							" has no instance but a positive activity"))
  		| Some prod_inj_hp ->
  			(try
					let radius = match radius_alt with None -> (-1) | Some v -> Num.int_of_num (value state ~var:v (-1) counter env) in
  				let injprod = InjProdHeap.random prod_inj_hp in (*injprod is an array of size #cc(mix_id) and injprod.(i):Injection.t a partial injection of cc(i)*)
  				let embedding = check_validity injprod radius false state counter env in (*returns either valid embedding or raises Null_event if injection is no longer valid --function also cleans inj_hp and nodes as a side effect*)
  				(CONNEX embedding)
  			with
  			| Invalid_argument msg -> invalid_arg ("State.select_injection: "^msg)
				(*| Null_event 4 -> let h = InjProdHeap.gc prod_inj_hp (fun injprod -> Mods.InjProduct.is_trashed injprod) in state.nl_injections.(mix_id) <- Some h ; raise (Null_event 4)*)
  			)
  	in
  	
  	let select_binary clash_if_unary = (*clash_if_unary is true if the embedding has to be binary otherwise ambiguous is OK*)
  		let opt =
  			try state.injections.(mix_id)
  			with
  			| Invalid_argument msg -> invalid_arg ("State.select_injection: " ^ msg)
  		in
  		match opt with
  		| None ->
  				invalid_arg
  					("State.select_injection: variable " ^
  						((string_of_int mix_id) ^
  							" has no instance but a positive activity"))
  		| Some comp_injs ->
  				let _,embedding,codomain,roots = (*building the complete embedding using components embeddings --and clashing if resulting embedding is not injective*)
  					Array.fold_left
  						(fun (i, total_inj, total_cod, roots) injheap_opt ->
  									match injheap_opt with
  									| None -> invalid_arg "State.select_injection"
  									| Some injheap ->
  											(try
  												let inj = InjectionHeap.random injheap in
  												let roots = 
  													let mix_id,cc_id = Injection.get_coordinate inj in
  													let mix = kappa_of_id mix_id state in
  													let u_i = match Mixture.root_of_cc mix cc_id with None -> invalid_arg "State.select_binary" | Some a_i -> Injection.find a_i inj 
  													in
  													IntSet.add u_i roots
  												in
  													(*match Injection.root_image inj with None -> invalid_arg "State.select_binary" | Some (_,u_i) -> IntSet.add u_i roots in*)
  												let total_inj,total_cod =
  													try Injection.codomain inj (total_inj,total_cod)
  													with 
  														| Injection.Clashing -> 
  															(if !Parameter.debugModeOn then Debug.tag "Clashing because codomains of selected partial injections are overlapping" ;
  															raise (Null_event 2))
  												in 
  												(i + 1, total_inj,total_cod, roots)
  											with
  											| Invalid_argument msg ->
  													invalid_arg ("State.select_injection: " ^ msg)))
  						(0, IntMap.empty, IntSet.empty, IntSet.empty) comp_injs
  				in
  				
					let radius = match radius_def with None -> (-1) | Some v -> Num.int_of_num (value state ~var:v (-1) counter env) in
					
  				let rec build_component_map (roots,codomain) depth_map component_map = 
  					if IntSet.is_empty roots then (depth_map,component_map) (*no more root to check*)
  					else
  						let root = IntSet.choose roots in
  						(*components will contain only node that can be the root of a non local rule because filter is enabled *)
  						let (_,d_map,components,remaining_roots) = connex ~d_map:depth_map ~filter:true ~start_with:root (roots,codomain) radius true state env 
  						in
  						if not ((IntSet.cardinal remaining_roots) = (IntSet.cardinal roots) - 1) then
  							(if !Parameter.debugModeOn then Debug.tag "Clashing because selected instance of n-nary rule is not totally disjoint" ; 
  							raise (Null_event 1)
  							)
  						else () ;
  						let component_map = IntMap.add root components component_map
  						in
  						build_component_map (remaining_roots,codomain) d_map component_map (*remaining roots should be empty if rule has only 2 CCs*)
  				in
  				
  				if clash_if_unary then (*now checking with the contex that the embedding is indeed binary*)
  					let (d_map,comp_map) = build_component_map (roots,codomain) IntMap.empty IntMap.empty in (*raises Null_event if roots are not connected*)
  					(DISJOINT {map=embedding; depth_map=Some d_map; roots = roots ; components = Some comp_map})
  				else
  					if not env.Environment.has_intra then (AMBIGUOUS {map=embedding;depth_map=None ; roots = roots ; components = None})
  					else
  						let r = rule_of_id (Mixture.get_id mix) state in 
  						match r.cc_impact with
  							| None ->	(AMBIGUOUS {map=embedding;depth_map=None ; roots = roots ; components = None})
  							| Some (con_map,_,_) ->
  								if IntMap.is_empty con_map then 
  									(AMBIGUOUS {map=embedding;depth_map=None ; roots = roots ; components = None})
  								else
  									(if !Parameter.debugModeOn then
  										Debug.tag "Connectedness is not required for this rule but will compute it nonetheless because rule might create more intras" ;
  									let (d_map,comp_map) = build_component_map (roots,codomain) IntMap.empty IntMap.empty in
  									(AMBIGUOUS {map=embedding;depth_map=Some d_map ; roots = roots ; components = Some comp_map}))
  	in
  	if not (Mixture.unary mix) then select_binary false 
  	else
  		let a2,a1 = if (a2 = infinity) && (a1 = infinity) then (1.,1.) else (a2,a1) in
  		if a1 = infinity then select_unary ()
  		else 
  			if a2 = infinity then select_binary true
  			else 
  				let x = Random.float (a1 +. a2) in
  				if x < a1 then select_unary () 
  				else select_binary true

(* Draw a rule at random in the state according to its activity *)
let draw_rule state counter env =
	try
		(*selects rule_id with a proba that respects activity*)
		let rule_id,alpha' = Random_tree.random state.activity_tree in
		let _ = if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Picked rule [%d] at random." rule_id) in
		let r =
			try rule_of_id rule_id state
			with | Not_found -> invalid_arg "State.draw_rule" 
		in
		let a2,a1 =
			try eval_activity r state counter env
			with | Not_found -> invalid_arg "State.draw_rule"
		in
		let alpha = Num.float_of_num (Num.add a2 a1) in
		(*correction: issue #40*)
		if alpha = 0. then Random_tree.add rule_id alpha state.activity_tree ;

		let (_:unit) =
			if alpha = infinity then ()
			else
				if alpha > alpha' then 
					if IntSet.mem rule_id state.silenced then (if !Parameter.debugModeOn then Debug.tag "Real activity is below approximation... but I knew it!") else invalid_arg "State.draw_rule: activity invariant violation"
				else ();
				let rd = Random.float 1.0
				in
				if rd > (alpha /. alpha')
				then 
					(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Clashing in order to correct for overestimation of activity of rule %d" rule_id);
					Random_tree.add rule_id alpha state.activity_tree ;
					raise (Null_event 3)) (*null event because of over approximation of activity*)
				else ()
		in
		let embedding_type = 
			let _,radius = r.k_alt
			in
			try select_injection (Num.float_of_num a2,radius) (Num.float_of_num a1,radius) state r.lhs counter env with 
			| Null_event 1 | Null_event 2 as exn -> (*null event because of clashing instance of a binary rule*)
				if counter.Counter.cons_null_events > !Parameter.maxConsecutiveClash then 
					begin
						(if !Parameter.debugModeOn then Debug.tag "Max consecutive clashes reached, I am giving up square approximation at this step" else ()) ;
						let _ = Counter.reset_consecutive_null_event counter in
					
						let radius = match radius with None -> (-1) | Some v -> Num.int_of_num (value state ~var:v (-1) counter env) in
  				
						let embeddings = instances_of_square ~disjoint:true rule_id radius state env in
						let alpha,_ = eval_activity ~using:(List.length embeddings) r state counter env in 
						let alpha = Num.float_of_num alpha in
						begin
							Random_tree.add rule_id alpha state.activity_tree ;
							silence rule_id state ; (*rule activity will be underestimated if not awaken when a rule creates more cc's*)
							if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Rule [%d]'s activity was corrected to %f" rule_id alpha) ;
							raise exn
						end
					end
				else (if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Rule [%d] is clashing" rule_id) ; raise exn )
		in
		((Some (r, embedding_type)), state)
	with 
		| Not_found -> (None,state)

let wake_up state modif_type modifs wake_up_map env =
	Int2Set.fold
	(fun (node_id, site_id) wake_up_map ->
			let opt =
				try Some (SiteGraph.node_of_id state.graph node_id)
				with | exn -> None
			in
			match opt with
			| None -> wake_up_map
			| Some node ->
					let old_candidates =
						(try IntMap.find node_id wake_up_map
						with | Not_found -> Int2Set.empty)
					in
					(* {(mix_id,cc_id),...} *)
					(match modif_type with
						| 0 -> (*internal state modif*)
								let new_candidates =
									Precondition.find_all (Node.name node) site_id (Node.internal_state (node, site_id)) None false	state.wake_up
								in
								(* adding pairs (mix_id,cc_id) to the potential new    *)
								(* matches to be tried at anchor node_id               *)
								IntMap.add node_id (Int2Set.union old_candidates new_candidates) wake_up_map
						| 1 -> (*link state modification*)
								let is_free = try not (Node.is_bound (node, site_id)) with Invalid_argument msg -> invalid_arg (Printf.sprintf "State.wake_up : no site %d in agent %s" site_id (Environment.name (Node.name node) env))
								in
								let new_candidates =
									if is_free
									then
										Precondition.find_all (Node.name node) site_id None	None is_free state.wake_up
									else
										(let link_opt =
												match Node.follow (node, site_id) with
												| None -> invalid_arg "State.wake_up"
												| Some (node', site_id') ->	Some (Node.name node', site_id')
											in
											Precondition.find_all (Node.name node) site_id None link_opt is_free state.wake_up)
								in
								IntMap.add node_id (Int2Set.union old_candidates new_candidates) wake_up_map 
						| _ -> (*intro*)
								let is_free = not (Node.is_bound (node, site_id)) in
								let new_candidates =
									let link_opt =
										(match Node.follow (node, site_id) with
											| None -> None
											| Some (node', site_id') ->
													Some (Node.name node', site_id'))
									in
									Precondition.find_all (Node.name node) site_id (Node.internal_state (node, site_id)) link_opt	is_free state.wake_up
								in
								IntMap.add node_id (Int2Set.union old_candidates new_candidates) wake_up_map
				)
		)	modifs wake_up_map

let update_dep state cause dep_in pert_ids counter env =
	let rec iter env dep_to_check pert_ids =
		if DepSet.is_empty dep_to_check then (env,pert_ids) 
		else
  		let dep_in = DepSet.choose dep_to_check in 
  		match dep_in with
  			| Mods.TOK t_id -> (*token counter is changed *)
  				let depset = 
  					Environment.get_dependencies (Mods.TOK t_id) env
  				in
  				begin
  					if !Parameter.debugModeOn then
  						Debug.tag 
  						(Printf.sprintf "Token %d is changed, updating %s" t_id (string_of_set Mods.string_of_dep DepSet.fold depset)) 
  				end;
  				iter env (DepSet.union (DepSet.remove dep_in dep_to_check) depset) pert_ids
  			| Mods.ALG v_id -> (*variable v_id is changed -by a perturbation if used as initial dep_in argument*)
  				let depset =
  					Environment.get_dependencies (Mods.ALG v_id) env
  				in
  				begin
  					if !Parameter.debugModeOn then
  						Debug.tag 
  						(Printf.sprintf "Variable %d is changed, updating %s" v_id (string_of_set Mods.string_of_dep DepSet.fold depset)) 
  				end;
  				iter env (DepSet.union (DepSet.remove dep_in dep_to_check) depset) pert_ids
  			| Mods.RULE r_id -> (*rule activity is changed -by a perturbation if used as initial dep_in argument*)
  				(update_activity state cause r_id counter env; 
  				let depset = Environment.get_dependencies (Mods.RULE r_id) env
  				in
  				if !Parameter.debugModeOn then if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Rule %d is changed, updating %s" r_id (string_of_set Mods.string_of_dep DepSet.fold depset)) ;
  				iter env (DepSet.union (DepSet.remove dep_in dep_to_check) depset) pert_ids
  				)
  			| Mods.PERT p_id -> 
  				if IntMap.mem p_id state.perturbations then (*pertubation p_id is still alive and should be tried*)
  					iter env (DepSet.remove dep_in dep_to_check) (IntSet.add p_id pert_ids)
  				else (*pertubation p_id is removed and should be discarded from dependencies*)
  					iter (Environment.remove_dependencies dep_in (Mods.PERT p_id) env) (DepSet.remove dep_in dep_to_check) pert_ids
  			| Mods.ABORT p_id ->
  				if IntMap.mem p_id state.perturbations then iter env (DepSet.remove dep_in dep_to_check) (IntSet.add p_id pert_ids)
  				else 
  					iter (Environment.remove_dependencies dep_in (Mods.PERT p_id) env) (DepSet.remove dep_in dep_to_check) pert_ids
  			| Mods.KAPPA i -> (*No need to update kappa observable, it will be updated if plotted*) 
  				let depset =
  					Environment.get_dependencies (Mods.KAPPA i) env
  				in
  				if !Parameter.debugModeOn && not (DepSet.is_empty depset) then Debug.tag (Printf.sprintf "Observable %d is changed, updating %s" i (string_of_set Mods.string_of_dep DepSet.fold depset)) ;
  					iter env (DepSet.union (DepSet.remove dep_in dep_to_check) depset) pert_ids
  			| Mods.EVENT | Mods.TIME -> 
  				let depset = Environment.get_dependencies dep_in env in
  					iter env (DepSet.union (DepSet.remove dep_in dep_to_check) depset) pert_ids
  in
		iter env (DepSet.singleton dep_in) pert_ids

let enabled r state = 
	let r_id = Mixture.get_id r.lhs in 
	try Hashtbl.find state.influence_map r_id with Not_found -> IntMap.empty
	

let positive_update ?(with_tracked=[]) state r ((phi: int IntMap.t),psi) (side_modifs,pert_intro) counter env = (*pert_intro is temporary*)
	
	(* sub function find_new_inj *)
	let find_new_inj state var_id mix cc_id node_id root pert_ids already_done_map new_injs tracked env =
		if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Trying to embed Var[%d] using root %d at node %d" var_id root node_id);
		let root_node_set =	try IntMap.find var_id already_done_map
			with Not_found -> Int2Set.empty in
		let opt =
			try state.injections.(var_id)
			with Invalid_argument msg -> invalid_arg ("State.positive_update: " ^ msg) 
		in
		let comp_injs =
			match opt with
			| None -> (*may happen when initial graph was empty*)
				let ar = Array.create (Mixture.arity mix) None in
				state.injections.(var_id) <- (Some ar) ;
				ar
			| Some injs -> injs 
		in
		let opt =
			try comp_injs.(cc_id)
			with
			| Invalid_argument msg ->
					invalid_arg ("State.positive_update: " ^ msg) in
		let cc_id_injections =
			match opt with
			| Some injections -> injections
			| None ->	InjectionHeap.create !Parameter.defaultHeapSize 
		in
		let reuse_embedding =
			match InjectionHeap.next_alloc cc_id_injections with
			| Some phi -> 
				(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "reusing injection: %s" (Injection.to_string phi));
					Injection.flush phi (var_id,cc_id))
			| None -> Injection.empty (Mixture.size_of_cc cc_id mix) (var_id,cc_id)
		in			
		let opt_emb = Matching.component ~already_done:root_node_set reuse_embedding root (state.graph, node_id) mix in 
		match opt_emb	with
		| None ->
				(if !Parameter.debugModeOn then Debug.tag "No new embedding was found";
				(env,state, pert_ids, already_done_map, new_injs,tracked)
				)
		| Some (embedding, port_map) ->
				if !Parameter.debugModeOn then Debug.tag	(Printf.sprintf "New embedding: %s" (Injection.to_string embedding)) ;
				let cc_id_injections = InjectionHeap.alloc embedding cc_id_injections in
				comp_injs.(cc_id) <- Some cc_id_injections ;
				let graph =	SiteGraph.add_lift state.graph embedding port_map env
				in
				let state = {state with graph = graph}
				in
				begin
					(*a new embedding was found for var_id*)
					let tracked = 
						if Environment.is_tracked var_id env then (*completing the embedding if incomplete*)
							try
								let m = kappa_of_id var_id state in	
								let cpt = ref 0 in
								let map,cod = (fun (x,y) -> (ref x,ref y)) (Injection.to_map embedding) in
								while !cpt < (Mixture.arity m) do
									if !cpt = cc_id then ()
									else 
										begin
											let embedding',codomain' = 
												match comp_injs.(!cpt) with
													| None -> raise (ExceptionDefn.Break 0)
													| Some hp -> 
														let s = InjectionHeap.size hp in
														if s = 0 then raise (Break 0)
														else
															let rec find_compatible cpt =
																if cpt < 0 then raise (Break 1)
																else
																let inj = InjectionHeap.find cpt hp
																in
																if (Injection.is_trashed inj) then failwith "Incorrect heap size"
																else
																	try
																		Injection.fold (fun i j (map,cod) -> (IntMap.add i j map, if IntSet.mem j cod then raise (Break 1) else IntSet.add j cod)) inj (!map,!cod)
																	with Break _ -> find_compatible (cpt-1)
															in
															find_compatible (s-1)
											in
												map := embedding' ; cod := codomain' ;
										end ;
									cpt := !cpt+1 ;
								done ;
								if !Parameter.debugModeOn then
									Debug.tag (Printf.sprintf "Observable %d was found with embedding %s" var_id (Tools.string_of_map string_of_int string_of_int IntMap.fold !map)) ;
								(var_id,!map)::tracked
							with 
								| Break 0 -> (if !Parameter.debugModeOn then Debug.tag "Incomplete embedding, no observable recorded" ; tracked)
								| Break 1 -> (if !Parameter.debugModeOn then Debug.tag "Cannot complete embedding, clashing instances" ; tracked)
					else tracked
					in
					update_activity state r.r_id var_id counter env;
					let env,pert_ids = (*updating rule activities that depend --transitively-- on var_id*)
						update_dep state r.r_id (Mods.KAPPA var_id) pert_ids counter env
					in
					(*Printf.printf "done (%d,%d) for var[%d]\n" root node_id var_id ;*) 
					let already_done_map' = IntMap.add var_id	(Int2Set.add (root, node_id) root_node_set) already_done_map 
					in
					let new_injs' = if Environment.is_nl_rule var_id env then embedding::new_injs else new_injs in 
					(env,state, pert_ids, already_done_map',new_injs',tracked)
				end
	in
	(* end of sub function find_new_inj definition *)
	
	let vars_to_wake_up = enabled r state in
	let env,state,pert_ids,already_done_map,new_injs,tracked =
		IntMap.fold 
		(fun var_id map_list (env, state,pert_ids,already_done_map,new_injs, tracked) ->
			if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Influence map tells me I should look for new injections of var[%d]" var_id) ;
			List.fold_left 
			(fun (env,state,pert_ids,already_done_map, new_injs,tracked) glue ->
				let opt = IntMap.root glue in
				match opt with
					| None -> invalid_arg "State.positive_update"
					| Some (root_mix,root_rhs) ->

						let node_id = 
							if IntSet.mem root_rhs r.added then 
								(try IntMap.find root_rhs psi with Not_found -> invalid_arg "State.positive_update 1")
							else	
								try					
									IntMap.find root_rhs phi 
								with 
									| Not_found -> 
										(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "I was looking for the image of agent %d by embedding %s" 
										root_rhs (Tools.string_of_map string_of_int string_of_int IntMap.fold phi)) ;
										if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Glueing was %s" (string_of_map string_of_int string_of_int IntMap.fold glue)) ; 
										invalid_arg "State.positive_update 3")
						in
						let mix =
							let opt =
								try state.kappa_variables.(var_id)
								with
								| Invalid_argument msg ->
										invalid_arg ("State.positive_update: " ^ msg)
							in
							match opt with
							| Some mix -> mix
							| None -> invalid_arg "State.positive_update" 
						in
						let cc_id = Mixture.component_of_id root_mix mix in
						(*already_done_map is empty because glueings are guaranteed to be different by construction*)
						let env,state,pert_ids,already_done_map,new_injs,tracked = 
							find_new_inj state var_id mix cc_id node_id root_mix pert_ids already_done_map new_injs tracked env
						in
						(env,state, pert_ids, already_done_map, new_injs,tracked)
			) (env,state, pert_ids, already_done_map, new_injs,tracked) map_list
		) vars_to_wake_up (env, state, IntSet.empty, IntMap.empty,[],with_tracked)  
	in
	
	(*updating tokens if rule is hybrid*)
	
	let env,pert_ids =
		List.fold_left
		(fun (env,pert_ids) (v,t_id) ->
			let value = Num.float_of_num (value state ~var:v (-1) counter env) in
			try
				if !Parameter.debugModeOn then
					(Debug.tag (Printf.sprintf "adding %f to token %d" value t_id)) ;
				state.token_vector.(t_id) <- state.token_vector.(t_id) +. value ;
				(*updating rule activities that depend on |t_id|*)
				update_dep state r.r_id (Mods.TOK t_id) pert_ids counter env
			with Invalid_argument _ -> failwith "State.positive_update: invalid token id"  
		) (env,pert_ids) r.Dynamics.add_token 
	in
	let env,pert_ids = 
		List.fold_left
		(fun (env,pert_ids) (v,t_id) ->
			let value = Num.float_of_num (value state ~var:v (-1) counter env) in
			try
				if !Parameter.debugModeOn then
					(Debug.tag (Printf.sprintf "removing %f to token %d" value t_id)) ;
				
				state.token_vector.(t_id) <- state.token_vector.(t_id) -. value ;
				update_dep state r.r_id (Mods.TOK t_id) pert_ids counter env
			with Invalid_argument _ -> failwith "State.positive_update: invalid token id"  
		) (env,pert_ids) r.Dynamics.rm_token
	in
	
	(*Checking if any side effect needs to be checked*)
	if Int2Set.is_empty side_modifs then (env,state,pert_ids,new_injs,tracked)
	
	else	(*Handling side effects*)
	let wu_map = IntMap.empty
	in
		if !Parameter.debugModeOn then Debug.tag "Checking positive update entailed by side effects";
	
		let wu_map = wake_up state 1 side_modifs wu_map env in
		let wu_map = wake_up state 2 pert_intro wu_map env in
		let (env,state, pert_ids,_,new_injs,tracked) =
		IntMap.fold
		(fun node_id candidates (env,state, pert_ids, already_done_map,new_injs,tracked) ->
			if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Side effect on node %d forces me to look for new embedding..." node_id);
			let node = SiteGraph.node_of_id state.graph node_id
			in
			Int2Set.fold
			(fun (var_id, cc_id) (env, state, pert_ids, already_done_map, new_injs,tracked) ->
				let mix =
					let opt =
						try state.kappa_variables.(var_id)
						with
						| Invalid_argument msg ->
								invalid_arg ("State.positive_update: " ^ msg)
					in
					match opt with
					| Some mix -> mix
					| None -> invalid_arg "State.positive_update" 
				in
				let possible_roots =
					Mixture.ids_of_name ((Node.name node), cc_id) mix
				in
					IntSet.fold 
					(fun root (env,state, pert_ids, already_done_map, new_injs,tracked) ->
						find_new_inj state var_id mix cc_id node_id root pert_ids already_done_map new_injs tracked env
					) possible_roots (env,state, pert_ids, already_done_map, new_injs, tracked)
			) candidates (env, state, pert_ids, already_done_map, new_injs, tracked)
	)	wu_map (env, state, pert_ids, already_done_map, new_injs,tracked)
	in
	(env,state,pert_ids, new_injs,tracked)
	

(* Negative update *)
let negative_upd state cause (u,i) int_lnk counter env = 
	
	if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Negative update as indicated by %s#%d site %d" 
	(Environment.name (Node.name u) env) (Node.get_address u) i);
		
	(* sub-function that removes all injections pointed by lifts --if they *)
	(* still exist                                                         *)
	let remove_injs state liftset pert_ids env =
		let injections = state.injections
		in
		LiftSet.fold
			(fun phi (env,pert_ids) ->
				if Injection.is_trashed phi then (LiftSet.remove liftset phi ; (env,pert_ids)) 
				else
				let (mix_id, cc_id, inj_id) =
					let i = Injection.get_address phi in
					let (m,c) = Injection.get_coordinate phi
					in
					(m,c,i)
				in
				let comp_injs_opt =
					try injections.(mix_id)
					with
					| Invalid_argument msg ->
							invalid_arg ("State.negative_upd: " ^ msg)
				in
				match comp_injs_opt with
				| None ->
						invalid_arg
							"State.negative_upd: rule was applied with no injection"
				| Some comp_injs ->
						let opt_inj_cc_id =
							(try comp_injs.(cc_id)
							with
							| Invalid_argument msg ->
									invalid_arg ("State.negative_upd: " ^ msg)) in
						let injs_cc_id =
							match opt_inj_cc_id with
								| None ->
										invalid_arg
											"State.negative_upd: rule was applied when a cc had no injection"
								| Some injs_cc_id -> injs_cc_id
						in
						let _ (*injs_cc_id*) =
						begin
							let mix = kappa_of_id mix_id state
							in
							Injection.fold
							(fun i j _ ->
								let a_i = Mixture.agent_of_id i mix
								and u_j =	try SiteGraph.node_of_id state.graph j with 
									| exn -> invalid_arg (Printf.sprintf "State.negative_update: Node #%d is no longer in the graph and injection %s of mixture %s was pointing on it!" j (Injection.to_string phi) (Mixture.to_kappa false mix env))
								in
								Mixture.fold_interface
								(fun site_id (int_opt, lnk_opt) _ ->
									let (_ : unit) =
										match int_opt with
										| None -> ()
										| Some _ ->
												let (lifts, _) = Node.get_lifts u_j site_id 
												in
												LiftSet.remove lifts phi
									in
									match lnk_opt with
									| Node.WLD -> ()
									| Node.BND | Node.TYPE _ |	Node.FREE ->
										let (_, lifts) = try Node.get_lifts u_j	site_id with exn -> invalid_arg ("State.negative_update: "^(Printexc.to_string exn))
										in
										LiftSet.remove lifts phi
									) a_i ()
								) phi () ;
								let _ = InjectionHeap.remove inj_id injs_cc_id in
								if !Parameter.fluxModeOn then update_activity state cause mix_id counter env ;
						end
						in
						(* comp_injs.(cc_id) <- Some injs_cc_id; *)
						(* not necessary because comp_injs.(cc_id) has been    *)
						(* modified by side effect                             *)
						update_dep state cause (KAPPA mix_id) pert_ids counter env (*TODO: use influence map for this?*)
					)
					liftset (env,pert_ids) 
	in
	(*end sub function*)
	let (liftset_int, liftset_lnk) = try Node.get_lifts u i with exn -> failwith "State.negative_udpate"
	in
	let env,pert_ids = 
		match int_lnk with
			| 0 -> remove_injs state liftset_int IntSet.empty env
			| 1 -> remove_injs state liftset_lnk IntSet.empty env
			| _ -> (*removing both dependencies*) 
				(let env,pert_ids = remove_injs state liftset_lnk IntSet.empty env in 
					remove_injs state liftset_int pert_ids env)
	in
	(env,pert_ids)

(* bind allow for looping bond *)
let bind state cause (u, i) (v, j) side_effects pert_ids counter env =
	
	let intf_u = Node.interface u and intf_v = Node.interface v in
	(* no side effect *)
	let (int_u_i, ptr_u_i) = try intf_u.(i).Node.status with Invalid_argument msg -> invalid_arg (Printf.sprintf "State.bind: agent %s has no site %d" (Environment.name (Node.name u) env) i)
	and (int_v_j, ptr_v_j) = try intf_v.(j).Node.status with Invalid_argument msg -> invalid_arg (Printf.sprintf "State.bind: agent %s has no site %d" (Environment.name (Node.name v) env) j)
	in
	
	let env,side_effects,pert_ids = (*checking for side effects*)
		match ptr_u_i with
		| Node.FPtr _ -> invalid_arg "State.bind"
		| Node.Null -> (env,side_effects,pert_ids)
		| Node.Ptr (u', i') ->
				begin
					Node.set_ptr (u', i') Node.Null;
					let env,pert_ids' = negative_upd state cause (u', i') 1 counter env in
					try (env,Int2Set.add ((Node.get_address u'), i') side_effects, IntSet.union pert_ids' pert_ids)	
					with  Not_found -> invalid_arg "State.bind: Not_found"
				end 
	in
	(* when node is not allocated *)
	let env,side_effects,pert_ids =
		match ptr_v_j with
		| Node.FPtr _ -> invalid_arg "State.bind"
		| Node.Null -> (env,side_effects,pert_ids)
		| Node.Ptr (v', j') ->
			begin
				Node.set_ptr (v', j') Node.Null;
				let env,pert_ids' = negative_upd state cause (v', j') 1 counter env in
				try 
					(env,Int2Set.add ((Node.get_address v'), j') side_effects, IntSet.union pert_ids pert_ids')
				with Not_found -> invalid_arg "State.bind: not found"
			end
	in
	intf_u.(i) <-	{ (intf_u.(i)) with Node.status = (int_u_i, Node.Ptr (v, j)) };
	let env,pert_ids' = negative_upd state cause (u, i) 1 counter env in
	let pert_ids = IntSet.union pert_ids pert_ids' in 
	intf_v.(j) <- { (intf_v.(j)) with Node.status = (int_v_j, Node.Ptr (u, i)) };
	let env,pert_ids' = negative_upd state cause (v, j) 1 counter env in
	let pert_ids = IntSet.union pert_ids pert_ids' in
	(env,side_effects,pert_ids)

let break state cause (u, i) side_effects pert_ids counter env side_effect_free =
	(*creating more cc's may wake up silenced rules*)
	
	let intf_u = Node.interface u and warn = 0 in
	let (int_u_i, ptr_u_i) = intf_u.(i).Node.status
	in
	match ptr_u_i with
	| Node.FPtr _ -> invalid_arg "State.break"
	| Node.Ptr (v, j) ->
			let intf_v = Node.interface v in
			let (int_v_j, ptr_v_j) = intf_v.(j).Node.status
			in
			(intf_u.(i) <-
				{ (intf_u.(i)) with Node.status = (int_u_i, Node.Null); };
				let env,pert_ids' = negative_upd state cause (u, i) 1 counter env in
				intf_v.(j) <-
				{ (intf_v.(j)) with Node.status = (int_v_j, Node.Null); };
				let env,pert_ids'' = negative_upd state cause (v, j) 1 counter env in
				let pert_ids = IntSet.union pert_ids (IntSet.union pert_ids' pert_ids'') in
				if side_effect_free then
					(warn,env,side_effects,pert_ids)
				else
					(warn,env,(Int2Set.add ((Node.get_address v), j) side_effects),pert_ids)
			)
	| Node.Null -> ((warn + 1),env, side_effects,pert_ids)

let modify state cause (u, i) s pert_ids counter env =
	let intf_u = Node.interface u and warn = 0 in
	let (int_u_i, lnk_u_i) = intf_u.(i).Node.status
	in
	match int_u_i with
	| Some j ->
			(intf_u.(i) <-
				{ (intf_u.(i)) with Node.status = ((Some s), lnk_u_i); };
				let warn = if s = j then warn + 1 else warn
				in
				(* if s=j then null event *)
				let env,pert_ids' = (*if s <> j then*) negative_upd state cause (u, i) 0 counter env in 
				(warn,env,IntSet.union pert_ids pert_ids')
			)
	| None ->
			invalid_arg
				("State.modify: node " ^
					((Environment.name (Node.name u) env)^" has no internal state to modify"))

let delete state cause u side_effects pert_ids counter env =
	Node.fold_status
	(fun i (_, lnk) (env,side_effects,pert_ids) ->
		let env,pert_ids' = negative_upd state cause (u, i) 2 counter env in
		let pert_ids = IntSet.union pert_ids pert_ids' in
			(* delete injection pointed by both lnk and int-lifts *)
			match lnk with
			| Node.FPtr _ -> invalid_arg "State.delete"
			| Node.Null -> (env,side_effects,pert_ids)
			| Node.Ptr (v, j) ->
					Node.set_ptr (v, j) Node.Null;
					let env,pert_ids' = negative_upd state cause (v, j) 1 counter env in
					let pert_ids = IntSet.union pert_ids pert_ids' in
					(env,Int2Set.add ((Node.get_address v), j) side_effects,pert_ids)
	)
	u (env,side_effects,pert_ids)

let apply state r embedding_t counter env =
		
	let app state embedding fresh_map (id, i) =
		try
			match id with
			| FRESH j -> (SiteGraph.node_of_id state.graph (IntMap.find j fresh_map), i)
			| KEPT j -> (SiteGraph.node_of_id state.graph (IntMap.find j embedding), i) 
		with 
			| Not_found -> invalid_arg (Printf.sprintf "State.apply: Incomplete embedding when applying rule %s on [%s -> %d]" r.kappa (match id with FRESH j -> (Printf.sprintf "F(%d)" j) | KEPT j -> string_of_int j) i)  
	in
	let rec edit state script psi side_effects pert_ids env =
		(* phi: embedding, psi: fresh map *)
		let sg = state.graph
		and phi = map_of embedding_t
		in
		match script with
		| [] -> (env,state, (side_effects:Int2Set.t), embedding_t, psi, pert_ids)
		| action :: script' ->
				begin
					match action with
					| BND (p, p') ->
							let ((u, i), (v, j)) =
								let (u, i) = app state phi psi p in
								let (v, j) = app state phi psi p'
								in ((u, i), (v, j)) 
							in
							let env,side_effects,pert_ids =
								bind state r.r_id (u, i) (v, j) side_effects pert_ids counter env
							in
							edit state script' psi side_effects pert_ids env
					| FREE (p,side_effect_free) ->
							let x = app state phi psi p in
							let (warn, env, side_effects,pert_ids) = break state r.r_id x side_effects pert_ids counter env side_effect_free
							in
							if warn > 0 then Counter.inc_null_action counter ;
							edit state script' psi side_effects pert_ids env
					| MOD (p, i) ->
							let x = app state phi psi p in
							let warn,env, pert_ids = modify state r.r_id x i pert_ids counter env
							in
							if warn > 0 then Counter.inc_null_action counter ; 
							edit state script' psi side_effects pert_ids env
					| DEL i ->
							let phi_i =
								(try IntMap.find i phi	with Not_found ->	invalid_arg "State.apply: incomplete embedding 3") 
							in
								let node_i = SiteGraph.node_of_id sg phi_i in
								let env,side_effects,pert_ids = delete state r.r_id node_i side_effects pert_ids counter env
								in
								SiteGraph.remove sg phi_i;
								edit state script' psi side_effects pert_ids env
					| ADD (i, name) ->
							let node = Node.create name env in
							let sg = SiteGraph.add sg node in
							(* sg might be different address than sg if max array size  *)
							(* was reached                                              *)
							let j =
								(try SiteGraph.( & ) node
								with
								| Not_found -> invalid_arg "State.apply: not allocated") 
							in
							edit {state with graph = sg} script' (IntMap.add i j psi) side_effects pert_ids env
				end
	in
	edit state r.script IntMap.empty Int2Set.empty IntSet.empty env


let snapshot state counter desc hr env =
	try
		Printf.fprintf desc "# Snapshot [Event: %d, Time: %f]\n" (Counter.event counter) (Counter.time counter) ; 
		let table = Species.of_graph state.graph env in
		if !Parameter.dotOutput then Species.dump desc table hr state.token_vector env 
		else
			begin 
			Hashtbl.iter
			(fun sign specs ->
				List.iter
				(fun (spec,k) -> 
					Printf.fprintf desc "%%init: %d \\\n" k ;
					Species.print desc spec env ;
					Printf.fprintf desc "\n"
				) specs
			) table ;
			Array.iteri
			(fun tk_id v ->
				if v = 0. then ()
				else
					begin
						let tk = Environment.token_of_num tk_id env in
						Printf.fprintf desc "%%init: %s <- %E \n" tk v
					end
			) state.token_vector ;
			Printf.fprintf desc "# End snapshot\n"
			end
	with
		| Sys_error msg -> ExceptionDefn.warning ("Cannot output snapshot: "^msg) 

let dump state counter env =
	if not !Parameter.debugModeOn
	then ()
	else
		(
			Printf.printf "#***[%f] Current state***\n" (Counter.time counter);
			if SiteGraph.size state.graph > 1000 then () else SiteGraph.dump ~with_lift:true state.graph env;
			Hashtbl.fold
			(fun i r _ ->
				let nme =
					try "'" ^ ((Environment.rule_of_num i env) ^ "'")
					with | Not_found -> ""
				in
				let a2,a1  = eval_activity r state counter env in
				if Environment.is_rule i env then
					Printf.printf "#rule[%d]: \t%s %s @ %f[upd:%f(%f)]\n" i nme (Dynamics.to_kappa r env)
					(Random_tree.find i state.activity_tree)
					(Num.float_of_num a2) (Num.float_of_num a1) 
				else
					Printf.printf "#\t%s %s [found %d]\n" nme (Dynamics.to_kappa r env)
					(Num.int_of_num (instance_number i state env))
			) state.rules ();
			Array.iteri
			(fun mix_id opt ->
					let injprod_hp_opt = try state.nl_injections.(mix_id) with Invalid_argument _ -> None in
					begin
					match injprod_hp_opt with
						| None -> ()
						| Some injprod_hp -> 
							(Printf.printf "#Unary[%d]: '%s' %s has %d unary instances\n" mix_id
									(Environment.kappa_of_num mix_id env)
									(Mixture.to_kappa false (kappa_of_id mix_id state) env)
									(InjProdHeap.size injprod_hp);
									if SiteGraph.size state.graph > 1000 then ()
									else
										InjProdHeap.iteri
										(fun inj_id inj_prod ->
											Printf.printf "#\t ip#%d: %s \n" inj_id	(InjProduct.to_string inj_prod)
										) injprod_hp 
							) 
					end ;
					match opt with
					| None -> ()
					| Some comp_injs ->
							(Printf.printf "#Var[%d]: '%s' %s has %d instances\n" mix_id
									(Environment.kappa_of_num mix_id env)
									(Mixture.to_kappa false (kappa_of_id mix_id state) env)
									(Num.int_of_num (instance_number mix_id state env));
									if SiteGraph.size state.graph > 1000 then ()
									else
										Array.iteri
										(fun cc_id injs_opt ->
													match injs_opt with
													| None -> Printf.printf "#\tCC[%d] : na\n" cc_id
													| Some injs ->
															InjectionHeap.iteri
																(fun ad injection ->
																			Printf.printf "#\tCC[%d]#%d: %s \n" cc_id ad
																				(Injection.to_string injection))
																injs
										)	comp_injs
							)	
		 	) state.injections ;
			Array.iteri
			(fun var_id opt ->
					match opt with
					| None ->
							Printf.printf "#x[%d]: '%s' na\n" var_id
								((fun (s,_) -> s) (Environment.alg_of_num var_id env))
					| Some v ->
						match value state var_id counter env with
							| Num.I x -> 
								Printf.printf "#x[%d]: '%s' %d \n" var_id 
								((fun (s,_) -> s) (Environment.alg_of_num var_id env))
								x
							| Num.F x -> 
								Printf.printf "#x[%d]: '%s' %E \n" var_id 
								((fun (s,_) -> s) (Environment.alg_of_num var_id env))
								x
							| Num.I64 x -> 
								Printf.printf "#x[%d]: '%s' %Ld \n" var_id 
								((fun (s,_) -> s) (Environment.alg_of_num var_id env))
								x
			) state.alg_variables;
			Array.iteri
			(fun mix_id mix_opt ->
				match mix_opt with
				| None -> () 
				| Some m -> 
					let num = instance_number mix_id state env 
					and name = Environment.kappa_of_num mix_id env
					in
					Printf.printf "kappa[%d] '%s' %s\n" mix_id name (Num.to_string num)
			) state.kappa_variables ;
			Array.iteri
			(fun tk_id v ->
				Printf.printf "token[%d]: '%s' %f\n" tk_id (Environment.token_of_num tk_id env) v
			) state.token_vector ;
			IntMap.fold
			(fun i pert _ ->
				Printf.printf "#pert[%d]: %s\n" i (Environment.pert_of_num i env)
			)
			state.perturbations ();
			Printf.printf "#**********\n"
	)

let dot_of_flux desc state  env =
	
	let print_flux flux pos =
		Hashtbl.iter
		(fun r_id map ->
			let str1 = try Environment.rule_of_num r_id env with Not_found -> Dynamics.to_kappa (rule_of_id r_id state) env
			in
			IntMap.iter
			(fun r_id' n ->
				if n=0. then () 
				else
				let color,arrowhead,edge = 
					if n<0. then ("red3","tee","filled") 
					else ("green3","normal","filled") 
				in 
				let str2 = try Environment.rule_of_num r_id' env with Not_found -> Dynamics.to_kappa (rule_of_id r_id' state) env in
				Printf.fprintf desc "\"%s\" -> \"%s\" [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];\n" str1 str2 (abs (int_of_float n)) n color arrowhead
			) map 
		) flux 
	in
	Printf.fprintf desc "digraph G{ label=\"Flux map\" ; labelloc=\"t\" ; node [shape=box,style=filled,fillcolor=lightskyblue]\n" ;
	Hashtbl.iter
	(fun r_id rule ->
		let r_nme = try Environment.rule_of_num r_id env with Not_found -> (*rule is anonymous*) Dynamics.to_kappa (rule_of_id r_id state) env in
		Printf.fprintf desc "\"%s\" ;\n" r_nme 
	) state.rules ;
	print_flux state.flux true;
	Printf.fprintf desc "}\n" ;
	close_out desc
