open Mods
open Tools
open ExceptionDefn
open Primitives
open Graph
open ValMap
open Random_tree

type obs = { label : string; expr : Expr.alg_expr }
type t =
    { graph : SiteGraph.t;
      injections : (component_injections option) array;
      nl_injections : (InjProdHeap.t option) array ;
      rules : (int, rule) Hashtbl.t;
      perturbations : perturbation IntMap.t;
      kappa_variables : (Mixture.t option) array;
      token_vector : float array ;
      alg_variables : Nbr.t option array;
      observables : obs list;
      influence_map : (int, (int IntMap.t list) IntMap.t) Hashtbl.t ;
      mutable activity_tree : Random_tree.tree;
      wake_up : Precondition.t ;
      flux : (int,float IntMap.t) Hashtbl.t ;
      mutable silenced : IntSet.t (*Set of rule ids such that eval-activity was overestimated and whose activity was manually set to a lower value*)
    }
and component_injections = (InjectionHeap.t option) array

let pp_injections f map =
  Pp.set IntMap.bindings Pp.comma
	 (fun f (i,j) -> Format.fprintf f " %i->%i" i j)
	 f map

let get_graph state = state.graph
let get_nl_injections state = state.nl_injections

let fold_graph f state init =
  Graph.SiteGraph.fold f state.graph init

let silence rule_id state =
  state.silenced <- (IntSet.add rule_id state.silenced)
let unsilence rule_id state =
  state.silenced <- (IntSet.remove rule_id state.silenced)

let kappa_of_id id state =
	try
		match state.kappa_variables.(id) with
		| None -> raise Not_found
		| Some mix -> mix
	with | Invalid_argument msg -> invalid_arg ("State.kappa_of_id: " ^ msg)

let rule_of_id id state = Hashtbl.find state.rules id
let update_rule id value state =
  let r = rule_of_id id state in
  Hashtbl.replace state.rules id {r with k_def = Expr.CONST value}
let update_token tk_id value state =
  state.token_vector.(tk_id) <- (Nbr.to_float value)

let maybe_find_perturbation pert_id state =
  try Some (IntMap.find pert_id state.perturbations)
  with | Not_found -> None
let remove_perturbation pert_id state =
  {state with perturbations = IntMap.remove pert_id state.perturbations}
let all_perturbations state =
  let rec aux min max =
    if min >= max then Mods.IntSet.singleton max
    else Mods.IntSet.union (aux min ((max+min)/2)) (aux ((max+min)/2 + 1) max)
  in
  if IntMap.is_empty state.perturbations
  then IntSet.empty
  else aux 0 (IntMap.max_key state.perturbations)

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

(** [instance_number mix_id state] returns the number of instances of mixture
    [mix_id] in implicit state [state] *)
let instance_number mix_id state env =
  if Environment.is_empty_lhs mix_id env then (Nbr.I64 Int64.one)
  else
    match state.injections.(mix_id) with
    | None -> (Nbr.I64 Int64.zero)
    | Some component_injections ->
       let act =
	 Array.fold_left
	   (fun act opt ->
	    match opt with
	    | Some injs ->
	       let n = InjectionHeap.size injs in
	       if n=0 then Int64.zero else (Int64.mul act (Int64.of_int n))
	    | None -> Int64.zero
	   )
	   Int64.one component_injections
       in
       (Nbr.I64 act)

(**[nl_instance_number mix_id state] returns the number of instances of non local mixture [mix_id] in implicit state [state]*)
let nl_instance_number mix_id state env =
	if Environment.is_empty_lhs mix_id env then (Nbr.I 1) 
	else
		match state.nl_injections.(mix_id) with
		| None -> (Nbr.I 0)
		| Some inj_prod_hp -> Nbr.I (InjProdHeap.size inj_prod_hp)


(**[instances_of_square mix_id state] returns [(inj_0,codom_0,prod_0);...] the list of full and valid embeddings inj_i, their codomains codom_j and the explicit product prod_i=[phi_cc0;phi_cc1;...]*)
let instances_of_square ?(disjoint=false) mix_id radius_def state env =
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
														 let opt = try Some (Injection.codomain phi (part_inj, part_codom))
															   with Injection.Clashing -> None
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

let value_state_alg_op state counter ?(time=Counter.time counter) env = function
  | Term.CPUTIME -> Nbr.F (Sys.time ())
  | Term.TIME_VAR -> Nbr.F time
  | Term.EVENT_VAR -> Nbr.I (Counter.event counter+Counter.null_event counter)
  | Term.NULL_EVENT_VAR -> Nbr.I (Counter.null_event counter)
  | Term.PROD_EVENT_VAR ->Nbr.I (Counter.event counter)

type alg_stack_element =
  | TO_EXEC_ALG of Term.bin_alg_op * Expr.alg_expr
  | TO_EXEC_COMP of Term.compare_op * Expr.alg_expr
  | TO_EXEC_BOOL of Term.bool_op * Expr.alg_expr Ast.bool_expr
  | TO_COMPUTE_ALG of Term.bin_alg_op * Nbr.t
  | TO_COMPUTE_COMP of Term.compare_op * Nbr.t
  | TO_COMPUTE_UN of Term.un_alg_op

let rec exec_alg :
type a. t -> Counter.t -> ?time:float -> Environment.t ->
     (t -> Counter.t -> ?time:float -> Environment.t ->
      Nbr.t -> alg_stack_element list -> a) ->
     Expr.alg_expr -> alg_stack_element list -> a =
    fun state counter ?time env with_value alg sk ->
    match alg with
    | Expr.BIN_ALG_OP (op,(a,_),(b,_)) ->
       exec_alg state counter ?time env with_value a (TO_EXEC_ALG (op,b)::sk)
    | Expr.UN_ALG_OP (op,(a,_)) ->
       exec_alg state counter ?time env with_value a (TO_COMPUTE_UN op::sk)
    | Expr.STATE_ALG_OP (op) ->
       with_value state counter ?time env (value_state_alg_op state counter ?time env op) sk
    | Expr.ALG_VAR i ->
       (match state.alg_variables.(i) with
       | None ->
	  exec_alg state counter ?time env with_value
		   (fst (snd env.Environment.algs.NamedDecls.decls.(i))) sk
       | Some var_f -> with_value state counter ?time env var_f sk)
    | Expr.KAPPA_INSTANCE i ->
       with_value state counter ?time env (instance_number i state env) sk
    | Expr.TOKEN_ID i ->
       with_value state counter ?time env (Nbr.F (state.token_vector.(i))) sk
    | Expr.CONST n ->
       with_value state counter ?time env n sk

let rec with_value_alg state counter ?time env n = function
  | [] -> n
  | TO_EXEC_ALG (op,alg) :: sk ->
     exec_alg state counter ?time env with_value_alg alg (TO_COMPUTE_ALG (op,n)::sk)
  | TO_COMPUTE_ALG (op,n1) :: sk ->
     with_value_alg state counter ?time env (Nbr.of_bin_alg_op op n1 n) sk
  | TO_COMPUTE_UN op :: sk ->
     with_value_alg state counter ?time env (Nbr.of_un_alg_op op n) sk
  | (TO_COMPUTE_COMP _ | TO_EXEC_COMP _ | TO_EXEC_BOOL _)
    :: _ -> failwith "type error in with_value_alg"
and with_value_alg_bool state counter ?time env n = function
  | TO_EXEC_ALG (op,alg) :: sk ->
     exec_alg state counter ?time env with_value_alg_bool alg (TO_COMPUTE_ALG (op,n)::sk)
  | TO_COMPUTE_ALG (op,n1) :: sk ->
     with_value_alg_bool state counter ?time env (Nbr.of_bin_alg_op op n1 n) sk
  | TO_COMPUTE_UN op :: sk ->
     with_value_alg_bool state counter ?time env (Nbr.of_un_alg_op op n) sk
  | TO_EXEC_COMP (op,alg) :: sk ->
     exec_alg state counter ?time env with_value_alg_bool alg (TO_COMPUTE_COMP (op,n)::sk)
  | TO_COMPUTE_COMP (op,n1) :: sk ->
     with_value_bool state counter ?time env (Nbr.of_compare_op op n1 n) sk
  | TO_EXEC_BOOL _ :: _ -> failwith "type error in with_value_alg_bool"
  | [] -> failwith "type error in with_value_alg_bool"
and exec_bool state counter ?time env expr sk =
  match expr with
  | Ast.TRUE -> with_value_bool state counter ?time env true sk
  | Ast.FALSE -> with_value_bool state counter ?time env false sk
  | Ast.BOOL_OP (op,(a,_),(b,_)) ->
     exec_bool state counter ?time env a (TO_EXEC_BOOL (op,b) :: sk)
  | Ast.COMPARE_OP (op,(a,_),(b,_)) ->
     exec_alg state counter ?time env with_value_alg_bool a (TO_EXEC_COMP (op,b) :: sk)
and with_value_bool state counter ?time env b = function
  | [] -> b
  | TO_EXEC_BOOL (Term.OR,expr) :: sk when b ->
     with_value_bool state counter ?time env true sk
  | TO_EXEC_BOOL (Term.AND,expr) :: sk when not b ->
     with_value_bool state counter ?time env false sk
  | TO_EXEC_BOOL (op,expr) :: sk ->
     exec_bool state counter ?time env expr sk
  | _ -> failwith "type error in with_value_bool"

let value_bool state counter ?time env expr =
  exec_bool state counter ?time env expr []
let value_alg state counter ?time env alg =
  exec_alg state counter ?time env with_value_alg alg []

let alg_of_id state counter ?time env id =
  try
    match state.alg_variables.(id) with
    | None ->
       value_alg state counter ?time env
		 (fst (snd env.Environment.algs.NamedDecls.decls.(id)))
    | Some var_f -> var_f
  with | Invalid_argument msg -> invalid_arg ("State.kappa_of_id: " ^ msg)

(*missing recomputation of dependencies*)
let set_variable id v state =
  try state.alg_variables.(id) <- Some v
  with Invalid_argument msg -> invalid_arg ("State.set_variable: "^msg)

let total_activity state =
  Random_tree.total state.activity_tree

(**[eval_activity rule state] returns the evaluation of the overestimated activity of rule [rule] in implicit state [state]*)
let eval_activity ?using rule state counter env =
  let mix_id = Mixture.get_id rule.lhs in
  let a_2 = (*overestimated activity of binary instances of the rule*)
    let k = value_alg state counter env rule.k_def in
    let n = (match using with None -> instance_number mix_id state env | Some x -> Nbr.I x) in
    if Nbr.is_zero n then (Nbr.I 0) else Nbr.mult k n
  in
  (*overestimated activity of unary instances of the rule*)
  let a_1 =
    match rule.k_alt with
    | (None,_) -> (Nbr.I 0)
    | (Some x ,_)->
       let k = value_alg state counter env x in
       let n = nl_instance_number mix_id state env in
       if Nbr.is_zero n then (Nbr.I 0) else Nbr.mult k n
  in
  (a_2,a_1)

let pert_of_id state id = IntMap.find id state.perturbations

let update_activity state ?cause var_id counter env =
	if not (Environment.is_rule var_id env) then ()
	else
		let rule = rule_of_id var_id state in
		let a2,a1 = eval_activity rule state counter env in
		let alpha = Nbr.to_float (Nbr.add a2 a1) in (*a1 is zero if rule doesn't have ambiguous molarity*)
		
		match cause with
		|Some cause when !Parameter.fluxModeOn ->
		  begin
		    try
		      let alpha_old = Random_tree.find var_id state.activity_tree in
		      Random_tree.add var_id alpha state.activity_tree ;
		      let w = (alpha -. alpha_old) in
		      update_flux state cause var_id w		
		    with Invalid_argument msg -> invalid_arg ("State.update_activity: "^msg)
		  end
		| Some _ | None -> 
			    Random_tree.add var_id alpha state.activity_tree 

let unsilence_rule state rule counter env =
  if IntSet.is_empty state.silenced
  then Debug.tag_if_debug "No silenced rule, skipping"
  else
    IntSet.fold
      (fun id _ ->
       Debug.tag_if_debug "Updating silenced rule %d" id;
       update_activity state ~cause:rule.r_id id counter env ;
       unsilence id state ;
      ) state.silenced ()

(* compute complete embedding of mix into sg at given root --for           *)
(* initialization phase                                                    *)
let generate_embeddings err_fmt sg u_i mix comp_injs env =
  let mix_id = Mixture.get_id mix in
  let rec iter cc_id sg comp_injs  =
    if cc_id = (Mixture.arity mix)
    then (sg, comp_injs)
    else
      let id_opt = Mixture.root_of_cc mix cc_id in
      match id_opt with
      | None -> iter (cc_id + 1) sg comp_injs
      | Some id_root ->
	 let opt_inj =
	   Matching.component
	     err_fmt
	     (Injection.empty (Mixture.size_of_cc cc_id mix) (mix_id,cc_id))
	     id_root (sg, u_i) mix
	 in
	 match opt_inj with
	 | None -> iter (cc_id + 1) sg comp_injs
	 | Some (injection, port_map) ->
	    (* port_map: u_i -> [(p_k,0/1);...] if port k of node i is   *)
	    (* int/lnk-tested by map                                     *)
	    let opt =
	      (try comp_injs.(cc_id)
	       with Invalid_argument msg ->
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
	    let () = comp_injs.(cc_id) <- Some cc_id_injections in
	    let sg =
	      SiteGraph.add_lift sg injection port_map env in
	    iter (cc_id + 1) sg comp_injs
  in
  iter 0 sg comp_injs

(**[initialize_embeddings state mix_list] *) (*mix list is the list of kappa observables one wishes to track during simulation*)
let initialize_embeddings err_fmt state mix_list counter env =
	SiteGraph.fold 
	(fun i node_i state ->
		List.fold_left
		(fun state mix ->
			let injs = state.injections in
			let opt = try injs.(Mixture.get_id mix) with exn -> (print_string ("caught: "^(Printexc.to_string exn)) ; raise exn) in 
			let comp_injs =
				match opt with
				| None -> Array.make (Mixture.arity mix) None
				| Some comp_injs -> comp_injs in
			(* complement the embeddings of mix in sg using node i  as anchor for matching *)
			let (sg, comp_injs) = generate_embeddings err_fmt state.graph i mix comp_injs env
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
     Array.iteri
       (fun j opt ->
	match opt with
	| None -> () (*empty pattern*)
	| Some mix ->
	   Debug.tag_if_debug "%s -+-> %a?" (Dynamics.to_kappa r env)
			      (Kappa_printer.mixture false env) mix;
	   let glueings = Dynamics.enable r mix env in
	   (*glueings: [phi_0;...;phi_n] partial embeddings list*)
	   match glueings with
	   | [] -> Debug.tag_if_debug "No"
	   | _ ->
	      Debug.tag_if_debug "Yes";
	      add_influence influence_map i j glueings) patterns
    ) rules;
  influence_map

let dot_of_influence_map desc state env =
  Format.fprintf desc
"@[<v>digraph G{ node [shape=box, style=filled, fillcolor=lightskyblue];@," ;
  Hashtbl.iter
    (fun r_id rule ->
     let opt = if rule.is_pert
	       then "[shape=invhouse,fillcolor=lightsalmon]"
	       else "" in
     Format.fprintf desc "@[\"%d:%s\" %s;@]@," r_id (Dynamics.to_kappa rule env) opt
    ) state.rules ;
  Array.iteri
    (fun mix_id mix_opt ->
     if Environment.is_rule mix_id env then ()
     else
       match mix_opt with
       | None -> ()
       | Some mix ->
	  Format.fprintf desc "@[@[<h>\"%d:%a\"@]@ [shape=ellipse,fillcolor=palegreen3] ;@]@,"
			 mix_id (Kappa_printer.mixture false env) mix
    ) state.kappa_variables ;
  Hashtbl.iter
    (fun r_id act_map ->
     let rule = rule_of_id r_id state in
     let n_label = Dynamics.to_kappa rule env in
     Pp.set IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (fun f (mix_id,glueings) ->
	     let n_label' f =
	       if Environment.is_rule mix_id env then
		 let rule'=rule_of_id mix_id state in
		 Format.fprintf f "%s" (Dynamics.to_kappa rule' env)
	       else
		 let mix = kappa_of_id mix_id state in
		 Kappa_printer.mixture false env f mix
	     in
	     let pp_glueing f glue =
	       Format.fprintf
		 f "[%a]" (Pp.set IntMap.bindings Pp.comma
				  (fun f (i,j) -> Format.fprintf f "%i->%i" j i))
		 glue in
	     Format.fprintf desc "@[\"%d:%s\" ->@ @[<h>\"%d:%t\"@]@ [label=@[<h>\"%a\"@]];@]"
			    r_id n_label mix_id n_label'
			    (Pp.list Pp.colon pp_glueing) glueings
	    ) desc act_map;
     Format.pp_print_cut desc ()
    ) state.influence_map ;
  Format.fprintf desc "@]}@."

let initialize err_fmt sg token_vector rules kappa_vars obs pert counter env =
  let dim_rule = max (List.length rules) 1 in
  let dim_kappa = (List.length kappa_vars) + 1 in
  let dim_var = Array.length env.Environment.algs.NamedDecls.decls in

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
	     let i = r.r_id in
	     let patterns =
	       if Mixture.is_empty r.lhs
	       then patterns (*nothing to track if left hand side is empty*)
	       else (kappa_var_table.(Mixture.get_id r.lhs) <- Some r.lhs;
		     r.lhs :: patterns)
	     in
	     (Hashtbl.replace rule_table i r; patterns)
	    )
	    kappa_vars rules
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
			  List.fold_left
			    (fun cont (expr, lbl) ->
			     {
			       expr = expr ;
			       label = replace_space lbl;
			     } :: cont
			    ) [] obs ;
			activity_tree = Random_tree.create dim_rule;
			influence_map = influence_table ;
			wake_up = wake_up_table;
			flux = if !Parameter.fluxModeOn then Hashtbl.create 5 else Hashtbl.create 0 ;
			silenced = IntSet.empty
		}
	in
	
	Debug.tag_if_debug "\t * Initializing injections...";
	let state = (*initializing injections*)
		initialize_embeddings err_fmt state_init kappa_variables counter env
	in
	
	Debug.tag_if_debug "\t * Initializing variables...";

	Debug.tag_if_debug "\t * Initializing wake up map for side effects...";
	let state =
		(* initializing preconditions on pattern list for wake up map *)
		List.fold_left
		(fun state mix ->
					{state with wake_up = Precondition.add mix state.wake_up}
		)
		state kappa_variables
	in
	
	Debug.tag_if_debug "\t * Initializing activity tree...";
	let () = (*initializing activity tree*)
		Hashtbl.iter
		(fun id rule ->
			(*rule could be a perturbation*)
			if not rule.is_pert then
				let a2,a1 = eval_activity rule state counter env in
				let alpha_rule = Nbr.to_float (Nbr.add a1 a2) in
				Random_tree.add id alpha_rule state.activity_tree
		)
			state.rules
	in
	Debug.tag_if_debug "\t * Computing influence map...";
	let im = build_influence_map state.rules state.kappa_variables env 
	in
	({state with influence_map = im (*activity_tree updated by side effect!*)}, env)
	
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
	update_activity state mix_id counter env

module Embedding:
sig
  type info = {map: int IntMap.t ;
	       roots : IntSet.t ;
	       components : IntSet.t IntMap.t option;
	       depth_map : int IntMap.t option}
  type t = DISJOINT of info
	 | CONNEX of info
	 | AMBIGUOUS of info

  val empty : t
  val map_of : t -> int IntMap.t
end
  = struct
  type info = {map: int IntMap.t ;
	       roots : IntSet.t ;
	       components : IntSet.t IntMap.t option;
	       depth_map : int IntMap.t option}
  type t = DISJOINT of info
	 | CONNEX of info
	 | AMBIGUOUS of info

  let empty = CONNEX {map=IntMap.empty ; roots = IntSet.empty ; components = None ; depth_map = None}
  let map_of embedding = match embedding with CONNEX e | DISJOINT e | AMBIGUOUS e -> e.map
end

(**returns either valid embedding or raises Null_event if injection is no longer valid --function also cleans inj_hp and nodes as a side effect*)
let check_validity injprod with_full_components radius state counter env =
	try
		let embedding,roots,codomain = 
			InjProduct.fold_left
			(fun (embedding,roots,codom) inj_i ->
				if Injection.is_trashed inj_i then (*injection product is no longer valid because one of its element is trashed*) 
					(Debug.tag_if_debug
					   "Clashing because one of the component of injection product is no longer valid" ;
					raise (Null_event 4))
				else
				(*injection product might be invalid because co-domains are no longer connected*)
				  let map,codom =
				    try Injection.codomain inj_i (embedding,codom)
						with Injection.Clashing -> raise (Null_event 2) (*clashing instance*) in
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
			{Embedding.map = embedding ;
			 Embedding.components = Some (IntMap.add 0 components IntMap.empty) ;
			 Embedding.depth_map = Some d_map ;
			 Embedding.roots = roots}
			)
		else
		  let () = Debug.tag_if_debug
			     "Clashing because injection product's codomain is no longer connex" in
		  raise (Null_event 0)
	with
		| Null_event i -> (*correcting over approximation*)
			begin
				clean_injprod injprod state counter env ;
				raise (Null_event i)
			end
	
let select_injection (a2,radius_def) (a1,radius_alt) state mix counter env =
	if Mixture.is_empty mix then Embedding.empty
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
					let radius = match radius_alt with None -> (-1) | Some v -> Nbr.to_int (value_alg state counter env v) in
  				let injprod = InjProdHeap.random prod_inj_hp in (*injprod is an array of size #cc(mix_id) and injprod.(i):Injection.t a partial injection of cc(i)*)
  				let embedding = check_validity injprod radius false state counter env in (*returns either valid embedding or raises Null_event if injection is no longer valid --function also cleans inj_hp and nodes as a side effect*)
  				(Embedding.CONNEX embedding)
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
  												  with Injection.Clashing ->
  												    let () = Debug.tag_if_debug
													       "Clashing because codomains of selected partial injections are overlapping" in
  												    raise (Null_event 2) in
  												(i + 1, total_inj,total_cod, roots)
  											  with
  											| Invalid_argument msg ->
  													invalid_arg ("State.select_injection: " ^ msg)))
  						(0, IntMap.empty, IntSet.empty, IntSet.empty) comp_injs
  				in
  				
					let radius = match radius_def with None -> (-1) | Some v -> Nbr.to_int (value_alg state counter env v) in
					
  				let rec build_component_map (roots,codomain) depth_map component_map = 
  					if IntSet.is_empty roots then (depth_map,component_map) (*no more root to check*)
  					else
  						let root = IntSet.choose roots in
  						(*components will contain only node that can be the root of a non local rule because filter is enabled *)
  						let (_,d_map,components,remaining_roots) = connex ~d_map:depth_map ~filter:true ~start_with:root (roots,codomain) radius true state env 
  						in
  						if not ((IntSet.cardinal remaining_roots) = (IntSet.cardinal roots) - 1) then
  							(Debug.tag_if_debug "Clashing because selected instance of n-nary rule is not totally disjoint" ; 
  							raise (Null_event 1)
  							)
  						else () ;
  						let component_map = IntMap.add root components component_map
  						in
  						build_component_map (remaining_roots,codomain) d_map component_map (*remaining roots should be empty if rule has only 2 CCs*)
  				in
  				
  				if clash_if_unary then (*now checking with the contex that the embedding is indeed binary*)
  					let (d_map,comp_map) = build_component_map (roots,codomain) IntMap.empty IntMap.empty in (*raises Null_event if roots are not connected*)
					(Embedding.DISJOINT
					   {Embedding.map=embedding;
					    Embedding.depth_map=Some d_map;
					    Embedding.roots = roots ;
					    Embedding.components = Some comp_map})
  				else
				  let ambiguous_embedding =
				    Embedding.AMBIGUOUS
				      {Embedding.map=embedding;
				       Embedding.depth_map=None;
				       Embedding.roots = roots;
				       Embedding.components = None} in
  				  if not env.Environment.has_intra
				  then ambiguous_embedding
				  else let r = rule_of_id (Mixture.get_id mix) state in
  				       match r.cc_impact with
  				       | None -> ambiguous_embedding
  				       | Some (con_map,_,_) ->
  					  if IntMap.is_empty con_map then ambiguous_embedding
  					  else
  					    (Debug.tag_if_debug "Connectedness is not required for this rule but will compute it nonetheless because rule might create more intras" ;
  					     let (d_map,comp_map) = build_component_map (roots,codomain) IntMap.empty IntMap.empty in
  					     (Embedding.AMBIGUOUS
						{Embedding.map=embedding;
						 Embedding.depth_map=Some d_map;
						 Embedding.roots = roots;
						 Embedding.components = Some comp_map}))
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
		let () =
		  Debug.tag_if_debug "Picked rule [%d] at random." rule_id in
		let r =
			try rule_of_id rule_id state
			with | Not_found -> invalid_arg "State.draw_rule" 
		in
		let a2,a1 =
			try eval_activity r state counter env
			with | Not_found -> invalid_arg "State.draw_rule"
		in
		let alpha = Nbr.to_float (Nbr.add a2 a1) in
		(*correction: issue #40*)
		if alpha = 0. then Random_tree.add rule_id alpha state.activity_tree ;

		let (_:unit) =
			if alpha = infinity then ()
			else
				if alpha > alpha' then 
					if IntSet.mem rule_id state.silenced then (Debug.tag_if_debug "Real activity is below approximation... but I knew it!") else invalid_arg "State.draw_rule: activity invariant violation"
				else ();
				let rd = Random.float 1.0
				in
				if rd > (alpha /. alpha')
				then
				  (Debug.tag_if_debug
				     "Clashing in order to correct for overestimation of activity of rule %d"
				     rule_id;
				   Random_tree.add rule_id alpha state.activity_tree ;
				   raise (Null_event 3)) (*null event because of over approximation of activity*)
				else ()
		in
		let embedding_type = 
			let _,radius = r.k_alt
			in
			try select_injection (Nbr.to_float a2,radius) (Nbr.to_float a1,radius) state r.lhs counter env with 
			| Null_event 1 | Null_event 2 as exn -> (*null event because of clashing instance of a binary rule*)
				if counter.Counter.cons_null_events > !Parameter.maxConsecutiveClash then 
					begin
					  Debug.tag_if_debug "Max consecutive clashes reached, I am giving up square approximation at this step" ;
						let _ = Counter.reset_consecutive_null_event counter in
					
						let radius = match radius with None -> (-1) | Some v -> Nbr.to_int (value_alg state counter env v) in
  				
						let embeddings = instances_of_square ~disjoint:true rule_id radius state env in
						let alpha,_ = eval_activity ~using:(List.length embeddings) r state counter env in 
						let alpha = Nbr.to_float alpha in
						begin
							Random_tree.add rule_id alpha state.activity_tree ;
							silence rule_id state ; (*rule activity will be underestimated if not awaken when a rule creates more cc's*)
							Debug.tag_if_debug
							  "Rule [%d]'s activity was corrected to %f"
							  rule_id alpha;
							raise exn
						end
					end
				else
				  (Debug.tag_if_debug "Rule [%d] is clashing" rule_id;
				   raise exn )
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
								let is_free =
								  try not (Node.is_bound (node, site_id))
								  with Invalid_argument msg ->
								    invalid_arg (Format.asprintf "State.wake_up : no site %d in agent %a"
												site_id (Environment.print_agent env) (Node.name node))
								in
								let new_candidates =
									if is_free
									then
										Precondition.find_all (Node.name node) site_id None	None is_free state.wake_up
									else
										(let link_opt =
												match Node.follow node site_id with
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
										(match Node.follow node site_id with
											| None -> None
											| Some (node', site_id') ->
													Some (Node.name node', site_id'))
									in
									Precondition.find_all (Node.name node) site_id (Node.internal_state (node, site_id)) link_opt	is_free state.wake_up
								in
								IntMap.add node_id (Int2Set.union old_candidates new_candidates) wake_up_map
				)
		)	modifs wake_up_map

let update_dep state ?cause dep_in pert_ids counter env =
  let rec iter env dep_to_check pert_ids =
    if Term.DepSet.is_empty dep_to_check then (env,pert_ids)
    else
      let dep_in = Term.DepSet.choose dep_to_check in
      let depset = Environment.get_dependencies dep_in env in
      let new_to_check =
	Term.DepSet.union (Term.DepSet.remove dep_in dep_to_check) depset in
      match dep_in with
      | Term.TOK t_id -> (* token counter is changed *)
	 let () = Debug.tag_if_debug
		    "Token %d is changed, updating %a" t_id
		    (Pp.set Term.DepSet.elements Pp.comma Term.print_dep_type)
		    depset in
	 iter env new_to_check pert_ids
      | Term.ALG v_id ->
	 (*variable v_id is changed -by a perturbation if used as
	 initial dep_in argument*)
	 let () = Debug.tag_if_debug
		    "Variable %d is changed, updating %a" v_id
		    (Pp.set Term.DepSet.elements Pp.comma Term.print_dep_type)
		    depset in
	 iter env new_to_check pert_ids
      | Term.RULE r_id ->
	 (*rule activity is changed -by a perturbation if used as
	 initial dep_in argument*)
	 let () = update_activity state ?cause r_id counter env in
	 let () = Debug.tag_if_debug
		    "Rule %d is changed, updating %a" r_id
		    (Pp.set Term.DepSet.elements Pp.comma Term.print_dep_type)
		    depset in
	 iter env new_to_check pert_ids
      | Term.PERT p_id ->
	 if IntMap.mem p_id state.perturbations
	 then (*pertubation p_id is still alive and should be tried*)
	   iter env new_to_check (IntSet.add p_id pert_ids)
	 else (*pertubation p_id is removed and should be discarded from dependencies*)
	   iter (Environment.remove_dependencies dep_in (Term.PERT p_id) env) new_to_check pert_ids
      | Term.ABORT p_id ->
	 if IntMap.mem p_id state.perturbations
	 then iter env new_to_check (IntSet.add p_id pert_ids)
	 else iter (Environment.remove_dependencies dep_in (Term.PERT p_id) env) new_to_check pert_ids
      | Term.KAPPA i ->
	 (*No need to update kappa observable, it will be updated if plotted*)
	 let () = if not (Term.DepSet.is_empty depset) then
		    Debug.tag_if_debug
		      "Observable %d is changed, updating %a" i
		      (Pp.set Term.DepSet.elements Pp.comma Term.print_dep_type)
		      depset in
	 iter env new_to_check pert_ids
      | Term.EVENT | Term.TIME ->
		      iter env new_to_check pert_ids
  in
  iter env (Term.DepSet.singleton dep_in) pert_ids

let update_dep_value state counter env v dep =
  let value = value_alg state counter env v in
  match dep with
  | Term.TOK t_id -> update_token t_id value state
  | Term.ALG v_id -> set_variable v_id value state
  | Term.RULE r_id -> update_rule r_id value state
  | Term.KAPPA _ | Term.PERT _ | Term.ABORT _ | Term.EVENT | Term.TIME -> ()

let enabled r state =
  try Hashtbl.find state.influence_map (Mixture.get_id r.lhs)
  with Not_found -> IntMap.empty

let positive_update ?(with_tracked=[]) err_fmt state r (phi: int IntMap.t) psi
		    side_modifs pert_intro counter env = (*pert_intro is temporary*)
  (* sub function find_new_inj *)
  let update_tracked var_id cc_id state embedding comp_injs tracked =
    try
      let m = kappa_of_id var_id state in
      let map,cod = (fun (x,y) -> (ref x,ref y))
		      (Injection.to_map embedding) in
      for cpt = 0 to Mixture.arity m - 1 do
	if cpt <> cc_id then
	  let embedding',codomain' =
	    match comp_injs.(cpt) with
	    | None -> raise (ExceptionDefn.Break 0)
	    | Some hp ->
	       let s = InjectionHeap.size hp in
	       if s = 0 then raise (Break 0)
	       else
		 let rec find_compatible cpt =
		   if cpt < 0 then raise (Break 1)
		   else
		     let inj = InjectionHeap.find cpt hp in
		     if (Injection.is_trashed inj)
		     then failwith "Incorrect heap size"
		     else
		       try Injection.codomain inj (!map,!cod)
		       with Injection.Clashing ->
			 find_compatible (cpt-1)
		 in
		 find_compatible (s-1)
	  in
	  map := embedding' ; cod := codomain';
      done ;
      Debug.tag_if_debug "Observable %d was found with embedding [%a]"
			 var_id pp_injections !map ;
      (var_id,!map)::tracked
    with
    | Break 0 ->
       let () = Debug.tag_if_debug
		  "Incomplete embedding, no observable recorded" in
       tracked
    | Break 1 ->
       let () = Debug.tag_if_debug
		  "Cannot complete embedding, clashing instances" in
       tracked
  in
  let find_new_inj state var_id mix cc_id node_id root pert_ids
		   already_done_map new_injs tracked env =
    Debug.tag_if_debug "Trying to embed Var[%d] using root %d at node %d"
		       var_id root node_id;
    let root_node_set =
      try IntMap.find var_id already_done_map
      with Not_found -> Int2Set.empty in
    let comp_injs =
      try
	match state.injections.(var_id) with
	| None -> (*may happen when initial graph was empty*)
	   let ar = Array.make (Mixture.arity mix) None in
	   state.injections.(var_id) <- (Some ar) ;
	   ar
	| Some injs -> injs
      with Invalid_argument msg ->
	invalid_arg ("State.positive_update: " ^ msg) in
    let cc_id_injections =
      try
	match comp_injs.(cc_id) with
	| Some injections -> injections
	| None -> InjectionHeap.create !Parameter.defaultHeapSize
      with Invalid_argument msg ->
	invalid_arg ("State.positive_update: " ^ msg) in
    let reuse_embedding =
      match InjectionHeap.next_alloc cc_id_injections with
      | Some phi ->
	 (Debug.tag_if_debug
	    "reusing injection: %a" Injection.print phi;
	  Injection.flush phi (var_id,cc_id))
      | None -> Injection.empty (Mixture.size_of_cc cc_id mix) (var_id,cc_id)
    in
    let opt_emb =
      Matching.component ~already_done:root_node_set err_fmt
			 reuse_embedding root (state.graph, node_id) mix in
    match opt_emb with
    | None ->
       (Debug.tag_if_debug "No new embedding was found";
	(env,state, pert_ids, already_done_map, new_injs,tracked)
       )
    | Some (embedding, port_map) ->
       Debug.tag_if_debug "New embedding: %a"
			  Injection.print embedding;
       let cc_id_injections =
	 InjectionHeap.alloc embedding cc_id_injections in
       comp_injs.(cc_id) <- Some cc_id_injections ;
       let graph =
	 SiteGraph.add_lift state.graph embedding port_map env in
       let state = {state with graph = graph} in
       begin
	 (*a new embedding was found for var_id*)
	 let tracked =
	   if Environment.is_tracked var_id env
	   then (*completing the embedding if incomplete*)
	     update_tracked var_id cc_id state embedding comp_injs tracked
	   else tracked
	 in
	 let () = update_activity state ~cause:r.r_id var_id counter env in
	 let env,pert_ids = (*updating rule activities that depend --transitively-- on var_id*)
	   update_dep state ~cause:r.r_id (Term.KAPPA var_id) pert_ids counter env
	 in
	 let already_done_map' =
	   IntMap.add var_id (Int2Set.add (root, node_id) root_node_set) already_done_map 
	 in
	 let new_injs' =
	   if Environment.is_nl_rule var_id env
	   then embedding::new_injs
	   else new_injs in
	 (env,state, pert_ids, already_done_map',new_injs',tracked)
       end
  in
  (* end of sub function find_new_inj definition *)
  
  let vars_to_wake_up = enabled r state in
  let env,state,pert_ids,already_done_map,new_injs,tracked =
    IntMap.fold
      (fun var_id map_list (env, state,pert_ids,already_done_map,new_injs, tracked) ->
       Debug.tag_if_debug
	 "Influence map tells me I should look for new injections of var[%d]"
	 var_id;
       List.fold_left
	 (fun (env,state,pert_ids,already_done_map, new_injs,tracked) glue ->
	  let opt = IntMap.root glue in
	  match opt with
	  | None -> invalid_arg "State.positive_update"
	  | Some (root_mix,root_rhs) ->
	     let node_id =
	       if IntSet.mem root_rhs r.added then
		 (try IntMap.find root_rhs psi
		  with Not_found -> invalid_arg "State.positive_update 1")
	       else
		 try IntMap.find root_rhs phi
		 with Not_found ->
		   (Debug.tag_if_debug
		      "I was looking for the image of agent %d by embedding %a"
		      root_rhs pp_injections phi;
		    Debug.tag_if_debug
		      "Glueing was [%a]" pp_injections glue;
		    invalid_arg "State.positive_update 3")
	     in
	     let mix =
	       let opt =
		 try state.kappa_variables.(var_id)
		 with Invalid_argument msg ->
		   invalid_arg ("State.positive_update: " ^ msg)
	       in
	       match opt with
	       | Some mix -> mix
	       | None -> invalid_arg "State.positive_update"
	     in
	     let cc_id = Mixture.component_of_id root_mix mix in
	     (*already_done_map is empty because glueings are guaranteed to be different by construction*)
	     find_new_inj state var_id mix cc_id node_id root_mix
			  pert_ids already_done_map new_injs tracked env
	 ) (env,state, pert_ids, already_done_map, new_injs,tracked) map_list
      ) vars_to_wake_up (env, state, IntSet.empty, IntMap.empty,[],with_tracked)  
  in

  (*updating tokens if rule is hybrid*)

  let env,pert_ids =
    List.fold_left
      (fun (env,pert_ids) (v,t_id) ->
       let value = Nbr.to_float (value_alg state counter env v) in
       try
	 Debug.tag_if_debug "adding %f token(s) %d" value t_id;
	 state.token_vector.(t_id) <- state.token_vector.(t_id) +. value;
	 (*updating rule activities that depend on |t_id|*)
	 update_dep state ~cause:r.r_id (Term.TOK t_id) pert_ids counter env
       with Invalid_argument _ ->
	 failwith "State.positive_update: invalid token id"
      ) (env,pert_ids) r.add_token
  in
  let env,pert_ids =
    List.fold_left
      (fun (env,pert_ids) (v,t_id) ->
       let value = Nbr.to_float (value_alg state counter env v) in
       try
	 Debug.tag_if_debug "removing %f token(s) %d" value t_id;

	 state.token_vector.(t_id) <- state.token_vector.(t_id) -. value ;
	 update_dep state ~cause:r.r_id (Term.TOK t_id) pert_ids counter env
       with Invalid_argument _ ->
	 failwith "State.positive_update: invalid token id"
      ) (env,pert_ids) r.rm_token
  in

  (*Checking if any side effect needs to be checked*)
  if Int2Set.is_empty side_modifs
  then (env,state,pert_ids,new_injs,tracked)
  else	(*Handling side effects*)
    let wu_map = IntMap.empty in
    let () = Debug.tag_if_debug
	       "Checking positive update entailed by side effects" in
    let wu_map = wake_up state 1 side_modifs wu_map env in
    let wu_map = wake_up state 2 pert_intro wu_map env in
    let (env,state, pert_ids,_,new_injs,tracked) =
      IntMap.fold
	(fun node_id candidates (env,state, pert_ids, already_done_map,new_injs,tracked) ->
	 Debug.tag_if_debug
	   "Side effect on node %d forces me to look for new embedding..."
	   node_id;
	 let node = SiteGraph.node_of_id state.graph node_id in
	 Int2Set.fold
	   (fun (var_id, cc_id) (env, state, pert_ids, already_done_map, new_injs,tracked) ->
	    let mix =
	      let opt =
		try state.kappa_variables.(var_id)
		with Invalid_argument msg ->
		  invalid_arg ("State.positive_update: " ^ msg)
	      in
	      match opt with
	      | Some mix -> mix
	      | None -> invalid_arg "State.positive_update"
	    in
	    let possible_roots =
	      Mixture.ids_of_name (Node.name node) cc_id mix
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
  Debug.tag_if_debug "Negative update as indicated by %a#%d site %d"
		     (Environment.print_agent env) (Node.name u) (Node.get_address u) i;

  (* sub-function that removes all injections pointed by lifts --if they *)
  (* still exist                                                         *)
	let remove_injs state liftset pert_ids env =
		let injections = state.injections
		in
		LiftSet.fold
			(fun phi (env,pert_ids) ->
				if Injection.is_trashed phi then (LiftSet.remove liftset phi ; (env,pert_ids)) 
				else
				let mix_id, cc_id, inj_id =
					let i = Injection.get_address phi in
					let (m,c) = Injection.get_coordinate phi
					in
					m,c,i
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
							   let a_i = Mixture.agent_of_id i mix in
							   let u_j = try SiteGraph.node_of_id state.graph j
								     with exn ->
								       invalid_arg (Format.asprintf
										      "State.negative_update: Node #%d is no longer in the graph and injection %a of mixture %a was pointing on it!"
										      j Injection.print phi (Kappa_printer.mixture false env) mix)
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
									| Mixture.WLD -> ()
									| Mixture.BND | Mixture.TYPE _ |	Mixture.FREE ->
										let (_, lifts) = try Node.get_lifts u_j site_id with exn -> invalid_arg ("State.negative_update: "^(Printexc.to_string exn))
										in
										LiftSet.remove lifts phi
									) a_i ()
								) phi () ;
								let _ = InjectionHeap.remove inj_id injs_cc_id in
								if !Parameter.fluxModeOn then update_activity state ~cause mix_id counter env ;
						end
						in
						(* comp_injs.(cc_id) <- Some injs_cc_id; *)
						(* not necessary because comp_injs.(cc_id) has been    *)
						(* modified by side effect                             *)
						update_dep state ~cause (Term.KAPPA mix_id) pert_ids counter env (*TODO: use influence map for this?*)
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
  Debug.tag_if_debug "Binding nodes %d and %d \n" (Node.name u) (Node.name v);
  let intf_u = Node.interface u and intf_v = Node.interface v in
  (* no side effect *)
  let (int_u_i, ptr_u_i) =
    try intf_u.(i).Node.status
    with Invalid_argument msg ->
      invalid_arg (Format.asprintf "State.bind: agent %a has no site %d"
				  (Environment.print_agent env) (Node.name u) i)
  and (int_v_j, ptr_v_j) =
    try intf_v.(j).Node.status
    with Invalid_argument msg ->
      invalid_arg (Format.asprintf "State.bind: agent %a has no site %d"
				  (Environment.print_agent env) (Node.name v) j)
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
	     (Format.asprintf "State.modify: node %a%s"
			      (Environment.print_agent env) (Node.name u)
			      " has no internal state to modify")

let delete state cause u side_effects pert_ids counter env =
  Debug.tag_if_debug "Deleting node %d" (Node.name u);
  Node.fold_status
    (fun i (_, lnk) (env,side_effects,pert_ids) ->
     let env,pert_ids' = negative_upd state cause (u, i) 2 counter env in
     let pert_ids = IntSet.union pert_ids pert_ids' in
     (* delete injection pointed by both lnk and int-lifts *)
     match lnk with
     | Node.FPtr _ -> invalid_arg "State.delete"
     | Node.Null -> (env,side_effects,pert_ids)
     | Node.Ptr (v, j) ->
        Debug.tag_if_debug "Deleting node %d which was connected to node %d"
			   (Node.name u) (Node.name v);
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
      | Primitives.FRESH j ->
	 (SiteGraph.node_of_id state.graph (IntMap.find j fresh_map), i)
      | Primitives.KEPT j ->
	 (SiteGraph.node_of_id state.graph (IntMap.find j embedding), i)
    with
    | Not_found ->
       invalid_arg
	 (Format.asprintf
	    "State.apply: Incomplete embedding when applying rule %a->%a on [%s -> %d]"
	    (Kappa_printer.mixture false env) r.lhs
	    (Kappa_printer.mixture false env) r.rhs
	    (match id with
	       Primitives.FRESH j -> (Format.sprintf "F(%d)" j)
	     | Primitives.KEPT j -> string_of_int j) i)
  in
  let rec edit state script psi side_effects pert_ids env =
    (* phi: embedding, psi: fresh map *)
    let sg = state.graph
    and phi = Embedding.map_of embedding_t
    in
    match script with
    | [] -> (env,state, (side_effects:Int2Set.t), embedding_t, psi, pert_ids)
    | action :: script' ->
       begin
	 match action with
	 | Primitives.BND (p, p') ->
	    let (u, i) = app state phi psi p in
	    let (v, j) = app state phi psi p' in
	    let env,side_effects,pert_ids =
	      bind state r.r_id (u, i) (v, j) side_effects pert_ids counter env
	    in
	    edit state script' psi side_effects pert_ids env
	 | Primitives.FREE (p,side_effect_free) ->
	    let x = app state phi psi p in
	    let (warn, env, side_effects,pert_ids) =
	      break state r.r_id x side_effects pert_ids
		    counter env side_effect_free in
	    if warn > 0 then Counter.inc_null_action counter;
	    edit state script' psi side_effects pert_ids env
	 | Primitives.MOD (p, i) ->
	    let x = app state phi psi p in
	    let warn,env, pert_ids =
	      modify state r.r_id x i pert_ids counter env in
	    if warn > 0 then Counter.inc_null_action counter;
	    edit state script' psi side_effects pert_ids env
	 | Primitives.DEL i ->
	    let phi_i =
	      (try IntMap.find i phi
	       with Not_found ->
		 invalid_arg "State.apply: incomplete embedding 3")
	    in
	    let node_i = SiteGraph.node_of_id sg phi_i in
	    let env,side_effects,pert_ids =
	      delete state r.r_id node_i side_effects pert_ids counter env in
	    SiteGraph.remove sg phi_i;
	    edit state script' psi side_effects pert_ids env
	 | Primitives.ADD (i, name) ->
	    let node = Node.create name env in
	    let sg = SiteGraph.add sg node in
	    (* sg might be different address than sg if max array size  *)
	    (* was reached                                              *)
	    let j =
	      (try SiteGraph.( & ) node
	       with
	       | Not_found -> invalid_arg "State.apply: not allocated")
	    in
	    edit {state with graph = sg} script' (IntMap.add i j psi)
		 side_effects pert_ids env
       end
  in
  edit state r.script IntMap.empty Int2Set.empty IntSet.empty env


let snapshot state counter desc hr env =
  let desc = Format.formatter_of_out_channel desc in
  try
    Format.fprintf desc "# Snapshot [Event: %d, Time: %f]@.@[<v>"
		   (Counter.event counter) (Counter.time counter);
    let table = Species.of_graph state.graph env in
    if !Parameter.dotOutput
    then Species.dump desc table hr state.token_vector env
    else
      begin
	Hashtbl.iter
	  (fun sign specs ->
	   List.iter
	     (fun (spec,k) ->
	      Format.fprintf desc "@[<v>%%init: %d\\@,@[<h>%a@]@]@," k
			     (Species.print env) spec
	     ) specs
	  ) table ;
	Array.iteri
	  (fun tk_id v ->
	   if v = 0. then ()
	   else
	       let tk = Environment.token_of_num tk_id env in
	       Format.fprintf desc "%%init: %s <- %E@," tk v
	  ) state.token_vector ;
	Format.fprintf desc "@]# End snapshot@."
      end
  with
  | Sys_error msg ->
     ExceptionDefn.warning
       (fun f -> Format.fprintf
		  f "Cannot output snapshot: %s" msg)

let dump_rules err_fmt state env =
  Hashtbl.iter (fun i r -> Dynamics.dump err_fmt r env) state.rules

let dump state counter env =
  if not !Parameter.debugModeOn then ()
  else
    (
      let f = Format.std_formatter in
      Format.fprintf f "#***[%f] Current state***\n" (Counter.time counter);
      if SiteGraph.size state.graph > 1000 then ()
      else SiteGraph.dump ~with_lift:true env f state.graph;
      Hashtbl.fold
	(fun i r _ ->
	 let nme =
	   try "'" ^ ((Environment.rule_of_num i env) ^ "'")
	   with | Not_found -> ""
	 in
	 let a2,a1  = eval_activity r state counter env in
	 if Environment.is_rule i env then
	   Format.fprintf f "#rule[%d]: \t%s %s @@ %f[upd:%f(%f)]@\n"
			  i nme (Dynamics.to_kappa r env)
			  (Random_tree.find i state.activity_tree)
			  (Nbr.to_float a2) (Nbr.to_float a1)
	 else
	   Format.fprintf f "#\t%s %s [found %d]@\n" nme (Dynamics.to_kappa r env)
			  (Nbr.to_int (instance_number i state env))
	) state.rules ();
      Array.iteri
	(fun mix_id opt ->
	 let injprod_hp_opt =
	   try state.nl_injections.(mix_id) with Invalid_argument _ -> None in
	 begin
	   match injprod_hp_opt with
	   | None -> ()
	   | Some injprod_hp ->
	      (Format.fprintf
		 f "#Unary[%d]: '%s' %a has %d unary instances@\n" mix_id
		 (Environment.kappa_of_num mix_id env)
		 (Kappa_printer.mixture false env) (kappa_of_id mix_id state)
		 (InjProdHeap.size injprod_hp);
	       if SiteGraph.size state.graph > 1000 then ()
	       else
		 InjProdHeap.iteri
		   (fun inj_id inj_prod ->
		    Format.fprintf f "#\t ip#%d: %a @\n"
				   inj_id InjProduct.print inj_prod
		   ) injprod_hp
	      )
	 end ;
	 match opt with
	 | None -> ()
	 | Some comp_injs ->
	    (Format.fprintf f "#Var[%d]: '%s' %a has %d instances@\n" mix_id
			    (Environment.kappa_of_num mix_id env)
			    (Kappa_printer.mixture false env)
			    (kappa_of_id mix_id state)
			    (Nbr.to_int (instance_number mix_id state env));
	     if SiteGraph.size state.graph > 1000 then ()
	     else
	       Array.iteri
		 (fun cc_id injs_opt ->
		  match injs_opt with
		  | None -> Format.fprintf f "#\tCC[%d] : na@\n" cc_id
		  | Some injs ->
		     InjectionHeap.iteri
		       (fun ad injection ->
			Format.fprintf f "#\tCC[%d]#%d: %a @\n" cc_id ad
				       Injection.print injection)
		       injs
		 ) comp_injs
	    )
	) state.injections ;
      Tools.iteri
	(fun var_id ->
	 Format.fprintf f "#x[%d]: '%s' %a @\n" var_id
			(fst (Environment.alg_of_num var_id env))
			Nbr.print
			(alg_of_id state counter env var_id)
	) (Array.length state.alg_variables);
      Array.iteri
	(fun mix_id mix_opt ->
	 match mix_opt with
	 | None -> ()
	 | Some m ->
	    let num = instance_number mix_id state env
	    and name = Environment.kappa_of_num mix_id env
	    in
	    Format.fprintf f "kappa[%d] '%s' %a@\n" mix_id name Nbr.print num
	) state.kappa_variables ;
      Array.iteri
	(fun tk_id v ->
	 Format.fprintf f "token[%d]: '%s' %f@\n" tk_id
			(Environment.token_of_num tk_id env) v
	) state.token_vector ;
      IntMap.fold
	(fun i pert () ->
	 Format.fprintf f "#pert[%d]: %a@\n"
			i (Kappa_printer.perturbation env) pert
	)
	state.perturbations ();
      Format.fprintf f "#**********@."
    )

let dot_of_flux desc state  env =
  let print_flux flux pos =
    Hashtbl.iter
      (fun r_id map ->
       let str1 = try Environment.rule_of_num r_id env
		  with
		    Not_found -> Dynamics.to_kappa (rule_of_id r_id state) env
       in
       IntMap.iter
	 (fun r_id' n ->
	  if n=0. then ()
	  else
	    let color,arrowhead,edge =
	      if n<0. then ("red3","tee","filled")
	      else ("green3","normal","filled")
	    in
	    let str2 = try Environment.rule_of_num r_id' env
		       with Not_found ->
			 Dynamics.to_kappa (rule_of_id r_id' state) env in
	    Format.fprintf desc "\"%s\" -> \"%s\" [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];@\n" str1 str2 (abs (int_of_float n)) n color arrowhead
			) map 
		) flux 
	in
	Format.fprintf desc
		       "digraph G{ label=\"Flux map\" ; labelloc=\"t\" ; node [shape=box,style=filled,fillcolor=lightskyblue]@\n";
	Hashtbl.iter
	  (fun r_id rule ->
	   let r_nme = try Environment.rule_of_num r_id env with Not_found -> (*rule is anonymous*) Dynamics.to_kappa (rule_of_id r_id state) env in
	   Format.fprintf desc "\"%s\" ;@\n" r_nme 
	) state.rules ;
	print_flux state.flux true;
	Format.fprintf desc "}@."

let observables_header state =
  Tools.array_map_of_list (fun obs -> obs.label) state.observables

let observables_values env counter ?(time=counter.Counter.time) state =
  (time, Tools.array_map_of_list
	   (fun obs -> value_alg state counter ~time env obs.expr)
	   state.observables)

module Safe = struct
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
    	   let a2,a1 = eval_activity rule state counter env in
  	   let alpha = Nbr.to_float (Nbr.add a2 a1) in
    	   if x < alpha then
    	     if (IntSet.mem r_id state.silenced || Random_tree.is_infinite r_id state.activity_tree) then ()
    	     else
      	       let msg = Format.sprintf
			   "Activity of rule %s is underapproximated (%f < %f)"
			   (Environment.rule_of_num r_id env) x alpha in
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
      		if j=0 then lnk_j
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
    			   raise (Invariant_violation
    				    (
    				      Format.sprintf
					"Injection (%d,%d) is missing in site '_' of node %d"
					(fst (Injection.get_coordinate inj))
					(snd (Injection.get_coordinate inj)) j
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
    			   raise (Invariant_violation
    				    (
    				      Format.sprintf
					"Injection (%d,%d) is missing in site '_' of node %d"
					(fst (Injection.get_coordinate inj))
					(snd (Injection.get_coordinate inj)) j
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
  	   (*let _ = try Environment.unary_rule_of_num r_id env
 with Not_found -> raise (Invariant_violation (Format.sprintf "Rule %d is not unary" r_id))
			in*)
  	   match injprod_hp_opt with
  	   | None -> ()
  	   | Some injprod_hp ->
  	      let n = InjProdHeap.size injprod_hp in
  	      ar.(r_id) <- n
  	  ) state.nl_injections ;
	Format.eprintf "%E " (Counter.time counter) ;
	Array.iteri (fun _ n -> Format.eprintf "%d " n) ar ;
	Format.eprintf "@." ;
      end

  with
  | Invariant_violation msg ->
     Parameter.debugModeOn := true;
     dump state counter env ;
     Format.eprintf "%s@." msg ;
     exit 1
end
