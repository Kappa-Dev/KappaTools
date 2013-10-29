open Mods
open ExceptionDefn

type t = {
	signatures : Signature.t IntMap.t ; 
	fresh_kappa : int ;
	num_of_kappa : int StringMap.t ; 
	kappa_of_num : string IntMap.t ;
	
	token_of_num : string IntMap.t ; 
	num_of_token : int StringMap.t ;
	fresh_token : int ;
	
	fresh_name : int ;
	num_of_name : int StringMap.t ;
	name_of_num : string IntMap.t ;
	
	dependencies :  DepSet.t DepMap.t ; (*ALG i or KAPPA i -> {ALG j, RULE j} = modifying i implies recomputing j --closure is done*)  
	
	num_of_rule : int StringMap.t ;
	rule_of_num : string IntMap.t ;
	
	num_of_unary_rule : int StringMap.t ;
	unary_rule_of_num : string IntMap.t ;
	
	fresh_alg : int ;
	num_of_alg : (int*Num.t option) StringMap.t ;
	alg_of_num : (string*Num.t option) IntMap.t ;
	
	fresh_pert : int ;
	num_of_pert : int StringMap.t ;
	pert_of_num : string IntMap.t ;
	(*rule_of_pert : IntSet.t IntMap.t ;*)
	
	rule_indices : IntSet.t ;
	empty_lhs : IntSet.t ;
	
	nl_elements : Int2Set.t IntMap.t ; (*ag_nme -> {(r_id,cc_id),...}*)
	root_of_nl_rule : int Int2Map.t ;
	has_intra : bool ;
	
	tracking_enabled : bool ;
	active_cflows : int ;
	track : IntSet.t ;
	
	desc_table : (string,out_channel) Hashtbl.t
	(*log : Log.t*)
}

let empty = 
	{signatures = IntMap.empty ; 
	fresh_name = 0;
	num_of_name = StringMap.empty ;
	name_of_num = IntMap.empty ;
	num_of_kappa = StringMap.empty ; 
	kappa_of_num = IntMap.empty ;
	num_of_rule = StringMap.empty ;
	num_of_unary_rule = StringMap.empty ;
	num_of_alg = StringMap.empty ;
	alg_of_num = IntMap.empty ;
	rule_of_num = IntMap.empty ;
	unary_rule_of_num = IntMap.empty ; 
	fresh_kappa = 0 ; 
	fresh_alg = 0 ;
	fresh_pert = 0 ;
	token_of_num = IntMap.empty ; 
	num_of_token = StringMap.empty ;
	fresh_token = 0 ;
	
	num_of_pert = StringMap.empty ;
	pert_of_num = IntMap.empty ;
	(*rule_of_pert = IntMap.empty ;*)
	rule_indices = IntSet.empty ;
	dependencies = DepMap.empty ;
	empty_lhs = IntSet.empty ;
	nl_elements = IntMap.empty ;
	root_of_nl_rule = Int2Map.empty ;
	has_intra = false ;
	tracking_enabled = false ;
  active_cflows = 0 ;
	track = IntSet.empty ;
	
	desc_table = Hashtbl.create 2 
}

let get_desc file env = 
	try Hashtbl.find env.desc_table file with 
		| Not_found -> let d = open_out file in (Hashtbl.add env.desc_table file d ; d)

let close_desc env =
	Hashtbl.iter (fun file d -> close_out d) env.desc_table 

let tracking_enabled env = env.tracking_enabled
let inc_active_cflows env = {env with active_cflows = env.active_cflows + 1}
let dec_active_cflows env = {env with active_cflows = env.active_cflows - 1}
let active_cflows env = env.active_cflows
let track id env = {env with track = IntSet.add id env.track}
	
let untrack id env = {env with track = IntSet.remove id env.track}
let is_tracked id env = IntSet.mem id env.track

(**in order to declare that mix_id -which is the lhs of a unary rule- is expecting an agent named [ag_nme] with a site named [ste_nme] as the root of component [cc_id] of the injection*)
let declare_nl_element mix_id cc_id ag_nme env =
	let coordSet = try IntMap.find ag_nme env.nl_elements with Not_found -> Int2Set.empty in
	let coordSet' = Int2Set.add (mix_id,cc_id) coordSet in
	let nl_elements' = IntMap.add ag_nme coordSet' env.nl_elements in
	{env with nl_elements = nl_elements'}
	
let init_roots_of_nl_rules env =
	let map =  
		IntMap.fold 
		(fun ag_nme coordSet map -> 
			Int2Set.fold 
			(fun (r_id,cc_id) map -> 
				Int2Map.add (r_id,cc_id) ag_nme map
			) coordSet map
		) env.nl_elements Int2Map.empty
	in
	{env with root_of_nl_rule = map}

(*for given rule id, and cc_id returns the name of the root agent used for injecting cc_id*)	
let root_of_nl_rule r_id cc_id env = try Some (Int2Map.find (r_id,cc_id) env.root_of_nl_rule) with Not_found -> None 

let get_nl_coord ag_nme env = IntMap.find ag_nme env.nl_elements
let is_nl_root ag_nme env = IntMap.mem ag_nme env.nl_elements

let is_nl_rule r_id env = IntMap.mem r_id env.unary_rule_of_num

let next_pert_id env = env.fresh_pert
let declare_empty_lhs id env = {env with empty_lhs = IntSet.add id env.empty_lhs}
let is_empty_lhs id env = IntSet.mem id env.empty_lhs

(*let log env = env.log*)
let name i env = IntMap.find i env.name_of_num
let num_of_name nme env = StringMap.find nme env.num_of_name
let num_of_kappa lab env = StringMap.find lab env.num_of_kappa 
let kappa_of_num i env =  IntMap.find i env.kappa_of_num 
let num_of_rule lab env = StringMap.find lab env.num_of_rule
let rule_of_num i env =  IntMap.find i env.rule_of_num

let num_of_unary_rule lab env = StringMap.find lab env.num_of_unary_rule
let unary_rule_of_num i env =  IntMap.find i env.unary_rule_of_num


let pert_of_num i env = IntMap.find i env.pert_of_num
let num_of_pert lab env = StringMap.find lab env.num_of_pert
let is_rule i env = IntSet.mem i env.rule_indices
let num_of_alg s env = StringMap.find s env.num_of_alg
let	alg_of_num i env = IntMap.find i env.alg_of_num
(*let bind_pert_rule pid rid env = {env with rule_of_pert = IntMap.add pid rid env.rule_of_pert}*)
(*let rule_of_pert pid env = try Some (IntMap.find pid env.rule_of_pert) with Not_found -> None *)

let declare_pert (lab,pos) env = 
	let opt = try Some (num_of_pert lab env) with Not_found -> None in
		match opt with
			| Some i -> (ExceptionDefn.warning ~with_pos:pos "Perturbation is defined twice, ignoring additional occurence" ; (env,i)) 
			| None ->
				let i,env = (env.fresh_pert,{env with fresh_pert = env.fresh_pert+1})
				in
				({env with
					pert_of_num = IntMap.add i lab env.pert_of_num ;
					num_of_pert = StringMap.add lab i env.num_of_pert
				},i)

let declare_name nme pos env = 
	let opt = try Some (num_of_name nme env) with Not_found -> None in
		match opt with
			| Some i -> 
				(ExceptionDefn.warning ~with_pos:pos ("Agent '"^nme^"' is defined twice, ignoring additional occurence") ; (env,i)) 
			| None ->
				let i,env = (env.fresh_name,{env with fresh_name = env.fresh_name+1})
				in
					({env with
						name_of_num = IntMap.add i nme env.name_of_num ;
						num_of_name = StringMap.add nme i env.num_of_name
					},i)

let name_number env = env.fresh_name

let add_dependencies dep dep' env = 
	let set = 
		try DepMap.find dep env.dependencies	with Not_found -> DepSet.empty
	in
		{env with dependencies = DepMap.add dep (DepSet.add dep' set) env.dependencies}	 

let remove_dependencies dep dep' env = 
	let set = try DepSet.remove dep' (DepMap.find dep env.dependencies) with Not_found -> DepSet.empty
	in
		if DepSet.is_empty set then {env with dependencies = DepMap.remove dep env.dependencies}
		else
			{env with dependencies = DepMap.add dep set env.dependencies}
			
let get_dependencies dep env = try DepMap.find dep env.dependencies with Not_found -> DepSet.empty

let declare_rule rule_lbl id env =
	match rule_lbl with
		| None -> env
		| Some (r_nme,pos) ->
			if StringMap.mem r_nme env.num_of_rule then raise (Semantics_Error (pos, ("Rule name "^r_nme^" is already used")))
			else
				let nr = StringMap.add r_nme id env.num_of_rule
				and rn = IntMap.add id r_nme env.rule_of_num
				in
					{env with num_of_rule = nr ; rule_of_num = rn}

let declare_unary_rule rule_lbl id env =
	match rule_lbl with
		| None -> env
		| Some (r_nme,pos) ->
			if StringMap.mem r_nme env.num_of_unary_rule then raise (Semantics_Error (pos, ("Rule name "^r_nme^" is already used")))
			else
				let nr = StringMap.add r_nme id env.num_of_unary_rule
				and rn = IntMap.add id r_nme env.unary_rule_of_num
				in
					{env with num_of_unary_rule = nr ; unary_rule_of_num = rn}

let id_of_site agent_name site_name env =
	let n = StringMap.find agent_name env.num_of_name in
	let sign = IntMap.find n env.signatures	in
		Signature.num_of_site site_name sign 

let site_of_id agent_id site_id env = 
	let sign = IntMap.find agent_id env.signatures in
		Signature.site_of_num site_id sign
		
let id_of_state agent_name site_name state env =
	let agent_id = StringMap.find agent_name env.num_of_name in 
	let sign = IntMap.find agent_id env.signatures in
		Signature.num_of_internal_state site_name state sign

let state_of_id agent_id id_site id_state env = 
	let sign = IntMap.find agent_id env.signatures in
		Signature.internal_state_of_num id_site id_state sign

let declare_sig sign pos env = 
	if IntMap.mem (Signature.control sign) env.signatures && not !Parameter.implicitSignature then
		raise (Semantics_Error (pos, "Signature already defined"))
	else
		{env with signatures = IntMap.add (Signature.control sign) sign env.signatures}

let declare_token name pos env = 
	if StringMap.mem name env.num_of_token && not !Parameter.implicitSignature then
		raise (Semantics_Error (pos, Printf.sprintf "Token %s already defined" name))
	else
		let new_id = env.fresh_token in
		{env with 
			token_of_num = IntMap.add new_id name env.token_of_num ; 
			num_of_token = StringMap.add name new_id env.num_of_token ;
			fresh_token = env.fresh_token + 1
		}

let num_of_token = fun str env -> StringMap.find str env.num_of_token
let token_of_num = fun id env -> IntMap.find id env.token_of_num

	
let declare_var_kappa ?(from_rule=false) label_pos_opt env =
	let label,pos = match label_pos_opt with
		| Some (label,pos) -> (label,pos)
		| None -> ("%anonymous"^(string_of_int env.fresh_kappa),Tools.no_pos) (*geek*)
	in 
	let already_defined = 
		(try let _ = num_of_kappa label env in true with Not_found -> false)
		||
		(try let _ = num_of_alg label env in true with Not_found -> false)
	in
		if already_defined then raise (Semantics_Error (pos, (Printf.sprintf "Label '%s' already defined" label)))
		else
			let np = StringMap.add label env.fresh_kappa env.num_of_kappa
			and pn = IntMap.add env.fresh_kappa label env.kappa_of_num
			and fp = env.fresh_kappa+1
			in
				({env with 
					rule_indices = if from_rule then IntSet.add env.fresh_kappa env.rule_indices else env.rule_indices ; 
					num_of_kappa = np ; 
					kappa_of_num = pn ; 
					fresh_kappa = fp},fp-1)

let declare_var_alg label_pos_opt const env =
	let label,pos = match label_pos_opt with
		| Some (label,pos) -> (label,pos)
		| None -> invalid_arg "Environment.declare_var_alg"
	in
	let already_defined = 
		(try let _ = num_of_kappa label env in true with Not_found -> false)
		||
		(try let _ = num_of_alg label env in true with Not_found -> false)
	in
		if already_defined then raise (Semantics_Error (pos, (Printf.sprintf "Label '%s' already defined" label)))
		else
			let np = StringMap.add label (env.fresh_alg,const) env.num_of_alg
			and pn = IntMap.add env.fresh_alg (label,const) env.alg_of_num
			and fp = env.fresh_alg+1
			in
				({env with 
					num_of_alg = np ; 
					alg_of_num = pn ; 
					fresh_alg = fp},fp-1)


let get_sig agent_id env = 
	if agent_id = -1 then invalid_arg "Environment.get_sig: Empty agent has no signature"
	else 
		IntMap.find agent_id env.signatures

let check agent_name pos_ag site_name pos_site int_state env =
	let agent_id = try StringMap.find agent_name env.num_of_name with
		Not_found -> raise (Semantics_Error (pos_ag, "Undeclared agent \""^agent_name^"\""))
	in
	let sign = try IntMap.find agent_id env.signatures with 
		Not_found -> raise (Semantics_Error (pos_ag, "Undeclared signature for agent \""^agent_name^"\""))
	in
		let _ = try Signature.num_of_site site_name sign with
			Not_found -> raise (Semantics_Error (pos_site, "Site "^site_name^" is not consistent with \""^agent_name^"\"'s signature"))
		and _ = try Signature.num_of_internal_state site_name int_state sign with  
			Not_found -> raise (Semantics_Error (pos_site, "Internal state \""^int_state^"\" of site '"^site_name^"' is not consistent with \""^agent_name^"\"'s signature "))
		in
			()

let default_state name_id site_id env = 
	let sign = try IntMap.find name_id env.signatures with
		| Not_found -> invalid_arg ("Undeclared agent id\""^(string_of_int name_id)^"\"")
	in
		Signature.default_num_value site_id sign
				
