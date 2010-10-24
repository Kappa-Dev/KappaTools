open Mods
open ExceptionDefn

type t = {
	signatures : Signature.t IntMap.t ; 
	fresh_kappa : int ;
	num_of_kappa : int StringMap.t ; 
	kappa_of_num : string IntMap.t ;
	
	fresh_name : int ;
	num_of_name : int StringMap.t ;
	name_of_num : string IntMap.t ;
	
	dependencies :  DepSet.t DepMap.t ; (*ALG i or KAPPA i -> {ALG j, RULE j} = modifying i implies recomputing j --closure is done*)  
	
	num_of_rule : int StringMap.t ;
	rule_of_num : string IntMap.t ;
	
	fresh_alg : int ;
	num_of_alg : int StringMap.t ;
	alg_of_num : string IntMap.t ;
	
	fresh_pert : int ;
	num_of_pert : int StringMap.t ;
	pert_of_num : string IntMap.t ;
	rule_of_pert : int IntMap.t ;
	
	rule_indices : IntSet.t
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
	num_of_alg = StringMap.empty ;
	alg_of_num = IntMap.empty ;
	rule_of_num = IntMap.empty ;
	fresh_kappa = 1 ; (*keeping 0 for empty mixture*)
	fresh_alg = 0 ;
	fresh_pert = 0 ;
	num_of_pert = StringMap.empty ;
	pert_of_num = IntMap.empty ;
	rule_of_pert = IntMap.empty ;
	rule_indices = IntSet.empty ;
	dependencies = DepMap.empty ;
}

let next_pert_id env = env.fresh_pert

(*let log env = env.log*)
let name i env = IntMap.find i env.name_of_num
let num_of_name nme env = StringMap.find nme env.num_of_name
let num_of_kappa lab env = StringMap.find lab env.num_of_kappa 
let kappa_of_num i env =  IntMap.find i env.kappa_of_num
let num_of_rule lab env = StringMap.find lab env.num_of_rule
let rule_of_num i env =  IntMap.find i env.rule_of_num
let pert_of_num i env = IntMap.find i env.pert_of_num
let num_of_pert lab env = StringMap.find lab env.num_of_pert
let is_rule i env = IntSet.mem i env.rule_indices
let num_of_alg s env = StringMap.find s env.num_of_alg
let	alg_of_num i env = IntMap.find i env.alg_of_num
let bind_pert_rule pid rid env = {env with rule_of_pert = IntMap.add pid rid env.rule_of_pert}
let rule_of_pert pid env = try Some (IntMap.find pid env.rule_of_pert) with Not_found -> None 

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
	
let declare_var_kappa ?(from_rule=false) label_pos_opt env =
	let label,pos = match label_pos_opt with
		| Some (label,pos) -> (label,pos)
		| None -> ("%anonymous"^(string_of_int env.fresh_kappa),Misc.no_pos) (*geek*)
	in
		try 
			let _ = num_of_kappa label env in
				raise (Semantics_Error (pos, "Label already used"))
		with Not_found ->
			let np = StringMap.add label env.fresh_kappa env.num_of_kappa
			and pn = IntMap.add env.fresh_kappa label env.kappa_of_num
			and fp = env.fresh_kappa+1
			in
				({env with 
					rule_indices = if from_rule then IntSet.add env.fresh_kappa env.rule_indices else env.rule_indices ; 
					num_of_kappa = np ; 
					kappa_of_num = pn ; 
					fresh_kappa = fp},fp-1)

let declare_var_alg label_pos_opt env =
	let label,pos = match label_pos_opt with
		| Some (label,pos) -> (label,pos)
		| None -> invalid_arg "Environment.declare_var_alg"
	in
		try 
			let _ = num_of_alg label env in
				raise (Semantics_Error (pos, "Label already used"))
		with Not_found ->
			let np = StringMap.add label env.fresh_alg env.num_of_alg
			and pn = IntMap.add env.fresh_alg label env.alg_of_num
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
				
