open Mods
open ExceptionDefn

type t = {
  signatures : Signature.s;
  tokens : unit NamedDecls.t;
  algs : (Expr.alg_expr Term.with_pos) NamedDecls.t;
  perturbations : unit NamedDecls.t;

	fresh_kappa : int ;
	num_of_kappa : int StringMap.t ; 
	kappa_of_num : string IntMap.t ;
	
	dependencies :  Term.DepSet.t Term.DepMap.t ; (*ALG i or KAPPA i -> {ALG j, RULE j} = modifying i implies recomputing j --closure is done*)  
	
	num_of_rule : int StringMap.t ;
	rule_of_num : string IntMap.t ;
	
	num_of_unary_rule : int StringMap.t ;
	unary_rule_of_num : string IntMap.t ;
	
	rule_indices : IntSet.t ;
	empty_lhs : IntSet.t ;
	
	nl_elements : Int2Set.t IntMap.t ; (*ag_nme -> {(r_id,cc_id),...}*)
	root_of_nl_rule : int Int2Map.t ;
	has_intra : bool ;
	
	tracking_enabled : bool ;
	active_cflows : int ;
	track : IntSet.t ;
	
	desc_table : (string,out_channel * Format.formatter) Hashtbl.t
	(*log : Log.t*)
}

let empty =
	{signatures = Signature.create [] ;
	num_of_kappa = StringMap.empty ; 
	kappa_of_num = IntMap.empty ;
	num_of_rule = StringMap.empty ;
	num_of_unary_rule = StringMap.empty ;
	algs = NamedDecls.create [||];
	rule_of_num = IntMap.empty ;
	unary_rule_of_num = IntMap.empty ; 
	fresh_kappa = 0 ;
	tokens = NamedDecls.create [||];
	perturbations = NamedDecls.create [||];
	rule_indices = IntSet.empty ;
	dependencies = Term.DepMap.empty ;
	empty_lhs = IntSet.empty ;
	nl_elements = IntMap.empty ;
	root_of_nl_rule = Int2Map.empty ;
	has_intra = false ;
	tracking_enabled = false ;
  active_cflows = 0 ;
	track = IntSet.empty ;
	
	desc_table = Hashtbl.create 2 
}

let init sigs tokens algs fresh_kappa =
  { empty with signatures = sigs; tokens = tokens;
	       algs = algs; fresh_kappa = fresh_kappa }

let get_desc file env =
  try snd (Hashtbl.find env.desc_table file)
  with Not_found ->
       let d_chan = Kappa_files.open_out file in
    let d = Format.formatter_of_out_channel d_chan in
    (Hashtbl.add env.desc_table file (d_chan,d) ; d)

let close_desc env =
	Hashtbl.iter (fun _file (d_chan,d) ->
		      let () = Format.pp_print_newline d () in
		      close_out d_chan) env.desc_table

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

let declare_empty_lhs id env = {env with empty_lhs = IntSet.add id env.empty_lhs}
let is_empty_lhs id env = IntSet.mem id env.empty_lhs

(*let log env = env.log*)
let print_agent env f i = Signature.print_agent env.signatures f i
let num_of_name nme env =
  Signature.num_of_agent nme env.signatures
let num_of_kappa lab env = StringMap.find lab env.num_of_kappa 
let kappa_of_num i env =  IntMap.find i env.kappa_of_num 
let num_of_rule lab env = StringMap.find lab env.num_of_rule
let rule_of_num i env =  IntMap.find i env.rule_of_num

let num_of_unary_rule lab env = StringMap.find lab env.num_of_unary_rule
let unary_rule_of_num i env =  IntMap.find i env.unary_rule_of_num

let is_rule i env = IntSet.mem i env.rule_indices
let num_of_alg s env = StringMap.find s env.algs.NamedDecls.finder
let alg_of_num i env = fst env.algs.NamedDecls.decls.(i)

let name_number env = Signature.size env.signatures

let add_dependencies dep dep' env =
  let set = try Term.DepMap.find dep env.dependencies
	    with Not_found -> Term.DepSet.empty in
  {env with dependencies =
	      Term.DepMap.add dep (Term.DepSet.add dep' set) env.dependencies}

let remove_dependencies dep dep' env =
  let set = try Term.DepSet.remove dep' (Term.DepMap.find dep env.dependencies)
	    with Not_found -> Term.DepSet.empty in
  if Term.DepSet.is_empty set
  then {env with dependencies = Term.DepMap.remove dep env.dependencies}
  else {env with dependencies = Term.DepMap.add dep set env.dependencies}

let get_dependencies dep env =
  try Term.DepMap.find dep env.dependencies with Not_found -> Term.DepSet.empty

let declare_rule rule_lbl id env =
  match rule_lbl with
  | None -> env
  | Some (r_nme,pos) ->
     if StringMap.mem r_nme env.num_of_rule
     then raise (Malformed_Decl (("Rule name "^r_nme^" is already used"),pos))
     else
       let nr = StringMap.add r_nme id env.num_of_rule
       and rn = IntMap.add id r_nme env.rule_of_num
       in
       {env with num_of_rule = nr ; rule_of_num = rn}

let declare_unary_rule rule_lbl id env =
	match rule_lbl with
		| None -> env
		| Some (r_nme,pos) ->
			if StringMap.mem r_nme env.num_of_unary_rule then raise (Malformed_Decl (("Rule name "^r_nme^" is already used"),pos))
			else
				let nr = StringMap.add r_nme id env.num_of_unary_rule
				and rn = IntMap.add id r_nme env.unary_rule_of_num
				in
					{env with num_of_unary_rule = nr ; unary_rule_of_num = rn}

let id_of_site agent_name site_name env =
  Signature.id_of_site (Term.with_dummy_pos agent_name)
		       (Term.with_dummy_pos site_name) env.signatures

let print_site env agent_id f site_id =
  Signature.print_site env.signatures agent_id f site_id

let id_of_state agent_name site_name state env =
  Signature.id_of_internal_state
    (Term.with_dummy_pos agent_name)
    (Term.with_dummy_pos site_name) state env.signatures

let print_site_state env agent_id id_site f id_state =
  Signature.print_site_internal_state env.signatures
				      agent_id id_site f id_state

let num_of_token = fun str env ->
  StringMap.find str env.tokens.NamedDecls.finder
let token_of_num = fun id env -> fst (fst env.tokens.NamedDecls.decls.(id))

let declare_var_kappa ?(from_rule=false) label_pos_opt env =
	let label,pos = match label_pos_opt with
		| Some (label,pos) -> (label,pos)
		| None ->
		   Term.with_dummy_pos ("%anonymous"^(string_of_int env.fresh_kappa)) (*geek*)
	in 
	let already_defined = 
		(try let _ = num_of_kappa label env in true with Not_found -> false)
		||
		(try let _ = num_of_alg label env in true with Not_found -> false)
	in
		if already_defined then
		  raise (Malformed_Decl ("Label "^label^"' already defined",pos))
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

let get_sig agent_id env =
  if agent_id = -1
  then invalid_arg "Environment.get_sig: Empty agent has no signature"
  else Signature.get env.signatures agent_id

let check agent site_name int_state env =
  let _ =
    Signature.id_of_internal_state agent site_name int_state env.signatures in
  ()

let default_state name_id site_id env =
  Signature.default_internal_state name_id site_id env.signatures

let print_rule env f id = Format.fprintf f "%s" (rule_of_num id env)
let print_alg env f id = Format.fprintf f "%s" (fst (alg_of_num id env))
let print_token env f id = Format.fprintf f "%s" (token_of_num id env)
