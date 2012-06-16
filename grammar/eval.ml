open Mods
open Mixture
open Dynamics
open Tools
open Ast

type context =
	{ pairing : link IntMap.t; curr_id : int; new_edges : (int * int) Int2Map.t
	}

and link =
	| Closed | Semi of int * int * pos

let eval_intf ast_intf =
	let rec iter ast_intf map =
		match ast_intf with
		| Ast.PORT_SEP (p, ast_interface) ->
				let int_state_list = p.Ast.port_int
				and lnk_state = p.Ast.port_lnk
				in
				if StringMap.mem p.Ast.port_nme map then
					raise (ExceptionDefn.Semantics_Error (p.Ast.port_pos,"Site '" ^ p.Ast.port_nme ^ "' is used multiple times"))
				else	
					iter ast_interface (StringMap.add p.Ast.port_nme (int_state_list, lnk_state, (p.Ast.port_pos)) map)
		| Ast.EMPTY_INTF -> StringMap.add "_" ([], Ast.FREE, no_pos) map
	in (*Adding default existential port*) iter ast_intf StringMap.empty

let eval_node env a link_map node_map node_id = 
	let ast_intf = a.Ast.ag_intf 
	and ag_name = a.Ast.ag_nme 
	and pos_ag = a.Ast.ag_pos
	in
	let env,name_id = 
		try (env,Environment.num_of_name ag_name env) with 
			| Not_found -> 
				if !Parameter.implicitSignature then (Environment.declare_name ag_name pos_ag env)
				else raise (ExceptionDefn.Semantics_Error (pos_ag,"Agent '" ^ ag_name ^ "' is not declared")) 
	in
	let env,sign = 
		try (env,Environment.get_sig name_id env) with 
			| Not_found -> 
				if !Parameter.implicitSignature then 
					let sign = Signature.create name_id (StringMap.add "_" ([], Ast.FREE, no_pos) StringMap.empty) in (Environment.declare_sig sign pos_ag env,sign)
				else raise (ExceptionDefn.Semantics_Error (pos_ag,"Agent '" ^ ag_name ^ "' is not declared"))
	in
	
	(*Begin sub function*)
	let rec build_intf ast link_map intf bond_list sign = 
		match ast with
			| Ast.PORT_SEP (p,ast') ->
				begin
				let int_state_list = p.Ast.port_int
				and lnk_state = p.Ast.port_lnk
				and port_id,sign = 
					try (Signature.num_of_site p.Ast.port_nme sign,sign) with 
						Not_found -> 
							if !Parameter.implicitSignature then
								let sign = Signature.add_site p.Ast.port_nme sign in
								(Signature.num_of_site p.Ast.port_nme sign,sign)
							else
								raise (ExceptionDefn.Semantics_Error (p.Ast.port_pos,"Site '" ^ p.Ast.port_nme ^ "' is not declared")) 
				in
				let link_map,bond_list = 
					match lnk_state with
						| Ast.LNK_VALUE (i,pos) -> 
							begin
							try 
								let opt_node = IntMap.find i link_map 
								in
								match opt_node with
									| None -> raise (ExceptionDefn.Semantics_Error (pos,"Edge identifier at site '" ^ p.Ast.port_nme ^ "' is used multiple times"))
									| Some (node_id',port_id',pos') ->
										(IntMap.add i None link_map,(node_id,port_id,node_id',port_id')::bond_list)
							with Not_found -> (IntMap.add i (Some (node_id,port_id,pos)) link_map,bond_list)
							end
						| Ast.FREE -> (link_map,bond_list)
						| _ -> raise (ExceptionDefn.Semantics_Error (p.Ast.port_pos,"Site '" ^ p.Ast.port_nme ^ "' is partially defined"))
				in
				match int_state_list with
					| [] -> build_intf ast' link_map (IntMap.add port_id (None,Node.WLD) intf) bond_list sign
					| s::_ -> 
						let i,sign = 
							try (Signature.num_of_internal_state p.Ast.port_nme s sign,sign) with 
								| Not_found -> 
									if !Parameter.implicitSignature then
										let sign,i = Signature.add_internal_state s port_id sign in
										(i,sign)	
									else 
										raise (ExceptionDefn.Semantics_Error (p.Ast.port_pos,"Internal state of site'" ^ p.Ast.port_nme ^ "' is not defined"))
						in
						build_intf ast' link_map (IntMap.add port_id (Some i,Node.WLD) intf) bond_list sign (*Geekish, adding Node.WLD to be consistent with Node.create*)
				end
		| Ast.EMPTY_INTF -> let env = if !Parameter.implicitSignature then Environment.declare_sig sign pos_ag env else env in (bond_list,link_map,intf,env)
	in
	(*end sub function*)
	
	let (bond_list,link_map,intf,env) = build_intf ast_intf link_map IntMap.empty [] sign in
	let node = Node.create ~with_interface:intf name_id env in  
	let node_map = IntMap.add node_id node node_map in
	List.iter 
	(fun (ni,pi,nj,pj) ->
		try 
		let node_i = IntMap.find ni node_map
		and node_j = IntMap.find nj node_map
		in
		Node.set_ptr (node_j,pj) (Node.Ptr (node_i,pi)) ;
		Node.set_ptr (node_i,pi) (Node.Ptr (node_j,pj)) 
		with Not_found -> invalid_arg "Eval.eval_node"
	) bond_list ; 
	(node_map,link_map,env)

let nodes_of_ast env ast_mixture =
	let rec iter ast_mixture node_map node_id link_map env =  
		match ast_mixture with
			| Ast.COMMA (a, ast_mix) ->
					let (node_map,link_map,env) = eval_node env a link_map node_map node_id in
						iter ast_mix node_map (node_id+1) link_map env
			| Ast.EMPTY_MIX -> 
				IntMap.iter 
				(fun i opt ->
					match opt with 
						| None -> () 
						| Some (_,_,pos) -> raise (ExceptionDefn.Semantics_Error (pos,Printf.sprintf "Edge identifier %d is dangling" i))
				) link_map ;
				(node_map,env)
	in
	iter ast_mixture IntMap.empty 0 IntMap.empty env
					
let eval_agent is_pattern tolerate_new_state env a ctxt =
	let (ag_name, ast_intf, pos_ag) = ((a.Ast.ag_nme), (a.Ast.ag_intf), (a.Ast.ag_pos)) in
	let env,name_id = 
		try (env,Environment.num_of_name ag_name env) with 
		| Not_found -> 
				if !Parameter.implicitSignature then (Environment.declare_name ag_name pos_ag env)
				else raise (ExceptionDefn.Semantics_Error (pos_ag,"Agent '" ^ ag_name ^ "' is not declared"))
	in
	let sign_opt =
		try Some (Environment.get_sig name_id env) with Not_found -> None in
	let env,sign =
		match sign_opt with
		| Some s -> (env,s)
		| None ->
			if !Parameter.implicitSignature then 
				let sign = Signature.create name_id (StringMap.add "_" ([], Ast.FREE, no_pos) StringMap.empty) in 
				(Environment.declare_sig sign pos_ag env,sign)
			else 	
				raise	(ExceptionDefn.Semantics_Error (pos_ag,"Agent '" ^ ag_name ^ "' is not declared")) 
	in
	let port_map = eval_intf ast_intf in
	let (interface, ctxt, sign) =
		StringMap.fold
			(fun site_name (int_state_list, lnk_state, pos_site) (interface, ctxt, sign)->
				
				let site_id,sign =
					try (Signature.num_of_site site_name sign,sign)
					with
					| Not_found ->
							if !Parameter.implicitSignature then
								let sign = Signature.add_site site_name sign in
								(Signature.num_of_site site_name sign,sign)
							else
							let msg =
								"Eval.eval_agent: site '" ^	(site_name ^ ("' is not defined for agent '" ^ ag_name ^"'"))
							in 
							raise (ExceptionDefn.Semantics_Error (pos_site, msg))
				in
				let int_s,sign =
					match int_state_list with
					| [] -> (None,sign)
					| h::_ ->
						try
							Environment.check ag_name pos_ag site_name pos_site h env;
							let i = Environment.id_of_state ag_name site_name h env
							in 
							(Some i,sign)
						with
							| exn -> 
								if tolerate_new_state then (
									let sign,i = Signature.add_internal_state h site_id sign in
									ExceptionDefn.warning ~with_pos:pos_site (Printf.sprintf "internal state '%s' of site '%s' is added to implicit signature of agent '%s'" h site_name ag_name) ;  
									(Some i,sign))
								else raise exn
				in
				let interface,ctx = 
					match lnk_state with
					| Ast.LNK_VALUE (n, pos) ->
							let lnk =
								(try Some (IntMap.find n ctxt.pairing)
								with | Not_found -> None)
							in
							(match lnk with
								| Some (Semi (b, j, _)) ->
										((IntMap.add site_id (int_s, Node.BND) interface),
											{ctxt	with
												pairing = IntMap.add n Closed ctxt.pairing;
												new_edges =
													Int2Map.add ((ctxt.curr_id), site_id) (b, j)
														ctxt.new_edges;
											})
								| Some Closed ->
										let msg =
											"edge identifier " ^
											((string_of_int n) ^ " is used too many times")
										in raise (ExceptionDefn.Semantics_Error (pos, msg))
								| None ->
										((IntMap.add site_id (int_s, Node.BND) interface),
											{
												(ctxt)
												with
												pairing =
													IntMap.add n (Semi (ctxt.curr_id, site_id, pos))
														ctxt.pairing;
											})
						)
					| Ast.LNK_SOME pos ->
							if is_pattern
							then ((IntMap.add site_id (int_s, Node.BND) interface), ctxt)
							else
								(let msg = "illegal use of '_' in concrete graph definition"
									in raise (ExceptionDefn.Semantics_Error (pos, msg)))
					| Ast.LNK_ANY pos ->
							if is_pattern
							then ((IntMap.add site_id (int_s, Node.WLD) interface), ctxt)
							else
								(let msg =
										"illegal use of wildcard '?' in concrete graph definition"
									in raise (ExceptionDefn.Semantics_Error (pos, msg)))
					| Ast.FREE ->
							((IntMap.add site_id (int_s, Node.FREE) interface), ctxt)
					| Ast.LNK_TYPE ((ste_nm, pos_ste), (ag_nm, pos_ag)) ->
							if is_pattern then
								(let site_num =
										try Environment.id_of_site ag_nm ste_nm env
										with
										| Not_found -> raise (ExceptionDefn.Semantics_Error (pos_ste, "binding type is not compatible with agent's signature"))
									in
									let ag_num = 
										try Environment.num_of_name ag_nm env with
											| Not_found -> raise
													(ExceptionDefn.Semantics_Error (pos_ste,
															"Illegal binding type, agent "^ag_nm^" is not delcared"))
									in
									((IntMap.add site_id (int_s, Node.TYPE (site_num, ag_num)) interface),ctxt)
							)
							else
								(let msg =
										"illegal use of binding type in concrete graph definition"
									in raise (ExceptionDefn.Semantics_Error (pos_ste, msg)))
					in
					(interface,ctx,sign)
			)
			port_map (IntMap.empty, ctxt, sign)
	in 
	let env = if !Parameter.implicitSignature then Environment.declare_sig sign pos_ag env else env
	in
		(ctxt, (Mixture.create_agent name_id interface), env)

(* returns partial evaluation of rate expression and a boolean that is set *)
(* to true if partial evaluation is a constant function                    *)
let rec partial_eval_alg ?(reduce_const=false) env ast =
	let bin_op ast ast' pos op op_str =
		let (f1, const1, dep1, lbl1) = partial_eval_alg env ast
		
		and (f2, const2, dep2, lbl2) = partial_eval_alg env ast' in
		let part_eval inst values t e e_null cpu_t tk =
			let v1 = f1 inst values t e e_null cpu_t tk and v2 = f2 inst values t e e_null cpu_t tk in op v1 v2 
		in
		let lbl = Printf.sprintf "(%s%s%s)" lbl1 op_str lbl2
		in (part_eval, (const1 && const2), (DepSet.union dep1 dep2), lbl)
	
	and un_op ast pos op op_str =
		let (f, const, dep, lbl) = partial_eval_alg env ast in
		let lbl = Printf.sprintf "%s(%s)" op_str lbl
		in
		((fun inst values t e e_null cpu_t tk -> let v = f inst values t e e_null cpu_t tk in op v), const,
			dep, lbl)
	in
	match ast with
		| EMAX pos -> 
			let v =
				match !Parameter.maxEventValue with
					| None -> (ExceptionDefn.warning ~with_pos:pos "[emax] constant is evaluated to infinity" ; infinity)
					| Some n -> (float_of_int n)
			in
			((fun _ _ _ _ _ _ _-> v), true, DepSet.empty, "e_max")
		| TMAX pos -> 
			let v =
				match !Parameter.maxTimeValue with
					| None -> (ExceptionDefn.warning ~with_pos:pos "[tmax] constant is evaluated to infinity" ; infinity)
					| Some t -> t
			in
			((fun _ _ _ _ _ _ _-> v), true, DepSet.empty, "t_max") 
		| INFINITY pos -> ((fun _ _ _ _ _ _ _-> infinity), true, DepSet.empty, "inf")
		| FLOAT (f, pos) ->
				((fun _ _ _ _ _ _ _-> f), true, DepSet.empty, (Printf.sprintf "%f" f))
		| CPUTIME pos -> 	((fun _ _ _ _ _ cpu_t _-> cpu_t -. !Parameter.cpuTime), false, (DepSet.singleton Mods.EVENT), "t_sim")
		| OBS_VAR (lab,pos) -> 
  			begin 
  				try
  				let i = Environment.num_of_kappa lab env (*lab is the label of a kappa expression*)
  				in
  				if Environment.is_rule i env
  				then
  					raise
  						(ExceptionDefn.Semantics_Error (pos,
  								lab ^ " is not a variable identifier"))
  				else
  					((fun f _ _ _ _ _ _-> f i), false,
  						(DepSet.singleton (Mods.KAPPA i)), ("'" ^ (lab ^ "'")))
  			with Not_found -> (*lab is the label of an algebraic expression*)
  				let i,const = 
  				try Environment.num_of_alg lab env with 
  					| Not_found -> raise (ExceptionDefn.Semantics_Error (pos,lab ^ " is not a declared variable"))
  				in
  				let v,is_const,dep =
  					match const with
  						| Some c -> 
  							if reduce_const then ((fun _ _ _ _ _ _ _-> c),true,DepSet.singleton (Mods.ALG i))
  							else ((fun _ v _ _ _ _ _-> v i),false,DepSet.singleton (Mods.ALG i))
  						| None -> ((fun _ v _ _ _ _ _-> v i),false,DepSet.singleton (Mods.ALG i))
  				in
  				(v,is_const,dep,("'" ^ (lab ^ "'")))
  			end
		| TOKEN_ID (tk_nme,pos) -> 
  			let i = 
  				try Environment.num_of_token tk_nme env with Not_found -> raise (ExceptionDefn.Semantics_Error (pos,tk_nme ^ " is not a declared token"))
  			in
  			let v,is_const,dep =
  				((fun _ _ _ _ _ _ tk -> tk i),false,DepSet.singleton (Mods.TOK i))
  			in
  			(v,is_const,dep,("'" ^ (tk_nme ^ "'")))
		| TIME_VAR pos ->
				((fun _ _ t _ _ _ _-> t), false, (DepSet.singleton Mods.TIME), "t")
		| EVENT_VAR pos ->
				((fun _ _ _ e ne _ _-> float_of_int (e+ne)), false,
					(DepSet.singleton Mods.EVENT), "e")
		| NULL_EVENT_VAR pos ->
			((fun _ _ _ _ ne _ _-> float_of_int ne), false,
					(DepSet.singleton Mods.EVENT), "null_e")
		| PROD_EVENT_VAR pos ->
			((fun _ _ _ e _ _ _-> float_of_int e), false,
					(DepSet.singleton Mods.EVENT), "prod_e")			
		| DIV (ast, ast', pos) -> bin_op ast ast' pos (fun x y -> x /. y) "/"
		| SUM (ast, ast', pos) -> bin_op ast ast' pos (fun x y -> x +. y) "+"
		| MULT (ast, ast', pos) -> bin_op ast ast' pos (fun x y -> x *. y) "*"
		| MINUS (ast, ast', pos) -> bin_op ast ast' pos (fun x y -> x -. y) "-"
		| POW (ast, ast', pos) -> bin_op ast ast' pos (fun x y -> x ** y) "^"
		| MODULO (ast, ast', pos) -> bin_op ast ast' pos (fun x y -> float_of_int ((int_of_float x) mod (int_of_float y))) " modulo "
		| COSINUS (ast, pos) -> un_op ast pos cos "cos"
		| TAN (ast,pos) -> un_op ast pos tan "tan"
		| SINUS (ast, pos) -> un_op ast pos sin "sin"
		| EXP (ast, pos) -> un_op ast pos exp "e^"
		| SQRT (ast, pos) -> un_op ast pos sqrt "sqrt"
		| ABS (ast, pos) -> un_op ast pos (fun x -> float_of_int (abs (int_of_float x))) "abs"
		| LOG (ast, pos) -> un_op ast pos log "log"

let rec partial_eval_bool env ast =
	let bin_op_bool ast ast' pos op op_str =
		let (f1, const1, dep1, lbl1) = partial_eval_bool env ast
		
		and (f2, const2, dep2, lbl2) = partial_eval_bool env ast' in
		let part_eval inst values t e e_null cpu_t tk =
			let b1 = f1 inst values t e e_null cpu_t tk and b2 = f2 inst values t e e_null cpu_t tk in op b1 b2 in
		let lbl = Printf.sprintf "(%s %s %s)" lbl1 op_str lbl2
		in (part_eval, (const1 && const2), (DepSet.union dep1 dep2), lbl)
	
	and bin_op_alg ast ast' pos op op_str =
		let (f1, const1, dep1, lbl1) = partial_eval_alg ~reduce_const:false env ast
		
	and (f2, const2, dep2, lbl2) = partial_eval_alg ~reduce_const:false env ast' in
		let part_eval inst values t e e_null cpu_t tk =
			let v1 = f1 inst values t e e_null cpu_t tk and v2 = f2 inst values t e e_null cpu_t tk in op v1 v2 in
		let lbl = Printf.sprintf "(%s%s%s)" lbl1 op_str lbl2
		in (part_eval, (const1 && const2), (DepSet.union dep1 dep2), lbl)
	
	and un_op ast pos op op_str =
		let (f, const, dep, lbl) = partial_eval_bool env ast in
		let lbl = Printf.sprintf "%s(%s)" op_str lbl
		in
		((fun inst values t e e_null cpu_t tk -> let b = f inst values t e e_null cpu_t tk in op b), const,
			dep, lbl)
	in
	match ast with
		| TRUE pos -> ((fun _ _ _ _ _ _ _-> true), true, DepSet.singleton Mods.EVENT, "true")
		| FALSE pos -> ((fun _ _ _ _ _ _ _-> false), true, DepSet.empty, "false")	
		| AND (ast, ast', pos) ->
				bin_op_bool ast ast' pos (fun b b' -> b && b') "and"
		| OR (ast, ast', pos) ->
				bin_op_bool ast ast' pos (fun b b' -> b || b') "or"
		| NOT (ast, pos) -> un_op ast pos (fun b -> not b) "not"
		| GREATER (ast, ast', pos) ->
				bin_op_alg ast ast' pos (fun v v' -> v > v') ">"
		| SMALLER (ast, ast', pos) ->
				bin_op_alg ast ast' pos (fun v v' -> v < v') "<"
		| EQUAL (ast, ast', pos) ->
				bin_op_alg ast ast' pos (fun v v' -> v = v') "="

let mixture_of_ast ?(tolerate_new_state=false) mix_id_opt is_pattern env ast_mix =
	let rec eval_mixture env ast_mix ctxt mixture =
		match ast_mix with
		| Ast.COMMA (a, ast_mix) ->
				let (ctxt, agent, env) = eval_agent is_pattern tolerate_new_state env a ctxt in
				let id = ctxt.curr_id and new_edges = ctxt.new_edges in
				let (ctxt, mixture, env) =
					eval_mixture env ast_mix
						{
							(ctxt)
							with
							curr_id = ctxt.curr_id + 1;
							new_edges = Int2Map.empty;
						} mixture
				in (ctxt, (Mixture.compose id agent mixture new_edges), env)
		| Ast.EMPTY_MIX -> (ctxt, mixture, env)
	in
	
	let ctxt = { pairing = IntMap.empty; curr_id = 0; new_edges = Int2Map.empty; } in
	let (ctxt, mix, env) = eval_mixture env ast_mix ctxt (Mixture.empty mix_id_opt)
	in
	begin
		IntMap.iter (*checking that all edge identifiers are pairwise defined*)
		(fun n link ->
					match link with
					| Closed -> ()
					| Semi (_, _, pos) ->
							let msg =
								"edge identifier " ^ ((string_of_int n) ^ " is not paired")
							in raise (ExceptionDefn.Semantics_Error (pos, msg))
		)
		ctxt.pairing;
		let mix = Mixture.enum_alternate_anchors mix in 
		let mix = Mixture.set_root_of_cc mix in
			(mix,env) (*Modifies mix as a side effect*)
	end

let signature_of_ast s env =
	let (name, ast_intf, pos) =
		((s.Ast.ag_nme), (s.Ast.ag_intf), (s.Ast.ag_pos)) in
	let intf_map = eval_intf ast_intf in 
	let env,name_id = Environment.declare_name name pos env in
	(Signature.create name_id intf_map,env)
	
let token_of_ast abs_tk env = 
	let (name, pos) = abs_tk in
	Environment.declare_token name pos env
	
let rule_of_ast ?(backwards=false) env (ast_rule_label, ast_rule) tolerate_new_state = 
	let ast_rule_label,ast_rule = if backwards then Ast.flip (ast_rule_label, ast_rule) else (ast_rule_label, ast_rule) in
	let (env, lhs_id) =
		Environment.declare_var_kappa ~from_rule:true ast_rule_label.lbl_nme env in
	(* reserving an id for rule's lhs in the pattern table *)
	let env = Environment.declare_rule ast_rule_label.lbl_nme lhs_id env in
	let (k_def, dep) =
		let (k, const, dep, _) = partial_eval_alg ~reduce_const:false env ast_rule.k_def
		in
		if const
		then (CONST (close_var k), dep)
		else ((VAR k), dep)
	
	and (env,k_alt, dep_alt) =
		match ast_rule.k_un with
		| None -> (env,None, DepSet.empty)
		| Some ast ->
				let env = Environment.declare_unary_rule (Some (Environment.kappa_of_num lhs_id env,Tools.no_pos)) (*ast_rule_label.lbl_nme*) lhs_id env in
				let (rate, const, dep, _) = partial_eval_alg ~reduce_const:false env ast
				in
				if const
				then
					(env,(Some (CONST (close_var rate))), dep)
				else (env,(Some (VAR rate)), dep)
	in
	let lhs,env = mixture_of_ast (Some lhs_id) true env ast_rule.lhs
	in 
	let lhs = match k_alt with None -> lhs | Some _ -> Mixture.set_unary lhs in
	let rhs,env = mixture_of_ast ~tolerate_new_state:tolerate_new_state None true env ast_rule.rhs in
	let (script, balance,added,modif_sites,side_effect) = Dynamics.diff ast_rule.rule_pos lhs rhs ast_rule_label.lbl_nme env
	
	and kappa_lhs = Mixture.to_kappa false lhs env
	
	and kappa_rhs = Mixture.to_kappa false rhs env
	in
	let tokenify l = 
		List.map 
		(fun (alg_expr,(nme,pos)) ->
			let id =  
				try Environment.num_of_token nme env 
				with Not_found -> raise (ExceptionDefn.Semantics_Error (pos,"Token "^nme^" is undefined"))
			in
			let (f, is_const, _ , _) = partial_eval_alg ~reduce_const:false env alg_expr (*dependencies are not important here since variable is evaluated only when rule is applied*) 
			in
			let v = 
  			if is_const then CONST (close_var f) 
  			else VAR f
			in
			(v,id)
		) l
	in
	let add_token = tokenify ast_rule.add_token 
	and rm_token = tokenify ast_rule.rm_token
	in
	let ref_id =
		match ast_rule_label.lbl_ref with
		| None -> None
		| Some (ref, pos) ->
				(try Some (Environment.num_of_kappa ref env)
				with
				| Not_found ->
						raise
							(ExceptionDefn.Semantics_Error (pos, "undefined label " ^ ref))) in
	let r_id = Mixture.get_id lhs in
	let env = if Mixture.is_empty lhs then Environment.declare_empty_lhs r_id env else env in
	let env =
		DepSet.fold
			(fun dep env ->
				(*Printf.printf "rule %d depends on %s\n" r_id (Mods.string_of_dep dep) ; *)
				Environment.add_dependencies dep (RULE r_id) env)
			(DepSet.union dep dep_alt) env
	in
	let pre_causal = Dynamics.compute_causal lhs rhs script env in
	
	let connect_impact,disconnect_impact,side_eff_impact =
		List.fold_left 
		(fun (con_map,dis_map,side_eff) action ->
			match action with
				| Dynamics.BND ((KEPT id,s),(KEPT id',s')) -> (*binding with a fresh agent doesn't impact connectedness*)
					let cc_id = Mixture.component_of_id id lhs
					and cc_id' = Mixture.component_of_id id' lhs
					in
					if cc_id = cc_id' then (con_map,dis_map,side_eff) (*rule binds two ports that were already part of the same cc*)
					else 
						let eq_id = try IntMap.find cc_id con_map with Not_found -> cc_id
						and eq_id'= try IntMap.find cc_id' con_map with Not_found -> cc_id'
						in
						let min_eq,max_eq = (min eq_id eq_id',max eq_id eq_id') in
						let con_map = IntMap.add cc_id min_eq con_map in
						let con_map = IntMap.add cc_id' min_eq con_map in
						let con_map = IntMap.add max_eq min_eq con_map in
						(con_map,dis_map,side_eff)
				| Dynamics.FREE ((KEPT id,s),side_effect_free) ->
					if side_effect_free then
						begin
						let id' = match Mixture.follow (id,s) lhs with None -> invalid_arg "Eval.build_cc_impact: Free action should be side effect free" | Some (id',s') -> id' in
						let is_deleted = not (IntMap.mem id' (Mixture.agents rhs)) in
						if is_deleted then (con_map,dis_map,side_eff) (*will deal with this case in the deletion action*)
						else
							let cc_id = Mixture.component_of_id id rhs
							and cc_id' = Mixture.component_of_id id' rhs
							in
							if cc_id = cc_id' then (con_map,dis_map,side_eff) (*rule breaks two ports that are still connected in the rhs*)
							else 
								let class1 = try IntMap.find cc_id dis_map with Not_found -> IntSet.empty
								and class2 =  try IntMap.find cc_id' dis_map with Not_found -> IntSet.empty
								in
								let dis_map = IntMap.add cc_id (IntSet.add cc_id' class1) dis_map in
								let dis_map = IntMap.add cc_id' (IntSet.add cc_id class2) dis_map in
								(con_map,dis_map,side_eff)
						end
					else (*semi link deletion*)
						let set = try IntMap.find id side_eff with Not_found -> IntSet.empty in
						let side_eff = IntMap.add id (IntSet.add s set) side_eff in
						(con_map,dis_map,side_eff)
				| Dynamics.DEL id -> 
					let ag = Mixture.agent_of_id id lhs in
					let sign = Environment.get_sig (Mixture.name ag) env in
					let set = try IntMap.find id (side_eff:IntSet.t IntMap.t) with Not_found -> IntSet.empty in
					let set = 
						Signature.fold 
						(fun site_id set -> 
							if site_id = 0 then set 
							else
								let opt = Mixture.follow (id,site_id) lhs in
								match opt with
									| None ->
										begin 
										try 
											let _,lnk = IntMap.find site_id (Mixture.interface (Mixture.agent_of_id id lhs)) in 
											match lnk with 
												| Node.BND | Node.TYPE _ -> IntSet.add site_id set (*semi link deletion*) 
												| _ -> set
										with
											| Not_found -> IntSet.add site_id set (*deletion of site that was not mentionned so possible side_effect*)
										end
									| Some _ -> IntSet.add site_id set (*link deletion because deleted agent was connected in the lhs*)
						) sign set
					in 
						(con_map,dis_map,IntMap.add id set side_eff)
				| _ -> (con_map,dis_map,side_eff)
		) (IntMap.empty,IntMap.empty,IntMap.empty) script
	in
	let connect_impact = 
		IntMap.fold 
		(fun cc_id eq_id map -> let set = try IntMap.find eq_id map with Not_found -> IntSet.empty in IntMap.add eq_id (IntSet.add cc_id set) map
		) connect_impact IntMap.empty 
	in
	let env = 
		(match k_alt with 
			| None -> env
			| Some _ -> (*rule has a unary version*)
				let ptr_env = ref {env with Environment.has_intra=true} in
				let cc_id = ref 0 in
				while !cc_id < (Mixture.arity lhs) do
					let ag_root = 
						let id = match Mixture.root_of_cc lhs !cc_id with None -> invalid_arg "Eval.rule_of_ast" | Some i -> i in
						Mixture.agent_of_id id lhs
					in
					let env' = Environment.declare_nl_element (Mixture.get_id lhs) !cc_id (Mixture.name ag_root) !ptr_env in
					ptr_env := env' ;
					cc_id := !cc_id+1 ;
				done ; !ptr_env)
	in
	(env,
		{
			Dynamics.add_token = add_token ;
			Dynamics.rm_token = rm_token ;
			Dynamics.k_def = k_def;
			Dynamics.k_alt = k_alt;
			Dynamics.over_sampling = None;
			Dynamics.script = script;
			Dynamics.kappa = kappa_lhs ^ ("->" ^ kappa_rhs);
			Dynamics.balance = balance;
			Dynamics.refines = ref_id;
			Dynamics.lhs = lhs;
			Dynamics.rhs = rhs;
			Dynamics.r_id = r_id;
			Dynamics.added = List.fold_left (fun set i -> IntSet.add i set) IntSet.empty added ;
			Dynamics.side_effect = side_effect ; 
			Dynamics.modif_sites = modif_sites ;
			Dynamics.is_pert = false ;
			Dynamics.pre_causal = pre_causal ;
			Dynamics.cc_impact = Some (connect_impact,disconnect_impact,side_eff_impact)  
		})

let variables_of_result env res =
	let is_pattern = true
	in
	List.fold_left
		(fun (env, mixtures, vars) var ->
					match var with
						| Ast.VAR_KAPPA (ast, label_pos) ->
								let (env, id) =
									Environment.declare_var_kappa (Some label_pos) env in
								let mix,env = mixture_of_ast (Some id) is_pattern env ast
								in (env, (mix :: mixtures), vars)
						| Ast.VAR_ALG (ast, label_pos) ->
								let (f, is_const, dep, lbl) = partial_eval_alg ~reduce_const:false env ast in
								let (env, var_id) =
									if is_const then
										Environment.declare_var_alg (Some label_pos) (Some ( close_var f)) env 
									else
										Environment.declare_var_alg (Some label_pos) None env (*cannot evaluate variable yet*)
								in
								let v =
									if is_const then Dynamics.CONST (close_var f)
									else 
										Dynamics.VAR f
								in (env, mixtures, ((v, dep, var_id) :: vars))
		)	(env, [], []) res.Ast.variables

let rules_of_result env res tolerate_new_state =
	let (env, l) =
		List.fold_left
			(fun (env, cont) (ast_rule_label, ast_rule) ->
					let (env, r) = rule_of_ast env (ast_rule_label, ast_rule) tolerate_new_state
					in
					match ast_rule.Ast.k_op with
						| None -> (env,r::cont)
						| Some k -> 
							let (env,back_r) = rule_of_ast ~backwards:true env (ast_rule_label, ast_rule) tolerate_new_state
							in
							(env,back_r::(r::cont))
			)
			(env, []) res.Ast.rules
	in (env, (List.rev l))

let environment_of_result res =
	let env = 
		List.fold_left
		(fun env (sign, pos) ->
			let sign,env = signature_of_ast sign env in
				Environment.declare_sig sign pos env
		)
		Environment.empty res.Ast.signatures
	in
	List.fold_left (fun env (tk, pos) -> token_of_ast (tk,pos) env) env res.Ast.tokens


let obs_of_result env res =
	List.fold_left
	(fun cont alg_expr ->
				let (obs, is_constant, dep, label) = partial_eval_alg ~reduce_const:false env alg_expr (*Not reducing const, because $UPDATE might change it later*)
				in (dep, is_constant, obs, label) :: cont
	)
	[] res.observables

let effects_of_modif variables env ast_list =
	let rec iter variables effects str_pert env ast_list = 
		match ast_list with
			| [] -> (variables,effects,str_pert,env)
			| ast::tl ->
				let (variables,effects,str_pert,env) =
					match ast with
					| INTRO (alg_expr, ast_mix, pos) ->
							let (x, is_constant, dep, str) = partial_eval_alg ~reduce_const:false env alg_expr in
							let m,env = mixture_of_ast None false env ast_mix in
							let v =
								if is_constant
								then Dynamics.CONST (close_var x) 
								else Dynamics.VAR x in
							let str =
								(Printf.sprintf "introduce %s * %s" str (Mixture.to_kappa false m env))::str_pert
							in (variables, (Dynamics.INTRO (v, m))::effects, str, env)
					| DELETE (alg_expr, ast_mix, pos) ->
							let (x, is_constant, dep, str) = partial_eval_alg ~reduce_const:false env alg_expr in
							let nme_pert = Printf.sprintf "pert_%d" (Environment.next_pert_id env) in
							let (env, id) =
								Environment.declare_var_kappa (Some (nme_pert,pos)) env 
							in
							let m,env = mixture_of_ast (Some id) true env ast_mix in
							let v =
								if is_constant
								then Dynamics.CONST (close_var x)
								else Dynamics.VAR x in
							let str =
								(Printf.sprintf "remove %s * %s" str (Mixture.to_kappa false m env))::str_pert
							in ((m :: variables), (Dynamics.DELETE (v, m))::effects, str, env)
					| UPDATE (nme, pos_rule, alg_expr, pos_pert) ->
							let i,is_rule =
								(try (Environment.num_of_rule nme env,true)
								with
								| Not_found ->
									try
										let (i,_) = Environment.num_of_alg nme env in (i,false)
									with Not_found -> raise (ExceptionDefn.Semantics_Error (pos_rule,"Variable " ^ (nme ^ " is neither a constant nor a rule")))
								)
							and (x, is_constant, dep, str) = partial_eval_alg ~reduce_const:false env alg_expr in
							let v =
								if is_constant
								then Dynamics.CONST (close_var x)
								else Dynamics.VAR x in
							let str =
								if is_rule then
									(Printf.sprintf "set rate of rule '%s' to %s" (Environment.rule_of_num i env) str)::str_pert
								else
									let v_str,_ = Environment.alg_of_num i env in
									(Printf.sprintf "set variable '%s' to %s" v_str str)::str_pert
							in 
							if is_rule then (variables, (Dynamics.UPDATE_RULE (i, v))::effects, str, env)
							else (variables, (Dynamics.UPDATE_VAR (i, v))::effects, str, env)
					| UPDATE_TOK (tk_nme,tk_pos,alg_expr,instr_pos) ->
						let tk_id = 
							try Environment.num_of_token tk_nme env with 
								| Not_found -> raise (ExceptionDefn.Semantics_Error (tk_pos,"Token " ^ (tk_nme ^ " is not defined")))
						in
						let (x, is_constant, dep, str) = partial_eval_alg ~reduce_const:false env alg_expr in
						let v =
								if is_constant
								then Dynamics.CONST (close_var x)
								else Dynamics.VAR x 
						in
						let str = (Printf.sprintf "set token '%s' to value %s" tk_nme str)::str_pert
						in 
						(variables, (Dynamics.UPDATE_TOK (tk_id, v))::effects, str, env)
					| SNAPSHOT (opt,pos) -> (*when specializing snapshots to particular mixtures, add variables below*)
						let str = ("snapshot state")::str_pert in
						let opt_name = 
							match opt with
							| None -> None
							| Some (nme,pos) -> Some nme
						in
						(variables, (Dynamics.SNAPSHOT opt_name)::effects, str, env)
					| STOP (opt,pos) ->
						let str = "interrupt simulation"::str_pert in
						let opt_name = 
							match opt with
							| None -> None
							| Some (nme,pos) -> Some nme
						in
						(variables, (Dynamics.STOP opt_name)::effects, str, env)
					| CFLOW (lab,pos_lab,pos_pert) ->
						let id = try Environment.num_of_rule lab env with Not_found -> try Environment.num_of_kappa lab env with Not_found ->
							raise	(ExceptionDefn.Semantics_Error (pos_lab, "Label '" ^ lab ^ "' is neither a rule nor a Kappa expression"))
						in
						let str = (Printf.sprintf "Enable causality analysis of %s" lab)::str_pert in
						(variables, (Dynamics.CFLOW id)::effects, str, env)
					| CFLOWOFF (lab,pos_lab,pos_pert) ->
						let id = try Environment.num_of_rule lab env with Not_found -> try Environment.num_of_kappa lab env with Not_found ->
							raise	(ExceptionDefn.Semantics_Error (pos_lab, "Label '" ^ lab ^ "' is neither a rule nor a Kappa expression"))
						in
						let str = (Printf.sprintf "Disable causality analysis of %s" lab)::str_pert in
						(variables, (Dynamics.CFLOWOFF id)::effects, str, env)
					| FLUX (lab,pos) ->
						let nme =
							match lab with
								| None -> None
								| Some (nme,_) -> Some nme
						in
						let str = "Activate flux tracking"::str_pert in
						(variables,(Dynamics.FLUX nme)::effects, str, env)
					| FLUXOFF (lab,pos) ->
						let nme =
							match lab with
								| None -> None
								| Some (nme,_) -> Some nme
						in
						let str = "Disable flux tracking"::str_pert in
						(variables,(Dynamics.FLUXOFF nme)::effects, str, env)
			in
			iter variables effects str_pert env tl
	in
	iter variables [] [] env ast_list  

let pert_of_result variables env res =
	let (variables, lpert, lrules, env) =
		List.fold_left
			(fun (variables, lpert, lrules, env) (bool_expr, modif_expr_list, pos, opt_post) ->				
				let (x, is_constant, dep, str_pre) = partial_eval_bool env bool_expr
				and (variables, effects, str_eff, env) =	effects_of_modif variables env modif_expr_list 
				in
				let str_eff = String.concat ";" (List.rev str_eff) in
				let bv =
					if is_constant
					then Dynamics.BCONST (close_var x)
					else Dynamics.BVAR x in
				let str_pert,opt_abort =
					match opt_post with
					| None -> (Printf.sprintf "whenever %s, %s" str_pre str_eff,None)
					| Some bool_expr -> 
						let (x, is_constant, dep, str_abort) = partial_eval_bool env bool_expr in
						let bv = 
							if is_constant then Dynamics.BCONST (close_var x)
							else Dynamics.BVAR x 
						in
						(Printf.sprintf "whenever %s, %s until %s" str_pre str_eff str_abort,Some (bv,dep,str_abort)) 
				in
				let env,p_id = Environment.declare_pert (str_pert,pos) env in
				let env,effect_list =
					List.fold_left 
					(fun (env,rule_list) effect -> 
						match effect with
						| Dynamics.INTRO (_,mix) ->
							begin
								let (env, id) =
									Environment.declare_var_kappa (Some (str_pert,pos)) env 
								in
								let lhs = Mixture.empty (Some id)
								and rhs = mix 
								in
								let (script,balance,added,modif_sites,side_effect) = Dynamics.diff pos lhs rhs (Some (str_pert,pos)) env
								and kappa_lhs = ""
								and kappa_rhs = Mixture.to_kappa false rhs env in
								let r_id = Mixture.get_id lhs in
								let str_pert = Printf.sprintf "pert_%d" p_id in
								let env = Environment.declare_rule (Some (str_pert,pos)) r_id env in
								let env =
									DepSet.fold
									(fun dep env -> Environment.add_dependencies dep (Mods.PERT p_id) env
									)
									dep env
								in 
								let pre_causal = Dynamics.compute_causal lhs rhs script env in
								let rule = 
								{
									(*TODO*)Dynamics.rm_token = [] ; Dynamics.add_token = [] ; 
									Dynamics.k_def = Dynamics.CONST 0.0;
									Dynamics.k_alt = None;
									Dynamics.over_sampling = None;
									Dynamics.script = script ;
									Dynamics.kappa = kappa_lhs ^ ("->" ^ kappa_rhs);
									Dynamics.balance = balance;
									Dynamics.refines = None;
									Dynamics.lhs = lhs;
									Dynamics.rhs = rhs;
									Dynamics.r_id = r_id;
									Dynamics.added = List.fold_left (fun set i -> IntSet.add i set) IntSet.empty added ;
									Dynamics.side_effect = side_effect ; 
									Dynamics.modif_sites = modif_sites ;
									Dynamics.is_pert = true ;
									Dynamics.pre_causal = pre_causal ;
									Dynamics.cc_impact = None 
								}
								in
								(env,(Some rule,effect)::rule_list)
							end
						| Dynamics.DELETE (_,mix) ->
							begin
								let lhs = mix
								and rhs = Mixture.empty None
								in
								let (script,balance,added,modif_sites,side_effect) = Dynamics.diff pos lhs rhs (Some (str_pert,pos)) env
								and kappa_lhs = Mixture.to_kappa false lhs env
								and kappa_rhs = "" in
								let r_id = Mixture.get_id lhs in
								let str_pert = Printf.sprintf "pert_%d" p_id in
								let env = Environment.declare_rule (Some (str_pert,pos)) r_id env in
								let env =
									DepSet.fold
									(fun dep env -> Environment.add_dependencies dep (Mods.PERT p_id) env
									)
									dep env
								in 
								let pre_causal = Dynamics.compute_causal lhs rhs script env in
								let rule = 
								{ (*TODO*) Dynamics.rm_token = [] ; Dynamics.add_token = [] ; 
									
									Dynamics.k_def = Dynamics.CONST 0.0;
									Dynamics.k_alt = None;
									Dynamics.over_sampling = None;
									Dynamics.script = script ;
									Dynamics.kappa = kappa_lhs ^ ("->" ^ kappa_rhs);
									Dynamics.balance = balance;
									Dynamics.refines = None;
									Dynamics.lhs = lhs;
									Dynamics.rhs = rhs;
									Dynamics.r_id = r_id;
									Dynamics.added = List.fold_left (fun set i -> IntSet.add i set) IntSet.empty added ;
									Dynamics.side_effect = side_effect ; 
									Dynamics.modif_sites = modif_sites ;
									Dynamics.pre_causal = pre_causal ;
									Dynamics.is_pert = true ;
									Dynamics.cc_impact = None ;
								}
								in
								(env,(Some rule,effect)::rule_list)
							end
						| Dynamics.CFLOW _ ->
							let env = {env with Environment.tracking_enabled = true} in
							let env =
								DepSet.fold
								(fun dep env -> Environment.add_dependencies dep (Mods.PERT p_id) env
								)
								dep env
							in 
							(env,(None,effect)::rule_list) 
						| Dynamics.UPDATE_RULE _ |Dynamics.UPDATE_VAR _ | Dynamics.UPDATE_TOK _ | Dynamics.SNAPSHOT _ | Dynamics.STOP _ | Dynamics.FLUX _ | Dynamics.FLUXOFF _ | Dynamics.CFLOWOFF _ -> 
							let env =
								DepSet.fold
								(fun dep env -> Environment.add_dependencies dep (Mods.PERT p_id) env
								)
								dep env
							in (env,(None,effect)::rule_list) 
				) (env,[]) effects
				in
				(*let env = List.fold_left (fun env (r_opt,effect) -> Environment.bind_pert_rule p_id r.r_id env) env effect_list in *)
				let opt,env =
					match opt_abort with
					| None -> (None,env)
					| Some (bv,dep,str_post) -> 
						let env =
							DepSet.fold
							(fun dep_type env ->
								Environment.add_dependencies dep_type (Mods.ABORT p_id) env
							)
							dep env
						in
						(Some bv,env)
				in
				let pert = 
					{ Dynamics.precondition = bv;
						Dynamics.effect = effect_list;
						Dynamics.abort = opt;
						Dynamics.flag = str_pert;
					}
				in
				let lrules = List.fold_left (fun cont (r_opt,_) -> match r_opt with None -> cont | Some r -> r::cont) lrules effect_list 
				in
				(variables, pert::lpert, lrules, env)
			)
			(variables, [], [], env) res.perturbations
		in 
		(variables, (List.rev lpert), (List.rev lrules), env)

let init_graph_of_result env res =
	let n = env.Environment.fresh_token in 
	let token_vector = Array.init n (fun i -> 0.) in
	let sg,env = 
	List.fold_left
		(fun (sg,env) init_t ->
			match init_t with
				| INIT_MIX (alg, ast, pos) ->
					begin
      			let cpt = ref 0
      			and sg = ref sg
      			and env = ref env
      			and (v, is_const, dep, lbl) = partial_eval_alg ~reduce_const:true env alg
      			in
      			if not is_const then raise (ExceptionDefn.Semantics_Error (pos, Printf.sprintf "%s is not a constant, cannot initialize graph." lbl))
      			else
      			let n = match !Parameter.rescale with 
      				| None -> int_of_float (close_var v) 
      				| Some i -> min i (int_of_float (close_var v)) 
      			in
      				(* Cannot do Mixture.to_nodes env m once for all because of        *)
      				(* references                                                      *)
      				while !cpt < n do
      					let nodes,env' = nodes_of_ast !env ast in
      					env := env' ;
      					sg := Graph.SiteGraph.add_nodes !sg nodes;
      					cpt := !cpt + 1 
      				done;
      				(!sg,!env)
					end
				| INIT_TOK (alg, tk_nme, pos) ->
					let (v, is_const, _, lbl) = partial_eval_alg ~reduce_const:true env alg
    			in
    			if not is_const then raise (ExceptionDefn.Semantics_Error (pos, Printf.sprintf "%s is not a constant, cannot initialize token value." lbl))
    			else
    			let x = close_var v in
    			let tok_id = try Environment.num_of_token tk_nme env with Not_found -> raise (ExceptionDefn.Semantics_Error (pos, Printf.sprintf "token %s is undeclared" tk_nme))
					in
					token_vector.(tok_id) <- x ;
					(sg,env)
		)	(Graph.SiteGraph.init !Parameter.defaultGraphSize,env) res.Ast.init
	in
	(sg,token_vector,env)
	
let configurations_of_result result =
	let set_value pos_p param value_list f ass = 
  	try 
			let v,pos = List.hd value_list in
  		ass := f (v,pos) 
  	with _ -> ExceptionDefn.warning ~with_pos:pos_p (Printf.sprintf "Empty value for parameter %s" param)
	in
	List.iter 
	(fun (param,pos_p,value_list) ->
		match param with
			| "displayCompression" -> 
				begin
					let rec parse l = 
						match l with
							| ("strong",pos_v)::tl -> (ExceptionDefn.warning ~with_pos:pos_v "Strong compression is not implemented yet" ; parse tl)
							| ("weak",_)::tl -> (Parameter.weakCompression := true ; parse tl)
							| ("none",_)::tl -> (Parameter.mazCompression := true ; parse tl)
							| [] -> ()
							| (error,pos)::tl -> raise (ExceptionDefn.Semantics_Error (pos,Printf.sprintf "Unkown value %s for compression mode" error))
					in
					parse value_list
				end
			| "cflowFileName"	-> set_value pos_p param value_list (fun (v,pos) -> v) Parameter.cflowFileName 
			| "progressBarSize" -> 
				set_value pos_p param value_list 
				(fun (v,p) -> 
					try int_of_string v
					with _ -> raise (ExceptionDefn.Semantics_Error (p,Printf.sprintf "Value %s should be an integer" v))
				) Parameter.progressBarSize 
				
			| "progressBarSymbol" -> 
				set_value pos_p param value_list 
				(fun (v,p) -> 
					try
					String.unsafe_get v 0 
					with _ -> raise (ExceptionDefn.Semantics_Error (p,Printf.sprintf "Value %s should be a character" v))
				) Parameter.progressBarSymbol 

			| "dumpIfDeadlocked" -> 
				set_value pos_p param value_list
				(fun (value,pos_v) ->	
					match value with 
						| "true" -> true 
						| "false" -> false 
						| _ as error -> raise (ExceptionDefn.Semantics_Error (pos_v,Printf.sprintf "Value %s should be either \"true\" or \"false\"" error))
				) Parameter.dumpIfDeadlocked
			| "plotSepChar" -> 
				set_value pos_p param value_list
				(fun (v,p) ->
				try 
					String.unsafe_get v 0  
				with _ -> raise (ExceptionDefn.Semantics_Error (p,Printf.sprintf "Value %s should be a character" v))
				) Parameter.plotSepChar
			| "maxConsecutiveClash" ->
				set_value pos_p param value_list 
				(fun (v,p) -> 
					try int_of_string v
					with _ -> raise (ExceptionDefn.Semantics_Error (p,Printf.sprintf "Value %s should be an integer" v))
				) Parameter.maxConsecutiveClash 
				 
			| "dotSnapshots" -> 
				set_value pos_p param value_list
				(fun (value,pos_v) ->	
					match value with 
						| "true" -> true 
						| "false" -> false 
						| _ as error -> raise (ExceptionDefn.Semantics_Error (pos_v,Printf.sprintf "Value %s should be either \"true\" or \"false\"" error))
				) Parameter.dotOutput
			| "colorDot" ->
				set_value pos_p param value_list
				(fun (value,pos_v) ->	
					match value with 
						| "true" -> true 
						| "false" -> false 
						| _ as error -> raise (ExceptionDefn.Semantics_Error (pos_v,Printf.sprintf "Value %s should be either \"true\" or \"false\"" error))
				) Parameter.useColor
			| "dumpInfluenceMap" ->
				set_value pos_p param value_list
				(fun (v,p) -> 
					match v with 
						| "true" -> if !Parameter.influenceFileName = "" then "im.dot" else !Parameter.influenceFileName
						| "false" -> "" 
						| _ as error -> raise (ExceptionDefn.Semantics_Error (p,Printf.sprintf "Value %s should be either \"true\" or \"false\"" error))
				) Parameter.influenceFileName
			| "influenceMapFileName" -> set_value pos_p param value_list (fun (v,p) -> v) Parameter.influenceFileName  
			| _ as error -> raise (ExceptionDefn.Semantics_Error (pos_p,Printf.sprintf "Unkown parameter %s" error))		  
	) result.configurations 
	
let initialize result counter =
	Debug.tag "+ Compiling..." ;
	Debug.tag "\t -simulation parameters" ;
	let _ = configurations_of_result result in

	Debug.tag "\t -agent signatures" ;
	let env = environment_of_result result in
	
	Debug.tag "\t -variable declarations";
	let (env, kappa_vars, alg_vars) = variables_of_result env result in
	
	Debug.tag "\t -initial conditions";
	let sg,token_vector,env = init_graph_of_result env result
	in
	
	let tolerate_new_state = !Parameter.implicitSignature in
	Parameter.implicitSignature := false ;

	Debug.tag "\t -rules";
	let (env, rules) = rules_of_result env result tolerate_new_state in
	
	Debug.tag "\t -observables";
	let observables = obs_of_result env result in
	Debug.tag "\t -perturbations" ;
	let (kappa_vars, pert, rule_pert, env) = pert_of_result kappa_vars env result
	in
	Debug.tag "\t Done";
	Debug.tag "+ Analyzing non local patterns..." ;
	let env = Environment.init_roots_of_nl_rules env in
	Debug.tag "+ Building initial simulation state...";
	Debug.tag "\t -Counting initial local patterns..." ;
	let (state, env) =
	State.initialize sg token_vector rules kappa_vars alg_vars observables (pert,rule_pert) counter env
	in
	let state =  
		if env.Environment.has_intra then
			begin
				Debug.tag "\t -Counting initial non local patterns..." ;
				NonLocal.initialize_embeddings state counter env
			end
		else state
	in
	(Debug.tag "\t Done"; (env, state))
