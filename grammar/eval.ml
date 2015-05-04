open Mods
open Tools
open Ast

type link = Closed | Semi of (int * int) Term.with_pos

type context =
    { pairing : link IntMap.t; curr_id : int;
      new_edges : (int * int) Int2Map.t }

let eval_intf ast_intf =
  let rec iter ast_intf map =
    match ast_intf with
    | p :: ast_interface ->
       let int_state_list = p.Ast.port_int
       and lnk_state = p.Ast.port_lnk
       in
       if StringMap.mem (fst p.Ast.port_nme) map then
	 raise
	   (ExceptionDefn.Malformed_Decl
	      ("Site '" ^ (fst p.Ast.port_nme) ^ "' is used multiple times",
	       snd p.Ast.port_nme))
       else
	 iter ast_interface
	      (StringMap.add
		 (fst p.Ast.port_nme)
		 (int_state_list, lnk_state, (snd p.Ast.port_nme))
		 map)
    | [] ->
       StringMap.add
	 "_" ([], Term.with_dummy_pos Ast.FREE,(Lexing.dummy_pos,Lexing.dummy_pos)) map
  in (*Adding default existential port*) iter ast_intf StringMap.empty

let eval_node env a link_map node_map node_id =
  let ast_intf = snd a in
  let (agent_name,_ as agent) = fst a in
  let name_id = Environment.num_of_name agent env in
  let sign = Environment.get_sig name_id env in

  (*Begin sub function*)
  let rec build_intf ast link_map intf bond_list =
    match ast with
    | p :: ast' ->
       begin
	 let int_state_list = p.Ast.port_int
	 and lnk_state = p.Ast.port_lnk in
	 let port_name = p.Ast.port_nme in
	 let prt_pos = snd p.Ast.port_nme in
	 let port_id = Signature.num_of_site ~agent_name port_name sign in
	 let link_map,bond_list =
	   match lnk_state with
	   | (Ast.LNK_VALUE (i),pos) ->
	      begin
		try
		  let opt_node = IntMap.find i link_map in
		  match opt_node with
		  | None ->
		     raise (ExceptionDefn.Malformed_Decl
			      ("Edge identifier at site '" ^ fst port_name
			       ^ "' is used multiple times",pos))
		  | Some (node_id',port_id',_) ->
		     (IntMap.add i None link_map,(node_id,port_id,node_id',port_id')::bond_list)
		with Not_found -> (IntMap.add i (Some (node_id,port_id,pos)) link_map,bond_list)
	      end
	   | (Ast.FREE,_) -> (link_map,bond_list)
	   | (Ast.LNK_SOME, _ | Ast.LNK_TYPE _, _ | Ast.LNK_ANY, _) ->
	      raise (ExceptionDefn.Malformed_Decl
		       ("Site '" ^ fst port_name ^ "' is partially defined",
			prt_pos))
	 in
	 match int_state_list with
	 | [] ->
	    build_intf ast' link_map (IntMap.add port_id (None,Mixture.WLD) intf) bond_list
	 | s::_ ->
	    let i = Signature.num_of_internal_state port_id s sign in
	    build_intf ast' link_map
		       (IntMap.add port_id (Some i,Mixture.WLD) intf) bond_list
       (*Geekish, adding Mixture.WLD to be consistent with Node.create*)
       end
    | [] -> (bond_list,link_map,intf)
  in
  (*end sub function*)

  let (bond_list,link_map,intf) =
    build_intf ast_intf link_map IntMap.empty [] in
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
  (node_map,link_map)

let nodes_of_ast env ast_mixture =
  let rec iter ast_mixture node_map node_id link_map =
    match ast_mixture with
    | a :: ast_mix ->
       let (node_map,link_map) = eval_node env a link_map node_map node_id in
       iter ast_mix node_map (node_id+1) link_map
    | [] ->
       IntMap.iter
	 (fun i opt ->
	  match opt with
	  | None -> ()
	  | Some (_,_,pos) ->
	     raise (ExceptionDefn.Malformed_Decl
		      ("Edge identifier "^string_of_int i^" is dangling",pos))
	 ) link_map ;
       (node_map)
  in
  iter ast_mixture IntMap.empty 0 IntMap.empty

let eval_agent env a ctxt =
  let ((agent_name, pos_ag),ast_intf) = a in
  let name_id =
    Environment.num_of_name (agent_name,pos_ag) env in
  let sign = Environment.get_sig name_id env in
  let port_map = eval_intf ast_intf in
  let (interface, ctxt) =
    StringMap.fold
      (fun site_name (int_state_list, lnk_state, prt_pos) (interface, ctxt) ->
       let site_id =
	 Signature.num_of_site ~agent_name (site_name,prt_pos) sign in
       let int_s =
	 match int_state_list with
	 | [] -> None
	 | h::_ ->
	    Environment.check (agent_name,pos_ag) (site_name,prt_pos) h env;
	    let i = Environment.id_of_state agent_name site_name h env
	    in Some i
       in
       let interface,ctx =
	 match fst lnk_state with
	 | Ast.LNK_VALUE n ->
	    let lnk =
	      (try Some (IntMap.find n ctxt.pairing)
	       with | Not_found -> None)
	    in
	    (match lnk with
	     | Some (Semi ((b, j), _)) ->
		((IntMap.add site_id (int_s, Mixture.BND) interface),
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
		in raise (ExceptionDefn.Malformed_Decl (msg,snd lnk_state))
	     | None ->
		((IntMap.add site_id (int_s, Mixture.BND) interface),
		 {
		   (ctxt)
		 with
		   pairing =
		     IntMap.add n (Semi ((ctxt.curr_id, site_id), snd lnk_state))
				ctxt.pairing;
		})
	    )
	 | Ast.LNK_SOME ->
	    ((IntMap.add site_id (int_s, Mixture.BND) interface), ctxt)
	 | Ast.LNK_ANY ->
	    ((IntMap.add site_id (int_s, Mixture.WLD) interface), ctxt)
	 | Ast.FREE ->
	    ((IntMap.add site_id (int_s, Mixture.FREE) interface), ctxt)
	 | Ast.LNK_TYPE ((ste_nm, pos_ste), (ag_nm, _)) ->
	    (let site_num =
	       try Environment.id_of_site ag_nm ste_nm env
	       with
	       | Not_found ->
		  raise
		    (ExceptionDefn.Malformed_Decl
		       ("binding type is not compatible with agent's signature",
			pos_ste))
	     in
	     let ag_num =
	       Environment.num_of_name (Term.with_dummy_pos ag_nm) env in
	     ((IntMap.add
		 site_id (int_s, Mixture.TYPE (site_num, ag_num)) interface),
	      ctxt)
	    )
       in
       (interface,ctx)
      )
      port_map (IntMap.empty, ctxt)
  in
  (ctxt, (Mixture.create_agent name_id interface))

let mixture_of_ast ?mix_id env ast_mix =
  let rec eval_mixture env ast_mix ctxt mixture =
    match ast_mix with
    | a :: ast_mix ->
       let (ctxt, agent) = eval_agent env a ctxt in
       let id = ctxt.curr_id in
       let new_edges = ctxt.new_edges in
       let (ctxt, mixture, env) =
	 eval_mixture env ast_mix
		      { ctxt with
			curr_id = ctxt.curr_id + 1;
			new_edges = Int2Map.empty;
		      } mixture
       in (ctxt, (Mixture.compose id agent mixture new_edges), env)
    | [] -> (ctxt, mixture, env)
  in
  let ctxt =
    { pairing = IntMap.empty; curr_id = 0; new_edges = Int2Map.empty; } in
  let (ctxt, mix, env) = eval_mixture env ast_mix ctxt (Mixture.empty mix_id)
  in
  begin
    IntMap.iter (*checking that all edge identifiers are pairwise defined*)
      (fun n link ->
       match link with
       | Closed -> ()
       | Semi (_, pos) ->
	  let msg =
	    "edge identifier " ^ ((string_of_int n) ^ " is not paired")
	  in raise (ExceptionDefn.Malformed_Decl (msg,pos))
      )
      ctxt.pairing;
    let mix = Mixture.enum_alternate_anchors mix in
    let mix = Mixture.set_root_of_cc mix in
    (mix,env) (*Modifies mix as a side effect*)
  end

let initial_value_alg counter env (ast, _) =
  Expr_interpreter.value_alg
    counter
    ~get_alg:(fun i ->
	      fst (snd env.Environment.algs.NamedDecls.decls.(i)))
    ~get_mix:(fun _ -> Nbr.zero) ~get_tok:(fun _ -> Nbr.zero) ast

let mixtures_of_result c_mixs env domain (free_id,mixs) =
  let (env',compiled_mixs,_final_id) =
    List.fold_left
      (fun (env,mixs,id) (lbl,ast) ->
       (* <awful hack> *)
       let open Environment in
       let dummy_name = match lbl with
	 | None ->"%anonymous"^string_of_int id
	 | Some lbl -> lbl in
       let env = {env with
		   num_of_kappa = StringMap.add dummy_name id env.num_of_kappa;
		   kappa_of_num = IntMap.add id dummy_name env.kappa_of_num;
		 } in
       (* </awful hack> *)
       let mix,env' = mixture_of_ast ~mix_id:id env ast in
       (env',mix::mixs,pred id)) (env,c_mixs,pred free_id) mixs
  in
  (env',domain,compiled_mixs)

let reduce_val v env domain a_mixs =
  let (domain',mixs',(alg,_pos)) =
    Expr.compile_alg env.Environment.algs.NamedDecls.finder
		     env.Environment.tokens.NamedDecls.finder
		     env.Environment.contact_map domain
		     a_mixs v in
  let dep = Expr.deps_of_alg_expr alg in
  (domain',mixs',alg,dep)

let tokenify algs tokens contact_map domain mixs l =
  List.fold_right
    (fun (alg_expr,(nme,pos)) (domain,mixs,out) ->
     let id =
       try StringMap.find nme tokens
       with Not_found ->
	 raise (ExceptionDefn.Malformed_Decl
		  ("Token "^nme^" is undefined",pos))
     in
     let (domain',mixs',(alg,_pos)) =
       Expr.compile_alg algs tokens contact_map domain mixs alg_expr in
     (domain',mixs',(alg,id)::out)
    ) l (domain,mixs,[])

let newrules_of_ast algs tokens contact_map domain mixs
		    label_opt (ast_rule,rule_pos) =
  let label = match label_opt with
    | None -> Term.with_dummy_pos ("%anonymous"^(string_of_float (Sys.time ())))
    | Some (lab,pos) -> (lab,pos) in
  let opposite (lab,pos) = (lab^"_op",pos) in
  let domain',mixs',rm_toks =
    tokenify algs tokens contact_map domain mixs ast_rule.rm_token in
  let domain'',mixs'',add_toks =
    tokenify algs tokens contact_map domain' mixs' ast_rule.add_token in
  let one_side label (domain,a_mixs,acc) rate lhs rhs rm add =
    let domain',a_mixs',(crate,_) =
      Expr.compile_alg algs tokens contact_map domain a_mixs rate in
    let count = let x = ref 0 in fun (lab,pos) ->
				 incr x; (lab^"__"^string_of_int !x,pos) in
    let build (ccs,(neg,pos)) =
      {
	Primitives.rate = crate;
	Primitives.connected_components = ccs;
	Primitives.removed = neg;
	Primitives.inserted = pos;
	Primitives.consumed_tokens = rm;
	Primitives.injected_tokens = add;
      } in
    let domain'',rule_mixtures =
      Snip.connected_components_sum_of_ambiguous_rule
	contact_map domain' lhs rhs in
    domain'',a_mixs',
    match rule_mixtures with
    | [] -> acc
    | [ r ] -> (label, build r) :: acc
    | _ ->
       List.fold_left
	 (fun out r ->
	  (count label,build r)::out) acc rule_mixtures in
  let rev = match ast_rule.arrow, ast_rule.k_op with
    | RAR, None -> domain'',mixs'',[]
    | LRAR, Some rate ->
       one_side (opposite label) (domain'',mixs'',[]) rate
		ast_rule.rhs ast_rule.lhs add_toks rm_toks
    | (RAR, Some _ | LRAR, None) ->
       raise
	 (ExceptionDefn.Malformed_Decl
	    ("Incompatible arrow and kinectic rate for inverse definition",
	     rule_pos))
  in
  one_side label rev ast_rule.k_def ast_rule.lhs ast_rule.rhs rm_toks add_toks

let rule_of_ast ?(backwards=false) ~is_pert env domain
		mixs (ast_rule_label,(ast_rule,rule_pos)) =
  let ast_rule_label,ast_rule =
    if backwards then Ast.flip (ast_rule_label, ast_rule)
    else (ast_rule_label, ast_rule) in
  let (env, lhs_id) =
    Environment.declare_var_kappa ~from_rule:true ast_rule_label env in
  (* reserving an id for rule's lhs in the pattern table *)
  let env = Environment.declare_rule ast_rule_label lhs_id env in
  let domain',a_mixs',k_def,dep =
    reduce_val ast_rule.k_def env domain (env.Environment.fresh_kappa,[]) in
  let (env,domain'',a_mixs'',k_alt,radius,dep_alt) =
    match ast_rule.k_un with
    | None -> (env,domain',a_mixs',None,None, Term.DepSet.empty)
    | Some (ast,ast_opt) ->
       (****TODO HERE treat ast_opt that specifies application radius****)
       let env =
	 Environment.declare_unary_rule
	   (Some (Term.with_dummy_pos (Environment.kappa_of_num lhs_id env)))
	   (*ast_rule_label*) lhs_id env in
       let domain'',a_mixs'',k_alt,dep =
	 reduce_val ast env domain' a_mixs' in
       let domain''',a_mixs''',radius_alt,dep = match ast_opt with
	   None -> (domain'',a_mixs'',None,dep)
	 | Some v ->
	    let domain''',a_mixs''',rad,dep' =
	      reduce_val v env domain'' a_mixs'' in
	    (domain''',a_mixs''',Some rad,Term.DepSet.union dep dep')
       in
       (env,domain''',a_mixs''',Some k_alt,radius_alt,dep)
  in
  let lhs,env = mixture_of_ast ~mix_id:lhs_id env ast_rule.lhs in
  let lhs = match k_alt with None -> lhs | Some _ -> Mixture.set_unary lhs in
  let rhs,env = mixture_of_ast env ast_rule.rhs in
  let (script, balance,added,modif_sites(*,side_effects*)) =
    Dynamics.diff rule_pos lhs rhs env in

  let domain''',a_mixs''',add_token =
    tokenify env.Environment.algs.NamedDecls.finder
	     env.Environment.tokens.NamedDecls.finder
	     env.Environment.contact_map  domain'' a_mixs'' ast_rule.add_token in
  let a_domain,a_mixs,rm_token =
    tokenify env.Environment.algs.NamedDecls.finder
	     env.Environment.tokens.NamedDecls.finder
	     env.Environment.contact_map domain''' a_mixs''' ast_rule.rm_token in
  let r_id = Mixture.get_id lhs in
  let env = if Mixture.is_empty lhs
	    then Environment.declare_empty_lhs r_id env
	    else env in
  let env =
    Term.DepSet.fold
      (*creating dependencies between variables in the kinetic rate and the activity of the rule*)
      (fun dep env ->
       Environment.add_dependencies dep (Term.RULE r_id) env)
      (Term.DepSet.union dep dep_alt) env
  in
  let pre_causal = Dynamics.compute_causal lhs rhs script env in

  let connect_impact,disconnect_impact,side_eff_impact =
    List.fold_left
      (fun (con_map,dis_map,side_eff) action ->
       match action with
       | Primitives.BND ((Primitives.KEPT id,s),(Primitives.KEPT id',s')) ->
	  (*binding with a fresh agent doesn't impact connectedness*)
	  let cc_id = Mixture.component_of_id id lhs
	  and cc_id' = Mixture.component_of_id id' lhs
	  in
	  if cc_id = cc_id' then (con_map,dis_map,side_eff)
	  (*rule binds two ports that were already part of the same cc*)
	  else
	    let eq_id = try IntMap.find cc_id con_map with Not_found -> cc_id
	    and eq_id'= try IntMap.find cc_id' con_map with Not_found -> cc_id'
	    in
	    let min_eq,max_eq = (min eq_id eq_id',max eq_id eq_id') in
	    let con_map = IntMap.add cc_id min_eq con_map in
	    let con_map = IntMap.add cc_id' min_eq con_map in
	    let con_map = IntMap.add max_eq min_eq con_map in
	    (con_map,dis_map,side_eff)
       | Primitives.FREE ((Primitives.KEPT id,s),side_effect_free) ->
	  if side_effect_free then
	    begin
	      let id' = match Mixture.follow (id,s) lhs with
		| None -> invalid_arg "Eval.build_cc_impact: Free action should be side effect free"
		| Some (id',_) -> id' in
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
       | Primitives.DEL id ->
	  let ag = Mixture.agent_of_id id lhs in
	  let sign = Environment.get_sig (Mixture.name ag) env in
	  let set = try IntMap.find id (side_eff:IntSet.t IntMap.t) with Not_found -> IntSet.empty in
	  let set =
	    Signature.fold
	      (fun site_id _ set ->
	       if site_id = 0 then set
	       else
		 let opt = Mixture.follow (id,site_id) lhs in
		 match opt with
		 | None ->
		    begin
		      try
			let _,lnk =
			  IntMap.find site_id (Mixture.interface (Mixture.agent_of_id id lhs)) in
			match lnk with
			| Mixture.BND | Mixture.TYPE _ ->
				      IntSet.add site_id set (*semi link deletion*)
			| Mixture.FREE | Mixture.WLD -> set
		      with
		      | Not_found ->
			 IntSet.add site_id set (*deletion of site that was not mentionned so possible side_effect*)
		    end
		 | Some _ ->
		    IntSet.add site_id set (*link deletion because deleted agent was connected in the lhs*)
	      ) sign set
	  in
	  (con_map,dis_map,IntMap.add id set side_eff)
       | (Primitives.MOD _ | Primitives.ADD _
	  | Primitives.BND _ | Primitives.FREE ((Primitives.FRESH _,_),_)) ->
	  (con_map,dis_map,side_eff)
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
  let (env,domain',mixs') = mixtures_of_result mixs env a_domain a_mixs in
  (env,domain',mixs',
   {
     Primitives.add_token = add_token ;
     Primitives.rm_token = rm_token ;
     Primitives.k_def = k_def ;
     Primitives.k_alt = (k_alt,radius) ;
     Primitives.over_sampling = None;
     Primitives.script = script;
     Primitives.balance = balance;
     Primitives.lhs = lhs;
     Primitives.rhs = rhs;
     Primitives.r_id = r_id;
     Primitives.added =
       List.fold_left (fun set i -> IntSet.add i set) IntSet.empty added ;
     (*Primitives.side_effect = side_effect ; *)
     Primitives.modif_sites = modif_sites ;
     Primitives.is_pert = is_pert ;
     Primitives.pre_causal = pre_causal ;
     Primitives.cc_impact =
       Some (connect_impact,disconnect_impact,side_eff_impact)
  })

let variables_of_result env cc_env mixs alg_a =
  let (env',cc_env',compiled_mixs) = mixtures_of_result [] env cc_env mixs in
  let env'',_ = Array.fold_left
		  (fun (env,var_id) (_,alg) ->
		   let deps = Expr.deps_of_alg_expr (fst alg) in
		   try
		     (Term.DepSet.fold
			(fun dep env ->
			 Environment.add_dependencies dep (Term.ALG var_id) env
			)
			deps env,
		      succ var_id)
		   with
		   | Invalid_argument msg ->
		      invalid_arg ("State.initialize: " ^ msg)
		  )
		  (env',0) alg_a
  in (env'',cc_env',compiled_mixs)

let rules_of_result env cc_env mixs res =
  let (env, cc_env, mixs, l) =
    List.fold_left
      (fun (env, cc_env, mixs, cont) (_,(ast_rule,_) as ast) ->
       let (env, cc_env, mixs, r) =
	 rule_of_ast ~is_pert:false env cc_env mixs ast in
       match ast_rule.Ast.k_op with
       | None -> (env,cc_env,mixs,r::cont)
       | Some _ ->
	  let (env,cc_env,mixs,back_r) =
	    rule_of_ast ~backwards:true ~is_pert:false env cc_env mixs ast in
	  (env,cc_env,mixs,back_r::(r::cont))
      )
      (env, cc_env, mixs, []) res.Ast.rules
  in (env, cc_env, mixs, (List.rev l))

let obs_of_result env domain mixs res =
  let (domain',a_mixs,cont) =
    List.fold_left
      (fun (domain,a_mixs,cont) alg_expr ->
       let (domain',a_mixs',(alg,_pos)) =
	 Expr.compile_alg env.Environment.algs.NamedDecls.finder
			  env.Environment.tokens.NamedDecls.finder
			  env.Environment.contact_map domain
			  a_mixs alg_expr in
       domain',a_mixs',
       (alg,Format.asprintf "%a" Expr.print_ast_alg (fst alg_expr)) :: cont
      )
      (domain,(env.Environment.fresh_kappa,[]),[]) res.observables in
  let (env',domain'',mixs') = mixtures_of_result mixs env domain' a_mixs in
  (env',domain'',mixs',cont)

let compile_print_expr env domain mixs ex =
  List.fold_right
    (fun (el,pos) (domain,mixs,out) ->
     match el with
     | Ast.Str_pexpr s -> (domain,mixs,(Ast.Str_pexpr s,pos)::out)
     | Ast.Alg_pexpr ast_alg ->
	let (domain',mixs', (alg,_pos)) =
	  Expr.compile_alg env.Environment.algs.NamedDecls.finder
			   env.Environment.tokens.NamedDecls.finder
			   env.Environment.contact_map domain
			   mixs (ast_alg,pos) in
	(domain',mixs',(Ast.Alg_pexpr alg,pos)::out))
    ex (domain,mixs,[])

let effects_of_modif variables lrules env domain ast_list =
  let rec iter mixs lrules rev_effects env domain ast_list =
    let rule_effect alg_expr ast_rule mix_pos =
      let (domain',mix,alg_pos) =
	Expr.compile_alg env.Environment.algs.NamedDecls.finder
			 env.Environment.tokens.NamedDecls.finder
			 env.Environment.contact_map domain
			 (env.Environment.fresh_kappa,[]) alg_expr in
      let domain'',mix',elem_rules =
	newrules_of_ast env.Environment.algs.NamedDecls.finder
			env.Environment.tokens.NamedDecls.finder
			env.Environment.contact_map domain'
			mix None (ast_rule,mix_pos) in
      let elem_rule = match elem_rules with
	| [ _, r ] -> r
	| _ ->
	   raise
	     (ExceptionDefn.Malformed_Decl
		("Ambiguous rule in perturbation is impossible",mix_pos)) in
      let (env',domain''',mixs') =
	mixtures_of_result mixs env domain'' mix' in
      let env,domain,mixs'',rule =
	rule_of_ast ~is_pert:true env' domain''' mixs'
		    (None, (ast_rule,mix_pos)) in
      (env,domain,mixs'',rule::lrules,
       (Primitives.ITER_RULE (alg_pos, rule, elem_rule))::rev_effects) in
    match ast_list with
    | [] -> (env,domain,mixs,lrules,List.rev rev_effects)
    | ast::tl ->
       let (env,domain,variables,lrules,rev_effects) =
	 match ast with
	 | INTRO (alg_expr, (ast_mix,mix_pos)) ->
	    let ast_rule =
	      { add_token=[]; rm_token=[]; lhs = []; arrow = Ast.RAR;
		rhs = ast_mix; k_def=Term.with_dummy_pos (Ast.CONST Nbr.zero);
		k_un=None;k_op=None;
	      } in
	    rule_effect alg_expr ast_rule mix_pos
	 | DELETE (alg_expr, (ast_mix, mix_pos)) ->
	    let ast_rule =
	      { add_token=[]; rm_token=[]; lhs = ast_mix; arrow = Ast.RAR;
		rhs = [];
		k_def=Term.with_dummy_pos (Ast.CONST Nbr.zero);
		k_un=None;k_op=None;
	      } in
	    rule_effect alg_expr ast_rule mix_pos
	 | UPDATE ((nme, pos_rule), alg_expr) ->
	    let i,is_rule =
	      (try (Environment.num_of_rule nme env,true)
	       with
	       | Not_found ->
		  try
		    (Environment.num_of_alg nme env, false)
		  with Not_found ->
		    raise (ExceptionDefn.Malformed_Decl
			     ("Variable " ^ (nme ^ " is neither a constant nor a rule")
			     ,pos_rule))
	      ) in
	    let (domain', mix, alg_pos) =
	      Expr.compile_alg env.Environment.algs.NamedDecls.finder
			       env.Environment.tokens.NamedDecls.finder
			       env.Environment.contact_map domain
			       (env.Environment.fresh_kappa,[]) alg_expr in
	    let (env',domain'',mixs') =
	      mixtures_of_result mixs env domain' mix in
	    (env',domain'',mixs',lrules,
	     (Primitives.UPDATE ((if is_rule then Term.RULE i
	      else Term.ALG i), alg_pos))::rev_effects)
	 | UPDATE_TOK ((tk_nme,tk_pos),alg_expr) ->
	    let ast_rule =
	      { add_token=[(alg_expr,(tk_nme,tk_pos))];
		rm_token=[Term.with_dummy_pos (Ast.TOKEN_ID tk_nme),
			  (tk_nme,tk_pos)];
		arrow = Ast.RAR; lhs=[]; rhs=[];
		k_def=Term.with_dummy_pos (Ast.CONST Nbr.zero);
		k_un=None; k_op= None; } in
	    rule_effect (Term.with_dummy_pos (Ast.CONST (Nbr.I 1)))
			ast_rule tk_pos
	 | SNAPSHOT (pexpr,_) ->
	    let (domain',mix,pexpr') =
	      compile_print_expr env domain
				 (env.Environment.fresh_kappa,[]) pexpr in
	    let (env',domain'',mixs') =
	      mixtures_of_result mixs env domain' mix in
	    (*when specializing snapshots to particular mixtures, add variables below*)
	    (env',domain'',mixs', lrules,
	     (Primitives.SNAPSHOT pexpr')::rev_effects)
	 | STOP (pexpr,_) ->
	    let (domain',mix,pexpr') =
	      compile_print_expr env domain
				 (env.Environment.fresh_kappa,[]) pexpr in
	    let (env',domain'',mixs') =
	      mixtures_of_result mixs env domain' mix in
	    (env',domain'',mixs', lrules,
	     (Primitives.STOP pexpr')::rev_effects)
	 | CFLOW ((lab,pos_lab),_) ->
	    let id =
	      try Environment.num_of_rule lab env
	      with Not_found ->
		try let var = Environment.num_of_alg lab env in
		    match env.Environment.algs.NamedDecls.decls.(var) with
		    |(_,(Expr.KAPPA_INSTANCE (i,_),_)) -> i
		    | (_,((Expr.CONST _ | Expr.BIN_ALG_OP _ | Expr.TOKEN_ID _ |
			   Expr.STATE_ALG_OP _ | Expr.UN_ALG_OP _ |
			   Expr.ALG_VAR _),_)) -> raise Not_found
		with Not_found ->
		  raise	(ExceptionDefn.Malformed_Decl
			   ("Label '" ^ lab ^ "' is neither a rule nor a Kappa expression"
			   ,pos_lab))
	    in
	    (env,domain,mixs, lrules,
	     (Primitives.CFLOW id)::rev_effects)
	 | CFLOWOFF ((lab,pos_lab),_) ->
	    let id =
	      try Environment.num_of_rule lab env
	      with Not_found ->
		try let var = Environment.num_of_alg lab env in
		    match env.Environment.algs.NamedDecls.decls.(var) with
		    |(_,(Expr.KAPPA_INSTANCE (i,_),_)) -> i
		    | (_,((Expr.CONST _ | Expr.BIN_ALG_OP _ | Expr.TOKEN_ID _ |
			   Expr.STATE_ALG_OP _ | Expr.UN_ALG_OP _ |
			   Expr.ALG_VAR _),_)) -> raise Not_found
		with Not_found ->
		  raise	(ExceptionDefn.Malformed_Decl
			   ("Label '" ^ lab ^ "' is neither a rule nor a Kappa expression"
			   ,pos_lab))
	    in
	    (env,domain,mixs, lrules,
	     (Primitives.CFLOWOFF id)::rev_effects)
	 | FLUX (pexpr,_) ->
	    let (domain',mix,pexpr') =
	      compile_print_expr env domain
				 (env.Environment.fresh_kappa,[]) pexpr in
	    let (env',domain'',mixs') =
	      mixtures_of_result mixs env domain' mix in
	    (env',domain'',mixs', lrules,
	     (Primitives.FLUX pexpr')::rev_effects)
	 | FLUXOFF (pexpr,_) ->
	    let (domain',mix,pexpr') =
	      compile_print_expr env domain
				 (env.Environment.fresh_kappa,[]) pexpr in
	    let (env',domain'',mixs') =
	      mixtures_of_result mixs env domain' mix in
	    (env',domain'',mixs', lrules,
	     (Primitives.FLUXOFF pexpr')::rev_effects)
	 | PRINT (pexpr,print,_) ->
	    let (domain',mix,pexpr') =
	      compile_print_expr env domain
				 (env.Environment.fresh_kappa,[]) pexpr in
	    let (domain'',mix',print') =
	      compile_print_expr env domain' mix print in
	    let (env',domain''', mixs') =
	      mixtures_of_result mixs env domain'' mix' in
	    (env',domain''',mixs',lrules,
	     (Primitives.PRINT (pexpr',print'))::rev_effects)
	 | PLOTENTRY ->
	    (env,domain,mixs, lrules,
	     (Primitives.PLOTENTRY)::rev_effects)
       in
       iter variables lrules rev_effects env domain tl
  in
  iter variables lrules [] env domain ast_list

let pert_of_result variables env domain rules res =
  let (env, domain, variables, _, lpert, lrules) =
    List.fold_left
      (fun (env, domain, variables, p_id, lpert, lrules)
	   ((pre_expr, modif_expr_list, opt_post),pos) ->
       let (domain',mix,(pre,_pos)) =
	 Expr.compile_bool env.Environment.algs.NamedDecls.finder
			   env.Environment.tokens.NamedDecls.finder
			   env.Environment.contact_map domain
			   (env.Environment.fresh_kappa,[]) pre_expr in
       let (env', domain'', variables') =
	 mixtures_of_result variables env domain' mix in
       let (dep, stopping_time) = try Expr.deps_of_bool_expr pre
		 with ExceptionDefn.Unsatisfiable ->
		   raise
		     (ExceptionDefn.Malformed_Decl
			("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
			,pos))
       in
       let (env,domain,variables, lrules', effects) =
	 effects_of_modif variables' lrules env' domain'' modif_expr_list in
       let env,domain,variables,opt_abort =
	 match opt_post with
	 | None ->
	    (env,domain,variables,None)
	 | Some post_expr ->
	    let (domain',mix,(post,_pos)) =
	      Expr.compile_bool env.Environment.algs.NamedDecls.finder
				env.Environment.tokens.NamedDecls.finder
				env.Environment.contact_map domain
				(env.Environment.fresh_kappa,[]) post_expr in
	    let (env', domain'', variables') =
	      mixtures_of_result variables env domain' mix in
	    let (dep,stopping_time') =
	      try Expr.deps_of_bool_expr post with
		ExceptionDefn.Unsatisfiable ->
		raise
		  (ExceptionDefn.Malformed_Decl
		     ("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
		     ,pos))
	    in
	    (env',domain'',variables',Some (post,dep))
       in
       let has_tracking = env.Environment.tracking_enabled
			  || List.exists
			       (function
				 | Primitives.CFLOW _ -> true
				 | (Primitives.CFLOWOFF _ | Primitives.PRINT _ |
				    Primitives.UPDATE _ | Primitives.SNAPSHOT _
				    | Primitives.FLUX _ | Primitives.FLUXOFF _ |
				    Primitives.PLOTENTRY | Primitives.STOP _ |
				    Primitives.ITER_RULE _) -> false) effects in
       let env =
	 Term.DepSet.fold
	   (fun dep -> Environment.add_dependencies dep (Term.PERT p_id))
	   dep
	   { env with Environment.tracking_enabled = has_tracking } in
       (*let env = List.fold_left (fun env (r_opt,effect) -> Environment.bind_pert_rule p_id r.r_id env) env effect_list in *)
       let opt,env =
	 match opt_abort with
	 | None -> (None,env)
	 | Some (post,dep) ->
	    let env =
	      Term.DepSet.fold
		(fun dep_type env ->
		 Environment.add_dependencies dep_type (Term.ABORT p_id) env
		)
		dep env
	    in
	    (Some post,env)
       in
       let pert =
	 { Primitives.precondition = pre;
	   Primitives.effect = effects;
	   Primitives.abort = opt;
	   Primitives.stopping_time = stopping_time
	 }
       in
       (env, domain, variables, succ p_id, pert::lpert, lrules')
      )
      (env, domain, variables, 0, [], rules) res.perturbations
  in
  (*making sure that perturbations containing a stopping time precondition are tested first*)
  let lpert = List.rev lpert in
  let pred = (fun p -> match p.Primitives.stopping_time with
			 None -> false | Some _ -> true) in
  let lpert_stopping_time = List.filter pred lpert in
  let lpert_ineq = List.filter (fun p -> not (pred p)) lpert in
  let lpert = lpert_stopping_time@lpert_ineq in
  (env, domain, variables, lpert, lrules)

let init_graph_of_result counter env domain res =
  let n = Array.length env.Environment.tokens.NamedDecls.decls in
  let token_vector = Array.init n (fun _ -> 0.) in
  let domain',init_state,sg =
    List.fold_left
      (fun (domain,state,sg) (opt_vol,init_t,_) -> (*TODO dealing with volumes*)
       match init_t with
       | INIT_MIX (alg, (ast,mix_pos)) ->
	  let (domain',_,alg') =
	    Expr.compile_alg env.Environment.algs.NamedDecls.finder
			     env.Environment.tokens.NamedDecls.finder
			     env.Environment.contact_map domain (0,[]) alg in
	  let value = initial_value_alg counter env alg' in
	  let fake_rule =
	    { lhs = []; rm_token = []; arrow = RAR; rhs = ast; add_token = [];
	      k_def = Term.with_dummy_pos (CONST Nbr.zero);
	      k_un = None; k_op = None; } in
	  let domain'',state' =
	    match
	      newrules_of_ast env.Environment.algs.NamedDecls.finder
			      env.Environment.tokens.NamedDecls.finder
			      env.Environment.contact_map domain' (0,[]) None
			      (fake_rule,mix_pos)
	    with
	    | domain'',_,[ _, compiled_rule ] ->
	       domain'',
	       Nbr.iteri
		 (fun _ s ->
		  Rule_interpreter.apply_rule
		    ~get_alg:(fun i ->
			      fst (snd env.Environment.algs.NamedDecls.decls.(i)))
		    domain'' counter s compiled_rule)
		 state value
	    | domain'',_,[] -> domain'',state
	    | _,_,_ ->
	       raise (ExceptionDefn.Malformed_Decl
			(Format.asprintf
			   "initial mixture %a is partially defined"
			   Expr.print_ast_mix ast,mix_pos)) in
	  let cpt = ref 0 in
	  let sg = ref sg in
	  let n = match !Parameter.rescale with
	    | None -> Nbr.to_int value
	    | Some i -> min i (Nbr.to_int value)
	  in
	  (* Cannot do Mixture.to_nodes env m once for all because of *)
	  (* references                                               *)
	  while !cpt < n do
	    let nodes = nodes_of_ast env ast in
	    sg := Graph.SiteGraph.add_nodes !sg nodes;
	    cpt := !cpt + 1
	  done;
	  domain'',state',!sg
       | INIT_TOK (alg, (tk_nme,pos_tk)) ->
	  let fake_rule =
	    { lhs = []; rm_token = []; arrow = RAR; rhs = [];
	      add_token = [(alg, (tk_nme,pos_tk))];
	      k_def = Term.with_dummy_pos (CONST Nbr.zero);
	      k_un = None; k_op = None; } in
	  let domain',state' =
	    match
	      newrules_of_ast env.Environment.algs.NamedDecls.finder
			      env.Environment.tokens.NamedDecls.finder
			      env.Environment.contact_map domain (0,[]) None
			      (Term.with_dummy_pos fake_rule)
	    with
	    | domain'',_,[ _, compiled_rule ] ->
	       domain'',
	       Rule_interpreter.apply_rule
		 ~get_alg:(fun i ->
			   fst (snd env.Environment.algs.NamedDecls.decls.(i)))
		 domain'' counter state compiled_rule
	    | _,_,_ -> assert false in
	  let (domain'',_,alg') =
	    Expr.compile_alg env.Environment.algs.NamedDecls.finder
			     env.Environment.tokens.NamedDecls.finder
			     env.Environment.contact_map domain' (0,[]) alg
	  in
	  let value = Nbr.to_float (initial_value_alg counter env alg') in
	  let tok_id =
	    try Environment.num_of_token tk_nme env
	    with Not_found ->
	      raise (ExceptionDefn.Malformed_Decl
		       ("token "^tk_nme^" is undeclared",pos_tk))
	  in
	  token_vector.(tok_id) <- value;
	  domain'',state',sg
      )	(domain,Rule_interpreter.empty env,
	 Graph.SiteGraph.init !Parameter.defaultGraphSize)
      res.Ast.init
  in
  (domain',init_state,sg,token_vector)

let configurations_of_result result =
  let raw_set_value pos_p param value_list f =
    match value_list with
    | (v,_) :: _ -> f v pos_p
    | [] -> ExceptionDefn.warning
	      ~pos:pos_p
	      (fun f -> Format.fprintf f "Empty value for parameter %s" param)
  in
  let set_value pos_p param value_list f ass =
    raw_set_value pos_p param value_list (fun x p -> ass := f x p) in
  List.iter
    (fun ((param,pos_p),value_list) ->
     match param with
     | "displayCompression" ->
	begin
	  let rec parse l =
	    match l with
	    | ("strong",_)::tl ->
	       (Parameter.strongCompression := true ; parse tl)
	    | ("weak",_)::tl -> (Parameter.weakCompression := true ; parse tl)
	    | ("none",_)::tl -> (Parameter.mazCompression := true ; parse tl)
	    | [] -> ()
	    | (error,_)::_ ->
	       raise (ExceptionDefn.Malformed_Decl
			("Unkown value "^error^" for compression mode", pos_p))
	  in
	  parse value_list
	end
     | "cflowFileName"	->
	raw_set_value pos_p param value_list (fun x _ -> Kappa_files.set_cflow x)
     | "progressBarSize" ->
	set_value pos_p param value_list
		  (fun v p ->
		   try int_of_string v
		   with _ ->
		     raise (ExceptionDefn.Malformed_Decl
			      ("Value "^v^" should be an integer", p))
		  ) Parameter.progressBarSize

     | "progressBarSymbol" ->
	set_value pos_p param value_list
		  (fun v p ->
		   try
		     String.unsafe_get v 0
		   with _ ->
		     raise (ExceptionDefn.Malformed_Decl
			      ("Value "^v^" should be a character",p))
		  ) Parameter.progressBarSymbol

     | "dumpIfDeadlocked" ->
	set_value pos_p param value_list
		  (fun value pos_v ->
		   match value with
		   | "true" | "yes" -> true
		   | "false" | "no" -> false
		   | _ as error ->
		      raise (ExceptionDefn.Malformed_Decl
			       ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
		  ) Parameter.dumpIfDeadlocked
     | "plotSepChar" ->
	set_value pos_p param value_list
		  (fun v _ ->
		   fun f ->  Format.fprintf f "%s" v
		  ) Parameter.plotSepChar
     | "maxConsecutiveClash" ->
	set_value pos_p param value_list
		  (fun v p ->
		   try int_of_string v
		   with _ ->
		     raise (ExceptionDefn.Malformed_Decl
			      ("Value "^v^" should be an integer",p))
		  ) Parameter.maxConsecutiveClash

     | "dotSnapshots" ->
	set_value pos_p param value_list
		  (fun value pos_v ->
		   match value with
		   | "true" | "yes" -> true
		   | "false" | "no" -> false
		   | _ as error ->
		      raise (ExceptionDefn.Malformed_Decl
			       ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
		  ) Parameter.dotOutput
     | "colorDot" ->
	set_value pos_p param value_list
		  (fun value pos_v ->
		   match value with
		   | "true" | "yes" -> true
		   | "false" | "no" -> false
		   | _ as error ->
		      raise (ExceptionDefn.Malformed_Decl
			       ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
		  ) Parameter.useColor
     | "dumpInfluenceMap" ->
	raw_set_value
	  pos_p param value_list
	  (fun v p ->
	   match v with
	   | "true" | "yes" -> Kappa_files.set_up_influence ()
	   | "false" | "no" -> Kappa_files.set_influence ""
	   | _ as error ->
	      raise (ExceptionDefn.Malformed_Decl
		       ("Value "^error^" should be either \"yes\" or \"no\"",p))
	     )
     | "influenceMapFileName" ->
	raw_set_value pos_p param value_list
		      (fun x _ -> Kappa_files.set_influence x)
     | "showIntroEvents" ->
	set_value pos_p param value_list
		  (fun v p -> match v with
				| "true" | "yes" -> true
				| "false" | "no" -> false
				| _ as error ->
				   raise (ExceptionDefn.Malformed_Decl
					    ("Value "^error^" should be either \"yes\" or \"no\"",p))
		  )
		  Parameter.showIntroEvents
     | _ as error ->
	raise (ExceptionDefn.Malformed_Decl ("Unkown parameter "^error, pos_p))
    ) result.configurations

let compile_alg_vars tokens contact_map domain overwrite vars =
  let alg_vars_over =
    Tools.list_rev_map_append
      (fun (x,v) -> (Term.with_dummy_pos x,
		     Term.with_dummy_pos (Ast.CONST v))) overwrite
      (List.filter
	 (fun ((x,_),_) ->
	  List.for_all (fun (x',_) -> x <> x') overwrite) vars) in
  let vars_nd = NamedDecls.create (Array.of_list alg_vars_over) in
  array_fold_left_mapi (fun i (domain,mixs) ((label,_ as lbl_pos),ast) ->
			let (domain',mixs',alg) =
			  Expr.compile_alg ~label vars_nd.NamedDecls.finder
					   tokens ~max_allowed_var:(pred i)
					   contact_map domain mixs ast
			in ((domain',mixs'),(lbl_pos,alg))) (domain,(0,[]))
		       vars_nd.NamedDecls.decls

let compile_rules algs tokens contact_map domain mixs rules =
  List.fold_left
    (fun (domain,mixs,acc) (rule_label,rule) ->
     let (domain',mixs',cr) =
       newrules_of_ast algs tokens contact_map domain mixs rule_label rule in
    domain',mixs',List.append cr acc)
    (domain,mixs,[]) rules

let initialize logger overwrite result =
  Debug.tag logger "+ Building initial simulation conditions...";
  let counter =
    Counter.create !Parameter.pointNumberValue
		   0.0 0 !Parameter.maxTimeValue !Parameter.maxEventValue in
  Debug.tag logger "+ Compiling..." ;
  Debug.tag logger "\t -simulation parameters" ;
  let _ = configurations_of_result result in

  Debug.tag logger "\t -agent signatures" ;
  let sigs_nd = Signature.create result.Ast.signatures in
  let () = Debug.global_sigs := sigs_nd in
  let tk_nd =
    NamedDecls.create (array_map_of_list (fun x -> (x,())) result.Ast.tokens) in

  let pre_kasa_state = Export_to_KaSim.Export_to_KaSim.init result in
  let _kasa_state,contact_map =
    Export_to_KaSim.Export_to_KaSim.get_contact_map pre_kasa_state in

  let domain = Connected_component.Env.empty sigs_nd in
  Debug.tag logger "\t -variable declarations";
  let (domain',mixs),alg_a =
    compile_alg_vars tk_nd.NamedDecls.finder contact_map domain
		     overwrite result.Ast.variables in
  let alg_nd = NamedDecls.create alg_a in

  let (domain',(fresh_kappa,_ as mixs'),compiled_rules) =
    compile_rules alg_nd.NamedDecls.finder tk_nd.NamedDecls.finder contact_map
		  domain' mixs result.Ast.rules in
  let rule_nd = NamedDecls.create (Array.of_list compiled_rules) in

  let env =
    Environment.init sigs_nd contact_map tk_nd alg_nd rule_nd fresh_kappa in
  let (env, domain, kappa_vars) =
    variables_of_result env domain' mixs' alg_a in

  Debug.tag logger "\t -rules";
  let (env, domain, kappa_vars, pure_rules) =
    rules_of_result env domain kappa_vars result in

  Debug.tag logger "\t -observables";
  let env,domain,kappa_vars,observables =
    obs_of_result env domain kappa_vars result in
  Debug.tag logger "\t -perturbations" ;
  let (env, domain, kappa_vars, pert, rules) =
    pert_of_result kappa_vars env domain pure_rules result in

  Debug.tag logger "\t -initial conditions";
  let domain,graph,sg,token_vector =
    init_graph_of_result counter env domain result in
  let () = Format.printf "%a@." (Rule_interpreter.print env) graph in
  let state = State_interpreter.initial env counter graph [] in

  Debug.tag logger "\t Done";
  Debug.tag logger "+ Analyzing non local patterns..." ;
  let env = Environment.init_roots_of_nl_rules env in
  Debug.tag logger "\t -Counting initial local patterns..." ;
  let (state, env) =
    State.initialize logger sg token_vector rules kappa_vars
		     observables pert counter env
  in
  let state =
    if env.Environment.has_intra then
      begin
	Debug.tag logger "\t -Counting initial non local patterns..." ;
	NonLocal.initialize_embeddings state counter env
      end
    else state
  in
  (Debug.tag logger "\t Done"; (env, domain, counter, state))
