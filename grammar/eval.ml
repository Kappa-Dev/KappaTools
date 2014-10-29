open Mods
open Mixture
open Dynamics
open Tools
open Ast

type link = Closed | Semi of int * int * pos

type context =
    { pairing : link IntMap.t; curr_id : int;
      new_edges : (int * int) Int2Map.t }

let interface_for_def ast_intf =
  NamedDecls.create (
    array_map_of_list
      (fun p ->
       match p.Ast.port_lnk with
       | (Ast.FREE,_) ->
	  (p.Ast.port_nme,
	   match p.Ast.port_int with
	   | [] -> None
	   | l ->
	      Some (NamedDecls.create (array_map_of_list (fun x -> (x,())) l))
	  )
       | (_, pos) ->
	  raise (ExceptionDefn.Malformed_Decl
		("Link status inside a definition of signature", pos))
      ) ({Ast.port_nme =Term.with_dummy_pos "_";
	  Ast.port_int=[];
	  Ast.port_lnk =Term.with_dummy_pos Ast.FREE;}
	 :: ast_intf))

let interface_for_decl ast_intf =()

let eval_intf ast_intf =
  let rec iter ast_intf map =
    match ast_intf with
    | p :: ast_interface ->
       let int_state_list = p.Ast.port_int
       and lnk_state = p.Ast.port_lnk
       in
       if StringMap.mem (fst p.Ast.port_nme) map then
	 raise
	   (ExceptionDefn.Semantics_Error
	      (pos_of_lex_pos (fst (snd p.Ast.port_nme)),
	       "Site '" ^ (fst p.Ast.port_nme) ^ "' is used multiple times"))
       else
	 iter ast_interface
	      (StringMap.add (fst p.Ast.port_nme) (int_state_list, lnk_state, (snd p.Ast.port_nme)) map)
    | [] ->
       StringMap.add
	 "_" ([], Term.with_dummy_pos Ast.FREE,(Lexing.dummy_pos,Lexing.dummy_pos)) map
  in (*Adding default existential port*) iter ast_intf StringMap.empty

let eval_node env a link_map node_map node_id =
  let ast_intf = snd a in
  let ag_name = fst (fst a) in
  let pos_ag = pos_of_lex_pos (fst (snd (fst a))) in
  let name_id =
    try Environment.num_of_name ag_name env with
    | Not_found ->
       raise (ExceptionDefn.Semantics_Error (pos_ag,"Agent '" ^ ag_name ^ "' is not declared"))
  in
  let sign =
    try Environment.get_sig name_id env with
    | Not_found ->
       raise (ExceptionDefn.Semantics_Error (pos_ag,"Agent '" ^ ag_name ^ "' is not declared"))
  in

  (*Begin sub function*)
  let rec build_intf ast link_map intf bond_list =
    match ast with
    | p :: ast' ->
       begin
	 let int_state_list = p.Ast.port_int
	 and lnk_state = p.Ast.port_lnk in
	 let port_name = fst p.Ast.port_nme in
	 let prt_pos = snd p.Ast.port_nme in
	 let port_id =
	   try Signature.num_of_site port_name sign with
	     Not_found ->
	     raise (ExceptionDefn.Malformed_Decl
		      ("Site '" ^ port_name ^ "' is not declared",prt_pos))
	 in
	 let link_map,bond_list =
	   match lnk_state with
	   | (Ast.LNK_VALUE (i),(beg_pos,_)) ->
	      begin
		let pos = pos_of_lex_pos beg_pos in
		try
		  let opt_node = IntMap.find i link_map in
		  match opt_node with
		  | None -> raise (ExceptionDefn.Semantics_Error
				     (pos,"Edge identifier at site '" ^ port_name ^ "' is used multiple times"))
		  | Some (node_id',port_id',pos') ->
		     (IntMap.add i None link_map,(node_id,port_id,node_id',port_id')::bond_list)
		with Not_found -> (IntMap.add i (Some (node_id,port_id,pos)) link_map,bond_list)
	      end
	   | (Ast.FREE,_) -> (link_map,bond_list)
	   | _ -> raise (ExceptionDefn.Malformed_Decl
			   ("Site '" ^ port_name ^ "' is partially defined",
			    prt_pos))
	 in
	 match int_state_list with
	 | [] ->
	    build_intf ast' link_map (IntMap.add port_id (None,Mixture.WLD) intf) bond_list
	 | s::_ ->
	    let i =
	      try Signature.num_of_internal_state port_name (fst s) sign
	      with Not_found ->
		raise (ExceptionDefn.Malformed_Decl
			 ("Internal state of site'" ^ port_name ^ "' is not defined"
			 ,prt_pos))
	    in
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
	  | Some (_,_,pos) ->
	     raise (ExceptionDefn.Semantics_Error (pos,Printf.sprintf "Edge identifier %d is dangling" i))
	 ) link_map ;
       (node_map,env)
  in
  iter ast_mixture IntMap.empty 0 IntMap.empty env

let eval_agent env a ctxt =
  let ((ag_name, (pos_ag)),ast_intf) = a in
  let name_id =
    try Environment.num_of_name ag_name env with
    | Not_found ->
       raise (ExceptionDefn.Malformed_Decl
		("Agent '" ^ ag_name ^ "' is not declared",pos_ag))
  in
  let sign =
    try (Environment.get_sig name_id env)
    with Not_found ->
      raise (ExceptionDefn.Malformed_Decl
	       ("Agent '" ^ ag_name ^ "' is not declared",pos_ag))
  in
  let port_map = eval_intf ast_intf in
  let (interface, ctxt) =
    StringMap.fold
      (fun site_name (int_state_list, lnk_state, prt_pos) (interface, ctxt) ->
       let site_id =
	 try (Signature.num_of_site site_name sign)
	 with Not_found ->
	   let msg =
	     "Eval.eval_agent: site '" ^
	       (site_name^("' is not defined for agent '"^ag_name^"'"))
	   in
	   raise (ExceptionDefn.Malformed_Decl (msg,prt_pos))
       in
       let int_s =
	 match int_state_list with
	 | [] -> None
	 | h::_ ->
	    Environment.check ag_name (pos_of_lex_pos (fst pos_ag)) site_name
			      (pos_of_lex_pos (fst prt_pos)) (fst h) env;
	    let i = Environment.id_of_state ag_name site_name (fst h) env
	    in Some i
       in
       let interface,ctx =
	 let pos = pos_of_lex_pos (fst (snd lnk_state)) in
	 match fst lnk_state with
	 | Ast.LNK_VALUE n ->
	    let lnk =
	      (try Some (IntMap.find n ctxt.pairing)
	       with | Not_found -> None)
	    in
	    (match lnk with
	     | Some (Semi (b, j, _)) ->
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
		in raise (ExceptionDefn.Semantics_Error (pos, msg))
	     | None ->
		((IntMap.add site_id (int_s, Mixture.BND) interface),
		 {
		   (ctxt)
		 with
		   pairing =
		     IntMap.add n (Semi (ctxt.curr_id, site_id, pos))
				ctxt.pairing;
		})
	    )
	 | Ast.LNK_SOME ->
	    ((IntMap.add site_id (int_s, Mixture.BND) interface), ctxt)
	 | Ast.LNK_ANY ->
	    ((IntMap.add site_id (int_s, Mixture.WLD) interface), ctxt)
	 | Ast.FREE ->
	    ((IntMap.add site_id (int_s, Mixture.FREE) interface), ctxt)
	 | Ast.LNK_TYPE ((ste_nm, pos_ste), (ag_nm, pos_ag)) ->
	    (let site_num =
	       try Environment.id_of_site ag_nm ste_nm env
	       with
	       | Not_found ->
		  raise
		    (ExceptionDefn.Semantics_Error
		       (pos_ste, "binding type is not compatible with agent's signature"))
	     in
	     let ag_num =
	       try Environment.num_of_name ag_nm env with
	       | Not_found ->
		  raise
		    (ExceptionDefn.Semantics_Error
		       (pos_ste, "Illegal binding type, agent "^ag_nm^" is not delcared"))
	     in
	     ((IntMap.add site_id (int_s, Mixture.TYPE (site_num, ag_num)) interface),ctxt)
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
    | Ast.COMMA (a, ast_mix) ->
       let (ctxt, agent) = eval_agent env a ctxt in
       let id = ctxt.curr_id in let new_edges = ctxt.new_edges in
       let (ctxt, mixture, env) =
	 eval_mixture env ast_mix
		      { ctxt with
			curr_id = ctxt.curr_id + 1;
			new_edges = Int2Map.empty;
		      } mixture
       in (ctxt, (Mixture.compose id agent mixture new_edges), env)
    | Ast.EMPTY_MIX -> (ctxt, mixture, env)
  in
  let ctxt = { pairing = IntMap.empty; curr_id = 0; new_edges = Int2Map.empty; } in
  let (ctxt, mix, env) = eval_mixture env ast_mix ctxt (Mixture.empty mix_id)
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

(* returns partial evaluation of rate expression and a boolean that is set *)
(* to true if partial evaluation is a constant function                    *)
let rec partial_eval_alg_of_alg env (ast, (beg_pos,end_pos)) =
  let bin_op ast ast' op =
    let (f1, const1, opt_value1) = partial_eval_alg_of_alg env ast in
    let (f2, const2, opt_value2) = partial_eval_alg_of_alg env ast' in
    let part_eval inst values t e e_null cpu_t tk =
      (*evaluation partielle symbolique*)
      let v1 = f1 inst values t e e_null cpu_t tk in
      let v2 = f2 inst values t e e_null cpu_t tk in op v1 v2 in
    let opt_value' = match opt_value1,opt_value2 with
	(Some a,Some b) -> Some (op a b)
      | _ -> None in (*evaluation complete si connue*)
    (part_eval, (const1 && const2), opt_value')
  in
  let un_op ast op =
    let (f, const, opt_v) = partial_eval_alg_of_alg env ast in
    let opt_v' = match opt_v with Some a -> Some (op a) | None -> None in
    ((fun inst values t e e_null cpu_t tk ->
      let v = f inst values t e e_null cpu_t tk in op v),
     const, opt_v')
  in
  match ast with
  | Expr.CONST n -> ((fun _ _ _ _ _ _ _-> n), true, (Some n))
  | Expr.STATE_ALG_OP (Term.CPUTIME) ->
     ((fun _ _ _ _ _ cpu_t _-> Nbr.F (cpu_t -. !Parameter.cpuTime)), false,
      Some (Nbr.F 0.))
  | Expr.KAPPA_INSTANCE id -> ((fun f _ _ _ _ _ _-> f id), false, Some (Nbr.I 0))
  | Expr.ALG_VAR i ->
     let (_,_,c) = partial_eval_alg_of_alg
		     env (snd env.Environment.algs.NamedDecls.decls.(i))
     in ((fun _ v _ _ _ _ _-> v i),false,c)
  | Expr.TOKEN_ID i -> ((fun _ _ _ _ _ _ tk -> tk i),false,Some (Nbr.F 0.))
  | Expr.STATE_ALG_OP (Term.TIME_VAR) ->
     ((fun _ _ t _ _ _ _-> Nbr.F t), false, Some (Nbr.F 0.))
  | Expr.STATE_ALG_OP (Term.EVENT_VAR) ->
     ((fun _ _ _ e ne _ _-> Nbr.I (e+ne)), false, Some (Nbr.I 0))
  | Expr.STATE_ALG_OP (Term.NULL_EVENT_VAR) ->
     ((fun _ _ _ _ ne _ _-> Nbr.I ne), false, Some (Nbr.I 0))
  | Expr.STATE_ALG_OP (Term.PROD_EVENT_VAR) ->
     ((fun _ _ _ e _ _ _-> Nbr.I e), false,Some (Nbr.I 0))
  | Expr.BIN_ALG_OP (op,ast, ast') ->
     bin_op ast ast' (Nbr.of_bin_alg_op op)
  | Expr.UN_ALG_OP (op,ast) -> un_op ast (Nbr.of_un_alg_op op)

let rec partial_eval_alg env mixs (ast, (beg_pos,end_pos)) =
  let bin_op ast ast' op =
    let (env1, mix1, f1, const1, opt_value1) =
      partial_eval_alg env mixs ast in
    let (env2, mix2, f2, const2, opt_value2) =
      partial_eval_alg env1 mix1 ast' in
    let part_eval inst values t e e_null cpu_t tk =
      (*evaluation partielle symbolique*)
      let v1 = f1 inst values t e e_null cpu_t tk in
      let v2 = f2 inst values t e e_null cpu_t tk in op v1 v2 in
    let opt_value' = match opt_value1,opt_value2 with
        (Some a,Some b) -> Some (op a b)
      | _ -> None in (*evaluation complete si connue*)
    (env2, mix2, part_eval, (const1 && const2), opt_value')
  in
  let un_op ast op =
    let (env', mix', f, const, opt_v) =
      partial_eval_alg env mixs ast in
    let opt_v' = match opt_v with Some a -> Some (op a) | None -> None in
    (env', mix', (fun inst values t e e_null cpu_t tk ->
		  let v = f inst values t e e_null cpu_t tk in op v),
     const, opt_v')
  in
  match ast with
  | EMAX ->
     let v = Parameter.getMaxEventValue () in
     (env,mixs,(fun _ _ _ _ _ _ _-> v), true, Some v)
  | TMAX ->
     let v = Parameter.getMaxTimeValue () in
     (env,mixs,(fun _ _ _ _ _ _ _-> v), true, Some v)
  | CONST n ->
     (env,mixs,(fun _ _ _ _ _ _ _-> n), true, (Some n))
  | STATE_ALG_OP (Term.CPUTIME) ->
     (env,mixs,(fun _ _ _ _ _ cpu_t _-> Nbr.F (cpu_t -. !Parameter.cpuTime)), false,
      Some (Nbr.F 0.))
  | KAPPA_INSTANCE ast ->
     let (env', id) =
       Environment.declare_var_kappa None env in
     let mix,env'' = mixture_of_ast ~mix_id:id env' ast in
     (env'', mix::mixs, (fun f _ _ _ _ _ _-> f id), false, Some (Nbr.I 0))
  | OBS_VAR lab ->
     let i =
         try Environment.num_of_alg lab env with
         | Not_found ->
            raise (ExceptionDefn.Semantics_Error
                     (pos_of_lex_pos beg_pos,lab ^ " is not a declared variable"))
     in
     let (_,_,c) = partial_eval_alg_of_alg
		     env (snd env.Environment.algs.NamedDecls.decls.(i)) in
       (env,mixs,(fun _ v _ _ _ _ _-> v i),false,c)
  | TOKEN_ID (tk_nme) ->
     let i =
       try Environment.num_of_token tk_nme env
       with Not_found ->
         raise (ExceptionDefn.Semantics_Error
                  (pos_of_lex_pos beg_pos,tk_nme ^ " is not a declared token"))
     in
     (env,mixs,(fun _ _ _ _ _ _ tk -> tk i),false,Some (Nbr.F 0.))
  | STATE_ALG_OP (Term.TIME_VAR) ->
     (env,mixs,(fun _ _ t _ _ _ _-> Nbr.F t), false, Some (Nbr.F 0.))
  | STATE_ALG_OP (Term.EVENT_VAR) ->
     (env,mixs,(fun _ _ _ e ne _ _-> Nbr.I (e+ne)), false, Some (Nbr.I 0))
  | STATE_ALG_OP (Term.NULL_EVENT_VAR) ->
     (env,mixs,(fun _ _ _ _ ne _ _-> Nbr.I ne), false, Some (Nbr.I 0))
  | STATE_ALG_OP (Term.PROD_EVENT_VAR) ->
     (env,mixs,(fun _ _ _ e _ _ _-> Nbr.I e), false,Some (Nbr.I 0))
  | BIN_ALG_OP (op,ast, ast') ->
     bin_op ast ast' (Nbr.of_bin_alg_op op)
  | UN_ALG_OP (op,ast) -> un_op ast (Nbr.of_un_alg_op op)

let rec partial_eval_bool env mixs (ast,_) =
  let bin_op_bool ast ast' op =
    let (env1, mix1, f1, const1) = partial_eval_bool env mixs ast in
    let (env2, mix2, f2, const2) = partial_eval_bool env1 mix1 ast' in
    let part_eval inst values t e e_null cpu_t tk =
      let b1 = f1 inst values t e e_null cpu_t tk in
      let b2 = f2 inst values t e e_null cpu_t tk in op b1 b2
    in (env2,mix2,part_eval, (const1 && const2))
  in let bin_op_alg ast ast' op =
       let (env1,mix1,f1, const1, _) = partial_eval_alg env mixs ast in
       let (env2,mix2,f2, const2, _) = partial_eval_alg env1 mix1 ast' in
       let part_eval inst values t e e_null cpu_t tk =
	 let v1 = f1 inst values t e e_null cpu_t tk in
	 let v2 = f2 inst values t e e_null cpu_t tk in
	 op v1 v2
       in
       (env2,mix2,part_eval, (const1 && const2))
     in
     match ast with
     | TRUE ->
	(env,mixs,(fun _ _ _ _ _ _ _-> true), true)
     | FALSE -> (env,mixs,(fun _ _ _ _ _ _ _-> false), true)
     | BOOL_OP(Term.AND,ast, ast') -> bin_op_bool ast ast' (&&)
     | BOOL_OP(Term.OR,ast, ast') -> bin_op_bool ast ast' (||)
     | COMPARE_OP(op,ast, ast') -> bin_op_alg ast ast' (Nbr.of_compare_op op)

let reduce_val v env mixs =
  let (env', mixs', k, const, opt_v) = partial_eval_alg env mixs v in
  let (_mix',(alg,_pos)) =
    Expr.compile_alg env.Environment.algs.NamedDecls.finder
		     env.Environment.tokens.NamedDecls.finder
		     (env.Environment.fresh_kappa,[]) v in
  let dep = Expr.deps_of_alg_expr alg in
  (env',mixs',
   (if const then
      match opt_v with
      | Some v -> Primitives.CONST v
      | None -> invalid_arg "Eval.reduce_val: Variable is constant but was not evaluated"
    else (Primitives.VAR k)),
   dep)

let rule_of_ast ?(backwards=false) ~is_pert env mixs (ast_rule_label,ast_rule) =
  let ast_rule_label,ast_rule =
    if backwards then Ast.flip (ast_rule_label, ast_rule)
    else (ast_rule_label, ast_rule) in
  let (env, lhs_id) =
    Environment.declare_var_kappa ~from_rule:true ast_rule_label env in
  (* reserving an id for rule's lhs in the pattern table *)
  let env = Environment.declare_rule ast_rule_label lhs_id env in
  let env,mixs',k_def,dep = reduce_val ast_rule.k_def env mixs in
  let (env,mixs'',k_alt,radius,dep_alt) =
    match ast_rule.k_un with
    | None -> (env,mixs',None,None, Term.DepSet.empty)
    | Some (ast,ast_opt) ->
       (****TODO HERE treat ast_opt that specifies application radius****)
       let env =
	 Environment.declare_unary_rule
	   (Some (Environment.kappa_of_num lhs_id env,Tools.no_pos)) (*ast_rule_label*) lhs_id env in
       let env,mixs'',k_alt,dep = reduce_val ast env mixs' in
       let env,mixs''',radius_alt,dep = match ast_opt with
	   None -> (env,mixs'',None,dep)
	 | Some v -> let env4,mix4,rad,dep' = reduce_val v env mixs'' in
		     (env4,mix4,Some rad,Term.DepSet.union dep dep')
       in
       (env,mixs''',Some k_alt,radius_alt,dep)
  in
  let lhs,env = mixture_of_ast ~mix_id:lhs_id env ast_rule.lhs
  in
  let lhs = match k_alt with None -> lhs | Some _ -> Mixture.set_unary lhs in
  let rhs,env = mixture_of_ast env ast_rule.rhs in
  let (script, balance,added,modif_sites(*,side_effects*)) =
    Dynamics.diff ast_rule.rule_pos lhs rhs ast_rule_label env

  and kappa_lhs = Kappa_printer.mixture_to_string false env () lhs

  and kappa_rhs = Kappa_printer.mixture_to_string false env () rhs in
  let tokenify env mixs l =
    List.fold_right
      (fun (alg_expr,(nme,pos)) (env,mixs,out) ->
       let id =
	 try Environment.num_of_token nme env
	 with Not_found ->
	   raise (ExceptionDefn.Semantics_Error (pos,"Token "^nme^" is undefined"))
       in
       let (env', mixs', f, is_const,opt_v) = partial_eval_alg env mixs alg_expr
       (*dependencies are not important here since variable is evaluated only when rule is applied*)
       in
       let v =
	 if is_const then
	   (match opt_v with
	      Some v -> Primitives.CONST v
	    | None ->
	       invalid_arg "Eval.rule_of_ast: Variable is constant but was not evaluated")
	 else Primitives.VAR f
       in
       (env',mixs',(v,id)::out)
      ) l (env,mixs,[])
  in
  let env12,mixs12,add_token = tokenify env mixs'' ast_rule.add_token in
  let env21,mixs21,rm_token = tokenify env12 mixs12 ast_rule.rm_token in
  let r_id = Mixture.get_id lhs in
  let env = if Mixture.is_empty lhs
	    then Environment.declare_empty_lhs r_id env21
	    else env21 in
  let env =
    Term.DepSet.fold
      (*creating dependencies between variables in the kinetic rate and the activity of the rule*)
      (fun dep env ->
       (*Printf.printf "rule %d depends on %s\n" r_id (Mods.string_of_dep dep); *)
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
       | Primitives.DEL id ->
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
			let _,lnk =
			  IntMap.find site_id (Mixture.interface (Mixture.agent_of_id id lhs)) in
			match lnk with
			| Mixture.BND | Mixture.TYPE _ ->
				      IntSet.add site_id set (*semi link deletion*)
			| _ -> set
		      with
		      | Not_found ->
			 IntSet.add site_id set (*deletion of site that was not mentionned so possible side_effect*)
		    end
		 | Some _ ->
		    IntSet.add site_id set (*link deletion because deleted agent was connected in the lhs*)
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
  (env,mixs21,
   {
     Primitives.add_token = add_token ;
     Primitives.rm_token = rm_token ;
     Primitives.k_def = k_def ;
     Primitives.k_alt = (k_alt,radius) ;
     Primitives.over_sampling = None;
     Primitives.script = script;
     Primitives.kappa = kappa_lhs ^ ("->" ^ kappa_rhs);
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

let variables_of_result env (free_id,mixs) alg_a =
  let (env',compiled_mixs,final_id) =
    List.fold_left (fun (env,mixs,id) (lbl,ast) ->
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
		    (env',mix::mixs,pred id)) (env,[],pred free_id) mixs
  in
  let () = assert (final_id = -1) in
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
  in (env'',compiled_mixs)

let rules_of_result env mixs res tolerate_new_state =
  let (env, mixs, l) =
    List.fold_left
      (fun (env, mixs, cont) (ast_rule_label, ast_rule) ->
       let (env, mixs, r) =
	 rule_of_ast ~is_pert:false env mixs (ast_rule_label, ast_rule)
       in
       match ast_rule.Ast.k_op with
       | None -> (env,mixs,r::cont)
       | Some k ->
	  let (env,mixs,back_r) =
	    rule_of_ast ~backwards:true ~is_pert:false
			env mixs (ast_rule_label, ast_rule) in
	  (env,mixs,back_r::(r::cont))
      )
      (env, mixs, []) res.Ast.rules
  in (env, mixs, (List.rev l))

let obs_of_result env mixs res =
  List.fold_left
    (fun (env,mix,cont) alg_expr ->
     let (env',mixs',f, const, opt_v) = partial_eval_alg env mix alg_expr in
     let (_mix',(alg,_pos)) =
       Expr.compile_alg env.Environment.algs.NamedDecls.finder
			env.Environment.tokens.NamedDecls.finder
			(env.Environment.fresh_kappa,[]) alg_expr in
     let dep = Expr.deps_of_alg_expr alg in
     env',mixs',(f,const,opt_v,dep,Expr.ast_alg_to_string () (fst alg_expr)) :: cont
    )
    (env,mixs,[]) res.observables

let effects_of_modif variables env ast_list =
  let rec iter variables effects str_pert env ast_list =
    match ast_list with
    | [] -> (variables,effects,str_pert,env)
    | ast::tl ->
       let (variables,effects,str_pert,env) =
	 match ast with
	 | INTRO (alg_expr, ast_mix, pos) ->
	    let (env', mixs',x, is_constant, opt_v) =
	      partial_eval_alg env variables alg_expr in
	    let ast_rule =
	      { rule_pos = Tools.no_pos; add_token=[]; rm_token=[];
		lhs = Ast.EMPTY_MIX; arrow = Ast.RAR(Tools.no_pos);
		rhs = ast_mix;
		k_def=Term.with_dummy_pos (Ast.CONST(Nbr.F 0.0));
		k_un=None;k_op=None;
	      } in
	    let nme_pert =
	      Printf.sprintf "pert_%d" (Environment.next_pert_id env) in
	    let env,mixs'',rule =
	      rule_of_ast ~is_pert:true env mixs'
			  (Some (Term.with_dummy_pos nme_pert),ast_rule) in
	    let v =
	      if is_constant
	      then (match opt_v with Some v -> Primitives.CONST v
				   | None -> invalid_arg "Eval.effects_of_modif")
	      else Primitives.VAR x
	    in
	    let str =
	      (Printf.sprintf "introduce %a * %a"
			      Expr.ast_alg_to_string (fst alg_expr)
			      (Kappa_printer.mixture_to_string false env)
			      rule.Primitives.rhs)::str_pert
	    in (mixs'', (Primitives.ITER_RULE (v, rule))::effects, str, env)
	 | DELETE (alg_expr, ast_mix, pos) ->
	    let (env',mixs',x, is_constant, opt_v) =
	      partial_eval_alg env variables alg_expr in
	    let ast_rule =
	      { rule_pos = Tools.no_pos; add_token=[]; rm_token=[];
		lhs = ast_mix; arrow = Ast.RAR(Tools.no_pos);
		rhs = Ast.EMPTY_MIX;
		k_def=Term.with_dummy_pos (Ast.CONST(Nbr.F 0.0));
		k_un=None;k_op=None;
	      } in
	    let nme_pert =
	      Printf.sprintf "pert_%d" (Environment.next_pert_id env) in
	    let env,mixs'',rule =
	      rule_of_ast ~is_pert:true env mixs'
			  (Some (Term.with_dummy_pos nme_pert),ast_rule) in
	    let v =
	      if is_constant
	      then (match opt_v with Some v -> Primitives.CONST v
				   | None -> invalid_arg "Eval.effects_of_modif")
	      else Primitives.VAR x
	    in
	    let str =
	      (Printf.sprintf "remove %a * %a"
			      Expr.ast_alg_to_string (fst alg_expr)
			      (Kappa_printer.mixture_to_string false env)
			      rule.Primitives.lhs)::str_pert
	    in (mixs'', (Primitives.ITER_RULE (v, rule))::effects, str, env)
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
	    let (env', mixs', x, is_constant, opt_v) =
	      partial_eval_alg env variables alg_expr in
	    let v =
	      if is_constant
	      then (match opt_v with Some v -> Primitives.CONST v
				   | None -> invalid_arg "Eval.effects_of_modif")
	      else Primitives.VAR x
	    in
	    let str =
	      (if is_rule then
		 Printf.sprintf "set rate of rule '%s' to %a"
				(Environment.rule_of_num i env)
	       else
		 Printf.sprintf "set variable '%s' to %a"
				(fst (Environment.alg_of_num i env)))
		Expr.ast_alg_to_string (fst alg_expr)::str_pert
	    in
	    (mixs',
	     (if is_rule then Primitives.UPDATE_RULE (i, v)
	      else Primitives.UPDATE_VAR (i, v))::effects, str, env')
	 | UPDATE_TOK ((tk_nme,tk_pos),alg_expr) ->
	    let tk_id =
	      try Environment.num_of_token tk_nme env with
	      | Not_found ->
		 raise (ExceptionDefn.Semantics_Error
			  (tk_pos,"Token " ^ (tk_nme ^ " is not defined")))
	    in
	    let (env', mixs', x, is_constant, opt_v) =
	      partial_eval_alg env variables alg_expr in
	    let v =
	      if is_constant
	      then (match opt_v with Some v -> Primitives.CONST v
				   | None -> invalid_arg "Eval.effects_of_modif")
	      else Primitives.VAR x
	    in
	    let str = (Printf.sprintf "set token '%s' to value %a" tk_nme
				      Expr.ast_alg_to_string (fst alg_expr))::str_pert
	    in
	    (mixs', (Primitives.UPDATE_TOK (tk_id, v))::effects, str, env')
	 | SNAPSHOT (pexpr,pos) ->
	    (*when specializing snapshots to particular mixtures, add variables below*)
	    let str = ("snapshot state")::str_pert in
	    (variables, (Primitives.SNAPSHOT pexpr)::effects, str, env)
	 | STOP (pexpr,pos) ->
	    let str = "interrupt simulation"::str_pert in
	    (variables, (Primitives.STOP pexpr)::effects, str, env)
	 | CFLOW ((lab,pos_lab),pos_pert) ->
	    let id =
	      try Environment.num_of_rule lab env
	      with Not_found ->
		try let var = Environment.num_of_alg lab env in
		    match env.Environment.algs.NamedDecls.decls.(var) with
		    |(_,(Expr.KAPPA_INSTANCE i,_)) -> i
		    | _ -> raise Not_found
		with Not_found ->
		  raise	(ExceptionDefn.Malformed_Decl
			   ("Label '" ^ lab ^ "' is neither a rule nor a Kappa expression"
			   ,pos_lab))
	    in
	    let str = (Printf.sprintf "Enable causality analysis for observable '%s'" lab)::str_pert in
	    (variables, (Primitives.CFLOW id)::effects, str, env)
	 | CFLOWOFF ((lab,pos_lab),pos_pert) ->
	    let id =
	      try Environment.num_of_rule lab env
	      with Not_found ->
		try let var = Environment.num_of_alg lab env in
		    match env.Environment.algs.NamedDecls.decls.(var) with
		    |(_,(Expr.KAPPA_INSTANCE i,_)) -> i
		    | _ -> raise Not_found
		with Not_found ->
		  raise	(ExceptionDefn.Malformed_Decl
			   ("Label '" ^ lab ^ "' is neither a rule nor a Kappa expression"
			   ,pos_lab))
	    in
	    let str = (Printf.sprintf "Disable causality analysis for observable '%s'" lab)::str_pert in
	    (variables, (Primitives.CFLOWOFF id)::effects, str, env)
	 | FLUX (pexpr,pos) ->
	    let str = "Activate flux tracking"::str_pert in
	    (variables,(Primitives.FLUX pexpr)::effects, str, env)
	 | FLUXOFF (pexpr,pos) ->
	    let str = "Disable flux tracking"::str_pert in
	    (variables,(Primitives.FLUXOFF pexpr)::effects, str, env)
	 | PRINT (pexpr,print,pos) ->
	    let str_l =
	      List.fold_left
		(fun cont (pexpr,pos) ->
		 match pexpr with
		 | Ast.Str_pexpr str -> str::cont
		 | Ast.Alg_pexpr alg -> Expr.ast_alg_to_string () alg::cont
		) [] print
	    in
	    let str = (Printf.sprintf "Print %s" (Tools.string_of_list (fun i->i) str_l))::str_pert
	    in
	    (variables,(Primitives.PRINT (pexpr,print))::effects, str, env)
       in
       iter variables effects str_pert env tl
  in
  iter variables [] [] env ast_list

let pert_of_result variables env res =
  let (variables, lpert, lrules, env) =
    List.fold_left
      (fun (variables, lpert, lrules, env)
	   ((pre_expr, modif_expr_list, opt_post),pos) ->
       let bpos = pos_of_lex_pos (fst pos) in
       let (env', variables', x, is_constant) =
	 partial_eval_bool env variables pre_expr in
       let (_mix',(pre,_pos)) =
	 Expr.compile_bool env.Environment.algs.NamedDecls.finder
			   env.Environment.tokens.NamedDecls.finder
			   (env.Environment.fresh_kappa,[]) pre_expr in
       let (dep, stopping_time) = try Expr.deps_of_bool_expr pre
		 with ExceptionDefn.Unsatisfiable ->
		   raise
		     (ExceptionDefn.Semantics_Error
			(bpos,"Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"))
       in
       let (variables, effects, str_eff, env) =
	 effects_of_modif variables' env' modif_expr_list in
       let str_eff = String.concat ";" (List.rev str_eff) in
       let bv =
	 if is_constant
	 then Primitives.CONST (close_var x)
	 else Primitives.VAR x in
       let str_pert,opt_abort =
	 match opt_post with
	 | None ->
	    (Printf.sprintf "whenever %a, %s"
			    Expr.ast_bool_to_string (fst pre_expr) str_eff,None)
	 | Some post_expr ->
	    let (env', variables', x, is_constant) =
	      partial_eval_bool env variables post_expr in
	    let (_mix',(post,_pos)) =
	      Expr.compile_bool env.Environment.algs.NamedDecls.finder
				env.Environment.tokens.NamedDecls.finder
				(env.Environment.fresh_kappa,[]) post_expr in
	    let (dep,stopping_time) = try Expr.deps_of_bool_expr post with
			       ExceptionDefn.Unsatisfiable ->
			       raise
				 (ExceptionDefn.Semantics_Error
				    (bpos,"Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"))
	    in
	    let bv =
	      if is_constant then Primitives.CONST (close_var x)
	      else Primitives.VAR x
	    in
	    (Printf.sprintf "whenever %a, %s until %a"
			    Expr.ast_bool_to_string (fst pre_expr) str_eff
			    Expr.ast_bool_to_string (fst post_expr),
	     Some (bv,dep))
       in
       let env,p_id = Environment.declare_pert (str_pert,pos) env' in
       let has_tracking,lrules'=
	 List.fold_left
	   (fun (track,lr) -> function
			     | Primitives.ITER_RULE (_,r) -> (track,r::lr)
			     | Primitives.CFLOW _ -> (true,lr)
			     | _ -> (track,lr))
	   (env.Environment.tracking_enabled,lrules) effects in
       let env =
	 Term.DepSet.fold
	   (fun dep env -> Environment.add_dependencies dep (Term.PERT p_id) env
	   )
	   dep
	   { env with Environment.tracking_enabled = has_tracking } in
       (*let env = List.fold_left (fun env (r_opt,effect) -> Environment.bind_pert_rule p_id r.r_id env) env effect_list in *)
       let opt,env =
	 match opt_abort with
	 | None -> (None,env)
	 | Some (bv,dep) ->
	    let env =
	      Term.DepSet.fold
		(fun dep_type env ->
		 Environment.add_dependencies dep_type (Term.ABORT p_id) env
		)
		dep env
	    in
	    (Some bv,env)
       in
       let pert =
	 { Primitives.precondition = bv;
	   Primitives.effect = effects;
	   Primitives.abort = opt;
	   Primitives.flag = str_pert;
	   Primitives.stopping_time = stopping_time
	 }
       in
       (variables', pert::lpert, lrules', env)
      )
      (variables, [], [], env) res.perturbations
  in
  (*making sure that perturbations containing a stopping time precondition are tested first*)
  let lpert = List.rev lpert in
  let pred = (fun p -> match p.Primitives.stopping_time with
			 None -> false | Some _ -> true) in
  let lpert_stopping_time = List.filter pred lpert in
  let lpert_ineq = List.filter (fun p -> not (pred p)) lpert in
  let lpert = lpert_stopping_time@lpert_ineq in
  (variables, lpert, (List.rev lrules), env)

let init_graph_of_result env res =
  let n = Array.length env.Environment.tokens.NamedDecls.decls in
  let token_vector = Array.init n (fun i -> 0.) in
  let sg,env =
    List.fold_left
      (fun (sg,env) (opt_vol,init_t,pos) -> (*TODO dealing with volumes*)
       match init_t with
       | INIT_MIX (alg, ast) ->
	  let (_,alg') =
	    Expr.compile_alg env.Environment.algs.NamedDecls.finder
			     env.Environment.tokens.NamedDecls.finder (0,[]) alg
	  in
	  let value =
	    match partial_eval_alg_of_alg env alg' with
	    | (_, _, Some v) -> v
	    | _ -> raise
		     (ExceptionDefn.Semantics_Error
			(pos,
			 Printf.sprintf "%a is not a constant, cannot initialize graph."
					Expr.ast_alg_to_string (fst alg)))
	  in
	  let cpt = ref 0 in
	  let sg = ref sg in
	  let env = ref env in
	  let n = match !Parameter.rescale with
	    | None -> Nbr.to_int value
	    | Some i -> min i (Nbr.to_int value)
	  in
	  (* Cannot do Mixture.to_nodes env m once for all because of *)
	  (* references                                               *)
	  while !cpt < n do
	    let nodes,env' = nodes_of_ast !env ast in
	    env := env' ;
	    sg := Graph.SiteGraph.add_nodes !sg nodes;
	    cpt := !cpt + 1
	  done;
	  (!sg,!env)
       | INIT_TOK (alg, (tk_nme,pos_tk)) ->
	  let (_,alg') =
	    Expr.compile_alg env.Environment.algs.NamedDecls.finder
			     env.Environment.tokens.NamedDecls.finder (0,[]) alg
	  in
	  let value =
	    match partial_eval_alg_of_alg env alg' with
	    | (_, _, Some v) -> Nbr.to_float v
	    | _ -> raise
		     (ExceptionDefn.Semantics_Error
			(pos_tk,
			 Printf.sprintf "%a is not a constant, cannot initialize token value."
					Expr.ast_alg_to_string (fst alg)))
	  in
	  let tok_id =
	    try Environment.num_of_token tk_nme env
	    with Not_found ->
	      raise (ExceptionDefn.Semantics_Error
		       (pos_tk, Printf.sprintf "token %s is undeclared" tk_nme))
	  in
	  token_vector.(tok_id) <- value;
	  (sg,env)
      )	(Graph.SiteGraph.init !Parameter.defaultGraphSize,env) res.Ast.init
  in
  (sg,token_vector,env)

let configurations_of_result result =
  let set_value pos_p param value_list f ass =
    match value_list with
    | (v,pos) :: _ -> ass := f (v,pos)
    | [] -> ExceptionDefn.warning
	      ~pos:pos_p
	      (fun f -> Printf.fprintf f "Empty value for parameter %s" param)
  in
  List.iter
    (fun ((param,pos_p),value_list) ->
     match param with
     | "displayCompression" ->
	begin
	  let rec parse l =
	    match l with
	    | ("strong",pos_v)::tl ->
	       (Parameter.strongCompression := true ; parse tl)
	    | ("weak",_)::tl -> (Parameter.weakCompression := true ; parse tl)
	    | ("none",_)::tl -> (Parameter.mazCompression := true ; parse tl)
	    | [] -> ()
	    | (error,pos)::tl ->
	       raise (ExceptionDefn.Semantics_Error
			(pos,Printf.sprintf "Unkown value %s for compression mode" error))
	  in
	  parse value_list
	end
     | "cflowFileName"	->
	set_value pos_p param value_list fst Parameter.cflowFileName
     | "progressBarSize" ->
	set_value pos_p param value_list
		  (fun (v,p) ->
		   try int_of_string v
		   with _ ->
		     raise (ExceptionDefn.Semantics_Error
			      (p,Printf.sprintf "Value %s should be an integer" v))
		  ) Parameter.progressBarSize

     | "progressBarSymbol" ->
	set_value pos_p param value_list
		  (fun (v,p) ->
		   try
		     String.unsafe_get v 0
		   with _ ->
		     raise (ExceptionDefn.Semantics_Error
			      (p,Printf.sprintf "Value %s should be a character" v))
		  ) Parameter.progressBarSymbol

     | "dumpIfDeadlocked" ->
	set_value pos_p param value_list
		  (fun (value,pos_v) ->
		   match value with
		   | "true" | "yes" -> true
		   | "false" | "no" -> false
		   | _ as error ->
		      raise (ExceptionDefn.Semantics_Error
			       (pos_v,Printf.sprintf "Value %s should be either \"yes\" or \"no\"" error))
		  ) Parameter.dumpIfDeadlocked
     | "plotSepChar" ->
	set_value pos_p param value_list
		  (fun (v,p) ->
		   try
		     String.unsafe_get v 0
		   with _ ->
		     raise (ExceptionDefn.Semantics_Error
			      (p,Printf.sprintf "Value %s should be a character" v))
		  ) Parameter.plotSepChar
     | "maxConsecutiveClash" ->
	set_value pos_p param value_list
		  (fun (v,p) ->
		   try int_of_string v
		   with _ ->
		     raise (ExceptionDefn.Semantics_Error
			      (p,Printf.sprintf "Value %s should be an integer" v))
		  ) Parameter.maxConsecutiveClash

     | "dotSnapshots" ->
	set_value pos_p param value_list
		  (fun (value,pos_v) ->
		   match value with
		   | "true" | "yes" -> true
		   | "false" | "no" -> false
		   | _ as error ->
		      raise (ExceptionDefn.Semantics_Error
			       (pos_v,Printf.sprintf "Value %s should be either \"yes\" or \"no\"" error))
		  ) Parameter.dotOutput
     | "colorDot" ->
	set_value pos_p param value_list
		  (fun (value,pos_v) ->
		   match value with
		   | "true" | "yes" -> true
		   | "false" | "no" -> false
		   | _ as error ->
		      raise (ExceptionDefn.Semantics_Error
			       (pos_v,Printf.sprintf "Value %s should be either \"yes\" or \"no\"" error))
		  ) Parameter.useColor
     | "dumpInfluenceMap" ->
	set_value pos_p param value_list
		  (fun (v,p) ->
		   match v with
		   | "true" | "yes" ->
			       if !Parameter.influenceFileName = ""
			       then "im.dot" else !Parameter.influenceFileName
		   | "false" | "no" -> ""
		   | _ as error ->
		      raise (ExceptionDefn.Semantics_Error
			       (p,Printf.sprintf "Value %s should be either \"yes\" or \"no\"" error))
		  ) Parameter.influenceFileName
     | "influenceMapFileName" ->
	set_value pos_p param value_list fst Parameter.influenceFileName
     | "showIntroEvents" ->
	set_value pos_p param value_list
		  (fun (v,p) -> match v with
				| "true" | "yes" -> true
				| "false" | "no" -> false
				| _ as error ->
				   raise (ExceptionDefn.Semantics_Error
					    (p,Printf.sprintf "Value %s should be either \"yes\" or \"no\"" error))
		  )
		  Parameter.showIntroEvents
     | _ as error ->
	raise (ExceptionDefn.Semantics_Error
		 (pos_of_lex_pos (fst pos_p),Printf.sprintf "Unkown parameter %s" error))
    ) result.configurations

let initialize result counter =
  Debug.tag "+ Compiling..." ;
  Debug.tag "\t -simulation parameters" ;
  let _ = configurations_of_result result in

  Debug.tag "\t -agent signatures" ;
  let sigs_nd =
    NamedDecls.create (array_map_of_list
			 (fun (name,intf) -> (name,interface_for_def intf))
			 result.Ast.signatures) in
  let tk_nd =
    NamedDecls.create (array_map_of_list (fun x -> (x,())) result.Ast.tokens) in

  Debug.tag "\t -variable declarations";
  let vars_nd = NamedDecls.create (Array.of_list result.Ast.variables) in
  let (fresh_kappa,_ as mixs),alg_a =
    array_fold_left_mapi (fun i mixs ((label,_ as lbl_pos),ast) ->
			  let (mixs',alg) =
			    Expr.compile_alg ~label vars_nd.NamedDecls.finder
					     tk_nd.NamedDecls.finder mixs
					     ~max_allowed_var:(pred i) ast
			  in (mixs',(lbl_pos,alg))) (0,[])
			 vars_nd.NamedDecls.decls
  in
  let env =
    Environment.init sigs_nd tk_nd (NamedDecls.create alg_a) fresh_kappa in

  let (env, kappa_vars) = variables_of_result env mixs alg_a in

  Debug.tag "\t -initial conditions";
  let sg,token_vector,env = init_graph_of_result env result in

  let tolerate_new_state = !Parameter.implicitSignature in
  Parameter.implicitSignature := false ;

  Debug.tag "\t -rules";
  let (env, kappa_vars, rules) =
    rules_of_result env kappa_vars result tolerate_new_state in

  Debug.tag "\t -observables";
  let env,kappa_vars,observables = obs_of_result env kappa_vars result in
  Debug.tag "\t -perturbations" ;
  let (kappa_vars, pert, rules_pert, env) =
    pert_of_result kappa_vars env result in
  Debug.tag "\t Done";
  Debug.tag "+ Analyzing non local patterns..." ;
  let env = Environment.init_roots_of_nl_rules env in
  Debug.tag "+ Building initial simulation state...";
  Debug.tag "\t -Counting initial local patterns..." ;
  let (state, env) =
    State.initialize sg token_vector rules kappa_vars
		     observables (pert,rules_pert) counter env
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
