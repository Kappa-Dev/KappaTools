open Mods
open Tools
open Ast

let initial_value_alg counter algs (ast, _) =
  Expr_interpreter.value_alg
    counter
    ~get_alg:(fun i ->
	      fst (snd algs.NamedDecls.decls.(i)))
    ~get_mix:(fun _ -> Nbr.zero) ~get_tok:(fun _ -> Nbr.zero) ast

let tokenify contact_map counter domain l =
  List.fold_right
    (fun (alg_expr,id) (domain,out) ->
     let (domain',(alg,_pos)) =
       Expr.compile_alg contact_map counter domain alg_expr in
     (domain',(alg,id)::out)
    ) l (domain,[])

let rules_of_ast ?deps_machinery contact_map counter domain ~syntax_ref (rule,_) =
  let domain',rm_toks =
    tokenify contact_map counter domain rule.LKappa.r_rm_tokens in
  let domain'',add_toks =
    tokenify contact_map counter domain' rule.LKappa.r_add_tokens in
  (*  let one_side syntax_ref label (domain,deps_machinery,unary_ccs,acc)
	       rate unary_rate lhs rhs rm add =*)
  let origin,deps =
    match deps_machinery with
    | None -> None,None
    | Some (o,d) -> Some o, Some d in
  let (crate,_ as crp) = Expr.compile_pure_alg counter rule.LKappa.r_rate in
  let unary_infos =
    match rule.LKappa.r_un_rate with
    | None -> fun _ uncc -> crate,None,uncc
    | Some (_,pos as rate) ->
       let (unrate,_) = Expr.compile_pure_alg counter rate in
       fun ccs uncc ->
       match Array.length ccs with
       | (0 | 1) -> unrate,None,uncc
       | 2 ->
	  crate,Some unrate,
	  Connected_component.Set.add
	    ccs.(0) (Connected_component.Set.add ccs.(1) uncc)
       | n ->
	  raise (ExceptionDefn.Malformed_Decl
		   ("Unary rule does not deal with "^
		      string_of_int n^" connected components.",pos)) in
  let build deps un_ccs (origin,ccs,syntax,(neg,pos)) =
    let rate,unrate,un_ccs' = unary_infos ccs un_ccs in
    Tools.option_map
      (fun x ->
       let origin =
	 match origin with Some o -> o | None -> failwith "ugly Eval.rule_of_ast" in
       Alg_expr.add_dep x origin crp)
      deps,un_ccs',{
	Primitives.unary_rate = unrate;
	Primitives.rate = rate;
	Primitives.rate_absolute = rule.LKappa.r_rate_absolute;
	Primitives.connected_components = ccs;
	Primitives.removed = neg;
	Primitives.inserted = pos;
	Primitives.consumed_tokens = rm_toks;
	Primitives.injected_tokens = add_toks;
	Primitives.syntactic_rule = syntax_ref;
	Primitives.instantiations = syntax;
      } in
  let (domain',origin'),rule_mixtures =
    Snip.connected_components_sum_of_ambiguous_rule
      contact_map domain'' ?origin rule.LKappa.r_mix rule.LKappa.r_created in
  let deps_algs',unary_ccs',rules_l =
    match rule_mixtures with
    | [] -> deps,Connected_component.Set.empty,[]
    | [ r ] ->
       let deps_algs',un_ccs',r' =
	 build deps Connected_component.Set.empty r in
       deps_algs', un_ccs',[r']
    | _ ->
       List.fold_right
	 (fun r (deps_algs,un_ccs,out) ->
	  let deps_algs',un_ccs',r' = build deps_algs un_ccs r in
	  deps_algs',un_ccs',r'::out)
	 rule_mixtures (deps,Connected_component.Set.empty,[]) in
  domain',(match origin' with
	   | None -> None
	   | Some o -> Some (o,
			     match deps_algs' with
			     | Some d -> d
			     | None -> failwith "ugly Eval.rule_of_ast")),
  unary_ccs',rules_l

let obs_of_result contact_map counter domain res =
  List.fold_left
    (fun (domain,cont) alg_expr ->
     let (domain',alg_pos) =
       Expr.compile_alg contact_map counter domain alg_expr in
     domain',alg_pos :: cont)
    (domain,[]) res.observables

let compile_print_expr contact_map counter domain ex =
  List.fold_right
    (fun el (domain,out) ->
     match el with
     | Ast.Str_pexpr s -> (domain,Ast.Str_pexpr s::out)
     | Ast.Alg_pexpr ast_alg ->
	let (domain', alg) =
	  Expr.compile_alg contact_map counter domain ast_alg in
	(domain',(Ast.Alg_pexpr alg::out)))
    ex (domain,[])

let cflows_of_label contact_map domain on algs rules (label,pos) rev_effects =
  let adds tests l x =
    if on then Primitives.CFLOW (Some label,x,tests) :: l
    else Primitives.CFLOWOFF x :: l in
  let mix =
    try
      let (_,(rule,_)) =
	List.find (function None,_ -> false | Some (l,_),_ -> l=label) rules in
      LKappa.to_maintained rule.LKappa.r_mix
  with Not_found ->
    try let (_,(var,_)) = List.find (fun ((l,_),_) -> l = label) algs in
	match var with
	| Ast.KAPPA_INSTANCE mix -> mix
	| (Ast.BIN_ALG_OP _ | Ast.UN_ALG_OP _ | Ast.STATE_ALG_OP _ |
	   Ast.OBS_VAR _ | Ast.TOKEN_ID _ | Ast.CONST _ | Ast.TMAX | Ast.EMAX |
	   Ast.PLOTNUM ) -> raise Not_found
    with Not_found ->
      raise (ExceptionDefn.Malformed_Decl
	       ("Label '" ^ label ^
		  "' does not refer to a non ambiguous Kappa expression"
	       ,pos)) in
  let domain',ccs =
    Snip.connected_components_sum_of_ambiguous_mixture
      contact_map domain ~origin:(Operator.PERT(-1)) mix in
  (domain',
   List.fold_left (fun x (y,t) -> adds t x y) rev_effects ccs)

let effects_of_modif
      algs ast_algs ast_rules contact_map counter domain ast_list =
  let rec iter rev_effects domain ast_list =
    let rule_effect alg_expr (mix,created,rm,add) mix_pos =
      let ast_rule =
	{ LKappa.r_mix = mix; LKappa.r_created = created;
	  LKappa.r_rm_tokens = rm; LKappa.r_add_tokens = add;
	  LKappa.r_rate = Location.dummy_annot (CONST Nbr.zero);
	  LKappa.r_rate_absolute = false; LKappa.r_un_rate = None; } in
      let (domain',alg_pos) =
	Expr.compile_alg contact_map counter domain alg_expr in
      let domain'',_,_,elem_rules =
	rules_of_ast
	  contact_map counter domain' ~syntax_ref:0 (ast_rule,mix_pos) in
      let elem_rule = match elem_rules with
	| [ r ] -> r
	| _ ->
	   raise
	     (ExceptionDefn.Malformed_Decl
		("Ambiguous rule in perturbation is impossible",mix_pos)) in
      (domain'',
       (Primitives.ITER_RULE (alg_pos, elem_rule))::rev_effects) in
    match ast_list with
    | [] -> (domain,List.rev rev_effects)
    | ast::tl ->
       let (domain,rev_effects) =
	 match ast with
	 | INTRO (alg_expr, (ast_mix,mix_pos)) ->
	    rule_effect
	      alg_expr ([],LKappa.to_raw_mixture
			     (Connected_component.Env.sigs domain) ast_mix,
			[],[]) mix_pos
	 | DELETE (alg_expr, (ast_mix, mix_pos)) ->
	    rule_effect
	      alg_expr (LKappa.to_erased ast_mix,[],[],[]) mix_pos
	 | UPDATE ((nme, pos_rule), alg_expr) ->
	    begin
	      match StringMap.find_option nme algs.NamedDecls.finder with
	      | Some i ->
		 let (domain', alg_pos) =
		   Expr.compile_alg contact_map counter domain alg_expr in
		 (domain',(Primitives.UPDATE (i, alg_pos))::rev_effects)
	      | None ->
		 raise (ExceptionDefn.Malformed_Decl
			  ("Variable " ^ (nme ^ " is not a constant")
			  ,pos_rule))
	    end
	 | UPDATE_TOK ((tk_id,tk_pos),alg_expr) ->
	    rule_effect (Location.dummy_annot (Ast.CONST (Nbr.one)))
			([],[],
			 [Location.dummy_annot (Ast.TOKEN_ID tk_id), tk_id],
			 [(alg_expr, tk_id)])
			tk_pos
	 | SNAPSHOT pexpr ->
	    let (domain',pexpr') =
	      compile_print_expr contact_map counter domain pexpr in
	    (*when specializing snapshots to particular mixtures, add variables below*)
	    (domain', (Primitives.SNAPSHOT pexpr')::rev_effects)
	 | STOP pexpr ->
	    let (domain',pexpr') =
	      compile_print_expr contact_map counter domain pexpr in
	    (domain', (Primitives.STOP pexpr')::rev_effects)
	 | CFLOWLABEL (on,lab) ->
	    cflows_of_label
	      contact_map domain on ast_algs ast_rules lab rev_effects
	 | CFLOWMIX (on,(ast,_)) ->
	    let adds tests l x =
	      if on then Primitives.CFLOW (None,x,tests) :: l
	      else Primitives.CFLOWOFF x :: l in
	    let domain',ccs =
	      Snip.connected_components_sum_of_ambiguous_mixture
		contact_map domain ~origin:(Operator.PERT(-1)) ast in
	    (domain',
	     List.fold_left (fun x (y,t) -> adds t x y) rev_effects ccs)
	 | FLUX pexpr ->
	    let (domain',pexpr') =
	      compile_print_expr contact_map counter domain pexpr in
	    (domain', (Primitives.FLUX pexpr')::rev_effects)
	 | FLUXOFF pexpr ->
	    let (domain',pexpr') =
	      compile_print_expr contact_map counter domain pexpr in
	    (domain', (Primitives.FLUXOFF pexpr')::rev_effects)
	 | PRINT (pexpr,print) ->
	    let (domain',pexpr') =
	      compile_print_expr contact_map counter domain pexpr in
	    let (domain'',print') =
	      compile_print_expr contact_map counter domain' print in
	    (domain'', (Primitives.PRINT (pexpr',print'))::rev_effects)
	 | PLOTENTRY ->
	    (domain, (Primitives.PLOTENTRY)::rev_effects)
       in
       iter rev_effects domain tl
  in
  iter [] domain ast_list

let pert_of_result
      algs algs_deps ast_algs ast_rules contact_map counter domain res =
  let (domain, _, lpert, stop_times,tracking_enabled) =
    List.fold_left
      (fun (domain, p_id, lpert, stop_times, tracking_enabled)
	   ((pre_expr, modif_expr_list, opt_post),pos) ->
       let (domain',(pre,pos_pre)) =
	 Expr.compile_bool contact_map counter domain pre_expr in
       let stopping_time =
	 try Expr.stops_of_bool_expr algs_deps pre
	 with ExceptionDefn.Unsatisfiable ->
	   raise
	     (ExceptionDefn.Malformed_Decl
		("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
		,pos_pre))
       in
       let (domain, effects) =
	 effects_of_modif algs ast_algs ast_rules
			  contact_map counter domain' modif_expr_list in
       let domain,opt,stopping_time =
	 match opt_post with
	 | None -> (domain,None,stopping_time)
	 | Some post_expr ->
	    let (domain',(post,_pos)) =
	      Expr.compile_bool contact_map counter domain post_expr in
	    let (stopping_time') =
	      try Expr.stops_of_bool_expr algs_deps post with
		ExceptionDefn.Unsatisfiable ->
		raise
		  (ExceptionDefn.Malformed_Decl
		     ("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
		     ,pos))
	    in
	    (domain',Some post,stopping_time'@stopping_time)
       in
       let has_tracking = tracking_enabled
			  || List.exists
			       (function
				 | Primitives.CFLOW _ -> true
				 | (Primitives.CFLOWOFF _ | Primitives.PRINT _ |
				    Primitives.UPDATE _ | Primitives.SNAPSHOT _
				    | Primitives.FLUX _ | Primitives.FLUXOFF _ |
				    Primitives.PLOTENTRY | Primitives.STOP _ |
				    Primitives.ITER_RULE _) -> false) effects in
       let pert =
	 { Primitives.precondition = pre;
	   Primitives.effect = effects;
	   Primitives.abort = opt;
	   Primitives.stopping_time = stopping_time
	 }
       in
       (domain, succ p_id, pert::lpert,
	List.fold_left (fun acc el -> (el,p_id)::acc) stop_times stopping_time,
       has_tracking)
      )
      (domain, 0, [],[],false) res.perturbations
  in
  (*making sure that perturbations containing a stopping time precondition are tested first*)
  let lpert = List.rev lpert in
  let pred = (fun p -> match p.Primitives.stopping_time with
			 [] -> false | _ :: _ -> true) in
  let lpert_stopping_time = List.filter pred lpert in
  let lpert_ineq = List.filter (fun p -> not (pred p)) lpert in
  let lpert = lpert_stopping_time@lpert_ineq in
  ( domain, lpert,stop_times,tracking_enabled)

let init_graph_of_result algs has_tracking contact_map counter env domain res =
  let domain',init_state =
    List.fold_left
      (fun (domain,state) (_opt_vol,init_t) -> (*TODO dealing with volumes*)
       match init_t with
       | INIT_MIX (alg, (ast,mix_pos)) ->
	  let sigs = Environment.signatures env in
	  let (domain',alg') =
	    Expr.compile_alg contact_map counter domain alg in
	  let value = initial_value_alg counter algs alg' in
	  let fake_rule =
	    { LKappa.r_mix = [];
	      LKappa.r_created = LKappa.to_raw_mixture sigs ast;
	      LKappa.r_rm_tokens = []; LKappa.r_add_tokens = [];
	      LKappa.r_rate = Location.dummy_annot (CONST Nbr.zero);
	      LKappa.r_rate_absolute = false; LKappa.r_un_rate = None; } in
	  let domain'',state' =
	    match
	      rules_of_ast
		contact_map counter domain' ~syntax_ref:0 (fake_rule,mix_pos)
	    with
	    | domain'',_,_,[ compiled_rule ] ->
	       let actions,_,_ = snd compiled_rule.Primitives.instantiations in
	       let creations_sort =
		 List.fold_left
		   (fun l -> function
			  | Instantiation.Create (x,_) -> Agent_place.get_type x :: l
			  | Instantiation.Mod_internal _ | Instantiation.Bind _
			  | Instantiation.Bind_to _ | Instantiation.Free _
			  | Instantiation.Remove _ -> l) [] actions in
	       domain'',
	       Nbr.iteri
		 (fun _ s ->
		  match Rule_interpreter.apply_rule
			  ~get_alg:(fun i ->
				    fst (snd algs.NamedDecls.decls.(i)))
			  env domain''
			  (Environment.connected_components_of_unary_rules env)
			  counter s (Causal.INIT creations_sort)
			  compiled_rule with
		  | Rule_interpreter.Success s -> s
		  | (Rule_interpreter.Clash | Rule_interpreter.Corrected _) ->
		     raise (ExceptionDefn.Internal_Error
			      ("Bugged initial rule",mix_pos))
      )
		 state value
	    | domain'',_,_,[] -> domain'',state
	    | _,_,_,_ ->
	       raise (ExceptionDefn.Malformed_Decl
			(Format.asprintf
			   "initial mixture %a is partially defined"
			   (LKappa.print_rule_mixture sigs) ast,mix_pos)) in
	  domain'',state'
       | INIT_TOK (alg, (tk_id,pos_tk)) ->
	  let fake_rule =
	    { LKappa.r_mix = []; LKappa.r_created = []; LKappa.r_rm_tokens = [];
	      LKappa.r_add_tokens = [(alg, tk_id)];
	      LKappa.r_rate = Location.dummy_annot (CONST Nbr.zero);
	      LKappa.r_rate_absolute = false; LKappa.r_un_rate = None; } in
	  let domain',state' =
	    match
	      rules_of_ast
		contact_map counter domain ~syntax_ref:0
		(Location.dummy_annot fake_rule)
	    with
	    | domain'',_,_,[ compiled_rule ] ->
	       (domain'',
	       match
		 Rule_interpreter.apply_rule
		   ~get_alg:(fun i ->
			     fst (snd algs.NamedDecls.decls.(i)))
		   env domain''
		   (Environment.connected_components_of_unary_rules env)
		   counter state (Causal.INIT []) compiled_rule with
	       | Rule_interpreter.Success s -> s
	       | (Rule_interpreter.Clash | Rule_interpreter.Corrected _) ->
		  raise (ExceptionDefn.Internal_Error
			   ("Bugged initial tokens",pos_tk)))
	    | _,_,_,_ -> assert false in
	  domain',state'
      )	(domain,Rule_interpreter.empty ~has_tracking env)
      res.Ast.init
  in
  (domain',init_state)

let configurations_of_result result =
  let raw_set_value pos_p param value_list f =
    match value_list with
    | (v,pos) :: _ -> f v pos
    | [] -> ExceptionDefn.warning
	      ~pos:pos_p
	      (fun f -> Format.fprintf f "Empty value for parameter %s" param)
  in
  let set_value pos_p param value_list f ass =
    raw_set_value pos_p param value_list (fun x p -> ass := f x p) in
  let set_bool_value pos_p param value_list ass =
    set_value pos_p param value_list
	      (fun value pos_v ->
	       match value with
	       | "true" | "yes" -> true
	       | "false" | "no" -> false
	       | _ as error ->
		  raise
		    (ExceptionDefn.Malformed_Decl
		       ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
	      ) ass in
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
	    | (error,pos)::_ ->
	       raise (ExceptionDefn.Malformed_Decl
			("Unkown value "^error^" for compression mode", pos))
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
	set_bool_value pos_p param value_list Parameter.dumpIfDeadlocked
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
	set_bool_value pos_p param value_list Parameter.dotSnapshots
     | "dotCflows" ->
	set_bool_value pos_p param value_list Parameter.dotCflows
     | "reduceCflows" ->
	set_bool_value pos_p param value_list Parameter.reduceCflows
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
	set_bool_value pos_p param value_list Parameter.showIntroEvents
     | _ as error ->
	raise (ExceptionDefn.Malformed_Decl ("Unkown parameter "^error, pos_p))
    ) result.configurations

let compile_alg_vars contact_map counter domain vars =
  array_fold_left_mapi
    (fun i domain (lbl_pos,ast) ->
     let (domain',alg) =
       Expr.compile_alg ~origin:(Operator.ALG i) contact_map counter domain ast
     in (domain',(lbl_pos,alg))) domain
    (Array.of_list vars)

let compile_rules alg_deps contact_map counter domain rules =
  match
    List.fold_left
      (fun (domain,syntax_ref,deps_machinery,unary_cc,acc) (_,rule) ->
       let (domain',origin',extra_unary_cc,cr) =
	 rules_of_ast ?deps_machinery contact_map counter domain
		      ~syntax_ref rule in
       (domain',succ syntax_ref,origin',
	Connected_component.Set.union unary_cc extra_unary_cc,
	List.append cr acc))
      (domain,1,Some (Operator.RULE 0,alg_deps),
       Connected_component.Set.empty,[])
      rules with
  | fdomain,_,Some (_,falg_deps),unary_cc,frules ->
     fdomain,falg_deps,List.rev frules,unary_cc
  | _, _, None, _, _ ->
     failwith "The origin of Eval.compile_rules has been lost"

let initialize logger overwrite counter result =
  Debug.tag logger "+ Building initial simulation conditions...";
  Debug.tag logger "+ Compiling..." ;
  Debug.tag logger "\t -simulation parameters" ;
  let () = configurations_of_result result in

  Debug.tag logger "\t -agent signatures" ;
  Debug.tag logger "\t -sanity checks";
  let (sigs_nd,tk_nd,result') = LKappa.compil_of_ast overwrite result in
  Debug.tag logger "\t -KaSa tools initialization";
  let pre_kasa_state = Export_to_KaSim.Export_to_KaSim.init logger result in
  let kasa_state,contact_map =
    Export_to_KaSim.Export_to_KaSim.get_contact_map pre_kasa_state in
  let () = Export_to_KaSim.Export_to_KaSim.dump_errors_light kasa_state in
  let kasa_state = Export_to_KaSim.Export_to_KaSim.flush_errors kasa_state in
  let domain = Connected_component.Env.empty sigs_nd in
  Debug.tag logger "\t -variable declarations";
  let domain',alg_a =
    compile_alg_vars contact_map counter domain result'.Ast.variables in
  let alg_nd = NamedDecls.create alg_a in
  let alg_deps = Alg_expr.setup_alg_vars_rev_dep tk_nd alg_a in

  Debug.tag logger "\t -rules";
  let (domain',alg_deps',compiled_rules,cc_unaries) =
    compile_rules alg_deps contact_map counter domain' result'.Ast.rules in
  let rule_nd = Array.of_list compiled_rules in

  Debug.tag logger "\t -observables";
  let domain,obs =
    obs_of_result contact_map counter domain' result' in
  Debug.tag logger "\t -perturbations" ;
  let (domain,pert,stops,tracking_enabled) =
    pert_of_result alg_nd alg_deps' result'.variables result'.rules
		   contact_map counter domain result' in

  let env =
    Environment.init sigs_nd tk_nd alg_nd alg_deps'
		     (Array.of_list result'.rules,rule_nd,cc_unaries)
		     (Array.of_list (List.rev obs)) (Array.of_list pert) in

  Debug.tag logger "\t -initial conditions";
  let domain,graph =
    init_graph_of_result
      alg_nd tracking_enabled contact_map counter env domain result' in
  let () =
    if !Parameter.compileModeOn || !Parameter.debugModeOn then
      Format.eprintf
	"@[<v>@[<v 2>Environment:@,%a@]@,@[<v 2>Domain:@,@[%a@]@]@,@[<v 2>Intial graph;@,%a@]@]@."
	Kappa_printer.env env
	Connected_component.Env.print domain
	(Rule_interpreter.print env) graph in
  let graph',state = State_interpreter.initial env counter graph stops in
  (Debug.tag logger "\t Done"; (kasa_state,env, domain, graph', state))
