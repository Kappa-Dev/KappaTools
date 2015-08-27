open Mods
open Tools
open Ast

let initial_value_alg counter algs (ast, _) =
  Expr_interpreter.value_alg
    counter
    ~get_alg:(fun i ->
	      fst (snd algs.NamedDecls.decls.(i)))
    ~get_mix:(fun _ -> Nbr.zero) ~get_tok:(fun _ -> Nbr.zero) ast

let name_and_purify_rule acc (label_opt,(r,r_pos)) =
  let (label,_ as label_pos) = match label_opt with
    | None -> Location.dummy_annot (Format.asprintf "%a" Expr.print_ast_rule r)
    | Some (lab,pos) -> (lab,pos) in
  let acc',k_def =
    if Expr.ast_alg_has_mix r.k_def then
      let rate_var = label^"_rate" in
      ((Location.dummy_annot rate_var,r.k_def)::acc,
       Location.dummy_annot (Ast.OBS_VAR rate_var))
    else (acc,r.k_def) in
  let acc'',k_op =
    match r.k_op with
    | Some k when Expr.ast_alg_has_mix k ->
       let rate_var = (Ast.flip_label label)^"_rate" in
       ((Location.dummy_annot rate_var,k)::acc',
	Some (Location.dummy_annot (Ast.OBS_VAR rate_var)))
    | (Some _ | None) -> (acc',r.k_op) in
  let () = match r.k_un with
    | None -> ()
    | Some ((_,pos),_) ->
       raise (ExceptionDefn.Internal_Error
		("KaSim does not deal with unary rule yet",pos))in
  acc'',(label_pos, ({r with k_def = k_def; k_op = k_op},r_pos))

let tokenify algs tokens contact_map domain l =
  List.fold_right
    (fun (alg_expr,(nme,pos)) (domain,out) ->
     let id =
       try StringMap.find nme tokens
       with Not_found ->
	 raise (ExceptionDefn.Malformed_Decl
		  ("Token "^nme^" is undefined",pos))
     in
     let (domain',(alg,_pos)) =
       Expr.compile_alg algs tokens contact_map domain alg_expr in
     (domain',(alg,id)::out)
    ) l (domain,[])

let rules_of_ast ?deps_machinery algs tokens contact_map domain
		    label (ast_rule,rule_pos) =
  let opposite (lab,pos) = (Ast.flip_label lab,pos) in
  let domain',rm_toks =
    tokenify algs tokens contact_map domain ast_rule.rm_token in
  let domain'',add_toks =
    tokenify algs tokens contact_map domain' ast_rule.add_token in
  let one_side label (domain,deps_machinery,acc) rate lhs rhs rm add =
    let origin,deps_algs =
      match deps_machinery with
      | None -> None,None
      | Some (o,d) -> Some o, Some d in
    let (crate,_ as crp) = Expr.compile_pure_alg algs tokens rate in
    let count = let x = ref 0 in fun (lab,pos) ->
				 incr x; (lab^"__"^string_of_int !x,pos) in
    let build deps_algs (origin,ccs,syntax,(neg,pos)) =
      Tools.option_map
	(fun x ->
	 let origin =
	   match origin with Some o -> o | None -> failwith "ugly Eval.rule_of_ast" in
	 Alg_expr.add_dep x origin crp) deps_algs,
      {
	Primitives.rate = crate;
	Primitives.connected_components = ccs;
	Primitives.removed = neg;
	Primitives.inserted = pos;
	Primitives.consumed_tokens = rm;
	Primitives.injected_tokens = add;
	Primitives.instantiations = syntax;
      } in
    let (domain',origin'),rule_mixtures =
      Snip.connected_components_sum_of_ambiguous_rule
	contact_map domain ?origin lhs rhs in
    let deps_algs',rules_l =
      match rule_mixtures with
    | [] -> deps_algs,acc
    | [ r ] ->
       let deps_algs',r' = build deps_algs r in
       deps_algs', ((label, r') :: acc)
    | _ ->
       List.fold_left
	 (fun (deps_algs,out) r ->
	  let deps_algs',r' = build deps_algs r in
	  deps_algs',(count label,r')::out) (deps_algs,acc) rule_mixtures in
    domain',(match origin' with
	    | None -> None
	    | Some o -> Some (o,
			      match deps_algs' with
			      | Some d -> d
			      | None -> failwith "ugly Eval.rule_of_ast")),
    rules_l in
  let rev = match ast_rule.arrow, ast_rule.k_op with
    | RAR, None -> domain'',deps_machinery,[]
    | LRAR, Some rate ->
       one_side (opposite label) (domain'',deps_machinery,[]) rate
		ast_rule.rhs ast_rule.lhs add_toks rm_toks
    | (RAR, Some _ | LRAR, None) ->
       raise
	 (ExceptionDefn.Malformed_Decl
	    ("Incompatible arrow and kinectic rate for inverse definition",
	     rule_pos))
  in
  one_side label rev ast_rule.k_def ast_rule.lhs ast_rule.rhs rm_toks add_toks

let obs_of_result algs tokens contact_map domain res =
  List.fold_left
    (fun (domain,cont) alg_expr ->
     let (domain',alg_pos) =
       Expr.compile_alg algs.NamedDecls.finder tokens.NamedDecls.finder
			contact_map domain alg_expr in
     domain',alg_pos :: cont)
    (domain,[]) res.observables

let compile_print_expr algs tokens contact_map domain ex =
  List.fold_right
    (fun (el,pos) (domain,out) ->
     match el with
     | Ast.Str_pexpr s -> (domain,(Ast.Str_pexpr s,pos)::out)
     | Ast.Alg_pexpr ast_alg ->
	let (domain', (alg,_pos)) =
	  Expr.compile_alg algs.NamedDecls.finder tokens.NamedDecls.finder
			   contact_map domain (ast_alg,pos) in
	(domain',(Ast.Alg_pexpr alg,pos)::out))
    ex (domain,[])

let cflows_of_label on algs rules (label,pos) rev_effects =
  let adds tests l x =
    (if on then Primitives.CFLOW (x,tests) else Primitives.CFLOWOFF x) :: l in
  try
    let rule_id = StringMap.find label rules.NamedDecls.finder in
    let rule = snd rules.NamedDecls.decls.(rule_id) in
    let tests = fst rule.Primitives.instantiations in
    Array.fold_left
      (adds tests) rev_effects rule.Primitives.connected_components
  with Not_found ->
    try let var = StringMap.find label algs.NamedDecls.finder in
	match algs.NamedDecls.decls.(var) with
	|(_,(Alg_expr.KAPPA_INSTANCE ccs,_)) ->
	  List.fold_left (fun acc (cc,tests) -> Array.fold_left (adds tests) acc cc) rev_effects ccs
	| (_,((Alg_expr.CONST _ | Alg_expr.BIN_ALG_OP _ | Alg_expr.TOKEN_ID _ |
	       Alg_expr.STATE_ALG_OP _ | Alg_expr.UN_ALG_OP _ |
	       Alg_expr.ALG_VAR _ ),_)) -> raise Not_found
    with Not_found ->
      raise (ExceptionDefn.Malformed_Decl
	       ("Label '" ^ label ^
		  "' does not refer to a non ambiguous Kappa expression"
	       ,pos))

let effects_of_modif algs tokens rules contact_map domain ast_list =
  let rec iter rev_effects domain ast_list =
    let rule_effect alg_expr ast_rule mix_pos =
      let (domain',alg_pos) =
	Expr.compile_alg algs.NamedDecls.finder tokens.NamedDecls.finder
			 contact_map domain alg_expr in
      let domain'',_,elem_rules =
	rules_of_ast algs.NamedDecls.finder tokens.NamedDecls.finder contact_map
		     domain' (Location.dummy_annot "__perturbation_modification")
		     (ast_rule,mix_pos) in
      let elem_rule = match elem_rules with
	| [ _, r ] -> r
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
	    let ast_rule =
	      { add_token=[]; rm_token=[]; lhs = []; arrow = Ast.RAR;
		rhs = ast_mix; k_def=Location.dummy_annot (Ast.CONST Nbr.zero);
		k_un=None;k_op=None;
	      } in
	    rule_effect alg_expr ast_rule mix_pos
	 | DELETE (alg_expr, (ast_mix, mix_pos)) ->
	    let ast_rule =
	      { add_token=[]; rm_token=[]; lhs = ast_mix; arrow = Ast.RAR;
		rhs = [];
		k_def=Location.dummy_annot (Ast.CONST Nbr.zero);
		k_un=None;k_op=None;
	      } in
	    rule_effect alg_expr ast_rule mix_pos
	 | UPDATE ((nme, pos_rule), alg_expr) ->
	    let i,is_rule =
	      (try (StringMap.find nme rules.NamedDecls.finder,true)
	       with
	       | Not_found ->
		  try
		    (StringMap.find nme algs.NamedDecls.finder, false)
		  with Not_found ->
		    raise (ExceptionDefn.Malformed_Decl
			     ("Variable " ^ (nme ^ " is neither a constant nor a rule")
			     ,pos_rule))
	      ) in
	    let (domain', alg_pos) =
	      Expr.compile_alg algs.NamedDecls.finder tokens.NamedDecls.finder
			       contact_map domain alg_expr in
	    (domain',
	     (Primitives.UPDATE ((if is_rule then Operator.RULE i
	      else Operator.ALG i), alg_pos))::rev_effects)
	 | UPDATE_TOK ((tk_nme,tk_pos),alg_expr) ->
	    let ast_rule =
	      { add_token=[(alg_expr,(tk_nme,tk_pos))];
		rm_token=[Location.dummy_annot (Ast.TOKEN_ID tk_nme),
			  (tk_nme,tk_pos)];
		arrow = Ast.RAR; lhs=[]; rhs=[];
		k_def=Location.dummy_annot (Ast.CONST Nbr.zero);
		k_un=None; k_op= None; } in
	    rule_effect (Location.dummy_annot (Ast.CONST (Nbr.one)))
			ast_rule tk_pos
	 | SNAPSHOT (pexpr,_) ->
	    let (domain',pexpr') =
	      compile_print_expr algs tokens contact_map domain pexpr in
	    (*when specializing snapshots to particular mixtures, add variables below*)
	    (domain', (Primitives.SNAPSHOT pexpr')::rev_effects)
	 | STOP (pexpr,_) ->
	    let (domain',pexpr') =
	      compile_print_expr algs tokens contact_map domain pexpr in
	    (domain', (Primitives.STOP pexpr')::rev_effects)
	 | CFLOWLABEL (on,lab) ->
	    (domain, cflows_of_label on algs rules lab rev_effects)
	 | CFLOWMIX (on,(ast,_)) ->
	    let adds tests l x =
	      (if on then Primitives.CFLOW (x,tests)
	       else Primitives.CFLOWOFF x) :: l in
	    let domain',ccs =
	      Snip.connected_components_sum_of_ambiguous_mixture
		contact_map domain ~origin:(Operator.PERT(-1)) ast in
	    (domain',
	     List.fold_left (fun x (y,t) -> Array.fold_left (adds t) x y)
			    rev_effects ccs)
	 | FLUX (pexpr,_) ->
	    let (domain',pexpr') =
	      compile_print_expr algs tokens contact_map domain pexpr in
	    (domain', (Primitives.FLUX pexpr')::rev_effects)
	 | FLUXOFF (pexpr,_) ->
	    let (domain',pexpr') =
	      compile_print_expr algs tokens contact_map domain pexpr in
	    (domain', (Primitives.FLUXOFF pexpr')::rev_effects)
	 | PRINT (pexpr,print,_) ->
	    let (domain',pexpr') =
	      compile_print_expr algs tokens contact_map domain pexpr in
	    let (domain'',print') =
	      compile_print_expr algs tokens contact_map domain' print in
	    (domain'', (Primitives.PRINT (pexpr',print'))::rev_effects)
	 | PLOTENTRY ->
	    (domain, (Primitives.PLOTENTRY)::rev_effects)
       in
       iter rev_effects domain tl
  in
  iter [] domain ast_list

let pert_of_result algs algs_deps tokens rules contact_map domain res =
  let (domain, _, lpert, stop_times,tracking_enabled) =
    List.fold_left
      (fun (domain, p_id, lpert, stop_times, tracking_enabled)
	   ((pre_expr, modif_expr_list, opt_post),pos) ->
       let (domain',(pre,pos_pre)) =
	 Expr.compile_bool algs.NamedDecls.finder tokens.NamedDecls.finder
			   contact_map domain pre_expr in
       let stopping_time =
	 try Expr.stops_of_bool_expr algs_deps pre
	 with ExceptionDefn.Unsatisfiable ->
	   raise
	     (ExceptionDefn.Malformed_Decl
		("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
		,pos_pre))
       in
       let (domain, effects) =
	 effects_of_modif algs tokens rules contact_map domain' modif_expr_list in
       let domain,opt,stopping_time =
	 match opt_post with
	 | None -> (domain,None,stopping_time)
	 | Some post_expr ->
	    let (domain',(post,_pos)) =
	      Expr.compile_bool algs.NamedDecls.finder tokens.NamedDecls.finder
				contact_map domain post_expr in
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

let init_graph_of_result algs tokens has_tracking contact_map counter env domain res =
  let domain',init_state =
    List.fold_left
      (fun (domain,state) (opt_vol,init_t,_) -> (*TODO dealing with volumes*)
       match init_t with
       | INIT_MIX (alg, (ast,mix_pos)) ->
	  let (domain',alg') =
	    Expr.compile_alg algs.NamedDecls.finder tokens.NamedDecls.finder
			     contact_map domain alg in
	  let value = initial_value_alg counter algs alg' in
	  let fake_rule =
	    { lhs = []; rm_token = []; arrow = RAR; rhs = ast; add_token = [];
	      k_def = Location.dummy_annot (CONST Nbr.zero);
	      k_un = None; k_op = None; } in
	  let causality =
	    Causal.INIT (Format.asprintf "@[<h>%a@]" Expr.print_ast_mix ast) in
	  let domain'',state' =
	    match
	      rules_of_ast algs.NamedDecls.finder tokens.NamedDecls.finder
			   contact_map domain' (Location.dummy_annot "__init_mix")
			   (fake_rule,mix_pos)
	    with
	    | domain'',_,[ _, compiled_rule ] ->
	       domain'',
	       Nbr.iteri
		 (fun _ s ->
		  fst
		    (Rule_interpreter.force_rule
		       ~get_alg:(fun i ->
				 fst (snd algs.NamedDecls.decls.(i)))
		       domain'' counter s causality compiled_rule))
		 state value
	    | domain'',_,[] -> domain'',state
	    | _,_,_ ->
	       raise (ExceptionDefn.Malformed_Decl
			(Format.asprintf
			   "initial mixture %a is partially defined"
			   Expr.print_ast_mix ast,mix_pos)) in
	  domain'',state'
       | INIT_TOK (alg, (tk_nme,pos_tk)) ->
	  let fake_rule =
	    { lhs = []; rm_token = []; arrow = RAR; rhs = [];
	      add_token = [(alg, (tk_nme,pos_tk))];
	      k_def = Location.dummy_annot (CONST Nbr.zero);
	      k_un = None; k_op = None; } in
	  let domain',(state',_) =
	    match
	      rules_of_ast algs.NamedDecls.finder tokens.NamedDecls.finder
			   contact_map domain (Location.dummy_annot "__init_tok")
			   (Location.dummy_annot fake_rule)
	    with
	    | domain'',_,[ _, compiled_rule ] ->
	       domain'',
	       Rule_interpreter.force_rule
		      ~get_alg:(fun i ->
				fst (snd algs.NamedDecls.decls.(i)))
		      domain'' counter state
		      (Causal.INIT
			 (Format.asprintf "@[<h>%a %s@]" Expr.print_ast_alg (fst alg) tk_nme))
		      compiled_rule
	    | _,_,_ -> assert false in
	  domain',state'
      )	(domain,Rule_interpreter.empty ~has_tracking env)
      res.Ast.init
  in
  (domain',init_state)

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
      (fun (x,v) -> (Location.dummy_annot x,
		     Location.dummy_annot (Ast.CONST v))) overwrite
      (List.filter
	 (fun ((x,_),_) ->
	  List.for_all (fun (x',_) -> x <> x') overwrite) vars) in
  let vars_nd = NamedDecls.create (Array.of_list alg_vars_over) in
  array_fold_left_mapi
    (fun i domain (lbl_pos,ast) ->
     let (domain',alg) =
       Expr.compile_alg ~origin:(Operator.ALG i) vars_nd.NamedDecls.finder tokens
			~max_allowed_var:(pred i) contact_map domain ast
     in (domain',(lbl_pos,alg))) domain
    vars_nd.NamedDecls.decls

let compile_rules algs alg_deps tokens contact_map domain rules =
  match
    List.fold_left
      (fun (domain,deps_machinery,acc) (rule_label,rule) ->
       let (domain',origin',cr) =
	 rules_of_ast algs tokens ?deps_machinery contact_map domain rule_label rule in
       domain',origin',List.rev_append cr acc)
      (domain,Some (Operator.RULE 0,alg_deps),[]) rules with
  | fdomain,Some (_,falg_deps),frules -> fdomain,falg_deps,List.rev frules
  | _, None, _ ->
     failwith "The origin of Eval.compile_rules has been lost"

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
  let (extra_vars,cleaned_rules) =
    Tools.list_fold_right_map name_and_purify_rule [] result.rules in
  let domain',alg_a =
    compile_alg_vars tk_nd.NamedDecls.finder contact_map domain
		     overwrite (result.Ast.variables@extra_vars) in
  let alg_nd = NamedDecls.create alg_a in
  let alg_deps = Alg_expr.setup_alg_vars_rev_dep alg_a in

  Debug.tag logger "\t -rules";
  let (domain',alg_deps',compiled_rules) =
    compile_rules alg_nd.NamedDecls.finder alg_deps tk_nd.NamedDecls.finder
		  contact_map domain' cleaned_rules in
  let rule_nd = NamedDecls.create (Array.of_list compiled_rules) in

  Debug.tag logger "\t -observables";
  let domain,obs =
    obs_of_result alg_nd tk_nd contact_map domain' result in
  Debug.tag logger "\t -perturbations" ;
  let (domain,pert,stops,tracking_enabled) =
    pert_of_result alg_nd alg_deps' tk_nd rule_nd contact_map domain result in

  let env =
    Environment.init sigs_nd tk_nd alg_nd alg_deps' rule_nd
		     (Array.of_list (List.rev obs)) (Array.of_list pert) in

  Debug.tag logger "\t -initial conditions";
  let domain,graph =
    init_graph_of_result
      alg_nd tk_nd tracking_enabled contact_map counter env domain result in
  let () =
    if !Parameter.compileModeOn || !Parameter.debugModeOn then
      Format.eprintf
	"@[<v>@[<v 2>Environment:@,%a@]@,@[<v 2>Domain:@,@[%a@]@]@,@[<v 2>Intial graph;@,%a@]@]@."
	Kappa_printer.env env
	Connected_component.Env.print domain
	(Rule_interpreter.print env) graph in
  let state = State_interpreter.initial env counter graph stops in
  (Debug.tag logger "\t Done"; (env, domain, counter, graph, state))
