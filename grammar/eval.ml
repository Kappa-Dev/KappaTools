open Ast

let rec compile_alg domain (alg,pos) =
  match alg with
  | Alg_expr.KAPPA_INSTANCE ast ->
    begin
      match domain with
      | Some (origin,contact_map,domain) ->
        let domain',ccs =
          Snip.connected_components_sum_of_ambiguous_mixture
            contact_map domain ?origin ast in
        (Some (origin,contact_map,domain'),
         (Alg_expr.KAPPA_INSTANCE (List.map fst ccs),pos))
      | None ->
        raise (ExceptionDefn.Internal_Error
                 ("Theoritically pure alg_expr has a mixture",pos))
    end
  | Alg_expr.ALG_VAR i -> (domain,(Alg_expr.ALG_VAR i,pos))
  | Alg_expr.TOKEN_ID i -> (domain,(Alg_expr.TOKEN_ID i,pos))
  | Alg_expr.STATE_ALG_OP (op) -> (domain,(Alg_expr.STATE_ALG_OP (op),pos))
  | Alg_expr.CONST n -> (domain,(Alg_expr.CONST n,pos))
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    let domain',a' = compile_alg domain a in
    let domain'',b' = compile_alg domain' b in
    (domain'',(Alg_expr.BIN_ALG_OP (op,a',b'),pos))
  | Alg_expr.UN_ALG_OP (op,a) ->
    let domain',a' = compile_alg domain a in
    (domain',(Alg_expr.UN_ALG_OP (op,a'),pos))

let compile_pure_alg (alg,pos) = snd @@ compile_alg None (alg,pos)

let compile_alg ?origin contact_map domain (alg,pos) =
  match compile_alg (Some (origin,contact_map,domain)) (alg,pos) with
  | Some (_, _,domain),alg -> domain,alg
  | None, _ -> failwith "domain has been lost in Expr.compile_alg"

let rec compile_bool contact_map domain = function
  | Alg_expr.TRUE,pos -> (domain,(Alg_expr.TRUE,pos))
  | Alg_expr.FALSE,pos -> (domain,(Alg_expr.FALSE,pos))
  | Alg_expr.BOOL_OP (op,a,b), pos ->
    let domain',a' = compile_bool contact_map domain a in
    let domain'',b' = compile_bool contact_map domain' b in
    (domain'',(Alg_expr.BOOL_OP (op,a',b'),pos))
  | Alg_expr.COMPARE_OP (op,a,b),pos ->
    let (domain',a') = compile_alg contact_map domain a in
    let (domain'',b') = compile_alg contact_map domain' b in
    (domain'',(Alg_expr.COMPARE_OP (op,a',b'), pos))

let tokenify contact_map domain l =
  List.fold_right
    (fun (alg_expr,id) (domain,out) ->
       let (domain',alg) = compile_alg contact_map domain alg_expr in
       (domain',(alg,id)::out)
    ) l (domain,[])

(* transform an LKappa rule into a Primitives rule *)
let rules_of_ast
    ?deps_machinery contact_map domain ~syntax_ref short_branch_agents (rule,_) =
  let domain',rm_toks =
    tokenify contact_map domain rule.LKappa.r_rm_tokens in
  let domain'',add_toks =
    tokenify contact_map domain' rule.LKappa.r_add_tokens in
  (*  let one_side syntax_ref label (domain,deps_machinery,unary_ccs,acc)
        rate unary_rate lhs rhs rm add =*)
  let origin,deps =
    match deps_machinery with
    | None -> None,None
    | Some (o,d) -> Some o, Some d in
  let crp = compile_pure_alg rule.LKappa.r_rate in
  let unary_infos =
    match rule.LKappa.r_un_rate with
    | None -> fun _ uncc -> crp,None,uncc
    | Some ((_,pos as rate),dist) ->
      let dist' = match dist with
        | None -> None
        | Some (dist, pos_dist) ->
          if dist = 0 then
            raise (ExceptionDefn.Malformed_Decl
                     ("Unary rule canot be applied at distance 0. ",pos_dist))
          else Some dist in
      let unrate = compile_pure_alg rate in
      fun ccs uncc ->
        match Array.length ccs with
        | (0 | 1) -> unrate,None,uncc
        | 2 ->
          crp,Some (unrate, dist'),
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
      Primitives.connected_components = ccs;
      Primitives.removed = neg;
      Primitives.inserted = pos;
      Primitives.fresh_bindings =
        Primitives.Transformation.fresh_bindings ~short_branch_agents pos;
      Primitives.consumed_tokens = rm_toks;
      Primitives.injected_tokens = add_toks;
      Primitives.syntactic_rule = syntax_ref;
      Primitives.instantiations = syntax;
    } in
  let rule_mixtures,(domain',origin') =
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

let obs_of_result contact_map domain res =
  List.fold_left
    (fun (domain,cont) alg_expr ->
       let (domain',alg_pos) =
         compile_alg contact_map domain alg_expr in
       domain',alg_pos :: cont)
    (domain,[]) res.observables

let compile_print_expr contact_map domain ex =
  List.fold_right
    (fun el (domain,out) ->
       match el with
       | Ast.Str_pexpr s -> (domain,Ast.Str_pexpr s::out)
       | Ast.Alg_pexpr ast_alg ->
         let (domain', alg) =
           compile_alg contact_map domain ast_alg in
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
      | Alg_expr.KAPPA_INSTANCE mix -> mix
      | (Alg_expr.BIN_ALG_OP _ | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
        | Alg_expr.ALG_VAR _ | Alg_expr.TOKEN_ID _ | Alg_expr.CONST _ ) ->
        raise Not_found
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

let rule_effect
    contact_map domain alg_expr (mix,created,rm,add) mix_pos rev_effects =
  let ast_rule =
    { LKappa.r_mix = mix; LKappa.r_created = created;
      LKappa.r_rm_tokens = rm; LKappa.r_add_tokens = add;
      LKappa.r_rate = Location.dummy_annot (Alg_expr.CONST Nbr.zero);
      LKappa.r_un_rate = None; } in
  let (domain',alg_pos) =
    compile_alg contact_map domain alg_expr in
  let domain'',_,_,elem_rules =
    rules_of_ast
      contact_map domain' ~syntax_ref:0 [] (ast_rule,mix_pos) in
  let elem_rule = match elem_rules with
    | [ r ] -> r
    | _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Ambiguous rule in perturbation is impossible",mix_pos)) in
  (domain'',
   (Primitives.ITER_RULE (alg_pos, elem_rule))::rev_effects)

let effects_of_modif
    ast_algs ast_rules contact_map (domain,rev_effects) = function
  | INTRO (alg_expr, (ast_mix,mix_pos)) ->
    rule_effect contact_map domain alg_expr
      ([],LKappa.to_raw_mixture (Connected_component.PreEnv.sigs domain) ast_mix,
       [],[]) mix_pos rev_effects
  | DELETE (alg_expr, (ast_mix, mix_pos)) ->
    rule_effect contact_map domain alg_expr
      (LKappa.to_erased (Connected_component.PreEnv.sigs domain) ast_mix,
       [],[],[]) mix_pos rev_effects
  | UPDATE ((i, _), alg_expr) ->
    let (domain', alg_pos) =
      compile_alg contact_map domain alg_expr in
    (domain',(Primitives.UPDATE (i, alg_pos))::rev_effects)
  | UPDATE_TOK ((tk_id,tk_pos),alg_expr) ->
    rule_effect contact_map domain
      (Location.dummy_annot (Alg_expr.CONST (Nbr.one)))
      ([],[],
       [Location.dummy_annot (Alg_expr.TOKEN_ID tk_id), tk_id],
       [(alg_expr, tk_id)])
      tk_pos rev_effects
  | SNAPSHOT pexpr ->
    let (domain',pexpr') =
      compile_print_expr contact_map domain pexpr in
    (*when specializing snapshots to particular mixtures, add variables below*)
    (domain', (Primitives.SNAPSHOT pexpr')::rev_effects)
  | STOP pexpr ->
    let (domain',pexpr') =
      compile_print_expr contact_map domain pexpr in
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
  | FLUX (rel,pexpr) ->
    let (domain',pexpr') =
      compile_print_expr contact_map domain pexpr in
    (domain', (Primitives.FLUX (rel,pexpr'))::rev_effects)
  | FLUXOFF pexpr ->
    let (domain',pexpr') =
      compile_print_expr contact_map domain pexpr in
    (domain', (Primitives.FLUXOFF pexpr')::rev_effects)
  | PRINT (pexpr,print) ->
    let (domain',pexpr') =
      compile_print_expr contact_map domain pexpr in
    let (domain'',print') =
      compile_print_expr contact_map domain' print in
    (domain'', (Primitives.PRINT (pexpr',print'))::rev_effects)
  | PLOTENTRY ->
    (domain, (Primitives.PLOTENTRY)::rev_effects)

let effects_of_modifs ast_algs ast_rules contact_map domain l =
  let domain',rev_effects =
    List.fold_left (effects_of_modif ast_algs ast_rules contact_map)
      (domain,[]) l in
  domain',List.rev rev_effects

let compile_modifications_no_track = effects_of_modifs [] []

let pert_of_result ast_algs ast_rules contact_map domain res =
  let (domain, _, lpert,tracking_enabled) =
    List.fold_left
      (fun (domain, p_id, lpert, tracking_enabled)
        ((pre_expr, modif_expr_list, opt_post),_) ->
        let (domain',(pre,pre_pos)) =
          compile_bool contact_map domain pre_expr in
        let (domain, effects) =
          effects_of_modifs
            ast_algs ast_rules contact_map domain' modif_expr_list in
        let domain,opt =
          match opt_post with
          | None -> (domain,None)
          | Some post_expr ->
            let (domain',(post,post_pos)) =
              compile_bool contact_map domain post_expr in
            (domain',Some (post,post_pos))
        in
        let has_tracking =
          tracking_enabled || List.exists
            (function
              | Primitives.CFLOW _ -> true
              | (Primitives.CFLOWOFF _ | Primitives.PRINT _ |
                 Primitives.UPDATE _ | Primitives.SNAPSHOT _
                | Primitives.FLUX _ | Primitives.FLUXOFF _ |
                Primitives.PLOTENTRY | Primitives.STOP _ |
                Primitives.ITER_RULE _) -> false) effects in
        let pert =
          { Primitives.precondition = (pre,pre_pos);
            Primitives.effect = effects;
            Primitives.abort = opt;
          } in
        (domain, succ p_id, pert::lpert,has_tracking)
      )
      (domain, 0, [], false) res.perturbations
  in
  (domain, List.rev lpert,tracking_enabled)

let inits_of_result ?rescale contact_map env preenv res =
  let init_l,preenv' =
    Tools.list_fold_right_map
      (fun (_opt_vol,alg,init_t) preenv -> (*TODO dealing with volumes*)
         let alg = match rescale with
           | None -> alg
           | Some r ->
             Location.dummy_annot
               (Alg_expr.BIN_ALG_OP
                  (Operator.MULT,alg,
                   Location.dummy_annot (Alg_expr.CONST (Nbr.F r)))) in
         match init_t with
         | INIT_MIX ast,mix_pos ->
           let sigs = Environment.signatures env in
           let (preenv',alg') =
             compile_alg contact_map preenv alg in
           let fake_rule =
             { LKappa.r_mix = [];
               LKappa.r_created = LKappa.to_raw_mixture sigs ast;
               LKappa.r_rm_tokens = []; LKappa.r_add_tokens = [];
               LKappa.r_rate = Location.dummy_annot (Alg_expr.CONST Nbr.zero);
               LKappa.r_un_rate = None; } in
           let preenv'',state' =
             match
               rules_of_ast
                 contact_map preenv' ~syntax_ref:0 [] (fake_rule,mix_pos)
             with
             | domain'',_,_,[ compiled_rule ] ->
               (fst alg',compiled_rule,mix_pos),domain''
             | _,_,_,_ ->
               raise (ExceptionDefn.Malformed_Decl
                        (Format.asprintf
                           "initial mixture %a is partially defined"
                           (LKappa.print_rule_mixture sigs) ast,mix_pos)) in
           preenv'',state'
         | INIT_TOK tk_id,pos_tk ->
           let fake_rule =
             { LKappa.r_mix = []; LKappa.r_created = []; LKappa.r_rm_tokens = [];
               LKappa.r_add_tokens = [(alg, tk_id)];
               LKappa.r_rate = Location.dummy_annot (Alg_expr.CONST Nbr.zero);
               LKappa.r_un_rate = None; } in
           match
             rules_of_ast
               contact_map preenv ~syntax_ref:0 []
               (Location.dummy_annot fake_rule)
           with
           | domain'',_,_,[ compiled_rule ] ->
             (Alg_expr.CONST (Nbr.I 1),compiled_rule,pos_tk),domain''
           | _,_,_,_ -> assert false
      ) res.Ast.init preenv in
  (preenv',init_l)

let configurations_of_result result =
  let get_value pos_p param value_list f =
    match value_list with
    | (v,pos) :: _ -> f v pos
    | [] ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Empty value for parameter "^param,pos_p)) in
  let set_value pos_p param value_list f ass =
    get_value pos_p param value_list (fun x p -> ass := f x p) in
  let get_bool_value pos_p param value_list =
    get_value pos_p param value_list
      (fun value pos_v ->
         match value with
         | "true" | "yes" -> true
         | "false" | "no" -> false
         | _ as error ->
           raise
             (ExceptionDefn.Malformed_Decl
                ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
      ) in
  List.fold_left
    (fun (unary_dist,story_compression,formatCflow as acc)
      ((param,pos_p),value_list) ->
      match param with
      | "displayCompression" ->
        let rec parse (a,b,c) l =
          match l with
          | ("strong",_)::tl -> parse (a,b,true) tl
          | ("weak",_)::tl -> parse (a,true,c) tl
          | ("none",_)::tl -> parse (true,b,c) tl
          | [] -> (unary_dist,(a,b,c),formatCflow)
          | (error,pos)::_ ->
            raise (ExceptionDefn.Malformed_Decl
                     ("Unkown value "^error^" for compression mode", pos))
        in
        parse story_compression value_list
      | "jsonUnaryDistance" ->
        if get_bool_value pos_p param value_list
        then (Some true,story_compression,formatCflow)
        else acc
      | "storeUnaryDistance" ->
        ((if get_bool_value pos_p param value_list then Some false else None),
         story_compression,formatCflow)
      | "cflowFileName" ->
        let () = get_value pos_p param value_list
            (fun x _ -> Kappa_files.set_cflow x) in
        acc
      | "progressBarSize" ->
        let () = set_value pos_p param value_list
            (fun v p ->
               try int_of_string v
               with _ ->
                 raise (ExceptionDefn.Malformed_Decl
                          ("Value "^v^" should be an integer", p))
            ) Parameter.progressBarSize in
        acc

      | "progressBarSymbol" ->
        let () = set_value pos_p param value_list
            (fun v p ->
               try
                 String.unsafe_get v 0
               with _ ->
                 raise (ExceptionDefn.Malformed_Decl
                          ("Value "^v^" should be a character",p))
            ) Parameter.progressBarSymbol in
        acc

      | "dumpIfDeadlocked" ->
        let () =
          Parameter.dumpIfDeadlocked := get_bool_value pos_p param value_list in
        acc
      | "plotSepChar" ->
        let () = set_value pos_p param value_list
            (fun v _ -> fun f ->  Format.fprintf f "%s" v)
            Parameter.plotSepChar in
        acc
      | "maxConsecutiveClash" ->
        let () = set_value pos_p param value_list
            (fun v p ->
               try int_of_string v
               with _ ->
                 raise (ExceptionDefn.Malformed_Decl
                          ("Value "^v^" should be an integer",p))
            ) Parameter.maxConsecutiveClash in
        acc
      | "dotCflows" ->
         let formatCflow = get_value pos_p param value_list
            (fun v p -> match v with
                        | "true" | "yes" | "dot" -> Dot
                        |"false" | "no" | "html" -> Html
                        | "json" -> Json
                        | _ as error  ->
                           raise
                             (ExceptionDefn.Malformed_Decl
                                ("Value "^error^
                                   " should be either \"html, dot\" or \"json\"", p))
            ) in
         (unary_dist,story_compression,formatCflow)
(*         if get_bool_value pos_p param value_list then
           (unary_dist,story_compression, Dot) else 
           (unary_dist,story_compression, Html)*)
      | "colorDot" ->
        let () = set_value pos_p param value_list
            (fun value pos_v ->
               match value with
               | "true" | "yes" -> true
               | "false" | "no" -> false
               | _ as error ->
                 raise (ExceptionDefn.Malformed_Decl
                          ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
            ) Parameter.useColor in
        acc
      | "influenceMapFileName" ->
        let () = get_value pos_p param value_list
            (fun x _ -> Kappa_files.set_influence x) in
        acc
      | _ as error ->
        raise (ExceptionDefn.Malformed_Decl ("Unkown parameter "^error, pos_p))
    ) (None,(false,false,false), Dot) result.configurations

let compile_alg_vars contact_map domain vars =
  Tools.array_fold_left_mapi
    (fun i domain (lbl_pos,ast) ->
       let (domain',alg) =
         compile_alg ~origin:(Operator.ALG i) contact_map domain ast
       in (domain',(lbl_pos,alg))) domain
    (Array.of_list vars)

let short_branch_agents contact_map =
  let rec aux oui non =
    let oui',non' =
      List.partition
        (fun (_,s) ->
           Array.fold_left
             (fun n (_,l) ->
                if List.filter (fun (x,_) -> not (List.mem x oui)) l <> []
                then n+1 else n)
             0 s = 1)
        non in
    if oui' = [] then oui
    else aux (Tools.list_rev_map_append fst oui' oui) non' in
  aux [] (Tools.array_fold_lefti (fun ag acc s -> (ag,s)::acc) [] contact_map)

let compile_rules alg_deps contact_map domain rules =
  let short_branch_agents = short_branch_agents contact_map in
  match
    List.fold_left
      (fun (domain,syntax_ref,deps_machinery,unary_cc,acc) (_,rule) ->
         let (domain',origin',extra_unary_cc,cr) =
           rules_of_ast ?deps_machinery contact_map domain
             ~syntax_ref short_branch_agents rule in
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

let translate_contact_map sigs kasa_contact_map =
  let wdl = Location.dummy_annot in
  let sol = Array.init
      (Signature.size sigs)
      (fun i -> Array.make (Signature.arity sigs i) ([],[])) in
  let () =
    Mods.StringMap.iter
      (fun agent_name sites ->
         let id_a = Signature.num_of_agent (wdl agent_name) sigs in
         Mods.StringMap.iter
           (fun site_name (states,links) ->
              let id_s =
                Signature.num_of_site
                  ~agent_name (wdl site_name) (Signature.get sigs id_a) in
              sol.(id_a).(id_s) <-
                (List.map
                   (fun state -> Signature.num_of_internal_state
                       id_s (wdl state) (Signature.get sigs id_a))
                   states,
                 List.map
                   (fun (agent_name,b) ->
                      let id_a =
                        Signature.num_of_agent (wdl agent_name) sigs in
                      let id_b =
                        Signature.num_of_site
                          ~agent_name (wdl b) (Signature.get sigs id_a) in
                      (id_a,id_b))
                   links)) sites) kasa_contact_map in
  sol

let init_kasa called_from sigs result =
  let pre_kasa_state = Export_to_KaSim.init ~compil:result ~called_from () in
  let kasa_state,contact_map = Export_to_KaSim.get_contact_map pre_kasa_state in
  let () = Export_to_KaSim.dump_errors_light kasa_state in
  translate_contact_map sigs contact_map,
  Export_to_KaSim.flush_errors kasa_state

let compile ~outputs ~pause ~return
    ?rescale_init sigs_nd tk_nd contact_map result =
  outputs (Data.Log "+ Building initial simulation conditions...");
  outputs (Data.Log "\t -simulation parameters");
  let unary_distances,story_compression,formatCflow =
    configurations_of_result result in
  pause @@ fun () ->
  let preenv = Connected_component.PreEnv.empty sigs_nd in
  outputs (Data.Log "\t -variable declarations");
  let preenv',alg_a =
    compile_alg_vars contact_map preenv result.Ast.variables in
  let alg_nd = NamedDecls.create alg_a in
  let alg_deps = Alg_expr.setup_alg_vars_rev_dep tk_nd alg_a in

  pause @@ fun () ->
  outputs (Data.Log "\t -rules");
  let (preenv',alg_deps',compiled_rules,cc_unaries) =
    compile_rules alg_deps contact_map preenv' result.Ast.rules in
  let rule_nd = Array.of_list compiled_rules in

  pause @@ fun () ->
  outputs (Data.Log "\t -perturbations");
  let (preenv,pert,has_tracking) =
    pert_of_result result.variables result.rules contact_map preenv' result in

  pause @@ fun () ->
  outputs (Data.Log "\t -observables");
  let preenv,obs =
    obs_of_result contact_map preenv result in

  let env =
    Environment.init sigs_nd tk_nd alg_nd alg_deps'
      (Array.of_list result.rules,rule_nd,cc_unaries)
      (Array.of_list (List.rev obs)) (Array.of_list pert) in

  outputs (Data.Log "\t -update_domain construction");
  pause @@ fun () ->
  let domain = Connected_component.PreEnv.finalize preenv in
  outputs (Data.Log "\t -initial conditions");
  pause @@ fun () ->
  let _,init_l =
    inits_of_result
      ?rescale:rescale_init contact_map env preenv result in
  return (env, domain,
          (if has_tracking then Some story_compression else None),
          unary_distances, formatCflow, init_l)

let build_initial_state
    ~bind ~return alg_overwrite counter env cc_env
    story_compression ~store_distances init_l =
  let stops = Environment.fold_perturbations
      (fun i acc p ->
         let s = Primitives.stops_of_perturbation
             (Environment.all_dependencies env) p in
         List.fold_left (fun acc s -> (s,i)::acc) acc s)
      [] env in
  let graph0 = Rule_interpreter.empty ?story_compression ~store_distances env in
  let state0 = State_interpreter.empty env stops alg_overwrite in
  State_interpreter.initialize
    ~bind ~return env cc_env counter graph0 state0 init_l
