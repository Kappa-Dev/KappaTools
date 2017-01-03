let do_interactive_directives
    ~outputs ~max_sharing contact_map env counter graph state e =
  let cc_preenv =
    Pattern.PreEnv.of_env (Environment.domain env) in
  let contact_map' = Array.map Array.copy contact_map in
  let e',_ =
    Tools.list_fold_right_map
      (LKappa.modif_expr_of_ast
         (Environment.signatures env)
         (Environment.tokens_finder env)
         (Environment.algs_finder env) contact_map') e [] in
  let () =
    if Tools.array_fold_lefti
        (fun n -> Tools.array_fold_lefti
            (fun s b x -> b || x != contact_map.(n).(s)))
        false contact_map' then
      raise (ExceptionDefn.Malformed_Decl
               (Location.dummy_annot "Creating new link type is forbidden")) in
  let cc_preenv', e'' = Eval.compile_modifications_no_track
      contact_map cc_preenv e' in
  let env',graph' =
    if cc_preenv == cc_preenv' then (env,graph)
    else
      let fenv,_ = Pattern.PreEnv.finalize ~max_sharing cc_preenv' in
      (Environment.new_domain fenv env,
       List.fold_left
         (Rule_interpreter.incorporate_extra_pattern fenv)
         graph
         (Primitives.extract_connected_components_modifications e'')) in
  env',
  List.fold_left
    (fun (stop,graph',state' as acc) x ->
       if stop then acc else
         State_interpreter.do_modification
           ~outputs env' counter graph' state' x)
    (false,graph',state) e''
