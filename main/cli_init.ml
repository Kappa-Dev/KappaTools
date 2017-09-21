(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type preprocessed_ast =
  Configuration.t * Counter.progressBar * (bool * bool * bool) * string *
  string option * Signature.s * Contact_map.t * unit NamedDecls.t *
  int Mods.StringMap.t * int list *
  (Ast.agent, LKappa.rule_agent list, Raw_mixture.t, int, unit, LKappa.rule) Ast.compil *
  (LKappa.rule_mixture, Raw_mixture.t, int) Ast.init_statment list option *
  float option

let preprocess ?(kasim_args=Kasim_args.default) cli_args ast =
  let () = Format.printf "+ simulation parameters@." in
  let conf, progressConf,
      story_compression, formatCflow, cflowFile =
    Configuration.parse ast.Ast.configurations in
  let () = Format.printf "+ Sanity checks@." in
  let syntax_version =
    Ast.merge_version
      cli_args.Run_cli_args.syntaxVersion conf.Configuration.syntaxVersion in
  let (sigs_nd,contact_map,tk_nd,alg_finder,updated_vars,result') =
    LKappa.compil_of_ast
      ~syntax_version
      kasim_args.Kasim_args.alg_var_overwrite ast in
  let overwrite_init,overwrite_t0 = match kasim_args.Kasim_args.initialMix with
    | None -> None,None
    | Some file ->
      let compil =
        KappaLexer.compile Format.std_formatter Ast.empty_compil file in
      let conf, _, _, _, _ =
        Configuration.parse compil.Ast.configurations in
      Some
        (LKappa.init_of_ast
           ~syntax_version sigs_nd contact_map ~with_counters:true
           tk_nd.NamedDecls.finder alg_finder compil.Ast.init),
      conf.Configuration.initial in
  conf, progressConf,
  story_compression, formatCflow, cflowFile,sigs_nd,contact_map,tk_nd,alg_finder,
  updated_vars,result',overwrite_init,overwrite_t0

let get_ast_from_list_of_files list =
  List.fold_left
    (KappaLexer.compile Format.std_formatter)
    Ast.empty_compil list

let get_ast_from_cli_args cli_args =
  get_ast_from_list_of_files cli_args.Run_cli_args.inputKappaFileNames

let get_preprocessed_ast_from_cli_args
    ?(kasim_args=Kasim_args.default) cli_args =
  let ast =
    get_ast_from_list_of_files cli_args.Run_cli_args.inputKappaFileNames
  in
  preprocess cli_args ~kasim_args ast

let get_pack_from_preprocessed_ast ?(kasim_args=Kasim_args.default)
    ~max_sharing ?bwd_bisim ~compileModeOn preprocessed_ast
  =
  let conf, progressConf,
  story_compression, formatCflow, cflowFile,sigs_nd,contact_map,tk_nd,_alg_finder,
      updated_vars,result',overwrite_init,overwrite_t0
    = preprocessed_ast
  in
  let n,w,s = story_compression in
  let () =  Format.printf "+ Compiling...@." in
  let outputs =
    Outputs.go (Signature.create false [||])
  in
  let (env, has_tracking,init_l) =
    Eval.compile
      ~outputs
      ~pause:(fun f -> f ()) ~return:(fun x -> x) ~max_sharing
      ?rescale_init:kasim_args.Kasim_args.rescale
      ?overwrite_init ?bwd_bisim ~compileModeOn
      sigs_nd tk_nd contact_map result' in
  let story_compression =
    if has_tracking && (n||w||s) then Some story_compression else None in
  (conf, progressConf, env, contact_map, updated_vars, story_compression,
 formatCflow, cflowFile,init_l),[],overwrite_t0

let get_pack_from_marshalizedfile
   kasim_args cli_args marshalized_file =
  assert (marshalized_file <> "");
  try
    let d = open_in_bin marshalized_file in
    let () =
      if cli_args.Run_cli_args.inputKappaFileNames <> [] then
        ExceptionDefn.warning
          (fun f ->
             Format.pp_print_string
               f "Simulation package loaded, all kappa files are ignored") in
    let () = Format.printf "+ Loading simulation package %s...@."
        marshalized_file in
    let conf,progress,env,contact,updated,compr,cflow,cflowfile,_ as pack =
      (Marshal.from_channel d :
         Configuration.t*Counter.progressBar*Model.t*Contact_map.t*int list*
         (bool*bool*bool) option*string*string option*
         (Primitives.alg_expr * Primitives.elementary_rule * Locality.t) list) in
    let () = Pervasives.close_in d  in
    let alg_overwrite =
      List.map
        (fun (s,v) ->
           Model.num_of_alg (Locality.dummy_annot s) env,
           Alg_expr.CONST v)
        kasim_args.Kasim_args.alg_var_overwrite in
    match kasim_args.Kasim_args.initialMix with
    | None -> pack,alg_overwrite,None
    | Some file ->
      let compil = get_ast_from_list_of_files [file] in
      let conf', _, _, _, _ =
        Configuration.parse compil.Ast.configurations in
      let raw_inits =
        LKappa.init_of_ast
          ~syntax_version:conf'.Configuration.syntaxVersion
          (Model.signatures env) contact ~with_counters:true
          (Model.tokens_finder env) (Model.algs_finder env) compil.Ast.init in
      let inits = Eval.compile_inits ?rescale:kasim_args.Kasim_args.rescale
          ~compileModeOn:false contact env raw_inits in
      (conf,progress,env,contact,updated,compr,cflow,cflowfile,inits),
      alg_overwrite,conf'.Configuration.initial
  with
  | ExceptionDefn.Malformed_Decl _ as e -> raise e
  | _exn ->
    Debug.tag
      Format.std_formatter
      "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
    exit 1

let get_compilation_from_pack
    ?(unit=Kasim_args.Time)
    cli_args pack =
    let (conf, progressConf, env0, contact_map, updated_vars, story_compression,
         formatCflows, cflowFile, init_l),
        alg_overwrite,overwrite_t0 = pack in
    let init_t_from_files =
      Option_util.unsome
        (Option_util.unsome 0. conf.Configuration.initial)
        overwrite_t0 in
    let init_t,max_time,init_e,max_event,plot_period =
    match unit with
    | Kasim_args.Time ->
      Option_util.unsome init_t_from_files cli_args.Run_cli_args.minValue,
      cli_args.Run_cli_args.maxValue,
      None,None,
      (match cli_args.Run_cli_args.plotPeriod with
       | Some a -> Counter.DT a
       | None ->
         Option_util.unsome (Counter.DT 1.) conf.Configuration.plotPeriod)
    | Kasim_args.Event ->
      init_t_from_files,None,
      Some (int_of_float (Option_util.unsome 0. cli_args.Run_cli_args.minValue)),
      Option_util.map int_of_float cli_args.Run_cli_args.maxValue,
      match cli_args.Run_cli_args.plotPeriod with
      | Some a -> Counter.DE (int_of_float (ceil a))
      | None ->
        Option_util.unsome (Counter.DE 1) conf.Configuration.plotPeriod in
  let counter =
    Counter.create ~init_t ?init_e ?max_time ?max_event ~plot_period in
  let env =
    if cli_args.Run_cli_args.batchmode then
      Model.propagate_constant
        ?max_time:(Counter.max_time counter)
        ?max_events:(Counter.max_events counter) updated_vars alg_overwrite env0
    else Model.overwrite_vars alg_overwrite env0 in
  (conf, progressConf, env, contact_map, updated_vars, story_compression,
   formatCflows, cflowFile, init_l),counter

let get_compilation_from_preprocessed_ast
    ?(unit=Kasim_args.Time)
    ?(max_sharing=false)
    ?bwd_bisim
    ?(compileModeOn=false)
    ?(kasim_args=Kasim_args.default)
    cli_args preprocessed
    =
    let pack =
      get_pack_from_preprocessed_ast
      ~kasim_args ~max_sharing ?bwd_bisim ~compileModeOn
      preprocessed
    in
    get_compilation_from_pack ~unit cli_args pack

let get_compilation
       ?(unit=Kasim_args.Time) ?(max_sharing=false) ?bwd_bisim ?(compileModeOn=false)
       ?(kasim_args=Kasim_args.default) cli_args =
     let pack =
       match kasim_args.Kasim_args.marshalizedInFile with
       | "" ->
         let preprocess = get_preprocessed_ast_from_cli_args cli_args in
         get_pack_from_preprocessed_ast
           ~kasim_args ~max_sharing ?bwd_bisim ~compileModeOn
           preprocess
       | marshalized_file ->
         get_pack_from_marshalizedfile
           kasim_args cli_args marshalized_file in
     get_compilation_from_pack ~unit cli_args pack
