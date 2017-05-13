(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type directive_unit = Time | Event

let get_compilation
    ?(unit=Time) ?(max_sharing=false) ?bwd_bisim ?(compileModeOn=false)
    cli_args =
  let (conf, progressConf, env0, contact_map, updated_vars, story_compression,
       formatCflows, cflowFile, init_l),
      alg_overwrite =
    match cli_args.Run_cli_args.marshalizedInFile with
    | "" ->
      let result =
        List.fold_left (KappaLexer.compile Format.std_formatter)
          Ast.empty_compil cli_args.Run_cli_args.inputKappaFileNames in
      let () = Format.printf "+ simulation parameters@." in
      let conf, progressConf,
          (n,w,s as story_compression), formatCflow, cflowFile =
        Configuration.parse result.Ast.configurations in
      let () = Format.printf "+ Sanity checks@." in
      let (sigs_nd,contact_map,tk_nd,updated_vars,result') =
        LKappa.compil_of_ast
          ~new_syntax:(cli_args.Run_cli_args.newSyntax ||
                       conf.Configuration.newSyntax)
          cli_args.Run_cli_args.alg_var_overwrite result in
      let () = Format.printf "+ Compiling...@." in
      let (env, has_tracking,init_l) =
        Eval.compile
          ~outputs:(Outputs.go (Signature.create [||]))
          ~pause:(fun f -> f ()) ~return:(fun x -> x) ~max_sharing
          ?rescale_init:cli_args.Run_cli_args.rescale
          ?bwd_bisim ~compileModeOn
          sigs_nd tk_nd contact_map result' in
      let story_compression =
        if has_tracking && (n||w||s) then Some story_compression else None in
      (conf, progressConf, env, contact_map, updated_vars, story_compression,
       formatCflow, cflowFile,init_l),[]
    | marshalized_file ->
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
        let _,_,env,_,_,_,_,_,_ as pack =
          (Marshal.from_channel d :
             Configuration.t*Counter.progressBar*Model.t*Contact_map.t*int list*
             (bool*bool*bool) option*string*string option*
             (Alg_expr.t * Primitives.elementary_rule * Locality.t) list) in
        let () = Pervasives.close_in d  in
        let alg_overwrite =
          List.map
            (fun (s,v) ->
               Model.num_of_alg (Locality.dummy_annot s) env,
               Alg_expr.CONST v)
            cli_args.Run_cli_args.alg_var_overwrite in
        pack,alg_overwrite
      with
      | ExceptionDefn.Malformed_Decl _ as e -> raise e
      | _exn ->
        Debug.tag
          Format.std_formatter
          "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
        exit 1 in

    let init_t,max_time,init_e,max_event,plot_period =
    match unit with
    | Time ->
      Option_util.unsome (Option_util.unsome 0. conf.Configuration.initial)
        cli_args.Run_cli_args.minValue,
      cli_args.Run_cli_args.maxValue,
      None,None,
      (match cli_args.Run_cli_args.plotPeriod with
       | Some a -> Counter.DT a
       | None ->
         Option_util.unsome (Counter.DT 1.) conf.Configuration.plotPeriod)
    | Event ->
      Option_util.unsome 0. conf.Configuration.initial,None,
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
