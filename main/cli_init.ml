type directive_unit = Time | Event

let get_compilation ?(unit=Time) cli_args =
  let init_t,max_time,init_e,max_event,plot_period =
    match unit with
    | Time ->
      Some cli_args.Run_cli_args.minValue, cli_args.Run_cli_args.maxValue,
      None,None,Counter.DT cli_args.Run_cli_args.plotPeriod
    | Event ->
      None,None,
      Some (int_of_float cli_args.Run_cli_args.minValue),
      Tools.option_map int_of_float cli_args.Run_cli_args.maxValue,
      Counter.DE (int_of_float (ceil cli_args.Run_cli_args.plotPeriod)) in
  let counter =
    Counter.create ?init_t ?init_e ?max_time ?max_event ~plot_period in
  let (env, contact_map, updated_vars, story_compression,
       unary_distances, formatCflows, cflowFile, init_l),
      counter,alg_overwrite =
    match cli_args.Run_cli_args.marshalizedInFile with
    | "" ->
      let result =
        List.fold_left (KappaLexer.compile Format.std_formatter)
          Ast.empty_compil cli_args.Run_cli_args.inputKappaFileNames in
      let () = Format.printf "+ Sanity checks@." in
      let (sigs_nd,contact_map,tk_nd,updated_vars,result') =
        LKappa.compil_of_ast cli_args.Run_cli_args.alg_var_overwrite result in
      let () = Format.printf "+ Compiling...@." in
      let (env, story_compression, unary_distances,
           formatCflow, cflowFile, init_l) =
        Eval.compile
          ~pause:(fun f -> f ())
          ~return:(fun x -> x)
          ?rescale_init:cli_args.Run_cli_args.rescale
          ~outputs:(Outputs.go (Signature.create [||]))
          sigs_nd tk_nd contact_map result' in
      (env, contact_map, updated_vars, story_compression,
       unary_distances, formatCflow, cflowFile,init_l),counter,[]
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
        let env,contact_map,updated_vars,story_compression,
            unary_distances,formatCflow,cflowFile,init_l =
          (Marshal.from_channel d :
             Environment.t*Signature.contact_map*int list*
             (bool*bool*bool) option*bool option*string*string option*
             (Alg_expr.t * Primitives.elementary_rule * Location.t) list) in
        let () = Pervasives.close_in d  in
        let alg_overwrite =
          List.map
            (fun (s,v) ->
               Environment.num_of_alg (Location.dummy_annot s) env,
               Alg_expr.CONST v)
            cli_args.Run_cli_args.alg_var_overwrite in
        let updated_vars' =
          List.fold_left
            (fun acc (i,_) -> i::acc) updated_vars alg_overwrite in
        (env,contact_map,updated_vars',story_compression,
         unary_distances,formatCflow,cflowFile,init_l),
        counter,alg_overwrite
      with
      | ExceptionDefn.Malformed_Decl _ as e -> raise e
      | _exn ->
        Debug.tag
          Format.std_formatter
          "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
        exit 1 in
  (env, contact_map, updated_vars, story_compression,
   unary_distances, formatCflows, cflowFile, init_l),counter,alg_overwrite
