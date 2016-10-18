let usage_msg =
  "KaSim "^Version.version_string^":\n"^
  "Usage is KaSim [-i] input_file [-e events | -t time] [-p points] [-o output_file]\n"

let () =
  let cli_args = Run_cli_args.default in
  let kasim_args = Kasim_args.default in
  let common_args = Common_args.default in
  let options =
    Run_cli_args.options cli_args @
    Kasim_args.options kasim_args @ Common_args.options common_args in
  try
    Arg.parse
      options
      (fun fic -> cli_args.Run_cli_args.inputKappaFileNames <-
          fic::(cli_args.Run_cli_args.inputKappaFileNames))
      usage_msg;
    let () = Kappa_files.set_data cli_args.Run_cli_args.outputDataFile in
    let () = Kappa_files.set_dir cli_args.Run_cli_args.outputDirectory in
    let () = match kasim_args.Kasim_args.marshalizeOutFile with
      | None -> ()
      | Some marshalizeOutFile ->
        Kappa_files.set_marshalized marshalizeOutFile
    in
    let () = match kasim_args.Kasim_args.domainOutputFile with
      | None -> ()
      | Some domainOutputFile ->
        Kappa_files.set_ccFile domainOutputFile
    in
    let () = match kasim_args.Kasim_args.traceFile with
      | None -> ()
      | Some traceFile ->
        Kappa_files.set_traceFile traceFile
    in
    let () = Parameter.debugModeOn := common_args.Common_args.debug in
    let () = Parameter.eclipseMode := kasim_args.Kasim_args.eclipseMode in
    let () = Parameter.compileModeOn := kasim_args.Kasim_args.compileMode in
    let () = Parameter.batchmode := cli_args.Run_cli_args.batchmode in
    let () =
      Parameter.time_independent := common_args.Common_args.timeIndependent in

    let abort =
      match cli_args.Run_cli_args.inputKappaFileNames with
      | [] -> cli_args.Run_cli_args.marshalizedInFile = ""
      | _ -> false in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let () = Sys.catch_break true in
    Printexc.record_backtrace
      (!Parameter.debugModeOn || common_args.Common_args.backtrace);
    (*Possible backtrace*)

    let theSeed,seed_arg =
      match kasim_args.Kasim_args.seedValue with
      | Some seed -> seed,[||]
      | None ->
        let () = Format.printf "+ Self seeding...@." in
        let () = Random.self_init() in
        let out = Random.bits () in
        out,[|"-seed";string_of_int out|]
    in Random.init theSeed ;
    let command_line =
      Format.asprintf "@[<h>%a%t%a@]"
        (Pp.array Pp.space
           (fun i f s ->
              Format.fprintf
                f "'%s'" (if i = 0 then "KaSim" else s)))
        Sys.argv
        (fun f -> if Array.length seed_arg > 0 then Format.pp_print_space f ())
        (Pp.array Pp.space (fun _ -> Format.pp_print_string)) seed_arg in
    Format.printf "+ Command line to rerun is: %s@." command_line;

    let (env0, cc_env, contact_map, updated_vars, story_compression,
         unary_distances, formatCflows, init_l as init_result),
        counter,alg_overwrite = Cli_init.get_compilation
        ?max_e:kasim_args.Kasim_args.maxEventValue cli_args in
    let env = Environment.propagate_constant updated_vars counter env0 in

    let () =
      Kappa_files.with_marshalized
        (fun d -> Marshal.to_channel d init_result []) in
    let () = Format.printf "+ Building initial state@." in
    let story_compression =
      match story_compression with
      | None ->
        if Kappa_files.has_traceFile ()
        then Some ((false,false,false),true)
        else None
      | Some x -> Some (x,Kappa_files.has_traceFile ()) in
    let (graph,state) =
      Eval.build_initial_state
        ~bind:(fun x f -> f x) ~return:(fun x -> x)
        alg_overwrite counter env cc_env
        story_compression ~store_distances:(unary_distances<>None) init_l in
    let () = Format.printf "Done@." in
    let () =
      if !Parameter.compileModeOn || !Parameter.debugModeOn then
        Format.eprintf
          "@[<v>@[<v 2>Environment:@,%a@]@,@[<v 2>Domain:@,@[%a@]@]@,@[<v 2>Intial graph;@,%a@]@]@."
          Kappa_printer.env env
          Connected_component.Env.print cc_env
          (Rule_interpreter.print env) graph in
    let () = Kappa_files.with_ccFile
        (fun f -> Connected_component.Env.print_dot f cc_env) in
    ExceptionDefn.flush_warning Format.err_formatter ;
    if !Parameter.compileModeOn then exit 0 else ();

    Kappa_files.setCheckFileExists ~batchmode:!Parameter.batchmode ;

    let () =
      let head =
        Environment.map_observables
          (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
          env in
      if cli_args.Run_cli_args.pointNumberValue > 0 || head <> [||] then
        let title = "Output of " ^ command_line in
        Outputs.create_plot
          (Kappa_files.get_data (),title,head) in
    let () =
      match unary_distances with
      | None -> ()
      | Some inJson ->
        let size = Environment.nb_syntactic_rules env + 1 in
        let names =
          Array.init
            size
            (Format.asprintf "%a" (Environment.print_ast_rule ~env)) in
        Outputs.create_distances names inJson in
    let () =
      if cli_args.Run_cli_args.pointNumberValue > 0 then
        Outputs.go (Environment.signatures env)
          (Data.Plot
             (Counter.current_time counter,
              State_interpreter.observables_values env counter graph state)) in

    Parameter.initSimTime () ;
    let () =
      let outputs = Outputs.go (Environment.signatures env) in
      if cli_args.Run_cli_args.batchmode then
        State_interpreter.batch_loop
          ~outputs ~formatCflows
          Format.std_formatter env cc_env counter graph state
      else
        let lexbuf = Lexing.from_channel stdin in
        let rec toplevel cc_env graph state =
          let () = Format.printf "@.> @?" in
          let cc_env',(stop,graph',state') =
            try
              match KappaParser.interactive_command KappaLexer.token lexbuf with
              | Ast.RUN b ->
                let cc_preenv = Connected_component.PreEnv.of_env cc_env in
                let b' =
                  LKappa.bool_expr_of_ast
                    (Environment.signatures env0) (Environment.tokens_finder env0)
                    (Environment.algs_finder env0) (Location.dummy_annot b) in
                let cc_preenv',(b'',pos_b'') =
                  Eval.compile_bool contact_map cc_preenv b' in
                let cc_env' =
                  if cc_preenv == cc_preenv' then cc_env
                  else Connected_component.PreEnv.finalize cc_preenv' in
                cc_env',
                if try Alg_expr.stops_of_bool_expr
                         (Environment.all_dependencies env0) b'' <> []
                  with ExceptionDefn.Unsatisfiable -> true then
                  let () =
                    Pp.error Format.pp_print_string
                      ("[T] can only be used in inequalities",pos_b'') in
                  (false,graph,state)
                else State_interpreter.interactive_loop
                    ~outputs
                    Format.std_formatter b'' env0 cc_env' counter graph state
              | Ast.QUIT -> cc_env,(true,graph,state)
              | Ast.MODIFY e ->
                let cc_preenv = Connected_component.PreEnv.of_env cc_env in
                let e',_ =
                  Tools.list_fold_right_map
                    (LKappa.modif_expr_of_ast
                       (Environment.signatures env0)
                       (Environment.tokens_finder env0)
                       (Environment.algs_finder env0)) e [] in
                let cc_preenv', e'' = Eval.compile_modifications_no_track
                    contact_map cc_preenv e' in
                let cc_env',graph' =
                  if cc_preenv == cc_preenv' then (cc_env,graph)
                  else
                    (Connected_component.PreEnv.finalize cc_preenv',
                     List.fold_left
                       Rule_interpreter.incorporate_extra_pattern
                       graph
                       (Primitives.extract_connected_components_modifications e''))
                in
                cc_env',
                List.fold_left
                    (fun (stop,graph',state' as acc) x ->
                       if stop then acc else
                         State_interpreter.do_modification
                           ~outputs env0 cc_env counter graph' state' x)
                    (false,graph',state) e''
            with
            | ExceptionDefn.Syntax_Error (msg,pos) ->
              let () = Pp.error Format.pp_print_string (msg,pos) in
              cc_env,(false,graph,state)
            | ExceptionDefn.Malformed_Decl er ->
              let () = Pp.error Format.pp_print_string er in
              cc_env,(false,graph,state) in
          if stop then
            State_interpreter.finalize
              ~outputs ~called_from:Remanent_parameters_sig.KaSim formatCflows
              Format.std_formatter env0 counter graph' state'
          else
            toplevel cc_env' graph' state' in
        if kasim_args.Kasim_args.interactive then
          let () =
            Format.printf
              "@[KaSim@ toplevel:@ type@ $RUN@ (optionally@ followed@ by@ a\
               @ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ perturbation\
               @ effect@ to@ perform@ it@]" in
          toplevel cc_env graph state
        else
          let (stop,graph',state') =
            State_interpreter.interactive_loop
              ~outputs Format.std_formatter
              Alg_expr.FALSE env0 cc_env counter graph state in
          if stop then
            State_interpreter.finalize
              ~outputs ~called_from:Remanent_parameters_sig.KaSim formatCflows
              Format.std_formatter env0 counter graph' state'
          else
          let () =
            Format.printf
              "@[KaSim@ toplevel:@ type@ $RUN@ (optionally@ followed@ by@ a\
               @ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ perturbation\
               @ effect@ to@ perform@ it@]" in
            toplevel cc_env graph' state' in
    Format.printf "Simulation ended";
    if Counter.nb_null_event counter = 0 then Format.print_newline()
    else
      let () =
        Format.printf " (eff.: %f, detail below)@."
          ((float_of_int (Counter.current_event counter)) /.
           (float_of_int
              (Counter.nb_null_event counter +
               Counter.current_event counter))) in
      Counter.print_efficiency Format.std_formatter counter ;
  with
  | ExceptionDefn.Malformed_Decl er ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Kappa_files.close_all_out_desc () in
    let () = Pp.error Format.pp_print_string er in
    exit 2
  | ExceptionDefn.Internal_Error er ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Kappa_files.close_all_out_desc () in
    let () =
      Pp.error
        (fun f x -> Format.fprintf f "Internal Error (please report):@ %s" x)
        er in
    exit 2
  | Invalid_argument msg ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Kappa_files.close_all_out_desc () in
    let s = "" (*Printexc.get_backtrace()*) in
    let () = Format.eprintf "@.@[<v>***Runtime error %s***@,%s@]@." msg s in
    exit 2
  | Sys.Break ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Kappa_files.close_all_out_desc () in
    let () =
      Format.eprintf "@.***Interrupted by user out of simulation loop***@." in
    exit 1
  | Sys_error msg ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Kappa_files.close_all_out_desc () in
    let () = Format.eprintf "%s@." msg in
    exit 2
