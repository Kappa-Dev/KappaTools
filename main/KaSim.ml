let usage_msg =
  "KaSim "^Version.version_string^":\n"^
    "Usage is KaSim [-i] input_file [-e events | -t time] [-p points] [-o output_file]\n"
let () = Odes.dummy () 
let backtrace = ref false

let tmp_var_name = ref ""
let alg_var_overwrite : (string * Nbr.t) list ref = ref []
let (seedValue:int option ref) = ref None
let (maxEventValue:int option ref) = ref None
let (maxTimeValue:float option ref) = ref None
let (pointNumberValue:int ref) = ref (-1)
let (rescale:float option ref) = ref None
let implicitSignature = ref false
let interactive = ref false

(*Name convention*)
let marshalizedInFile = ref ""
let inputKappaFileNames:(string list ref) = ref []

let () =
  let options = [
    ("--version",
     Arg.Unit (fun () -> Format.print_string Version.version_msg;
			 Format.print_newline () ; exit 0),
     "display KaSim version");
    ("-i",
     Arg.String (fun fic ->
		 inputKappaFileNames:= fic:: (!inputKappaFileNames)),
     "name of a kappa file to use as input (can be used multiple times for multiple input files)");
    ("-e",
     Arg.Int (fun i -> if i < 0 then maxEventValue := None
		       else maxEventValue := Some i),
     "Number of total simulation events, including null events (negative value for unbounded simulation)");
    ("-t",
     Arg.Float(fun t -> maxTimeValue := Some t),
     "Max time of simulation (arbitrary time unit)");
    ("-p", Arg.Set_int pointNumberValue,
     "Number of points in plot");
    ("-var",
     Arg.Tuple
       [Arg.Set_string tmp_var_name;
	Arg.String
	  (fun var_val ->
	   alg_var_overwrite :=
	     (!tmp_var_name,
	      try Nbr.of_string var_val with
		Failure _ ->
		raise (Arg.Bad ("\""^var_val^"\" is not a valid value")))
	       ::!alg_var_overwrite)],
     "Set a variable to a given value");
    ("-o", Arg.String Kappa_files.set_data,
     "file name for data output") ;
    ("-d",
     Arg.String Kappa_files.set_dir,
     "Specifies directory name where output file(s) should be stored") ;
    ("-load-sim", Arg.Set_string marshalizedInFile,
     "load simulation package instead of kappa files") ;
    ("-make-sim", Arg.String Kappa_files.set_marshalized,
     "save kappa files as a simulation package") ;
    ("-dump-cc", Arg.String Kappa_files.set_ccFile,
     "file name for dumping the domain of observables") ;
    ("-dump-trace", Arg.String Kappa_files.set_traceFile,
     "file name for dumping the simulation trace") ;
    ("--implicit-signature", Arg.Set implicitSignature,
     "Program will guess agent signatures automatically") ;
    ("--interactive", Arg.Set interactive,
     "Run interactively") ;
    ("-seed", Arg.Int (fun i -> seedValue := Some i),
     "Seed for the random number generator") ;
    ("--eclipse", Arg.Set Parameter.eclipseMode,
     "enable this flag for running KaSim behind eclipse plugin") ;
    ("--emacs-mode", Arg.Set Parameter.emacsMode,
     "enable this flag for running KaSim using emacs-mode") ;
    ("--compile", Arg.Set Parameter.compileModeOn,
     "Display rule compilation as action list") ;
    ("--debug", Arg.Set Parameter.debugModeOn,
     "Enable debug mode") ;
    ("--backtrace", Arg.Set backtrace,
     "Backtracing exceptions") ;
    ("--batch", Arg.Set Parameter.batchmode,
     "Set non interactive mode (always assume default answer)") ;
    ("--gluttony",
     Arg.Unit (fun () -> Gc.set { (Gc.get()) with
				  Gc.space_overhead = 500 (*default 80*) } ;),
     "Lower gc activity for a faster but memory intensive simulation") ;
    ("-rescale", Arg.Float (fun i -> rescale:=Some i),
     "Apply rescaling factor to initial condition");
    ("--time-independent",
     Arg.Set Parameter.time_independent,
     "Disable the use of time is story heuritics (for test suite)")
  ]
  in
  try
    Arg.parse
      options (fun fic -> inputKappaFileNames:= fic::(!inputKappaFileNames))
      usage_msg;
    let abort =
      match !inputKappaFileNames with
      | [] -> !marshalizedInFile = ""
      | _ -> false
    in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let () = Sys.catch_break true in

    Printexc.record_backtrace
      (!Parameter.debugModeOn || !backtrace); (*Possible backtrace*)

    let theSeed,seed_arg =
      match !seedValue with
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

    let result =
      List.fold_left (KappaLexer.compile Format.std_formatter)
		     Ast.empty_compil !inputKappaFileNames in

    let counter =
      Counter.create
	~init_t:0. ~init_e:0 ?max_t:!maxTimeValue ?max_e:!maxEventValue
	~nb_points:!pointNumberValue in
    let (env_store, cc_env, contact_map, updated_vars, story_compression,
	 unary_distances, dotCflows, init_l as init_result),
      alg_overwrite =
      match !marshalizedInFile with
      | "" ->
	let result =
	  if !implicitSignature then Ast.implicit_signature result
	  else result in
	let () = Format.printf "+ Sanity checks@." in
	let (sigs_nd,tk_nd,updated_vars,result') =
	  LKappa.compil_of_ast !alg_var_overwrite result in
	let () = Format.printf "+ KaSa tools initialization@." in
	let contact_map,_kasa_state =
	  Eval.init_kasa Remanent_parameters_sig.KaSim sigs_nd result in
	let () = Format.printf "+ Compiling...@." in
	let (env, cc_env, story_compression, unary_distances, dotCflow, init_l)=
	  Eval.compile
	    ~pause:(fun f -> f ()) ~return:(fun x -> x)
	    ?rescale_init:!rescale ~outputs:(Outputs.go (Signature.create []))
	    sigs_nd tk_nd contact_map counter result' in
	(env, cc_env, contact_map, updated_vars, story_compression,
	 unary_distances, dotCflow, init_l),[]
      | marshalized_file ->
	 try
	   let d = open_in_bin marshalized_file in
	   let () =
	     if !inputKappaFileNames <> [] then
	       ExceptionDefn.warning
		 (fun f ->
		  Format.pp_print_string
		    f "Simulation package loaded, all kappa files are ignored") in
	   let () = Format.printf "+ Loading simulation package %s...@."
				  marshalized_file in
	   let env,cc_env,contact_map,updated_vars,story_compression,
	       unary_distances,dotCflow,init_l =
	     (Marshal.from_channel d :
	 Environment.t*Connected_component.Env.t*Primitives.contact_map*
  int list* (bool*bool*bool) option*bool option*bool*
		    (Alg_expr.t * Primitives.elementary_rule * Location.t) list) in
	   let () = Pervasives.close_in d  in
	   let alg_overwrite =
	     List.map
               (fun (s,v) ->
		Environment.num_of_alg (Location.dummy_annot s) env,
		Alg_expr.CONST v)
               !alg_var_overwrite in
	   let updated_vars' =
	     List.fold_left
	       (fun acc (i,_) -> i::acc) updated_vars alg_overwrite in
	   (env,cc_env,contact_map,updated_vars',story_compression,
	    unary_distances,dotCflow,init_l),
	   alg_overwrite
	 with
	 | ExceptionDefn.Malformed_Decl _ as e -> raise e
	 | _exn ->
	    Debug.tag
	      Format.std_formatter
	      "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
	    exit 1 in
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
    let (env,(graph,state)) =
      Eval.build_initial_state
	~bind:(fun x f -> f x) ~return:(fun x -> x)
	alg_overwrite counter env_store cc_env
	story_compression unary_distances updated_vars init_l in
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
      if !pointNumberValue > 0 || head <> [||] then
	let title = "Output of " ^ command_line in
	Outputs.create_plot
	  (Kappa_files.get_data (),title,head)
	  (match unary_distances with Some x -> x | None -> false) in
    let () =
      if !pointNumberValue > 0 then
	Outputs.go (Environment.signatures env)
	  (Data.Plot
	     (Counter.current_time counter,
	      State_interpreter.observables_values env counter graph state)) in

    Parameter.initSimTime () ;
    let () =
      let outputs = Outputs.go (Environment.signatures env) in
      if !interactive then
        let () =
          Format.printf
            "@[KaSim@ toplevel:@ type@ $RUN@ (optionally@ followed@ by@ a\
@ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ perturbation\
@ effect@ to@ perform@ it@]" in
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
                    (Environment.signatures env) (Environment.tokens_finder env)
                    (Environment.algs_finder env) (Location.dummy_annot b) in
                let cc_preenv',(b'',pos_b'') =
                  Eval.compile_bool contact_map cc_preenv b' in
                let cc_env' =
                  if cc_preenv == cc_preenv' then cc_env
                  else Connected_component.PreEnv.finalize cc_preenv' in
                cc_env',
                if try Alg_expr.stops_of_bool_expr
                         (Environment.all_dependencies env) b'' <> []
                  with ExceptionDefn.Unsatisfiable -> true then
                  let () =
                    Pp.error Format.pp_print_string
                      ("[T] can only be used in inequalities",pos_b'') in
                  (false,graph,state)
                else State_interpreter.interactive_loop
                    ~outputs
                    Format.std_formatter b'' env cc_env' counter graph state
              | Ast.QUIT -> cc_env,(true,graph,state)
              | Ast.MODIFY e ->
                let cc_preenv = Connected_component.PreEnv.of_env cc_env in
                match LKappa.modif_expr_of_ast
                        (Environment.signatures env)
                        (Environment.tokens_finder env)
                        (Environment.algs_finder env) e [] with
                | _, _::_ ->
                  let () =
                    Pp.error Format.pp_print_string
                      (Location.dummy_annot "$UPDATE is not implemented (yet?)") in
                  cc_env,(false,graph,state)
                | e', [] ->
                  let cc_preenv', e'' = Eval.compile_modification_no_update
                      contact_map cc_preenv e' in
                  let cc_env' =
                    if cc_preenv == cc_preenv' then cc_env
                    else Connected_component.PreEnv.finalize cc_preenv' in
                  cc_env',
                  List.fold_left
                    (fun (stop,graph',state' as acc) x ->
                       if stop then acc else
                         State_interpreter.do_modification
                           ~outputs env cc_env' counter graph' state' x)
                    (false,graph,state) e''
            with
            | ExceptionDefn.Syntax_Error (msg,pos) ->
              let () = Pp.error Format.pp_print_string (msg,pos) in
              cc_env,(false,graph,state)
            | ExceptionDefn.Malformed_Decl er ->
              let () = Pp.error Format.pp_print_string er in
              cc_env,(false,graph,state) in
          if stop then
            State_interpreter.finalize
              ~outputs ~called_from:Remanent_parameters_sig.KaSim dotCflows
              Format.std_formatter env counter graph' state'
          else
            toplevel cc_env' graph' state' in
        toplevel cc_env graph state
      else
        State_interpreter.loop
          ~outputs ~dotCflows
          Format.std_formatter env cc_env counter graph state in
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
