let usage_msg =
  "KaSim "^Version.version_string^":\n"^
    "Usage is KaSim [-i] input_file [-e events | -t time] [-p points] [-o output_file]\n"

let (maxEventValue:int option ref) = ref None
let (maxTimeValue:float option ref) = ref None
let (pointNumberValue:int ref) = ref (-1)
let (rescale:float option ref) = ref None

let () =
  let options = [
    ("--version",
     Arg.Unit (fun () -> Format.print_string Version.version_msg;
			 Format.print_newline () ; exit 0),
     "display KaSim version");
    ("-i",
     Arg.String (fun fic ->
		 Parameter.inputKappaFileNames:= fic:: (!Parameter.inputKappaFileNames)),
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
       [Arg.Set_string Parameter.tmp_var_name;
	Arg.String
	  (fun var_val ->
	   Parameter.alg_var_overwrite :=
	     (!Parameter.tmp_var_name,
	      try Nbr.of_string var_val with
		Failure _ ->
		raise (Arg.Bad ("\""^var_val^"\" is not a valid value")))
	       ::!Parameter.alg_var_overwrite)],
     "Set a variable to a given value");
    ("-o", Arg.String Kappa_files.set_data,
     "file name for data output") ;
    ("-d",
     Arg.String Kappa_files.set_dir,
     "Specifies directory name where output file(s) should be stored") ;
    ("-load-sim", Arg.Set_string Parameter.marshalizedInFile,
     "load simulation package instead of kappa files") ;
    ("-make-sim", Arg.String Kappa_files.set_marshalized,
     "save kappa files as a simulation package") ;
    ("-dump-cc", Arg.String Kappa_files.set_ccFile,
     "file name for dumping the domain of observables") ;
    ("--implicit-signature",
     Arg.Unit (fun () -> Parameter.implicitSignature := true),
     "Program will guess agent signatures automatically") ;
    ("-seed", Arg.Int (fun i -> Parameter.seedValue := Some i),
     "Seed for the random number generator") ;
    ("--eclipse", Arg.Set Parameter.eclipseMode,
     "enable this flag for running KaSim behind eclipse plugin") ;
    ("--emacs-mode", Arg.Set Parameter.emacsMode,
     "enable this flag for running KaSim using emacs-mode") ;
    ("--compile", Arg.Set Parameter.compileModeOn,
     "Display rule compilation as action list") ;
    ("--debug", Arg.Set Parameter.debugModeOn,
     "Enable debug mode") ;
    ("--safe", Arg.Set Parameter.safeModeOn,
     "Enable safe mode") ;
    ("--backtrace", Arg.Set Parameter.backtrace,
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
    Arg.parse options
	      (fun fic ->
	       Parameter.inputKappaFileNames:=
		 fic::(!Parameter.inputKappaFileNames))
	      usage_msg;
    let abort =
      match !Parameter.inputKappaFileNames with
      | [] -> !Parameter.marshalizedInFile = ""
      | _ -> false
    in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let sigint_handle = fun _ ->
      raise (ExceptionDefn.UserInterrupted
	       (fun t e ->
		Format.sprintf
		  "Abort signal received after %E t.u (%d events)" t e))
    in
    let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle sigint_handle) in

    Printexc.record_backtrace !Parameter.backtrace ; (*Possible backtrace*)

    (*let _ = Printexc.record_backtrace !Parameter.debugModeOn in*)

    Format.printf "+ Command line is: @[<h>%a@]@."
		  (Pp.array Pp.space
			    (fun i f s ->
			     Format.fprintf
			       f "'%s'" (if i = 0 then "KaSim" else s)))
		  Sys.argv;

    let result =
      List.fold_left (KappaLexer.compile Format.std_formatter)
		     Ast.empty_compil !Parameter.inputKappaFileNames in

    let theSeed =
      match !Parameter.seedValue with
      | Some seed -> seed
      | None ->
	 begin
	   Format.printf "+ Self seeding...@." ;
	   Random.self_init() ;
	   Random.bits ()
	 end
    in
    Random.init theSeed ;
    Format.printf
      "+ Initialized random number generator with seed %d@." theSeed;

    let counter =
      Counter.create
	~init_t:0. ~init_e:0 ?max_t:!maxTimeValue ?max_e:!maxEventValue
	~nb_points:!pointNumberValue in
    let (env_store, cc_env, updated_vars, has_tracking, init_l as init_result),
      alg_overwrite =
      match !Parameter.marshalizedInFile with
      | "" ->
	let result =
	  if !Parameter.implicitSignature
	  then Ast.implicit_signature result
	  else result in
	let () = Format.printf "+ Sanity checks@." in
	let (sigs_nd,tk_nd,updated_vars,result') =
	  LKappa.compil_of_ast !Parameter.alg_var_overwrite result in
	let () = Format.printf "+ KaSa tools initialization@." in
	let contact_map,_kasa_state =
	  Eval.init_kasa Remanent_parameters_sig.KaSim result in
	let () = Format.printf "+ Compiling...@." in
	let (env, cc_env, has_tracking, init_l) =
	  Eval.compile
	    ~pause:(fun f -> f ()) ~return:(fun x -> x)
	    ?rescale_init:!rescale ~outputs:(Outputs.go (Signature.create []))
	    sigs_nd tk_nd contact_map counter result' in
	(env, cc_env, updated_vars, has_tracking, init_l),[]
      | marshalized_file ->
	 try
	   let d = open_in_bin marshalized_file in
	   let () =
	     if !Parameter.inputKappaFileNames <> [] then
	       Format.printf
		 "+ Loading simulation package %s (kappa files are ignored)...@."
		 marshalized_file
	     else
	       Format.printf "+Loading simulation package %s...@."
			     marshalized_file in
	   let env,cc_env,updated_vars,has_tracking,init_l =
	     (Marshal.from_channel d :
		Environment.t*Connected_component.Env.t*int list*bool*
		  (Alg_expr.t * Primitives.elementary_rule * Location.t) list) in
	   let () = Pervasives.close_in d  in
	   let alg_overwrite =
	     List.map
               (fun (s,v) ->
		Environment.num_of_alg (Location.dummy_annot s) env,
		Alg_expr.CONST v)
               !Parameter.alg_var_overwrite in
	   let updated_vars' =
	     List.fold_left
	       (fun acc (i,_) -> i::acc) updated_vars alg_overwrite in
	   (env,cc_env,updated_vars',has_tracking,init_l),alg_overwrite
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
    let () =
      if has_tracking &&
	   not (!Parameter.causalModeOn || !Parameter.weakCompression ||
		  !Parameter.mazCompression || !Parameter.strongCompression)
      then
	ExceptionDefn.warning
	  (fun f ->
	   Format.fprintf
	     f
	     "An observable may be tracked but no compression level to render stories has been specified")
    in
    let (env,graph,state) =
      Eval.build_initial_state
	alg_overwrite counter env_store cc_env has_tracking updated_vars init_l in
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

    Kappa_files.setCheckFileExists() ;

    let () =
      let head =
	Environment.map_observables
	  (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
	  env in
      if !pointNumberValue > 0 || head <> [||] then
	Outputs.create_plot (Kappa_files.get_data ()) head in
    let () =
      if !pointNumberValue > 0 then
	Outputs.go (Environment.signatures env)
	  (Data.Plot
	     (Counter.current_time counter,
	      State_interpreter.observables_values env counter graph state)) in

    Parameter.initSimTime () ;
    let () =
      State_interpreter.loop
	~outputs:(Outputs.go (Environment.signatures env))
	~called_from:Remanent_parameters_sig.KaSim
	Format.std_formatter env cc_env counter graph state
    in
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
  | ExceptionDefn.UserInterrupted f ->
     let () = ExceptionDefn.flush_warning Format.err_formatter in
     let () = Kappa_files.close_all_out_desc () in
     let msg = f 0. 0 in
     let () =Format.eprintf "@.***Interrupted by user: %s***@." msg in
     exit 1
  | Sys_error msg ->
     let () = ExceptionDefn.flush_warning Format.err_formatter in
     let () = Kappa_files.close_all_out_desc () in
     let () = Format.eprintf "%s@." msg in
     exit 2
