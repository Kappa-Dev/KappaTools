open Mods

let version = "4.0-refactoring"

let usage_msg =
  "KaSim "^version^":\n"^
    "Usage is KaSim [-i] input_file [-e events | -t time] [-p points] [-o output_file]\n"
let version_msg = "Kappa Simulator: "^version^"\n"

let close_desc opt_env =
  let () = Kappa_files.close_all_out_desc () in
  List.iter (fun d -> close_in d) !Parameter.openInDescriptors ;
  match opt_env with
  | None -> ()
  | Some env -> Environment.close_desc env

let () =
  let options = [
    ("--version",
     Arg.Unit (fun () -> print_string (version_msg^"\n") ; flush stdout ; exit 0),
     "display KaSim version");
    ("-i",
     Arg.String (fun fic ->
		 Parameter.inputKappaFileNames:= fic:: (!Parameter.inputKappaFileNames)),
     "name of a kappa file to use as input (can be used multiple times for multiple input files)");
    ("-e",
     Arg.Int (fun i -> if i < 0 then Parameter.maxEventValue := None
		       else let () = Parameter.maxTimeValue:= None in
			    Parameter.maxEventValue := Some i),
     "Number of total simulation events, including null events (negative value for unbounded simulation)");
    ("-t",
     Arg.Float(fun t -> Parameter.maxTimeValue := Some t ;
			Parameter.maxEventValue := None),
     "Max time of simulation (arbitrary time unit)");
    ("-p", Arg.Set_int Parameter.pointNumberValue,
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
    ("--implicit-signature",
     Arg.Unit (fun () ->
	       Format.eprintf "--implicitSignature is currently unplugged.@.";
	       Parameter.implicitSignature := true),
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
    ("-rescale-to", Arg.Int (fun i -> Parameter.rescale:=Some i),
     "Rescale initial concentration to given number for quick testing purpose");
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
      Ast.init_compil() ;
      List.iter (fun fic -> KappaLexer.compile Format.std_formatter fic)
		!Parameter.inputKappaFileNames ;
      !Ast.result in

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

    let (env, counter, state) =
      match !Parameter.marshalizedInFile with
      | "" ->
	 Eval.initialize Format.std_formatter !Parameter.alg_var_overwrite result
      | marshalized_file ->
	 try
	   let d = open_in_bin marshalized_file in
	   begin
	     if !Parameter.inputKappaFileNames <> [] then
	       Format.printf
		 "+ Loading simulation package %s (kappa files are ignored)...@."
		 marshalized_file
	     else
	       Format.printf "+Loading simulation package %s...@."
			     marshalized_file;
	     let env,counter,state =
	       (Marshal.from_channel d : Environment.t * Counter.t * State.t) in
	     Pervasives.close_in d ;
	     Format.printf "Done@." ;
	     (env,counter,state)
	   end
	 with
	 | _exn ->
	    Debug.tag
	      Format.std_formatter
	      "!Simulation package seems to have been created with a different version of KaSim, aborting...";
	    exit 1
    in

    Kappa_files.setCheckFileExists() ;

    let () = Plot.create (Kappa_files.get_data ()) in
    let () = if !Parameter.pointNumberValue > 0 then
	       Plot.plot_now env counter state in

    let () = Kappa_files.with_marshalized
	       (fun d -> Marshal.to_channel
			   d (env,counter,state) [Marshal.Closures]) in

    Kappa_files.with_influence
      (fun d -> State.dot_of_influence_map d state env);
    if !Parameter.compileModeOn
    then (State.dump_rules Format.err_formatter state env; exit 0);
    let profiling = Compression_main.init_secret_log_info () in
    let _grid,profiling,event_list =
      if Environment.tracking_enabled env then
	let () =
	  if !Parameter.mazCompression
	     || !Parameter.weakCompression
	     || !Parameter.strongCompression then ()
	  else (
	    ExceptionDefn.warning
	      (fun f ->
	       Format.pp_print_string
		 f "Causal flow compution is required but no compression is specified, will output flows with no compresion");
	    Parameter.mazCompression := true)
	in
	let grid = Causal.empty_grid() in
        let event_list = [] in
        let profiling,event_list =
          Compression_main.secret_store_init profiling state event_list in
        grid,profiling,event_list
      else (Causal.empty_grid(),profiling,[])
    in
    ExceptionDefn.flush_warning Format.err_formatter ;
    Parameter.initSimTime () ;
    let () =
      try Run.loop Format.std_formatter state profiling event_list counter env
      with
      | ExceptionDefn.UserInterrupted f ->
	 begin
	   let () = Format.print_newline() in
	   let msg = f (Counter.time counter) (Counter.event counter) in
	   let () =
	     Format.eprintf
	       "@.***%s: would you like to record the current state? (y/N)***"
	       msg in
	   let () = close_desc (Some env) in
	   (*closes all other opened descriptors*)
	   if !Parameter.batchmode
	   then raise (ExceptionDefn.UserInterrupted f)
	   else
	     match String.lowercase (Tools.read_input ()) with
	     | ("y" | "yes") ->
		let () =  Parameter.dotOutput := false in
		Kappa_files.with_dump
		  (fun desc ->
		   State.snapshot
		     state counter desc !Parameter.snapshotHighres env;
		   Parameter.debugModeOn:=true;
		   State.dump state counter env)
	     | _ -> ()
	 end
      | ExceptionDefn.Deadlock ->
	 Format.printf
	   "?@.A deadlock was reached after %d events and %Es (Activity = %.5f)"
	   (Counter.event counter) (Counter.time counter)
	   (State.total_activity state) in
    Format.print_newline() ;
    Format.printf "Simulation ended";
    if Counter.null_event counter = 0 then Format.print_newline()
    else
      let () =
	Format.printf " (eff.: %f, detail below)@."
		      ((float_of_int (Counter.event counter)) /.
			 (float_of_int
			    (Counter.null_event counter + Counter.event counter))) in
      Array.iteri
	(fun i n ->
	 match i with
	 | 0 ->
	    Format.printf "\tValid embedding but no longer unary when required: %f@."
			  ((float_of_int n) /. (float_of_int (Counter.null_event counter)))
	 | 1 ->
	    Format.printf "\tValid embedding but not binary when required: %f@."
			  ((float_of_int n) /. (float_of_int (Counter.null_event counter)))
	 | 2 ->
	    Format.printf "\tClashing instance: %f@."
			  ((float_of_int n) /. (float_of_int (Counter.null_event counter)))
	 | 3 ->
	    Format.printf "\tLazy negative update: %f@."
			  ((float_of_int n) /. (float_of_int (Counter.null_event counter)))
	 | 4 -> Format.printf "\tLazy negative update of non local instances: %f@."
			      ((float_of_int n) /. (float_of_int (Counter.null_event counter)))
	 | 5 -> Format.printf "\tPerturbation interrupting time advance: %f@."
			      ((float_of_int n) /. (float_of_int (Counter.null_event counter)))
	 |_ -> Format.printf "\tna@."
	) counter.Counter.stat_null ;
      if !Parameter.fluxModeOn then
	Kappa_files.with_flux "" (fun d -> State.dot_of_flux d state env)
  with
  | ExceptionDefn.Malformed_Decl er ->
     let () = close_desc None in
     let () = Pp.error Format.pp_print_string er in
     exit 2
  | ExceptionDefn.Internal_Error er ->
     let () = close_desc None in
     let () =
       Pp.error
	 (fun f x -> Format.fprintf f "Internal Error (please report):@ %s" x)
	 er in
     exit 2
  | Invalid_argument msg ->
     let () = close_desc None in
     let s = "" (*Printexc.get_backtrace()*) in
     let () = Format.eprintf "@.@[<v>***Runtime error %s***@,%s@]@." msg s in
    exit 2
  | ExceptionDefn.UserInterrupted f ->
     let () = close_desc None in
     let msg = f 0. 0 in
     let () =Format.eprintf "@.***Interrupted by user: %s***@." msg in
     exit 2
  | Sys_error msg ->
     let () = close_desc None in
     let () = Format.eprintf "%s@." msg in
     exit 2
