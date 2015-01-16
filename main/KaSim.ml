open Tools
open Mods
open Random_tree

let version = "4.0-refactoring"

let usage_msg =
  "KaSim "^version^":\n"^
    "Usage is KaSim -i input_file [-e events | -t time] [-p points] [-o output_file]\n"
let version_msg = "Kappa Simulator: "^version^"\n"

let checkFileExists () =
  let check file =
    match file with
    | "" -> ()
    | file ->
       let file = Tools.kasim_path file in
       if Sys.file_exists file then
	 let () =
	   Format.eprintf
	     "File '%s' already exists do you want to erase (y/N)?@." file in
	 let answer = Tools.read_input () in
	 if answer<>"y" then exit 1
  in
  check !Parameter.influenceFileName ;
  check !Parameter.fluxFileName ;
  check !Parameter.marshalizedOutFile ;
  if !Parameter.pointNumberValue > 0 then check !Parameter.outputDataName

let close_desc opt_env =
  List.iter (fun d -> close_out d) !Parameter.openOutDescriptors ;
  List.iter (fun d -> close_in d) !Parameter.openInDescriptors ;
  match opt_env with
  | None -> ()
  | Some env -> Environment.close_desc env

let main =
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
		       else Parameter.maxTimeValue:= None ;
		       Parameter.maxEventValue := Some i),
     "Number of total simulation events, including null events (negative value for unbounded simulation)");
    ("-t",
     Arg.Float(fun t -> Parameter.maxTimeValue := Some t ;
			Parameter.maxEventValue := None),
     "Max time of simulation (arbitrary time unit)");
    ("-p", Arg.Int (fun i -> Parameter.pointNumberValue:= i),
     "Number of points in plot");
    ("-o", Arg.String (fun s -> Parameter.outputDataName:=s ),
     "file name for data output") ;
    ("-d",
     Arg.String
       (fun s ->
	let () = try
	    if not (Sys.is_directory s)
	    then (Format.eprintf "'%s' is not a directory@." s ; exit 1)
	  with Sys_error msg -> Tools.mk_dir_r s in
	Parameter.outputDirName := s
       ), "Specifies directory name where output file(s) should be stored") ;
    ("-load-sim", Arg.String (fun file -> Parameter.marshalizedInFile := file),
     "load simulation package instead of kappa files") ;
    ("-make-sim", Arg.String (fun file -> Parameter.marshalizedOutFile := file),
     "save kappa files as a simulation package") ;
    ("--implicit-signature",
     Arg.Unit (fun () ->
	       Format.eprintf "--implicitSignature is currently unplugged.@.";
	       Parameter.implicitSignature := true),
     "Program will guess agent signatures automatically") ;
    ("-seed", Arg.Int (fun i -> Parameter.seedValue := Some i),
     "Seed for the random number generator") ;
    ("--eclipse", Arg.Unit (fun () -> Parameter.eclipseMode:= true),
     "enable this flag for running KaSim behind eclipse plugin") ;
    ("--emacs-mode", Arg.Unit (fun () -> Parameter.emacsMode:= true),
     "enable this flag for running KaSim using emacs-mode") ;
    ("--compile", Arg.Unit (fun _ -> Parameter.compileModeOn := true),
     "Display rule compilation as action list") ;
    ("--debug", Arg.Unit (fun () -> Parameter.debugModeOn:= true),
     "Enable debug mode") ;
    ("--safe", Arg.Unit (fun () -> Parameter.safeModeOn:= true),
     "Enable safe mode") ;
    ("--backtrace", Arg.Unit (fun () -> Parameter.backtrace:= true),
     "Backtracing exceptions") ;
    ("--gluttony",
     Arg.Unit (fun () -> Gc.set { (Gc.get()) with
				  Gc.space_overhead = 500 (*default 80*) } ;),
     "Lower gc activity for a faster but memory intensive simulation") ;
    ("-rescale-to", Arg.Int (fun i -> Parameter.rescale:=Some i),
     "Rescale initial concentration to given number for quick testing purpose");
  ]
  in
  try
    Arg.parse options (fun _ -> Arg.usage options usage_msg ; exit 1) usage_msg;
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
    let result =
      Ast.init_compil() ;
      List.iter (fun fic -> KappaLexer.compile fic)
		!Parameter.inputKappaFileNames ;
      !Ast.result in
    let () =
      match !Parameter.seedValue with
      | Some seed -> Random.init seed
      | None ->
	 begin
	   Format.printf "+ Self seeding...@." ;
	   Random.self_init() ;
	   let i = Random.bits () in
	   Random.init i ;
	   Format.printf
	     "+ Initialized random number generator with seed %d@." i
	 end
    in

    let counter =
      Counter.create 0.0 0 !Parameter.maxTimeValue !Parameter.maxEventValue in
    let (env, state) =
      match !Parameter.marshalizedInFile with
      | "" -> Eval.initialize result counter
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
	     let env,state = (Marshal.from_channel d : Environment.t * State.t) in
	     Pervasives.close_in d ;
	     Format.printf "Done@." ;
	     (env,state)
	   end
	 with
	 | exn ->
	    Debug.tag "!Simulation package seems to have been created with a different version of KaSim, aborting...";
	    exit 1
    in

    Parameter.setOutputName ();
    checkFileExists() ;

    let () =
      if !Parameter.pointNumberValue > 0 then
	Plot.create !Parameter.outputDataName env state counter
      else
	ExceptionDefn.warning
	  (fun f ->
	   Format.fprintf
	     f "No data points are required,@ use -p option for plotting data.")
    in

    let () =
      match !Parameter.marshalizedOutFile with
      | "" -> ()
      | file ->
	 let d = open_out_bin (kasim_path file) in
	 begin
	   Marshal.to_channel d (env,state) [Marshal.Closures] ;
	   close_out d
	 end
    in
    if !Parameter.influenceFileName <> ""  then
      begin
	let desc = Tools.kasim_open_out !Parameter.influenceFileName in
	State.dot_of_influence_map (Format.formatter_of_out_channel desc) state env; 
	close_out desc
      end ;
    if !Parameter.compileModeOn then (State.dump_rules Format.err_formatter state env; exit 0);
    let profiling = Compression_main.D.S.PH.B.PB.CI.Po.K.P.init_log_info () in
    let grid,profiling,event_list =
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
          Compression_main.D.S.PH.B.PB.CI.Po.K.store_init profiling state event_list in 
        grid,profiling,event_list
      else (Causal.empty_grid(),profiling,[])
    in
    ExceptionDefn.flush_warning () ;
    Parameter.initSimTime () ;
    try
      Run.loop state profiling event_list counter env ;
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
	begin
	  let d = kasim_open_out !Parameter.fluxFileName in
	  State.dot_of_flux (Format.formatter_of_out_channel d) state env ;
	  close_out d
	end
    with
    | Invalid_argument msg ->
       begin
	 (*if !Parameter.debugModeOn then (Debug.tag "State dumped! (dump.ka)";
 let desc = kasim_open_out "dump.ka" in State.snapshot state counter desc env;
 close_out desc); *)
	 let s = (* Printexc.get_backtrace() *) "" in
	 Format.eprintf "@.***Runtime error %s***@,%s@." msg s ;
	 exit 1
       end
    | ExceptionDefn.UserInterrupted f ->
       begin
	 flush stdout ;
	 let msg = f (Counter.time counter) (Counter.event counter) in
	 Format.eprintf
	   "@.***%s: would you like to record the current state? (y/N)***@."
	   msg;
	 (match String.lowercase (Tools.read_input ()) with
	  | ("y" | "yes") ->
	     begin
	       Parameter.dotOutput := false ;
	       let desc = kasim_open_out !Parameter.dumpFileName in
	       State.snapshot state counter desc !Parameter.snapshotHighres env;
	       Parameter.debugModeOn:=true ; State.dump state counter env ;
	       close_out desc ;
	       Format.eprintf "Final state dumped (%s)@." !Parameter.dumpFileName
	     end
	  | _ -> ()
	 ) ;
	 close_desc (Some env) (*closes all other opened descriptors*)
       end
    | ExceptionDefn.Deadlock ->
       Format.printf
	 "?@.A deadlock was reached after %d events and %Es (Activity = %.5f)@."
	 (Counter.event counter) (Counter.time counter)
	 (State.total_activity state)
  with
  | ExceptionDefn.Semantics_Error (pos, msg) ->
     (close_desc None;
      Format.eprintf "***Error (%s) line %d, char %d: %s***@."
		     (fn pos) (ln pos) (cn pos) msg)
  | ExceptionDefn.Malformed_Decl er -> Pp.error Format.pp_print_string er
  | Invalid_argument msg ->
     (close_desc None;
      let s = "" (*Printexc.get_backtrace()*) in
      Format.eprintf "@.@[<v>***Runtime error %s***@,%s@]@." msg s)
  | ExceptionDefn.UserInterrupted f ->
     let msg = f 0. 0 in
     let () =Format.eprintf "@.***Interrupted by user: %s***@." msg in
     close_desc None
  | ExceptionDefn.StopReached msg ->
     (Format.eprintf "@.***%s***@." msg ; close_desc None)
  | Sys_error msg -> (close_desc None; Format.eprintf "%s@." msg)
