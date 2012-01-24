open Tools
open Mods
open State
open Random_tree

let version = "2.01-110112"

let usage_msg = "KaSim "^version^": \n"^"Usage is KaSim -i input_file [-e events | -t time] [-p points] [-o output_file]\n"
let version_msg = "Kappa Simulator: "^version^"\n"

let close_desc () =
	List.iter (fun d -> close_out d) !Parameter.openOutDescriptors ;
	List.iter (fun d -> close_in d) !Parameter.openInDescriptors

let main =
	let options = [
		("--version", Arg.Unit (fun () -> print_string (version_msg^"\n") ; flush stdout ; exit 0), "display kaSim version");
		("-i", Arg.String (fun fic -> Parameter.inputKappaFileNames:= fic:: (!Parameter.inputKappaFileNames)),
			"name of a kappa file to use as input (can be used multiple times for multiple input files)");
		("-e", Arg.Int (fun i -> if i < 0 then Parameter.maxEventValue := None else Parameter.maxTimeValue:= None ; Parameter.maxEventValue := Some i) ,
			"Number of total simulation events, including null events (negative value for unbounded simulation)");
		("-t", Arg.Float(fun t -> Parameter.maxTimeValue := Some t ; Parameter.maxEventValue := None), "Max time of simulation (arbitrary time unit)");
		("-p", Arg.Int (fun i -> Parameter.plotModeOn := true ; Parameter.pointNumberValue:= Some i), "Number of points in plot");
		("-o", Arg.String (fun s -> Parameter.outputDataName:=s ), "file name for data output") ;
		("-d", 
		Arg.String 
			(fun s -> 
				try 
					if Sys.is_directory s then Parameter.outputDirName := s 
					else (Printf.eprintf "'%s' is not a directory\n" s ; exit 1)  
				with Sys_error msg -> (*directory does not exists*) 
					(Printf.eprintf "%s\n" msg ; exit 1)
			), "Specifies directory name where output file(s) should be stored") ;
		("-load-sim", Arg.String (fun file -> Parameter.marshalizedInFile := file) , "load simulation package instead of kappa files") ; 
		("-make-sim", Arg.String (fun file -> Parameter.marshalizedOutFile := file) , "save kappa files as a simulation package") ; 
		("-im", Arg.String (fun file -> Parameter.influenceFileName:=file), "produces the influence map of the model") ;
		("-flux", Arg.String (fun file -> Parameter.fluxFileName:=file ; Parameter.fluxModeOn := true), "will measure activation/inhibition fluxes during the simulation") ;
		("--cflow", Arg.Unit (fun _ -> Parameter.causalModeOn:=true), "Causality analysis mode") ;
		("--dot-output", Arg.Unit (fun () -> Parameter.dotOutput := true), "(no argument required) Dot format for outputting snapshots") ;
		("--implicit-signature", Arg.Unit (fun () -> Parameter.implicitSignature := true), "Program will guess agent signatures automatically") ;
		("-seed", Arg.Int (fun i -> Parameter.seedValue := Some i), "Seed for the random number generator") ;
		("--eclipse", Arg.Unit (fun () -> Parameter.eclipseMode:= true), "enable this flag for running KaSim behind eclipse plugin") ;
		("--compile", Arg.Unit (fun _ -> Parameter.compileModeOn := true), "Display rule compilation as action list") ;
		("--debug", Arg.Unit (fun () -> Parameter.debugModeOn:= true), "Enable debug mode") ;
		("--backtrace", Arg.Unit (fun () -> Parameter.backtrace:= true), "Backtracing exceptions") ;
		("--glutony", Arg.Unit (fun () -> Gc.set { (Gc.get()) with Gc.space_overhead = 500 (*default 80*) } ;), "Lower gc activity for a faster but memory intensive simulation") ;
		("-rescale-to", Arg.Int (fun i -> Parameter.rescale:=Some i), "Rescale initial concentration to given number for quick testing purpose") ; 
		]
	in
	try
		Arg.parse options (fun _ -> Arg.usage options usage_msg ; exit 1) usage_msg ;
		if not !Parameter.plotModeOn then ExceptionDefn.warning "No data points are required, use -p option for plotting data.";
		let abort =
			match !Parameter.inputKappaFileNames with
			| [] -> if !Parameter.marshalizedInFile = "" then true else false
			| _ -> false
		in
		if abort then (prerr_string usage_msg ; exit 1) ;
		let sigint_handle = fun _ ->
     raise (ExceptionDefn.UserInterrupted "abort signal received")
    in
    let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle sigint_handle) in
    
		Parameter.setOutputName() ;
		Parameter.checkFileExists() ;
		(*Printexc.record_backtrace !Parameter.backtrace ; (*Possible backtrace*)*)
		
		(*let _ = Printexc.record_backtrace !Parameter.debugModeOn in*) 
		let result =
			Ast.init_compil() ;
			List.iter (fun fic -> let _ = KappaLexer.compile fic in ()) !Parameter.inputKappaFileNames ;
			!Ast.result
		in
		let (_: unit) =
			match !Parameter.seedValue with
			| Some seed -> Random.init seed
			| None ->
					begin
						Printf.printf "+Self seeding...\n" ;
						Random.self_init() ;
						let i = Random.bits () in
						Random.init i ;
						Printf.printf "+Initialized random number generator with seed %d\n" i
					end
		in
		
		let counter =	Counter.create 0.0 0 !Parameter.maxTimeValue !Parameter.maxEventValue in
		
		let (env, state) = 
			match !Parameter.marshalizedInFile with
				| "" -> Eval.initialize result counter
				| marshalized_file ->
					try
						let d = open_in_bin marshalized_file in 
						begin
							if !Parameter.inputKappaFileNames <> [] then Printf.printf "+ Loading simulation package %s (kappa files are ignored)...\n" marshalized_file 
							else Printf.printf "+Loading simulation package %s...\n" marshalized_file ;
							let env,state = (Marshal.from_channel d : Environment.t * State.implicit_state) in
							Pervasives.close_in d ;
							Printf.printf "Done\n" ;
							(env,state) 
						end
					with
						| exn -> (Debug.tag "!Simulation package seems to have been created with a different version of KaSim, aborting..." ; exit 1) 
		in
		
		let (_:unit) = 
			match !Parameter.marshalizedOutFile with
				| "" -> ()
				| file -> 
					let d = open_out_bin file in
					begin
						Marshal.to_channel d (env,state) [Marshal.Closures] ;
						close_out d
					end
		in
		if !Parameter.influenceFileName <> ""  then 
			begin
				let desc = open_out !Parameter.influenceFileName in
				State.dot_of_influence_map desc state env ; 
				close_out desc 
			end ;  
		if !Parameter.compileModeOn then (Hashtbl.iter (fun i r -> Dynamics.dump r env) state.State.rules ; exit 0)
		else () ;
		let plot = Plot.create !Parameter.outputDataName
		and grid = 
			if !Parameter.causalModeOn then 
				let grid = Causal.empty_grid() in Causal.init state grid
			else Hashtbl.create 0
		in
		ExceptionDefn.flush_warning () ; 
		try
			Run.loop state grid counter plot env ;
			print_newline() ;
			Printf.printf "Simulation ended (eff.: %f)\n" 
			((float_of_int (Counter.event counter)) /. (float_of_int (Counter.null_event counter + Counter.event counter))) ;
			if !Parameter.fluxModeOn then 
				begin
					let d = open_out !Parameter.fluxFileName in
					State.dot_of_flux d state env ;
					close_out d
				end 
			else () ;
		with
			| Invalid_argument msg -> 
				begin
					(*if !Parameter.debugModeOn then (Debug.tag "State dumped! (dump.ka)" ; let desc = open_out "dump.ka" in State.snapshot state counter desc env ; close_out desc) ; *)
				  let s = (* Printexc.get_backtrace() *) "" in Printf.eprintf "\n***Runtime error %s***\n%s\n" msg s ;
					exit 1
				end
			| ExceptionDefn.UserInterrupted msg -> 
				begin
					flush stdout ; 
					Printf.eprintf "\n***%s: would you like to record the current state? (y/N)***\n" msg ; flush stderr ;
					(match String.lowercase (Tools.read_input ()) with
						| "y" | "yes" ->
							begin 
								let desc = open_out !Parameter.dumpFileName in 
								State.snapshot state counter desc !Parameter.snapshotHighres env ;
								Parameter.debugModeOn:=true ; State.dump state counter env ;
								close_out desc ;
								Printf.eprintf "Final state dumped (%s)\n" !Parameter.dumpFileName
							end
						| _ -> ()
					) ;
					close_desc() (*closes all other opened descriptors*)
				end
			| ExceptionDefn.Deadlock ->
				if !Parameter.dumpIfDeadlocked then	Graph.SiteGraph.to_dot state.graph "deadlock.dot" env ;
				(Printf.printf "?\nSimulation ended because a deadlock was reached (Activity = %f)\n" ((*Activity.total*) Random_tree.total state.activity_tree))
	with
	| ExceptionDefn.Semantics_Error (pos, msg) -> 
		(close_desc () ; Printf.eprintf "***Error (%s) line %d, char %d: %s***\n" (fn pos) (ln pos) (cn pos) msg)
	| Invalid_argument msg ->	(close_desc (); let s = "" (*Printexc.get_backtrace()*) in Printf.eprintf "\n***Runtime error %s***\n%s\n" msg s)
	| ExceptionDefn.UserInterrupted msg -> (Printf.eprintf "\n***Interrupted by user: %s***\n" msg ; close_desc())
	| ExceptionDefn.StopReached msg -> (Printf.eprintf "\n***%s***\n" msg ; close_desc())
	| Sys_error msg -> (close_desc (); Printf.eprintf "%s\n" msg)
