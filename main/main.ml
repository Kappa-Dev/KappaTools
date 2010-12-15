open Misc
open Mods
open State
open Random_tree

let version = "1.05_151210"
let usage_msg = "KaSim "^version^": \n"^"Usage is KaSim -i input_file [-e events | -t time] [-p points] [-o output_file]\n"
let version_msg = "Kappa Simulator: "^version^"\n"

let close_desc () =
	List.iter (fun d -> close_out d) !Parameter.openOutDescriptors ;
	List.iter (fun d -> close_in d) !Parameter.openInDescriptors

let main =
	let options = [
		("--version", Arg.Unit (fun () -> print_string (version_msg^"\n") ; flush stdout ; exit 0), "display kaSim version");
		("-i", Arg.String (fun fic -> FileName.input:= fic:: (!FileName.input)),
			"name of a kappa file to use as input (can be used multiple times for multiple input files)");
		("-e", Arg.Int (fun i -> if i < 0 then Parameter.maxEventValue := None else Parameter.maxTimeValue:= None ; Parameter.maxEventValue := Some i) ,
			"Number of total simulation events, including null events (negative value for unbounded simulation)");
		("-t", Arg.Float(fun t -> Parameter.maxTimeValue := Some t ; Parameter.maxEventValue := None), "Max time of simulation (arbitrary time unit)");
		("-p", Arg.Int (fun i -> Parameter.plotModeOn := true ; Parameter.pointNumberValue:= Some i), "Number of points in plot");
		("-o", Arg.String (fun s -> 
			if Sys.file_exists s then 
				begin
					Printf.printf "File '%s' already exists do you want to erase (y/n)? " s ; flush stdout ;
					Scanf.scanf "%s" (fun answer -> if answer="y" then FileName.output:=s else exit 1)
				end
			else FileName.output:=s ), "file name for data output") ;
		("-d", 
		Arg.String 
			(fun s -> 
				try 
					if Sys.is_directory s then Parameter.outputDirName := s 
					else (Printf.eprintf "'%s' is not a directory\n" s ; exit 1)  
				with Sys_error msg -> (*directory does not exists*) 
					(Printf.eprintf "%s\n" msg ; exit 1)
			), "Specifies directory name where output file(s) should be stored") ;
		("--dot-output", Arg.Unit (fun () -> Parameter.dotOutput := true), "Dot format for outputting snapshots") ;
		("--implicit-signature", Arg.Unit (fun () -> Parameter.implicitSignature := true), "Program will guess agent signatures automatically") ;
		("--seed", Arg.Int (fun i -> Parameter.seedValue := Some i), "Seed for the random number generator") ;
		("--compile", Arg.Unit (fun _ -> Parameter.compileModeOn := true), "Display rule compilation as action list") ;
		("--debug", Arg.Unit (fun () -> Parameter.debugModeOn:= true), "Enable debug mode")
		]
	in
	(*Gc.set { (Gc.get()) with Gc.space_overhead = 500 (*default 80*) } ;*)
	try
		Arg.parse options (fun _ -> Arg.usage options usage_msg ; exit 1) usage_msg ;
		let abort =
			match !FileName.input with
			| [] -> true
			| _ -> false
		in
		if abort then (prerr_string usage_msg ; exit 1) ;
		let sigint_handle = fun _ ->
     raise (ExceptionDefn.UserInterrupted "abort signal received")
    in
    let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle sigint_handle) in
      
		let result =
			Ast.init_compil() ;
			List.iter (fun fic -> let _ = KappaLexer.compile fic in ()) !FileName.input ;
			!Ast.result
		in
		let (_: unit) =
			match !Parameter.seedValue with
			| Some seed -> Random.init seed
			| None ->
					begin
						Printf.printf "Self seeding...\n" ;
						Random.self_init() ;
						let i = Random.bits () in
						Random.init i ;
						Printf.printf "Initialized random number generator with seed %d\n" i
					end
		in
		let (env, state, counter) = Eval.initialize result in
		if !Parameter.compileModeOn then (Hashtbl.iter (fun i r -> Dynamics.dump r env) state.State.rules ; exit 0)
		else () ;
		let plot = Plot.create !FileName.output
		in
		try
			Run.loop state counter plot env ;
			print_newline() ;
			Printf.printf "Simulation ended (eff.: %f)\n" 
			((float_of_int (Counter.event counter)) /. (float_of_int (Counter.null_event counter + Counter.event counter))) ;
		with
			| ExceptionDefn.UserInterrupted msg -> 
				begin
					Printf.eprintf "\n***%s: state dumped (%s)***\n" msg Parameter.dumpFileName ; 
					let desc = open_out Parameter.dumpFileName in 
						State.snapshot state counter desc env ;
						close_out desc ;
						close_desc() (*closes all other opened descriptors*)
				end
			| ExceptionDefn.Deadlock ->
				if !Parameter.dumpIfDeadlocked then	Graph.SiteGraph.to_dot state.graph "deadlock.dot" env ;
				(Printf.printf "?\nSimulation ended because a deadlock was reached (Activity = %f)\n" ((*Activity.total*) Random_tree.total state.activity_tree))
	with
	| ExceptionDefn.Semantics_Error (pos, msg) -> 
		(close_desc () ; Printf.eprintf "***Error (%s) line %d, char %d: %s***\n" (fn pos) (ln pos) (cn pos) msg)
	| Invalid_argument msg -> (close_desc (); Printf.eprintf "\n***Runtime error %s***\n" msg)
	| ExceptionDefn.UserInterrupted msg -> (Printf.eprintf "\n***Interrupted by user: %s***\n" msg ; close_desc())
	| Sys_error msg -> (close_desc (); Printf.eprintf "%s\n" msg)