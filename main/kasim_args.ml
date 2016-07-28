type t = { mutable alg_var_overwrite   : (string * Nbr.t) list;
	   mutable seedValue           : int option;
	   mutable maxEventValue       : int option;
	   mutable maxTimeValue        : float option;
	   mutable pointNumberValue    : int;
	   mutable rescale             : float option;
           mutable interactive         : bool;
	   mutable marshalizedInFile   : string;
           mutable inputKappaFileNames : string list;
           mutable outputDataFile      : string;

           mutable outputDirectory : string;
           mutable marshalizeOutFile : string option;
           mutable domainOutputFile : string option;
           mutable traceFile : string option;
           mutable eclipseMode : bool;
           mutable emacsMode : bool;
           mutable compileMode : bool;
           mutable batchmode : bool;
         }

let default : t = { alg_var_overwrite = [];
	            seedValue  = None;
	            maxEventValue = None;
	            maxTimeValue = None;
	            pointNumberValue = -1;
	            rescale = None;
                    interactive = false;
	            marshalizedInFile = "";
                    inputKappaFileNames = [];
                    outputDataFile = "data.out";

                    outputDirectory = ".";
                    marshalizeOutFile = None;
                    domainOutputFile = None;
                    traceFile = None;
                    eclipseMode = false;
                    emacsMode = false;
                    compileMode = false;
                    batchmode  = false;

                  }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("-i",
   Arg.String (fun fic ->
       t.inputKappaFileNames <- fic::t.inputKappaFileNames),
   "name of a kappa file to use as input (can be used multiple times for multiple input files)");
  ("-e",
   Arg.Int
     (fun i ->
        if i < 0 then
          t.maxEventValue <- None
        else
          t.maxEventValue <- Some i),
   "Number of total simulation events, including null events (negative value for unbounded simulation)");
  ("-t",
   Arg.Float(fun time -> t.maxTimeValue <- Some time),
   "Max time of simulation (arbitrary time unit)");
  ("-p",
   Arg.Int(fun pointNumberValue -> t.pointNumberValue <- pointNumberValue),
   "Number of points in plot");
  ("-var",
   Arg.Tuple
   (let tmp_var_name = ref "" in
    [Arg.String (fun name -> tmp_var_name := name);
     Arg.String (fun var_val ->
        t.alg_var_overwrite <-
	  (!tmp_var_name,
	      try Nbr.of_string var_val with
		Failure _ ->
		raise (Arg.Bad ("\""^var_val^"\" is not a valid value")))
	       ::t.alg_var_overwrite)]),
     "Set a variable to a given value");
    ("-o", Arg.String (fun outputDataFile -> t.outputDataFile <- outputDataFile),
     "file name for data output") ;
    ("-d",
     Arg.String (fun outputDirectory -> t.outputDirectory <- outputDirectory),
     "Specifies directory name where output file(s) should be stored") ;
  ("-load-sim",
   Arg.String (fun file -> t.marshalizedInFile <- file),
   "load simulation package instead of kappa files") ;
  ("-make-sim",
   Arg.String
     (fun marshalizeOutFile -> t.marshalizeOutFile <- Some marshalizeOutFile),
   "save kappa files as a simulation package") ;
  ("-dump-cc",
   Arg.String
     (fun domainOutputFile -> t.domainOutputFile <- Some domainOutputFile),
   "file name for dumping the domain of observables") ;
  ("-dump-trace",
   Arg.String
     (fun traceFile -> t.traceFile <- Some traceFile),
   "file name for dumping the simulation trace") ;
  ("--interactive",
   Arg.Unit
     (fun () -> t.interactive <- true),
   "Run interactively") ;
  ("-seed", Arg.Int (fun i -> t.seedValue <- Some i),
   "Seed for the random number generator") ;
  ("--eclipse",
   Arg.Unit
     (fun () -> t.eclipseMode <- true),
   "enable this flag for running KaSim behind eclipse plugin") ;
  ("--emacs-mode",
   Arg.Unit
     (fun () -> t.emacsMode <- true),
   "enable this flag for running KaSim using emacs-mode") ;
  ("--compile",
   Arg.Unit
     (fun () -> t.compileMode <- true),
   "Display rule compilation as action list") ;
  ("--batch",
   Arg.Unit
     (fun () -> t.batchmode <- true),
   "Set non interactive mode (always assume default answer)") ;
  ("-rescale", Arg.Float (fun i -> t.rescale <- Some i),
   "Apply rescaling factor to initial condition")
  ]
