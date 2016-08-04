type t = {
  mutable seedValue           : int option;
  mutable maxEventValue       : int option;
  mutable interactive         : bool;

  mutable marshalizeOutFile : string option;
  mutable domainOutputFile : string option;
  mutable traceFile : string option;
  mutable eclipseMode : bool;
  mutable emacsMode : bool;
  mutable compileMode : bool;
}

let default : t = {
  seedValue  = None;
  maxEventValue = None;
  interactive = false;

  marshalizeOutFile = None;
  domainOutputFile = None;
  traceFile = None;
  eclipseMode = false;
  emacsMode = false;
  compileMode = false;
}

let options (t :t)  : (string * Arg.spec * string) list = [
  ("-e",
   Arg.Int
     (fun i ->
        if i < 0 then
          t.maxEventValue <- None
        else
          t.maxEventValue <- Some i),
   "Number of total simulation events, including null events (negative value for unbounded simulation)");
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
  ]
