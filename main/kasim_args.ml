type t = {
  mutable seedValue : int option;
  mutable unit : Cli_init.directive_unit;

  mutable marshalizeOutFile : string option;
  mutable domainOutputFile : string option;
  mutable traceFile : string option;
  mutable eclipseMode : bool;
  mutable compileMode : bool;
}

let default : t = {
  seedValue  = None;
  unit = Cli_init.Time;

  marshalizeOutFile = None;
  domainOutputFile = None;
  traceFile = None;
  eclipseMode = false;
  compileMode = false;
}

let options (t :t)  : (string * Arg.spec * string) list = [
  ("-u",
   Arg.String
     (function
       | "time" | "Time" | "t" -> t.unit <- Cli_init.Time
       | "event" | "events" | "e" | "Event" | "Events" -> t.unit <-
           Cli_init.Event
       | s -> raise (Arg.Bad ("Unrecognized unit: "^s))),
   "unit (time/event) in which limit and plot period are specified");
  ("-make-sim",
   Arg.String
     (fun marshalizeOutFile -> t.marshalizeOutFile <- Some marshalizeOutFile),
   "save kappa files as a simulation package") ;
  ("-dump-cc",
   Arg.String
     (fun domainOutputFile -> t.domainOutputFile <- Some domainOutputFile),
   "file name for dumping the domain of observables") ;
  ("-trace",
   Arg.String
     (fun traceFile -> t.traceFile <- Some traceFile),
   "file name for dumping the simulation trace") ;
  ("-seed", Arg.Int (fun i -> t.seedValue <- Some i),
   "Seed for the random number generator") ;
  ("--eclipse",
   Arg.Unit
     (fun () -> t.eclipseMode <- true),
   "enable this flag for running KaSim behind eclipse plugin") ;
  ("--compile",
   Arg.Unit
     (fun () -> t.compileMode <- true),
   "Display rule compilation as action list") ;
  ]
