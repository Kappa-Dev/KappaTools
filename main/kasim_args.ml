(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  mutable seedValue : int option;
  mutable unit : Cli_init.directive_unit;

  mutable marshalizeOutFile : string option;
  mutable domainOutputFile : string option;
  mutable traceFile : string option;
  mutable compileMode : bool;
  mutable maxSharing : bool;
  mutable showEfficiency : bool
}

let default : t = {
  seedValue  = None;
  unit = Cli_init.Time;

  marshalizeOutFile = None;
  domainOutputFile = None;
  traceFile = None;
  compileMode = false;
  maxSharing = false;
  showEfficiency = false;
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
  ("-e",
   Arg.Int (fun e ->
       raise (Arg.Bad ("Option '-e' has been replace by '-u event -l "^
                       string_of_int e^"'"))),"Deprecated option");
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
  ("--print-efficiency",
   Arg.Unit (fun () -> t.showEfficiency <- true),
   "KaSim tells how fast it runs") ;
  ("--max-sharing",
   Arg.Unit (fun () -> t.maxSharing <- true),
   "Initialization is heavier but simulation is faster");
  ("--compile",
   Arg.Unit (fun () -> t.compileMode <- true),
   "Display rule compilation as action list") ;
  ]
