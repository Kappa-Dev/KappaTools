(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type directive_unit = Time | Event

type t = {
  mutable alg_var_overwrite: (string * Nbr.t) list;
  mutable marshalizedInFile: string;
  mutable initialMix: string option;
  mutable rescale: float option;
  mutable seedValue: int option;
  mutable unit: directive_unit;
  mutable marshalizeOutFile: string option;
  mutable domainOutputFile: string option;
  mutable traceFile: string option;
  mutable logFile: string option;
  mutable compile_mode: bool;
  mutable sharing: Pattern.sharing_level;
  mutable showEfficiency: bool;
  mutable timeIndependent: bool;
}

let default : t =
  {
    alg_var_overwrite = [];
    rescale = None;
    marshalizedInFile = "";
    initialMix = None;
    seedValue = None;
    unit = Time;
    marshalizeOutFile = None;
    domainOutputFile = None;
    traceFile = None;
    logFile = Some "inputs";
    compile_mode = false;
    sharing = Pattern.Compatible_patterns;
    showEfficiency = false;
    timeIndependent = false;
  }

let options (t : t) : (string * Arg.spec * string) list =
  [
    ( "-mixture",
      Arg.String (fun fic -> t.initialMix <- Some fic),
      "Take the initial state from this file (ignore %init from other files)" );
    ( "-var",
      Arg.Tuple
        (let tmp_var_name = ref "" in
         [
           Arg.String (fun name -> tmp_var_name := name);
           Arg.String
             (fun var_val ->
               t.alg_var_overwrite <-
                 ( !tmp_var_name,
                   try Nbr.of_string var_val
                   with Failure _ ->
                     raise
                       (Arg.Bad ("\"" ^ var_val ^ "\" is not a valid value")) )
                 :: t.alg_var_overwrite);
         ]),
      "Set a variable to a given value" );
    ( "-load-sim",
      Arg.String (fun file -> t.marshalizedInFile <- file),
      "load simulation package instead of kappa files" );
    ( "-rescale",
      Arg.Float (fun i -> t.rescale <- Some i),
      "Apply rescaling factor to initial condition" );
    ( "-u",
      Arg.String
        (function
        | "time" | "Time" | "t" -> t.unit <- Time
        | "event" | "events" | "e" | "Event" | "Events" -> t.unit <- Event
        | s -> raise (Arg.Bad ("Unrecognized unit: " ^ s))),
      "unit (time/event) in which limit and plot period are specified" );
    ( "-e",
      Arg.Int
        (fun e ->
          raise
            (Arg.Bad
               ("Option '-e' has been replace by '-u event -l "
              ^ string_of_int e ^ "'"))),
      "Deprecated option" );
    ( "-make-sim",
      Arg.String
        (fun marshalizeOutFile -> t.marshalizeOutFile <- Some marshalizeOutFile),
      "save kappa files as a simulation package" );
    ( "-dump-cc",
      Arg.String
        (fun domainOutputFile -> t.domainOutputFile <- Some domainOutputFile),
      "file name for dumping the domain of observables" );
    ( "-trace",
      Arg.String (fun traceFile -> t.traceFile <- Some traceFile),
      "file name for dumping the simulation trace" );
    ( "--time-independent",
      Arg.Unit (fun () -> t.timeIndependent <- true),
      "Disable the use of time is story heuritics (for test suite)" );
    ( "-seed",
      Arg.Int (fun i -> t.seedValue <- Some i),
      "Seed for the random number generator" );
    ( "--print-efficiency",
      Arg.Unit (fun () -> t.showEfficiency <- true),
      "KaSim tells how fast it runs" );
    ( "-sharing",
      Arg.String
        (function
        | "no" | "none" | "None" -> t.sharing <- Pattern.No_sharing
        | "Compatible" -> t.sharing <- Pattern.Max_sharing
        | "max" | "Max" -> t.sharing <- Pattern.Max_sharing
        | s -> raise (Arg.Bad ("Unrecognized sharing level: " ^ s))),
      "Level of sharing computed between patterns during initialization \
       (None/Compatible/Max)" );
    ( "--compile",
      Arg.Unit (fun () -> t.compile_mode <- true),
      "Display rule compilation as action list" );
    ( "-log",
      Arg.String (fun logFile -> t.logFile <- Some logFile),
      "file name of the file to regenerate the exact same simulation" );
    ( "--no-log",
      Arg.Unit (fun () -> t.logFile <- None),
      "Do not generate a file to redo the exact same simulation" );
  ]
