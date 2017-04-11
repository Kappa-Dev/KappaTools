(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  mutable alg_var_overwrite   : (string * Nbr.t) list;
  mutable minValue            : float option;
  mutable maxValue            : float option;
  mutable plotPeriod          : float option;
  mutable rescale             : float option;
  mutable marshalizedInFile   : string;
  mutable inputKappaFileNames : string list;
  mutable outputDataFile      : string option;
  mutable outputDirectory     : string;
  mutable batchmode           : bool;
  mutable interactive         : bool;
  mutable newSyntax          : bool;
}

type t_gui =
  {
    alg_var_overwrite_gui   : (string * string) list ref;
    minValue_gui            : float option ref;
    maxValue_gui            : float option ref;
    plotPeriod_gui          : float option ref;
    rescale_gui             : float option ref;
    marshalizedInFile_gui   : string ref;
    inputKappaFileNames_gui : string list ref;
    (*  initialMix_gui          : string option ref;*)
    outputDataFile_gui      : string option ref;
    outputDirectory_gui     : string ref;
    batchmode_gui           : string ref;
    newSyntax_gui          : bool ref;
  }

let default : t = {
  alg_var_overwrite = [];
  minValue = None ;
  maxValue = None;
  plotPeriod = None;
  rescale = None;
  marshalizedInFile = "";
  inputKappaFileNames = [];
  outputDataFile = None;
  outputDirectory = ".";
  batchmode  = false;
  interactive = false;
  newSyntax = false;
}

let default_gui =
  {
    alg_var_overwrite_gui = ref[];
    minValue_gui = ref None;
    maxValue_gui = ref None;
    plotPeriod_gui = ref None;
    rescale_gui = ref None;
    marshalizedInFile_gui = ref "";
    inputKappaFileNames_gui = ref [];
    (*  initialMix_gui = ref None;*)
    outputDataFile_gui = ref None;
    outputDirectory_gui = ref ".";
    batchmode_gui  = ref "";
    newSyntax_gui = ref false;
  }

let rec aux l accu =
  match l with
  | (v,var_val)::tail ->
    aux tail
      ((v,
        (try Nbr.of_string var_val with
           Failure _ ->
           raise (Arg.Bad ("\""^var_val^"\" is not a valid value"))))   ::accu)
  | [] -> accu

let get_from_gui t_gui =
  {
    alg_var_overwrite =
     aux (List.rev (!(t_gui.alg_var_overwrite_gui))) [];
    minValue = !(t_gui.minValue_gui);
    maxValue = !(t_gui.maxValue_gui);
    plotPeriod = !(t_gui.plotPeriod_gui);
    rescale = !(t_gui.rescale_gui);
    marshalizedInFile = !(t_gui.marshalizedInFile_gui);
    inputKappaFileNames = !(t_gui.inputKappaFileNames_gui);
    (*initialMix = !(t_gui.initialMix_gui);*)
    outputDataFile = !(t_gui.outputDataFile_gui);
    outputDirectory = !(t_gui.outputDirectory_gui);
    batchmode  = (Tools.lowercase (!(t_gui.batchmode_gui)))="batch" ;
    interactive = (Tools.lowercase (!(t_gui.batchmode_gui)))="interactive";
    newSyntax = !(t_gui.newSyntax_gui);
  }

let copy_from_gui t_gui t =
  let t_tmp = get_from_gui t_gui in
  t.alg_var_overwrite <- t_tmp.alg_var_overwrite;
  t.minValue <- t_tmp.minValue;
  t.maxValue <- t_tmp.maxValue;
  t.plotPeriod <- t_tmp.plotPeriod;
  t.rescale <- t_tmp.rescale;
  t.marshalizedInFile <- t_tmp.marshalizedInFile;
  t.inputKappaFileNames <- t_tmp.inputKappaFileNames;
  (*t.initialMix <- t_tmp.initialMix;*)
  t.outputDataFile <- t_tmp.outputDataFile;
  t.outputDirectory <- t_tmp.outputDirectory;
  t.batchmode  <- t_tmp.batchmode;
  t.interactive <- t_tmp.interactive;
  t.newSyntax <- t_tmp.newSyntax

let options_gen (t :t) (t_gui :t_gui) : (string * Arg.spec * Superarg.spec * string * string list * Superarg.level) list = [
  ("-i",
   Arg.String (fun fic ->
       t.inputKappaFileNames <- fic::t.inputKappaFileNames),
   Superarg.String_list t_gui.inputKappaFileNames_gui,
   "name of a kappa file to use as input (can be used multiple times for multiple input files)",
  [],Superarg.Hidden);
  (*  ("-mixture",
   Arg.String (fun fic -> t.initialMix <- Some fic),
   (Superarg.String_opt t_gui.initialMix_gui),
   "Take the initial state from this file (ignore %init from other files)",
      [],Superarg.Hidden);*)
  ("-initial",
   Arg.Float (fun time -> t.minValue <- Some time),
   (Superarg.Float_opt t_gui.minValue_gui),
   "Min time of simulation (arbitrary time unit)",
   ["0_model";"3_integration_settings"],Superarg.Normal);
  ("-l",
   Arg.Float(fun time -> t.maxValue <- Some time),
   (Superarg.Float_opt t_gui.maxValue_gui),
   "Limit of the simulation",
   ["0_model";"3_integration_settings"],Superarg.Normal);
  ("-t",
   Arg.Float (fun f ->
       raise (Arg.Bad ("Option '-t' has been replace by '[-u time] -l "^
                       string_of_float f^"'"))),
  (Superarg.Float_opt t_gui.maxValue_gui),
   "Deprecated option",
  [],Superarg.Hidden);
  ("-p",
   Arg.Float (fun pointNumberValue -> t.plotPeriod <- Some pointNumberValue),
   Superarg.Float_opt t_gui.plotPeriod_gui,
   "plot period: time interval between points in plot (default: 1.0)",
  ["0_model";"3_integration_settings"],Superarg.Normal);
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
   Superarg.StringNbr_list t_gui.alg_var_overwrite_gui,
   "Set a variable to a given value",
   ["0_model"],Superarg.Hidden);
  ("-o",
   Arg.String
     (fun outputDataFile -> t.outputDataFile <- Some outputDataFile),
   Superarg.String_opt t_gui.outputDataFile_gui,
   "file name for data output",
   ["0_model"; "3_integration_settings"], Superarg.Normal) ;
  ("-d",
   Arg.String (fun outputDirectory -> t.outputDirectory <- outputDirectory),
   Superarg.String t_gui.outputDirectory_gui,
   "Specifies directory name where output file(s) should be stored",
   ["0_model"; "3_integration_settings"], Superarg.Normal) ;
  ("-load-sim",
   Arg.String (fun file -> t.marshalizedInFile <- file),
   Superarg.String t_gui.marshalizedInFile_gui,
   "load simulation package instead of kappa files",
   ["0_model"], Superarg.Normal);
  ("-mode",
   Arg.String
     (fun m -> if m = "batch" then t.batchmode <- true
       else if m = "interactive" then t.interactive <- true),
   Superarg.Choice
     (["batch","batch mode";"intercative","interactive mode"],[],t_gui.batchmode_gui),
   "either \"batch\" to never ask anything to the user or \"interactive\" to ask something before doing anything",
   [], Superarg.Hidden) ;
   ("--new-syntax",
    Arg.Unit (fun () -> t.newSyntax <- true),
    Superarg.Bool t_gui.newSyntax_gui,
    "Use explicit notation for free site",
    [], Superarg.Hidden);
   ("-rescale",
    Arg.Float (fun i -> t.rescale <- Some i),
    Superarg.Float_opt t_gui.rescale_gui,
    "Apply rescaling factor to initial condition",
    ["0_model"; "3_integration_settings"], Superarg.Normal ;)
]

let options t =
  List.rev_map
    (fun (a,b,_,c,_,_) -> a,b,c)
    (List.rev (options_gen t default_gui))

let options_gui t_gui =
  List.rev_map
    (fun (a,_,b,c,d,e) -> a,b,c,d,e)
    (List.rev (options_gen default t_gui))
