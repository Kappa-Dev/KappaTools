type t = {
  mutable alg_var_overwrite   : (string * Nbr.t) list;
  mutable minTimeValue        : float;
  mutable maxTimeValue        : float option;
  mutable pointNumberValue    : int;
  mutable rescale             : float option;
  mutable marshalizedInFile   : string;
  mutable inputKappaFileNames : string list;
  mutable outputDataFile      : string;
  mutable outputDirectory     : string;
  mutable batchmode           : bool;
  mutable interactive         : bool;
}

let default : t = {
  alg_var_overwrite = [];
  minTimeValue = 0. ;
  maxTimeValue = None;
  pointNumberValue = -1;
  rescale = None;
  marshalizedInFile = "";
  inputKappaFileNames = [];
  outputDataFile = "data.out";
  outputDirectory = ".";
  batchmode  = false;
  interactive = false;
}

let options (t :t)  : (string * Arg.spec * string) list = [
  ("-i",
   Arg.String (fun fic ->
       t.inputKappaFileNames <- fic::t.inputKappaFileNames),
   "name of a kappa file to use as input (can be used multiple times for multiple input files)");
  ("-t-init",
   Arg.Float (fun time -> t.minTimeValue <- time),
   "Min time of simulation (arbitrary time unit)");
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
  ("-mode",
   Arg.String
     (fun m -> if m = "batch" then t.batchmode <- true
       else if m = "interactive" then t.interactive <- true),
   "either \"batch\" to never ask anything to the user or \"interactive\" to ask something before doing anything") ;
  ("-rescale", Arg.Float (fun i -> t.rescale <- Some i),
   "Apply rescaling factor to initial condition")
]
