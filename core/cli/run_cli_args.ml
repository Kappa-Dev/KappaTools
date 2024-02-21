(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  mutable inputKappaFileNames: string list;
  mutable minValue: float option;
  mutable maxValue: float option;
  mutable plotPeriod: float option;
  mutable outputDataFile: string option;
  mutable outputDirectory: string;
  mutable batchmode: bool;
  mutable interactive: bool;
  mutable syntaxVersion: Ast.syntax_version;
}

type t_gui = {
  inputKappaFileNames_gui: string list ref;
  minValue_gui: float option ref;
  maxValue_gui: float option ref;
  plotPeriod_gui: float option ref;
  outputDataFile_gui: string option ref;
  outputDirectory_gui: string ref;
  syntaxVersion_gui: string ref;
  batchmode_gui: string ref;
}

let default : t =
  {
    inputKappaFileNames = [];
    minValue = None;
    maxValue = None;
    plotPeriod = None;
    outputDataFile = None;
    outputDirectory = ".";
    syntaxVersion = Ast.V4;
    batchmode = false;
    interactive = false;
  }

let default_gui =
  {
    inputKappaFileNames_gui = ref [];
    minValue_gui = ref (Some 0.);
    maxValue_gui = ref (Some 1.);
    plotPeriod_gui = ref (Some 0.01);
    outputDataFile_gui = ref (Some "data.csv");
    outputDirectory_gui = ref ".";
    syntaxVersion_gui = ref "4";
    batchmode_gui = ref "interactive";
  }

let rec aux l accu =
  match l with
  | (v, var_val) :: tail ->
    aux tail
      (( v,
         try Nbr.of_string var_val
         with Failure _ ->
           raise (Arg.Bad ("\"" ^ var_val ^ "\" is not a valid value")) )
      :: accu)
  | [] -> accu

let get_from_gui t_gui =
  {
    minValue = !(t_gui.minValue_gui);
    maxValue = !(t_gui.maxValue_gui);
    plotPeriod = !(t_gui.plotPeriod_gui);
    inputKappaFileNames = !(t_gui.inputKappaFileNames_gui);
    outputDataFile = !(t_gui.outputDataFile_gui);
    outputDirectory = !(t_gui.outputDirectory_gui);
    syntaxVersion =
      (match !(t_gui.syntaxVersion_gui) with
      | "3" | "v3" | "V3" -> Ast.V3
      | "4" | "v4" | "V4" -> Ast.V4
      | _s -> Ast.V4);
    batchmode = Tools.lowercase !(t_gui.batchmode_gui) = "batch";
    interactive = Tools.lowercase !(t_gui.batchmode_gui) = "interactive";
  }

let copy_from_gui t_gui t =
  let t_tmp = get_from_gui t_gui in
  t.minValue <- t_tmp.minValue;
  t.maxValue <- t_tmp.maxValue;
  t.plotPeriod <- t_tmp.plotPeriod;
  t.inputKappaFileNames <- t_tmp.inputKappaFileNames;
  t.outputDataFile <- t_tmp.outputDataFile;
  t.outputDirectory <- t_tmp.outputDirectory;
  t.syntaxVersion <- t_tmp.syntaxVersion;
  t.batchmode <- t_tmp.batchmode;
  t.interactive <- t_tmp.interactive

let options_gen (t : t) (t_gui : t_gui) :
    (string
    * Arg.spec
    * Superarg.spec
    * string
    * (Superarg.category * Superarg.position) list
    * Superarg.level)
    list =
  [
    ( "-i",
      Arg.String
        (fun fic -> t.inputKappaFileNames <- fic :: t.inputKappaFileNames),
      Superarg.String_list t_gui.inputKappaFileNames_gui,
      "name of a kappa file to use as input (can be used multiple times for \
       multiple input files)",
      [],
      Superarg.Hidden );
    ( "-initial",
      Arg.Float (fun time -> t.minValue <- Some time),
      Superarg.Float_opt t_gui.minValue_gui,
      "Min time of simulation (arbitrary time unit)",
      [ Common_args.data_set, 0; Common_args.integration_settings, 0 ],
      Superarg.Normal );
    ( "-l",
      Arg.Float (fun time -> t.maxValue <- Some time),
      Superarg.Float_opt t_gui.maxValue_gui,
      "Limit of the simulation",
      [ Common_args.data_set, 1; Common_args.integration_settings, 1 ],
      Superarg.Normal );
    ( "-t",
      Arg.Float
        (fun f ->
          raise
            (Arg.Bad
               ("Option '-t' has been replace by '[-u time] -l "
              ^ string_of_float f ^ "'"))),
      Superarg.Float_opt t_gui.maxValue_gui,
      "Deprecated option",
      [],
      Superarg.Hidden );
    ( "-p",
      Arg.Float (fun pointNumberValue -> t.plotPeriod <- Some pointNumberValue),
      Superarg.Float_opt t_gui.plotPeriod_gui,
      "plot period: time interval between points in plot (default: 1.0)",
      [ Common_args.data_set, 2; Common_args.integration_settings, 2 ],
      Superarg.Normal );
    ( "-o",
      Arg.String (fun outputDataFile -> t.outputDataFile <- Some outputDataFile),
      Superarg.String_opt t_gui.outputDataFile_gui,
      "file name for data output",
      [
        Common_args.data_set, 3;
        Common_args.output, 3;
        Common_args.integration_settings, 3;
      ],
      Superarg.Hidden );
    ( "-d",
      Arg.String (fun outputDirectory -> t.outputDirectory <- outputDirectory),
      Superarg.String t_gui.outputDirectory_gui,
      "Specifies directory name where output file(s) should be stored",
      [
        Common_args.data_set, 100;
        Common_args.output, 100;
        Common_args.semantics, 100;
        Common_args.integration_settings, 100;
        Common_args.model_reduction, 100;
        Common_args.static_analysis, 100;
        Common_args.debug_mode, 100;
      ],
      Superarg.Normal );
    ( "-mode",
      Arg.String
        (fun m ->
          if m = "batch" then
            t.batchmode <- true
          else if m = "interactive" then
            t.interactive <- true),
      Superarg.Choice
        ( [ "batch", "batch mode"; "interactive", "interactive mode" ],
          [],
          t_gui.batchmode_gui ),
      "either \"batch\" to never ask anything to the user or \"interactive\" \
       to ask something before doing anything",
      [ Common_args.output, 7; Common_args.debug_mode, 7 ],
      Superarg.Expert );
    ( "-syntax",
      Arg.String
        (function
        | "3" | "v3" | "V3" -> t.syntaxVersion <- Ast.V3
        | "4" | "v4" | "V4" -> t.syntaxVersion <- Ast.V4
        | s -> raise (Arg.Bad ("\"" ^ s ^ "\" is not a valid syntax version"))),
      Superarg.Choice
        ( [
            "3", "old";
            "v3", "old";
            "V3", "old";
            "4", "new";
            "v4", "new";
            "V4", "new";
          ],
          [],
          t_gui.syntaxVersion_gui ),
      "Use explicit notation for free site",
      [ Common_args.semantics, 8 ],
      Superarg.Normal );
  ]

let options t =
  List.rev_map
    (fun (a, b, _, c, _, _) -> a, b, c)
    (List.rev (options_gen t default_gui))

let options_gui t_gui =
  List.rev_map
    (fun (a, _, b, c, d, e) -> a, b, c, d, e)
    (List.rev (options_gen default t_gui))
  @ [
      ( "--output-plot",
        Superarg.String_opt t_gui.outputDataFile_gui,
        "file name for data output",
        [
          Common_args.output, 1;
          Common_args.semantics, 1;
          Common_args.integration_settings, 1;
        ],
        Superarg.Normal );
      ( "--data-file",
        Superarg.String_opt t_gui.outputDataFile_gui,
        "file name for data output",
        [
          Common_args.output, 1;
          Common_args.semantics, 2;
          Common_args.integration_settings, 3;
        ],
        Superarg.Hidden );
    ]
