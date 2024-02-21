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

val default : t
val options : t -> (string * Arg.spec * string) list

val options_gui :
  t_gui ->
  (string
  * Superarg.spec
  * string
  * (Superarg.category * int) list
  * Superarg.level)
  list

val copy_from_gui : t_gui -> t -> unit
val aux : ('a * string) list -> ('a * Nbr.t) list -> ('a * Nbr.t) list
val default_gui : t_gui
