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

val options : t -> (string * Arg.spec * string) list
val default : t
