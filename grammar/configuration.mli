(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  dumpIfDeadlocked : bool;
  initial : float option;
  maxConsecutiveClash : int;
  syntaxVersion : Ast.syntax_version;
  outputFileName : string option;
  plotPeriod : Counter.period option;
  seed : int option;
  traceFileName : string option;
  deltaActivitiesFileName : string option;
}

val parse :
  ((string * Locality.t) * (string * Locality.t) list) list ->
  t * Counter.progressBar * (bool * bool * bool) *
  string (*cflowFormat*) * string option (*cflowFile*)

val print : Format.formatter -> t -> unit
