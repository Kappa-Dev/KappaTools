(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type period = DE of int | DT of float

type t = {
  progressSize: int;
  progressChar: char;
  dumpIfDeadlocked: bool;
  initial: float option;
  maxConsecutiveClash: int;
  outputFileName: string option;
  plotPeriod: period option;
  seed: int option;
  traceFileName: string option;
  deltaActivitiesFileName: string option;
}

val empty : t

val parse :
  ((string * Loc.t) * (string * Loc.t) list) list ->
  t
  * (bool * bool * bool)
  * string (*cflowFormat*)
  * string option (*cflowFile*)

val print : Format.formatter -> t -> unit
