(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  seed : int option;
  traceFileName : string option;
  plotPeriod : Counter.period option;
  outputFileName : string option;
  initial : float option;
}

val parse :
  ((string * Locality.t) * (string * Locality.t) list) list ->
  t * (bool * bool * bool) *
  string (*cflowFormat*) * string option (*cflowFile*)

val print : Format.formatter -> t -> unit
