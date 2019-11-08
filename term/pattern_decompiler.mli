(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val patterns_of_mixture :
  debugMode:bool -> Contact_map.t -> Signature.s ->
  Pattern.Env.t -> Edges.t -> Pattern.Env.t * Pattern.cc list
