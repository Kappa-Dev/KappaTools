(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val patterns_of_mixture :
  debugMode:bool ->
  Contact_map.t ->
  Signature.s ->
  Pattern.PreEnv.t ->
  Edges.t ->
  Pattern.PreEnv.t * Pattern.cc list
