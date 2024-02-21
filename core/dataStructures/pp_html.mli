(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val graph_page :
  (Format.formatter -> unit) ->
  ?subtitle:(Format.formatter -> unit) ->
  string list ->
  (Format.formatter -> unit) ->
  (Format.formatter -> unit) ->
  Format.formatter ->
  unit
(** [graph_page title deps header core f] *)
