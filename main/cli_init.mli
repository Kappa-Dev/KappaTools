(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type directive_unit = Time | Event

val get_compilation :
  ?unit:directive_unit -> ?max_sharing:bool ->
  ?bwd_bisim:Symmetries_sig.bwd_bisim_info ->
  ?compileModeOn:bool -> Run_cli_args.t ->
  (Configuration.t * Counter.progressBar * Model.t * Contact_map.t * int list *
   (bool*bool*bool) option * string * string option *
   (Alg_expr.t * Primitives.elementary_rule * Locality.t) list) *
  Counter.t
