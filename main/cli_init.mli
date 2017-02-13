(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type directive_unit = Time | Event

val get_compilation :
  ?unit:directive_unit -> ?max_sharing:bool -> Run_cli_args.t ->
  (Eval.configuration * Model.t * Contact_map.t * int list *
   (bool*bool*bool) option * string * string option *
   (Alg_expr.t * Primitives.elementary_rule * Locality.t) list) *
  Counter.t
