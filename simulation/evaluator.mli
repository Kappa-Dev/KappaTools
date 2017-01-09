(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val do_interactive_directives :
  outputs:(Data.t -> unit) -> max_sharing:bool -> Signature.contact_map ->
  Model.t -> Counter.t -> Rule_interpreter.t -> State_interpreter.t ->
  (((String.t * Locality.t) * Ast.port list) list, Mods.StringMap.elt)
    Ast.modif_expr list ->
  Model.t * (bool * Rule_interpreter.t * State_interpreter.t)
