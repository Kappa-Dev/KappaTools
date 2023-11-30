(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val do_interactive_directives :
  debug_mode:bool ->
  outputs:(Data.t -> unit) ->
  sharing:Pattern.sharing_level ->
  syntax_version:Ast.syntax_version ->
  Contact_map.t ->
  Model.t ->
  Counter.t ->
  Rule_interpreter.t ->
  State_interpreter.t ->
  (Ast.mixture, Ast.mixture, string, Ast.rule) Ast.modif_expr list ->
  Primitives.modification list
  * (Model.t * (bool * Rule_interpreter.t * State_interpreter.t))

val get_pause_criteria :
  debug_mode:bool ->
  outputs:(Data.t -> unit) ->
  sharing:Pattern.sharing_level ->
  syntax_version:Ast.syntax_version ->
  Contact_map.t ->
  Model.t ->
  Rule_interpreter.t ->
  (Ast.mixture, string) Alg_expr.bool Loc.annoted ->
  Model.t * Rule_interpreter.t * (Pattern.id array list, int) Alg_expr.bool

val find_all_embeddings :
  debug_mode:bool ->
  Model.t ->
  Instantiation.concrete Primitives.Transformation.t list ->
  (Pattern.id * Renaming.t) list
