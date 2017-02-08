(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Main compilation functions *)

(*val init_kasa :
  Remanent_parameters_sig.called_from -> Signature.s ->
  (string Locality.annot * Ast.port list, Ast.mixture, string, Ast.rule)
    Ast.compil ->
  Primitives.contact_map * Export_to_KaSim.state
*)
val compile_bool:
  ?origin:Operator.rev_dep -> Signature.contact_map -> Pattern.PreEnv.t ->
  (LKappa.rule_mixture, int) Alg_expr.bool Locality.annot ->
  Pattern.PreEnv.t *
  (Pattern.id array list,int) Alg_expr.bool Locality.annot

val compile_modifications_no_track:
  Signature.contact_map -> Pattern.PreEnv.t ->
  (LKappa.rule_mixture, int) Ast.modif_expr list ->
  Pattern.PreEnv.t * Primitives.modification list

val compile :
  outputs:(Data.t -> 'a) -> pause:((unit -> 'b) -> 'b) ->
  return:(int option (*seed*) * Model.t * (bool*bool*bool) option *
          string (*cflowFormat*) * string option (*cflowFile*) *
          (Alg_expr.t * Primitives.elementary_rule * Locality.t) list -> 'b) ->
  max_sharing:bool -> ?rescale_init:float -> Signature.s -> unit NamedDecls.t ->
  Signature.contact_map ->
  ('c, LKappa.rule_mixture, int, LKappa.rule, unit) Ast.compil -> 'b

val build_initial_state :
  bind:('a -> (bool * Rule_interpreter.t * State_interpreter.t -> 'a) -> 'a) ->
  return:(bool * Rule_interpreter.t * State_interpreter.t -> 'a) ->
  outputs:(Data.t -> unit) -> (int * Alg_expr.t) list -> Counter.t ->
  Model.t -> with_trace:bool -> Random.State.t ->
  (Alg_expr.t * Primitives.elementary_rule * Locality.t) list ->
  'a
