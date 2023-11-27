(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Main compilation functions *)

(*val init_kasa :
  Remanent_parameters_sig.called_from -> Signature.s ->
  (string Loc.annoted * Ast.port list, Ast.mixture, string, Ast.rule)
    Ast.compil ->
  Primitives.contact_map * Export_to_KaSim.state
*)

val compile_bool :
  debug_mode:bool ->
  compile_mode_on:bool ->
  ?origin:Operator.rev_dep ->
  Contact_map.t ->
  Pattern.PreEnv.t ->
  (LKappa.rule_mixture, int) Alg_expr.bool Loc.annoted ->
  Pattern.PreEnv.t * (Pattern.id array list, int) Alg_expr.bool Loc.annoted

val compile_modifications_no_track :
  debug_mode:bool ->
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  compile_mode_on:bool ->
  Contact_map.t ->
  Pattern.PreEnv.t ->
  (LKappa.rule_mixture, Raw_mixture.t, int, LKappa.rule) Ast.modif_expr list ->
  Pattern.PreEnv.t * Primitives.modification list

val compile_inits :
  debug_mode:bool ->
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  ?rescale:float ->
  compile_mode_on:bool ->
  Contact_map.t ->
  Model.t ->
  (LKappa.rule_mixture, Raw_mixture.t, int) Ast.init_statement list ->
  (Primitives.alg_expr * Primitives.elementary_rule) list

val compile :
  outputs:(Data.t -> unit) ->
  pause:((unit -> 'b) -> 'b) ->
  return:
    (Model.t
     * bool (*has_tracking*)
     * (Primitives.alg_expr * Primitives.elementary_rule) list ->
    'b) ->
  sharing:Pattern.sharing_level ->
  debug_mode:bool ->
  compile_mode_on:bool ->
  ?overwrite_init:
    (LKappa.rule_mixture, Raw_mixture.t, int) Ast.init_statement list ->
  ?overwrite_t0:float ->
  ?rescale_init:float ->
  Signature.s ->
  unit NamedDecls.t ->
  Contact_map.t ->
  ('c, LKappa.rule_mixture, Raw_mixture.t, int, LKappa.rule) Ast.compil ->
  'b

val build_initial_state :
  bind:('a -> (bool * Rule_interpreter.t * State_interpreter.t -> 'a) -> 'a) ->
  return:(bool * Rule_interpreter.t * State_interpreter.t -> 'a) ->
  debug_mode:bool ->
  outputs:(Data.t -> unit) ->
  Counter.t ->
  Model.t ->
  with_trace:bool ->
  with_delta_activities:bool ->
  Random.State.t ->
  (Primitives.alg_expr * Primitives.elementary_rule) list ->
  'a
