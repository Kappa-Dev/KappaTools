(**
  * remanent_('global,'static, 'compile) state.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June, the 25th of 2016
  * Last modification: Time-stamp: <Mar 18 2020>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type compilation = Ast.parsing_compil
type init = Compil of compilation | Files of string list
type initial_state = (Primitives.alg_expr * Primitives.elementary_rule) list

type internal_contact_map =
  (Ckappa_sig.c_state list
  * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) list)
  Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t

type internal_scc_decomposition =
  ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
  * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name))
  list
  list

type quark_map = Quark_type.quarks
type rule_id = int
type var_id = int
type dead_agents = (Ckappa_sig.c_rule_id, int) Public_data.agent_kind list

val info_to_rule :
  string
  * Loc.t
  * Public_data.rule_direction
  * string
  * string LKappa.guard option
  * Ckappa_sig.c_rule_id ->
  Public_data.rule

val info_to_agent :
  string * (Loc.t * (Ckappa_sig.c_rule_id, int) Public_data.ast_origin option) list * Ckappa_sig.c_agent_name -> (Ckappa_sig.c_rule_id, int) Public_data.agent_kind

type separating_transitions = Public_data.separating_transitions

type refined_compilation =
  ( Ckappa_sig.agent,
    Ckappa_sig.agent_sig,
    Ckappa_sig.mixture,
    Ckappa_sig.mixture,
    string,
    Ckappa_sig.mixture Ckappa_sig.rule )
  Ast.compil

type distance = { fwd: int; bwd: int; total: int }

type local_influence_map_blackboard = {
  blackboard_distance: distance option array;
  blackboard_is_done: bool array;
  blackboard_to_be_explored: bool array;
}

type internal_influence_map =
  Ckappa_sig.c_rule_id list
  * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t
  * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type ('global, 'static, 'dynamic) reachability_result =
  ('global * 'static) * 'dynamic

type subviews_info = unit

type flow =
  Ckappa_sig.Site_union_find.t
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t

(*******************************************************************)

type internal_constraint_list =
  Site_graphs.KaSa_site_graph.t Public_data.poly_constraint_list

type agent =
  string
  * (*agent name*)
  (string option
  * Site_graphs.KaSa_site_graph.binding_state option
  * (int option * int option) option)
  Wrapped_modules.LoggedStringMap.t

type constraint_list = agent list Public_data.poly_constraint_list

val lemmas_list_to_json : constraint_list -> Yojson.Basic.t
val lemmas_list_of_json : Yojson.Basic.t -> constraint_list
(*******************************************************************)

type symmetric_sites = Symmetries.symmetries option

(*******************************************************************)

type influence_edge = Quark_type.Labels.label_set_couple

type bidirectional_influence_map = {
  positive_influence_fwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
  positive_influence_bwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
  negative_influence_fwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
  negative_influence_bwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
}

type ('global, 'static, 'dynamic) state

val get_global_static_information :
  ('global, 'static, 'dynamic) state -> 'global option

val set_global_static_information :
  'global ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state
(*******************************************************************)

val to_json : ('global, 'static, 'dynamic) state -> Yojson.Basic.t

val of_json :
  Yojson.Basic.t ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * Public_data.contact_map Public_data.AccuracyMap.t
  * Public_data.influence_map Public_data.AccuracyMap.t
  * Public_data.dead_rules option
  * constraint_list option
  * Public_data.separating_transitions option

val create_state :
  ?errors:Exception.exceptions_caught_and_uncaught ->
  ?env:Model.t option ->
  ?init_state:initial_state ->
  ?reset:bool ->
  Remanent_parameters_sig.parameters ->
  init ->
  ('global, 'static, 'dynamic) state

val set_parameters :
  Remanent_parameters_sig.parameters ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

val get_parameters :
  ('global, 'static, 'dynamic) state -> Remanent_parameters_sig.parameters

val add_event :
  StoryProfiling.step_kind ->
  (unit -> int) option ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

val close_event :
  StoryProfiling.step_kind ->
  (unit -> int) option ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

val get_init : ('global, 'static, 'dynamic) state -> init
val get_env : ('global, 'static, 'dynamic) state -> Model.t option option

val set_env :
  Model.t option ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

val get_init_state : ('global, 'static, 'dynamic) state -> initial_state option

val set_init_state :
  initial_state ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

(*contact map int*)

val get_contact_map_int :
  ('global, 'static, 'dynamic) state -> Contact_map.t option option

val set_contact_map_int :
  Contact_map.t option ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

val set_compilation :
  compilation ->
  ('global, 'static, 'dynamic) state ->
  ('global, 'static, 'dynamic) state

val get_compilation : ('global, 'static, 'dynamic) state -> compilation option

val set_handler :
  Cckappa_sig.kappa_handler ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_handler :
  ('global, 'static, 'compile) state -> Cckappa_sig.kappa_handler option

val set_refined_compil :
  refined_compilation ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_refined_compil :
  ('global, 'static, 'compile) state -> refined_compilation option

val set_c_compil :
  Cckappa_sig.compil ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_c_compil :
  ('global, 'static, 'compile) state -> Cckappa_sig.compil option

val get_errors :
  ('global, 'static, 'compile) state -> Exception.exceptions_caught_and_uncaught

val set_errors :
  Exception.exceptions_caught_and_uncaught ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val set_internal_contact_map :
  Public_data.accuracy_level ->
  internal_contact_map ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_internal_contact_map :
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  internal_contact_map option

val get_internal_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  internal_scc_decomposition option

val get_internal_scc_decomposition_map :
  ('global, 'static, 'compile) state ->
  internal_scc_decomposition Public_data.AccuracyMap.t Public_data.AccuracyMap.t

val set_internal_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  internal_scc_decomposition ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  Public_data.scc option

val set_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  Public_data.scc ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val set_contact_map :
  Public_data.accuracy_level ->
  Public_data.contact_map ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_contact_map :
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  Public_data.contact_map option

val set_signature :
  Signature.s ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_signature : ('global, 'static, 'compile) state -> Signature.s option

val set_quark_map :
  quark_map ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_quark_map : ('global, 'static, 'compile) state -> quark_map option

val set_pos_of_rules_and_vars :
  Public_data.pos_of_rules_and_vars ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_pos_of_rules_and_vars :
  ('global, 'static, 'compile) state -> Public_data.pos_of_rules_and_vars option

val set_internal_influence_map :
  Public_data.accuracy_level ->
  internal_influence_map ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_internal_influence_map :
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  internal_influence_map option

val set_influence_map :
  Public_data.accuracy_level ->
  Public_data.influence_map ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_influence_map :
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  Public_data.influence_map option

val set_bidirectional_influence_map :
  Public_data.accuracy_level ->
  bidirectional_influence_map ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_bidirectional_influence_map :
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  bidirectional_influence_map option

val set_local_influence_map_blackboard :
  local_influence_map_blackboard ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_local_influence_map_blackboard :
  ('global, 'static, 'compile) state -> local_influence_map_blackboard option

val set_ode_flow :
  Ode_fragmentation_type.ode_frag ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_ode_flow :
  ('global, 'static, 'compile) state -> Ode_fragmentation_type.ode_frag option

val set_ctmc_flow :
  flow ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_ctmc_flow : ('global, 'static, 'compile) state -> flow option

val get_bdu_handler :
  ('global, 'static, 'compile) state -> Mvbdu_wrapper.Mvbdu.handler

val set_bdu_handler :
  Mvbdu_wrapper.Mvbdu.handler ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val set_reachability_result :
  ('global, 'static, 'compile) reachability_result ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_reachability_result :
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) reachability_result option

val get_subviews_info :
  ('global, 'static, 'compile) state -> subviews_info option

val set_subviews_info :
  subviews_info ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_dead_rules :
  ('global, 'static, 'compile) state -> Public_data.dead_rules option

val set_dead_rules :
  Public_data.dead_rules ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_conditionally_dead_rules :
  ('global, 'static, 'compile) state ->
  Public_data.rule_deadness_conditions option

val set_conditionally_dead_rules :
  Public_data.rule_deadness_conditions ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_dead_agents : ('global, 'static, 'compile) state -> dead_agents option

val set_dead_agents :
  dead_agents ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_conditionally_dead_agents :
  ('global, 'static, 'compile) state ->
  (Ckappa_sig.c_rule_id, int) Public_data.agent_deadness_conditions option

val set_conditionally_dead_agents :
  (Ckappa_sig.c_rule_id, int) Public_data.agent_deadness_conditions ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_influence_map_map :
  ('global, 'static, 'compile) state ->
  Public_data.influence_map Public_data.AccuracyMap.t

val set_separating_transitions :
  separating_transitions ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_separating_transitions :
  ('global, 'static, 'compile) state -> separating_transitions option

val set_transition_system_length :
  int list ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_transition_system_length :
  ('global, 'static, 'compile) state -> int list option

val get_contact_map_map :
  ('global, 'static, 'compile) state ->
  Public_data.contact_map Public_data.AccuracyMap.t

val get_internal_influence_map_map :
  ('global, 'static, 'compile) state ->
  internal_influence_map Public_data.AccuracyMap.t

val get_internal_contact_map_map :
  ('global, 'static, 'compile) state ->
  internal_contact_map Public_data.AccuracyMap.t

val set_log_info :
  StoryProfiling.StoryStats.log_info ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_log_info :
  ('global, 'static, 'compile) state -> StoryProfiling.StoryStats.log_info

val get_internal_constraint_list :
  ('global, 'static, 'compile) state -> internal_constraint_list option

val set_internal_constraint_list :
  internal_constraint_list ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_constraint_list :
  ('global, 'static, 'compile) state -> constraint_list option

val set_constraint_list :
  constraint_list ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_symmetries :
  Public_data.accuracy_level ->
  ('global, 'static, 'compile) state ->
  symmetric_sites option

val set_symmetries :
  Public_data.accuracy_level ->
  symmetric_sites ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_data :
  ('global, 'static, 'compile) state ->
  Cckappa_sig.kappa_handler option
  * Public_data.dead_rules option
  * separating_transitions option
  * int list option

val get_working_set_elements :
  ('global, 'static, 'compile) state -> Public_data.working_set_elements

val reset_reachability_memoized_values :
  ('global, 'static, 'compile) state -> ('global, 'static, 'compile) state

val rename_pos :
  ( Remanent_parameters_sig.parameters,
    Exception.exceptions_caught_and_uncaught,
    'global )
  Loc.rename_pos_with_errors ->
  ( Remanent_parameters_sig.parameters,
    Exception.exceptions_caught_and_uncaught,
    'static )
  Loc.rename_pos_with_errors ->
  ( Remanent_parameters_sig.parameters,
    Exception.exceptions_caught_and_uncaught,
    'compile )
  Loc.rename_pos_with_errors ->
  ('global, 'static, 'compile) state Loc.rename_pos

val store_patch :
  Cckappa_sig.compil ->
  ('global, 'static, 'compile) state ->
  ('global, 'static, 'compile) state

val get_patch : ('global, 'static, 'compile) state -> Cckappa_sig.compil option

val reset_patch :
  ('global, 'static, 'compile) state -> ('global, 'static, 'compile) state
