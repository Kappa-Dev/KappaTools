(**
  * remanent_('static, 'compile) state.ml
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
type dead_agents = Public_data.agent_kind list

val info_to_rule :
  string * Loc.t * Public_data.rule_direction * string * Ckappa_sig.c_rule_id ->
  Public_data.rule

val info_to_agent :
  string * Loc.t list * Ckappa_sig.c_agent_name -> Public_data.agent_kind

type separating_transitions = Public_data.separating_transitions

type refined_compilation =
  ( Ckappa_sig.agent,
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

type ('static, 'dynamic) reachability_result = 'static * 'dynamic
type subviews_info = unit

type flow =
  Ckappa_sig.Site_union_find.t
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t

(*******************************************************************)

type internal_constraints_list =
  Site_graphs.KaSa_site_graph.t Public_data.poly_constraints_list

type agent =
  string
  * (*agent name*)
  (string option
  * Site_graphs.KaSa_site_graph.binding_state option
  * (int option * int option) option)
  Wrapped_modules.LoggedStringMap.t

type constraints_list = agent list Public_data.poly_constraints_list

val lemmas_list_to_json : constraints_list -> Yojson.Basic.t
val lemmas_list_of_json : Yojson.Basic.t -> constraints_list

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

type ('static, 'dynamic) state

(*******************************************************************)

val to_json : ('static, 'dynamic) state -> Yojson.Basic.t

val of_json :
  Yojson.Basic.t ->
  Exception_without_parameter.method_handler
  * Public_data.contact_map Public_data.AccuracyMap.t
  * Public_data.influence_map Public_data.AccuracyMap.t
  * Public_data.dead_rules option
  * constraints_list option
  * Public_data.separating_transitions option

val create_state :
  ?errors:Exception.method_handler ->
  ?env:Model.t option ->
  ?init_state:initial_state ->
  ?reset:bool ->
  Remanent_parameters_sig.parameters ->
  init ->
  ('static, 'dynamic) state

val set_parameters :
  Remanent_parameters_sig.parameters ->
  ('static, 'dynamic) state ->
  ('static, 'dynamic) state

val get_parameters :
  ('static, 'dynamic) state -> Remanent_parameters_sig.parameters

val add_event :
  StoryProfiling.step_kind ->
  (unit -> int) option ->
  ('static, 'dynamic) state ->
  ('static, 'dynamic) state

val close_event :
  StoryProfiling.step_kind ->
  (unit -> int) option ->
  ('static, 'dynamic) state ->
  ('static, 'dynamic) state

val get_init : ('static, 'dynamic) state -> init
val get_env : ('static, 'dynamic) state -> Model.t option option

val set_env :
  Model.t option -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val get_init_state : ('static, 'dynamic) state -> initial_state option

val set_init_state :
  initial_state -> ('static, 'dynamic) state -> ('static, 'dynamic) state

(*contact map int*)

val get_contact_map_int :
  ('static, 'dynamic) state -> Contact_map.t option option

val set_contact_map_int :
  Contact_map.t option -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val set_compilation :
  compilation -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val get_compilation : ('static, 'dynamic) state -> compilation option

val set_handler :
  Cckappa_sig.kappa_handler ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_handler : ('static, 'compile) state -> Cckappa_sig.kappa_handler option

val set_refined_compil :
  refined_compilation -> ('static, 'compile) state -> ('static, 'compile) state

val get_refined_compil : ('static, 'compile) state -> refined_compilation option

val set_c_compil :
  Cckappa_sig.compil -> ('static, 'compile) state -> ('static, 'compile) state

val get_c_compil : ('static, 'compile) state -> Cckappa_sig.compil option
val get_errors : ('static, 'compile) state -> Exception.method_handler

val set_errors :
  Exception.method_handler ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val set_internal_contact_map :
  Public_data.accuracy_level ->
  internal_contact_map ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_internal_contact_map :
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  internal_contact_map option

val get_internal_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  internal_scc_decomposition option

val get_internal_scc_decomposition_map :
  ('static, 'compile) state ->
  internal_scc_decomposition Public_data.AccuracyMap.t Public_data.AccuracyMap.t

val set_internal_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  internal_scc_decomposition ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  Public_data.scc option

val set_scc_decomposition :
  Public_data.accuracy_level ->
  Public_data.accuracy_level ->
  Public_data.scc ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val set_contact_map :
  Public_data.accuracy_level ->
  Public_data.contact_map ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_contact_map :
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  Public_data.contact_map option

val set_signature :
  Signature.s -> ('static, 'compile) state -> ('static, 'compile) state

val get_signature : ('static, 'compile) state -> Signature.s option

val set_quark_map :
  quark_map -> ('static, 'compile) state -> ('static, 'compile) state

val get_quark_map : ('static, 'compile) state -> quark_map option

val set_pos_of_rules_and_vars :
  Public_data.pos_of_rules_and_vars ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_pos_of_rules_and_vars :
  ('static, 'compile) state -> Public_data.pos_of_rules_and_vars option

val set_internal_influence_map :
  Public_data.accuracy_level ->
  internal_influence_map ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_internal_influence_map :
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  internal_influence_map option

val set_influence_map :
  Public_data.accuracy_level ->
  Public_data.influence_map ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_influence_map :
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  Public_data.influence_map option

val set_bidirectional_influence_map :
  Public_data.accuracy_level ->
  bidirectional_influence_map ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_bidirectional_influence_map :
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  bidirectional_influence_map option

val set_local_influence_map_blackboard :
  local_influence_map_blackboard ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_local_influence_map_blackboard :
  ('static, 'compile) state -> local_influence_map_blackboard option

val set_ode_flow :
  Ode_fragmentation_type.ode_frag ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_ode_flow :
  ('static, 'compile) state -> Ode_fragmentation_type.ode_frag option

val set_ctmc_flow :
  flow -> ('static, 'compile) state -> ('static, 'compile) state

val get_ctmc_flow : ('static, 'compile) state -> flow option
val get_bdu_handler : ('static, 'compile) state -> Mvbdu_wrapper.Mvbdu.handler

val set_bdu_handler :
  Mvbdu_wrapper.Mvbdu.handler ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val set_reachability_result :
  ('static, 'compile) reachability_result ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_reachability_result :
  ('static, 'compile) state -> ('static, 'compile) reachability_result option

val get_subviews_info : ('static, 'compile) state -> subviews_info option

val set_subviews_info :
  subviews_info -> ('static, 'compile) state -> ('static, 'compile) state

val get_dead_rules : ('static, 'compile) state -> Public_data.dead_rules option

val set_dead_rules :
  Public_data.dead_rules ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_dead_agents : ('static, 'compile) state -> dead_agents option

val set_dead_agents :
  dead_agents -> ('static, 'compile) state -> ('static, 'compile) state

val get_influence_map_map :
  ('static, 'compile) state ->
  Public_data.influence_map Public_data.AccuracyMap.t

val set_separating_transitions :
  separating_transitions ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_separating_transitions :
  ('static, 'compile) state -> separating_transitions option

val set_transition_system_length :
  int list -> ('static, 'compile) state -> ('static, 'compile) state

val get_transition_system_length : ('static, 'compile) state -> int list option

val get_contact_map_map :
  ('static, 'compile) state -> Public_data.contact_map Public_data.AccuracyMap.t

val get_internal_influence_map_map :
  ('static, 'compile) state -> internal_influence_map Public_data.AccuracyMap.t

val get_internal_contact_map_map :
  ('static, 'compile) state -> internal_contact_map Public_data.AccuracyMap.t

val set_log_info :
  StoryProfiling.StoryStats.log_info ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_log_info :
  ('static, 'compile) state -> StoryProfiling.StoryStats.log_info

val get_internal_constraints_list :
  ('static, 'compile) state -> internal_constraints_list option

val set_internal_constraints_list :
  internal_constraints_list ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_constraints_list : ('static, 'compile) state -> constraints_list option

val set_constraints_list :
  constraints_list -> ('static, 'compile) state -> ('static, 'compile) state

val get_symmetries :
  Public_data.accuracy_level ->
  ('static, 'compile) state ->
  symmetric_sites option

val set_symmetries :
  Public_data.accuracy_level ->
  symmetric_sites ->
  ('static, 'compile) state ->
  ('static, 'compile) state

val get_data :
  ('static, 'compile) state ->
  Cckappa_sig.kappa_handler option
  * Public_data.dead_rules option
  * separating_transitions option
  * int list option
