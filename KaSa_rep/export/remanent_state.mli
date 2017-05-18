(**
  * remanent_('static, 'compile) state.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June, the 25th of 2016
  * Last modification: Time-stamp: <Apr 25 2017>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type compilation = Ast.parsing_compil

type init =
    Compil of compilation
  | Files of string list

type initial_state = (Alg_expr.t * Primitives.elementary_rule * Locality.t) list

type accuracy_level = Low | Medium | High | Full

val accuracy_to_json : accuracy_level -> Yojson.Basic.json
val accuracy_of_json : Yojson.Basic.json -> accuracy_level

module AccuracyMap: SetMap.Map with type elt = accuracy_level

type internal_contact_map =
  (Ckappa_sig.c_state list *
   (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) list)
    Ckappa_sig.Site_map_and_set.Map.t Ckappa_sig.Agent_map_and_set.Map.t

type contact_map =
  ((string list) * (string*string) list)
    Mods.StringSetMap.Map.t Mods.StringSetMap.Map.t

val contact_map_to_json:
  accuracy_level * contact_map -> Yojson.Basic.json

val contact_map_of_json:
  Yojson.Basic.json -> accuracy_level * contact_map

type quark_map = Quark_type.quarks

type rule_id = int
type var_id =  int

type refined_compilation =
  (Ckappa_sig.agent, Ckappa_sig.mixture, string, Ckappa_sig.direction * Ckappa_sig.mixture Ckappa_sig.rule,unit) Ast.compil

type influence_node =
  | Rule of rule_id
  | Var of var_id

module InfluenceNodeMap: SetMap.Map with type elt = influence_node

type location =
  | Direct of int
  | Side_effect of int

type 'a pair = 'a * 'a

type influence_map =
  {
    positive: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
    negative: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
  }

val influence_map_to_json:
  accuracy_level * influence_map -> Yojson.Basic.json

val influence_map_of_json:
  Yojson.Basic.json -> accuracy_level * influence_map

type internal_influence_map =
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t
  * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type ('static,'dynamic) reachability_result = 'static * 'dynamic

type subviews_info = unit

type dead_rules = Ckappa_sig.c_rule_id list

val dead_rules_to_json : dead_rules -> Yojson.Basic.json
val dead_rules_of_json : Yojson.Basic.json -> dead_rules

type dead_agents = Ckappa_sig.c_agent_name list

type separating_transitions =
  (string * Ckappa_sig.c_rule_id * string) list

val separating_transitions_to_json: separating_transitions -> Yojson.Basic.json
val separating_transitions_of_json: Yojson.Basic.json -> separating_transitions

type flow =
    Ckappa_sig.Site_union_find.t
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t

(*******************************************************************)

type 'site_graph lemma =
  {
    hyp : 'site_graph ;
    refinement : 'site_graph list
  }

type 'site_graph poly_constraints_list =
  (string * 'site_graph lemma list) list

type internal_constraints_list =
  Ckappa_backend.Ckappa_backend.t poly_constraints_list

type agent =
    string * (*agent name*)
     (string option *  Ckappa_backend.Ckappa_backend.binding_state option)
       Wrapped_modules.LoggedStringMap.t

type constraints_list = agent list poly_constraints_list

val lemmas_list_to_json : constraints_list -> Yojson.Basic.json

val lemmas_list_of_json : Yojson.Basic.json -> constraints_list

val get_hyp : 'site_graph lemma -> 'site_graph
val get_refinement : 'site_graph lemma -> 'site_graph list

(*******************************************************************)
type symmetric_sites = Symmetries.symmetries option
(*******************************************************************)

type ('static, 'dynamic) state

(*******************************************************************)
val to_json: ('static, 'dynamic) state -> Yojson.Basic.json

val of_json: Yojson.Basic.json ->
  Exception_without_parameter.method_handler *
  contact_map AccuracyMap.t *
  influence_map AccuracyMap.t *
  Ckappa_sig.c_rule_id list option *
  constraints_list option *
  separating_transitions option

val create_state:
  ?errors:Exception.method_handler -> ?env:Model.t option ->
  ?init_state:initial_state option ->
  Remanent_parameters_sig.parameters -> init -> ('static, 'dynamic) state

val set_parameters: Remanent_parameters_sig.parameters -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val get_parameters: ('static, 'dynamic) state ->
  Remanent_parameters_sig.parameters

val add_event: StoryProfiling.step_kind -> (unit -> int) option -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val close_event: StoryProfiling.step_kind -> (unit -> int) option -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val get_init: ('static, 'dynamic) state -> init

val get_env: ('static, 'dynamic) state -> Model.t option option

val set_env: Model.t option -> ('static, 'dynamic) state -> ('static, 'dynamic) state

val get_init_state: ('static, 'dynamic) state -> initial_state option option

val set_init_state:
  initial_state option -> ('static, 'dynamic) state ->
  ('static, 'dynamic) state

(*contact map int*)

val get_contact_map_int: ('static, 'dynamic) state ->
  Contact_map.t option option

val set_contact_map_int:
  Contact_map.t option -> ('static, 'dynamic) state ->
  ('static, 'dynamic) state

val set_compilation: compilation -> ('static, 'dynamic) state ->
  ('static, 'dynamic) state

val get_compilation: ('static, 'dynamic) state -> compilation option

val set_handler: Cckappa_sig.kappa_handler -> ('static, 'compile) state -> ('static, 'compile) state

val get_handler: ('static, 'compile) state -> Cckappa_sig.kappa_handler option

val set_refined_compil: refined_compilation -> ('static, 'compile) state -> ('static, 'compile) state

val get_refined_compil:
  ('static, 'compile) state
  -> refined_compilation option

val set_c_compil: Cckappa_sig.compil -> ('static, 'compile) state -> ('static, 'compile) state

val get_c_compil: ('static, 'compile) state -> Cckappa_sig.compil option

val get_errors: ('static, 'compile) state -> Exception.method_handler

val set_errors: Exception.method_handler -> ('static, 'compile) state -> ('static, 'compile) state

val set_internal_contact_map: accuracy_level -> internal_contact_map -> ('static, 'compile) state -> ('static, 'compile) state

val get_internal_contact_map: accuracy_level -> ('static, 'compile) state ->
  internal_contact_map option

val set_contact_map: accuracy_level -> contact_map -> ('static, 'compile) state
  -> ('static, 'compile) state

val get_contact_map: accuracy_level -> ('static, 'compile) state -> contact_map
    option

val set_signature: Signature.s -> ('static, 'compile) state -> ('static, 'compile) state

val get_signature: ('static, 'compile) state -> Signature.s option

val set_quark_map: quark_map -> ('static, 'compile) state -> ('static, 'compile) state

val get_quark_map: ('static, 'compile) state -> quark_map option

val set_internal_influence_map: accuracy_level -> internal_influence_map -> ('static, 'compile) state -> ('static, 'compile) state

val get_internal_influence_map: accuracy_level -> ('static, 'compile) state -> internal_influence_map option

val set_influence_map: accuracy_level -> influence_map -> ('static, 'compile) state -> ('static, 'compile) state

val get_influence_map: accuracy_level -> ('static, 'compile) state -> influence_map option

val set_ode_flow: Ode_fragmentation_type.ode_frag -> ('static, 'compile) state -> ('static, 'compile) state

val get_ode_flow: ('static, 'compile) state -> Ode_fragmentation_type.ode_frag option

val set_ctmc_flow: flow -> ('static, 'compile) state -> ('static, 'compile) state

val get_ctmc_flow: ('static, 'compile) state -> flow option

val get_bdu_handler: ('static, 'compile) state -> Mvbdu_wrapper.Mvbdu.handler

val set_bdu_handler: Mvbdu_wrapper.Mvbdu.handler -> ('static, 'compile) state -> ('static, 'compile) state

val set_reachability_result: ('static, 'compile) reachability_result -> ('static, 'compile) state -> ('static, 'compile) state

val get_reachability_result: ('static, 'compile) state -> ('static, 'compile) reachability_result option

val get_subviews_info: ('static, 'compile) state -> subviews_info option

val set_subviews_info: subviews_info -> ('static, 'compile) state -> ('static, 'compile) state

val get_dead_rules: ('static, 'compile) state -> dead_rules option

val set_dead_rules: dead_rules -> ('static, 'compile) state -> ('static, 'compile) state

val get_dead_agents: ('static, 'compile) state -> dead_agents option

val set_dead_agents: dead_agents -> ('static, 'compile) state -> ('static, 'compile) state

val get_influence_map_map: ('static, 'compile) state -> influence_map AccuracyMap.t

val set_separating_transitions: separating_transitions -> ('static, 'compile) state -> ('static, 'compile) state
val get_separating_transitions: ('static, 'compile) state -> separating_transitions option

val get_contact_map_map: ('static, 'compile) state -> contact_map AccuracyMap.t

val get_internal_influence_map_map: ('static, 'compile) state -> internal_influence_map AccuracyMap.t

val get_internal_contact_map_map: ('static, 'compile) state -> internal_contact_map AccuracyMap.t

val set_log_info: StoryProfiling.StoryStats.log_info -> ('static, 'compile) state -> ('static, 'compile) state

val get_log_info: ('static, 'compile) state ->
  StoryProfiling.StoryStats.log_info

val get_internal_constraints_list : ('static, 'compile) state ->
  internal_constraints_list option

val set_internal_constraints_list : internal_constraints_list -> ('static, 'compile) state -> ('static, 'compile) state

val get_constraints_list : ('static, 'compile) state ->
  constraints_list option

val set_constraints_list : constraints_list -> ('static, 'compile) state ->
  ('static, 'compile) state

val get_symmetries : accuracy_level -> ('static, 'compile) state -> symmetric_sites option

val set_symmetries : accuracy_level -> symmetric_sites
  -> ('static, 'compile) state -> ('static, 'compile) state

val get_data:
  ('static, 'compile) state ->
  Cckappa_sig.kappa_handler option * dead_rules option * separating_transitions option
