(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* JSon labels *)
val prop : string
val bind : string
val counter : string
val domain_name : string
val refinements_list : string
val refinement_lemmas : string
val free : string
val bound : string
val wildcard : string
val influencemap : string
val scc : string
val inf : string
val sup : string

(* Backend *)
val binding_type_backend_symbol : string
val free_backend_symbol : string
val wildcard_backend_symbol : string
val missing_binding_site_backend_symbol : string
val bound_to_unknown_backend_symbol : string
val internal_state_introduction_backend_symbol : string
val internal_state_delimiter_backend_symbol : string
val binding_state_delimiter_backend_symbol : string
val binding_state_opening_backend_symbol : string
val binding_state_closing_backend_symbol : string
val internal_state_opening_backend_symbol : string
val internal_state_closing_backend_symbol : string
val counter_state_opening_backend_symbol : string
val counter_state_closing_backend_symbol : string
val counter_state_range_backend_symbol : string
val open_interval_inclusive_symbol : string
val close_interval_inclusive_symbol : string
val open_interval_exclusive_symbol : string
val close_interval_exclusive_symbol : string
val plus_infinity_symbol : string
val minus_infinity_symbol : string

type accuracy_level = Low | Medium | High | Full

val accuracy_levels : accuracy_level list
val contact_map_accuracy_levels : accuracy_level list
val influence_map_accuracy_levels : accuracy_level list
val reduction_accuracy_levels : accuracy_level list
val accuracy_to_string : accuracy_level -> string
val accuracy_of_string : string -> accuracy_level option
val accuracy_to_json : accuracy_level -> Yojson.Basic.t
val accuracy_of_json : Yojson.Basic.t -> accuracy_level

module AccuracyMap : SetMap.Map with type elt = accuracy_level

type contact_map = User_graph.connected_component

val contact_map_to_json : accuracy_level * contact_map -> Yojson.Basic.t
val contact_map_of_json : Yojson.Basic.t -> accuracy_level * contact_map

type scc = ((string * string) * (string * string)) list list

val scc_to_json : accuracy_level * accuracy_level * scc -> Yojson.Basic.t
val scc_of_json : Yojson.Basic.t -> accuracy_level * accuracy_level * scc

type rule_direction =
  | Direct_rule
  | Reverse_rule
  | Both_directions
  | Dummy_rule_direction
  | Variable

type rule = {
  rule_id: int;
  rule_label: string;
  rule_ast: string;
  rule_position: Locality.t;
  rule_direction: rule_direction;
  rule_hidden: bool;
}

type var = {
  var_id: int;
  var_label: string;
  var_ast: string;
  var_position: Locality.t;
}

type ('rule, 'var) influence_node = Rule of 'rule | Var of 'var
type pos_of_rules_and_vars = ((int, int) influence_node * Locality.t) list

val pos_of_rules_and_vars_of_json : Yojson.Basic.t -> pos_of_rules_and_vars
val pos_of_rules_and_vars_to_json : pos_of_rules_and_vars -> Yojson.Basic.t

val short_node_of_refined_node :
  (rule, var) influence_node -> (int, int) influence_node

val short_influence_node_of_json : Yojson.Basic.t -> (int, int) influence_node
val short_influence_node_to_json : (int, int) influence_node -> Yojson.Basic.t

val refined_influence_node_of_json :
  Yojson.Basic.t -> (rule, var) influence_node

val refined_influence_node_to_json :
  (rule, var) influence_node -> Yojson.Basic.t

val position_of_refined_influence_node :
  (rule, var) influence_node -> Locality.t

module InfluenceNodeMap : SetMap.Map with type elt = (int, int) influence_node

type location = Direct of int | Side_effect of int
type 'a pair = 'a * 'a

type influence_map = {
  nodes: (rule, var) influence_node list;
  positive: location pair list InfluenceNodeMap.t InfluenceNodeMap.t;
  negative: location pair list InfluenceNodeMap.t InfluenceNodeMap.t;
}

val influence_map_to_json : accuracy_level * influence_map -> Yojson.Basic.t
val influence_map_of_json : Yojson.Basic.t -> accuracy_level * influence_map

val nodes_of_influence_map_to_json :
  accuracy_level * (rule, var) influence_node list -> Yojson.Basic.t

val nodes_of_influence_map_of_json :
  Yojson.Basic.t -> accuracy_level * (rule, var) influence_node list

val local_influence_map_to_json :
  accuracy_level
  * int
  * int option
  * int option
  * (rule, var) influence_node option
  * influence_map ->
  Yojson.Basic.t

val local_influence_map_of_json :
  Yojson.Basic.t ->
  accuracy_level
  * int
  * int option
  * int option
  * (rule, var) influence_node option
  * influence_map

type dead_rules = rule list

val dead_rules_of_json : Yojson.Basic.t -> dead_rules
val dead_rules_to_json : dead_rules -> Yojson.Basic.t

type agent_kind = {
  agent_id: int;
  agent_ast: string;
  agent_position: Locality.t list;
}

type dead_agents = agent_kind list

val json_to_dead_agents : Yojson.Basic.t -> dead_agents
val json_of_dead_agents : dead_agents -> Yojson.Basic.t

type separating_transitions = (rule * (string * string) list) list

val separating_transitions_of_json : Yojson.Basic.t -> separating_transitions
val separating_transitions_to_json : separating_transitions -> Yojson.Basic.t

type 'site_graph lemma = { hyp: 'site_graph; refinement: 'site_graph list }

type binding_state =
  | Free
  | Wildcard
  | Bound_to_unknown
  | Bound_to of int
  | Binding_type of string * string

type agent =
  string
  * (string
    * string option
    * binding_state option
    * (int option * int option) option)
    list

type 'site_graph poly_constraints_list = (string * 'site_graph lemma list) list

val lemma_to_json :
  ('site_graph -> Yojson.Basic.t) -> 'site_graph lemma -> Yojson.Basic.t

val lemma_of_json :
  (Yojson.Basic.t -> 'site_graph) -> Yojson.Basic.t -> 'site_graph lemma

val lemmas_list_to_json_gen :
  ('a -> Yojson.Basic.t) ->
  (string * (string * 'a) list lemma list) list ->
  Yojson.Basic.t

val lemmas_list_of_json_gen :
  (Yojson.Basic.t -> 'a) ->
  Yojson.Basic.t ->
  (string * (string * 'a) list lemma list) list

val lemmas_list_to_json :
  (string * agent list lemma list) list -> Yojson.Basic.t

val lemmas_list_of_json :
  Yojson.Basic.t -> (string * agent list lemma list) list

val get_hyp : 'site_graph lemma -> 'site_graph
val get_refinement : 'site_graph lemma -> 'site_graph list

val string_of_binding_type :
  ?binding_type_symbol:string ->
  agent_name:string ->
  site_name:string ->
  unit ->
  string

val string_of_label_list : location pair list -> string
