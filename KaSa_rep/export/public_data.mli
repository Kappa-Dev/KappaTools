(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* JSon labels *)
val prop:string
val bind:string
val domain_name:string
val refinements_list:string
val refinement_lemmas:string
val free:string
val bound:string
val wildcard:string

type accuracy_level = Low | Medium | High | Full
val accuracy_levels : accuracy_level list
val contact_map_accuracy_levels : accuracy_level list
val influence_map_accuracy_levels : accuracy_level list

val accuracy_to_string : accuracy_level -> string
val accuracy_of_string : string -> accuracy_level option

val accuracy_to_json : accuracy_level -> Yojson.Basic.json
val accuracy_of_json : Yojson.Basic.json -> accuracy_level

module AccuracyMap: SetMap.Map with type elt = accuracy_level

type site = {
     site_name: string;
     site_links: (int * int) list;
     site_states: string list;
}

type site_node = {
     site_node_name: string;
     site_node_sites: site list (*ocaml repr="array">*)
}

type site_graph = site_node list (*ocaml repr="array"*)
type contact_map = site_graph

val contact_map_to_json:
  accuracy_level * contact_map -> Yojson.Basic.json

val contact_map_of_json:
  Yojson.Basic.json -> accuracy_level * contact_map

type rule =
  {
    rule_id: int;
    rule_label: string ;
    rule_ast: string;
    rule_position: Locality.t
  }

type var =
  {
    var_id: int;
    var_label: string ;
    var_ast: string;
    var_position: Locality.t
  }

type ('rule, 'var) influence_node =
  | Rule of 'rule
  | Var of 'var

val short_influence_node_of_json:
  Yojson.Basic.json -> (int, int) influence_node

val short_influence_node_to_json:
  (int, int) influence_node -> Yojson.Basic.json

val refined_influence_node_of_json:
    Yojson.Basic.json -> (rule, var) influence_node

val refined_influence_node_to_json:
  (rule, var) influence_node -> Yojson.Basic.json

type dead_rules = rule list

val dead_rules_of_json : Yojson.Basic.json -> dead_rules
val dead_rules_to_json : dead_rules -> Yojson.Basic.json

type separating_transitions = (string * int (*rule_id*) * string) list

val separating_transitions_of_json: Yojson.Basic.json -> separating_transitions

type 'site_graph lemma =
  {
    hyp : 'site_graph ;
    refinement : 'site_graph list
  }

type binding_state =
  | Free
  | Wildcard
  | Bound_to_unknown
  | Bound_to of int
  | Binding_type of string * string


type agent = string * (string * string option * binding_state option) list

val lemma_to_json:
  ('site_graph -> Yojson.Basic.json) -> 'site_graph lemma -> Yojson.Basic.json

val lemma_of_json:
  (Yojson.Basic.json -> 'site_graph) -> Yojson.Basic.json -> 'site_graph lemma

val lemmas_list_of_json:
  Yojson.Basic.json -> (string * agent list lemma list ) list

val agent_gen_of_json:
  (Yojson.Basic.json -> 'interface) -> Yojson.Basic.json -> string * 'interface



val agent_of_json:
  Yojson.Basic.json -> agent

type 'site_graph poly_constraints_list =
  (string * 'site_graph lemma list) list

val poly_constraints_list_of_json:
  (Yojson.Basic.json -> 'site_graph) ->
  Yojson.Basic.json -> 'site_graph poly_constraints_list

val get_hyp: 'site_graph lemma -> 'site_graph
val get_refinement: 'site_graph lemma -> 'site_graph list
