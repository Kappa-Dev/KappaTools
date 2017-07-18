(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type accuracy_level = Low | Medium | High | Full

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

val dead_rules_of_json : Yojson.Basic.json -> int (*rule_id*) list

type separating_transitions = (string * int (*rule_id*) * string) list

val separating_transitions_of_json: Yojson.Basic.json -> separating_transitions
