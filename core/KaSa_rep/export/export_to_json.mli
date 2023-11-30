(**
  * export.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Mar 19 2020>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type = sig
  type state

  val init : ?compil:Ast.parsing_compil -> unit -> state

  val get_contact_map :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * Yojson.Basic.t

  val get_scc_decomposition :
    ?accuracy_level_cm:Public_data.accuracy_level ->
    ?accuracy_level_scc:Public_data.accuracy_level ->
    state ->
    state * Yojson.Basic.t

  val get_influence_map_nodes_location : state -> state * Yojson.Basic.t

  val get_influence_map :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * Yojson.Basic.t

  val get_all_nodes_of_influence_map :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * Yojson.Basic.t

  val get_local_influence_map :
    ?accuracy_level:Public_data.accuracy_level ->
    ?bwd:int ->
    ?fwd:int ->
    total:int ->
    ?origin:(int, int) Public_data.influence_node ->
    state ->
    state * Yojson.Basic.t

  val origin_of_influence_map : state -> state * Yojson.Basic.t

  val next_node_in_influence_map :
    state ->
    (int, int) Public_data.influence_node option ->
    state * Yojson.Basic.t

  val previous_node_in_influence_map :
    state ->
    (int, int) Public_data.influence_node option ->
    state * Yojson.Basic.t

  val get_dead_rules : state -> state * Yojson.Basic.t
  val get_dead_agents : state -> state * Yojson.Basic.t
  val get_separating_transitions : state -> state * Yojson.Basic.t
  val get_constraints_list : state -> state * Yojson.Basic.t
  val get_errors : state -> Exception_without_parameter.method_handler
  val get_errors_json : state -> Yojson.Basic.t
  val to_json : state -> Yojson.Basic.t

  val of_json :
    Yojson.Basic.t ->
    Exception_without_parameter.method_handler
    * Public_data.contact_map Public_data.AccuracyMap.t
    * Public_data.influence_map Public_data.AccuracyMap.t
    * Public_data.dead_rules option
    * Remanent_state.constraints_list option
    * Public_data.separating_transitions option
end

module Export : functor (Reachability : Analyzer.Analyzer) -> Type

(*val get_internal_constraints_list: state -> state * Yojson.Basic.t*)
