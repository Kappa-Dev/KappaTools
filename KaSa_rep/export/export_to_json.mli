(**
  * export.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Aug 13 2017>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type =
  sig
    type state

    val init: ?compil:Ast.parsing_compil -> unit -> state

    val get_contact_map:
      ?accuracy_level:Public_data.accuracy_level ->
      state -> state * Yojson.Basic.json

    val get_influence_map:
      ?accuracy_level:Public_data.accuracy_level ->
      state -> state * Yojson.Basic.json

    val get_local_influence_map:
      ?accuracy_level:Public_data.accuracy_level ->
      ?bwd:int -> ?fwd:int -> total:int ->
      origin:Yojson.Basic.json ->
      state -> state * Yojson.Basic.json

    val origin_of_influence_map: state -> state * Yojson.Basic.json
    val next_node_in_influence_map:
      state -> Yojson.Basic.json -> state * Yojson.Basic.json
    val previous_node_in_influence_map:
      state -> Yojson.Basic.json -> state * Yojson.Basic.json

    val get_dead_rules: state -> state * Yojson.Basic.json

    val get_constraints_list: state -> state * Yojson.Basic.json

    val get_errors: state -> Exception_without_parameter.method_handler
    val get_errors_json: state -> Yojson.Basic.json

    val to_json: state -> Yojson.Basic.json

    val of_json:
      Yojson.Basic.json ->
      Exception_without_parameter.method_handler *
      Public_data.contact_map Public_data.AccuracyMap.t *
      Remanent_state.influence_map Public_data.AccuracyMap.t *
      Public_data.dead_rules option * Remanent_state.constraints_list option *
      Public_data.separating_transitions option


  end

module Export:
  functor (Reachability : Analyzer.Analyzer) ->
    Type

(*val get_internal_constraints_list: state -> state * Yojson.Basic.json*)
