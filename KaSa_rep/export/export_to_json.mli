(**
  * export.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Mar 15 2017>
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

    val get_dead_rules: state -> state * Yojson.Basic.json

    val get_constraints_list: state -> state * Yojson.Basic.json

    val get_errors: state -> Yojson.Basic.json

    val to_json: state -> Yojson.Basic.json

    val of_json:
      Yojson.Basic.json ->
      Exception_without_parameter.method_handler *
      Public_data.contact_map Public_data.AccuracyMap.t *
      Remanent_state.influence_map Public_data.AccuracyMap.t *
      int list option * Remanent_state.constraints_list option *
      Public_data.separating_transitions option


  end

module Export:
  functor (Reachability : Analyzer.Analyzer) ->
    Type

(*val get_internal_constraints_list: state -> state * Yojson.Basic.json*)
