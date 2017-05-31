(**
  * export_to_KaSim.ml
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

module Export =
functor (A:Analyzer.Analyzer) ->
  struct

    include Export.Export(A)

    let init ?compil () =
      init ?compil ~called_from:Remanent_parameters_sig.Server ()

    let get_contact_map
        ?accuracy_level:(accuracy_level=Public_data.Low) state =
      let state, cm = get_contact_map ~accuracy_level state in
      state, Public_data.contact_map_to_json (accuracy_level,cm)

    let get_influence_map
        ?accuracy_level:(accuracy_level=Public_data.Low) state =
      let state, influence_map = get_influence_map ~accuracy_level state in
      state, Remanent_state.influence_map_to_json (accuracy_level,influence_map)

    let get_dead_rules state =
      let state, rules = get_dead_rules state in
      state, Remanent_state.dead_rules_to_json rules

    let get_constraints_list state =
      get_constraints_list_to_json state

    let get_errors state =
      let error = get_errors state in
      Exception_without_parameter.to_json error

    let to_json = Remanent_state.to_json

    let of_json = Remanent_state.of_json
  end
