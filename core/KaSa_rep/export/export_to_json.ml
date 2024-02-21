(**
  * export_to_KaSim.ml
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

module Export =
functor
  (A : Analyzer.Analyzer)
  ->
  struct
    include Export.Export (A)

    let init ?compil () =
      init ?compil ~called_from:Remanent_parameters_sig.Server ()

    let get_contact_map ?(accuracy_level = Public_data.Low) state =
      let state, cm = get_contact_map ~accuracy_level state in
      state, Public_data.contact_map_to_json (accuracy_level, cm)

    let get_scc_decomposition ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low) state =
      let state, scc =
        get_scc_decomposition ~accuracy_level_cm ~accuracy_level_scc state
      in
      state, Public_data.scc_to_json (accuracy_level_cm, accuracy_level_scc, scc)

    let get_influence_map_nodes_location state =
      let state, list = get_pos_of_rules_and_vars state in
      state, Public_data.pos_of_rules_and_vars_to_json list

    let get_influence_map ?(accuracy_level = Public_data.Low) state =
      let state, influence_map = get_influence_map ~accuracy_level state in
      state, Public_data.influence_map_to_json (accuracy_level, influence_map)

    let get_all_nodes_of_influence_map ?(accuracy_level = Public_data.Low) state
        =
      let state, nodes = get_all_nodes_of_influence_map ~accuracy_level state in
      state, Public_data.nodes_of_influence_map_to_json (accuracy_level, nodes)

    let convert_id_refined state i =
      let parameters = get_parameters state in
      let state, handler = get_handler state in
      let state, compil = get_c_compilation state in
      let error = get_errors state in
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let error, refined_id_opt =
        if i < nrules + nvars then (
          let error, refined_id =
            convert_id_refined parameters error handler compil
              (Ckappa_sig.rule_id_of_int i)
          in
          error, Some refined_id
        ) else
          Exception.warn parameters error __POS__ Exit None
      in
      let state = set_errors error state in
      state, refined_id_opt

    let origin_of_influence_map state = convert_id_refined state 0

    let get_local_influence_map ?(accuracy_level = Public_data.Low) ?bwd ?fwd
        ~total ?origin state =
      let state, rule_id_int =
        match origin with
        | Some (Public_data.Rule a) -> state, a
        | Some (Public_data.Var a) ->
          let state, n = nrules state in
          state, a + n
        | None -> state, 0
      in
      let rule_id = Ckappa_sig.rule_id_of_int rule_id_int in
      let state, influence_map =
        get_local_influence_map ~accuracy_level ?fwd ?bwd ~total rule_id state
      in
      let state, origin = convert_id_refined state rule_id_int in
      ( state,
        Public_data.local_influence_map_to_json
          (accuracy_level, total, bwd, fwd, origin, influence_map) )

    let short_origin_of_influence_map state =
      let state, origin_opt = origin_of_influence_map state in
      state, Option_util.map Public_data.short_node_of_refined_node origin_opt

    let previous_node_in_influence_map state short_id_opt =
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let n = nrules + nvars - 1 in
      let state, next_opt =
        if n = -1 then
          state, None
        else (
          let state, short_id_opt =
            match short_id_opt with
            | None -> short_origin_of_influence_map state
            | Some _ -> state, short_id_opt
          in
          let parameters = get_parameters state in
          let error = get_errors state in
          let error, id_int =
            match short_id_opt with
            | Some (Public_data.Rule a) -> error, a
            | Some (Public_data.Var a) -> error, a + nrules
            | None -> Exception.warn parameters error __POS__ Exit 0
          in
          let state = set_errors error state in
          if id_int = 0 then
            convert_id_refined state (max 0 n)
          else
            convert_id_refined state (id_int - 1)
        )
      in
      let json =
        JsonUtil.of_option Public_data.refined_influence_node_to_json next_opt
      in
      state, json

    let next_node_in_influence_map state short_id_opt =
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let n = nrules + nvars - 1 in
      let state, node_opt =
        if n = -1 then
          state, None
        else (
          let state, short_id_opt =
            match short_id_opt with
            | None -> short_origin_of_influence_map state
            | Some _ -> state, short_id_opt
          in
          let parameters = get_parameters state in
          let error = get_errors state in
          let error, id_int =
            match short_id_opt with
            | Some (Public_data.Rule a) -> error, a
            | Some (Public_data.Var a) -> error, a + nrules
            | None -> Exception.warn parameters error __POS__ Exit 0
          in
          let state = set_errors error state in
          if id_int = n then
            origin_of_influence_map state
          else
            convert_id_refined state (id_int + 1)
        )
      in
      let json =
        JsonUtil.of_option Public_data.refined_influence_node_to_json node_opt
      in
      state, json

    let origin_of_influence_map state =
      let state, node = origin_of_influence_map state in
      state, JsonUtil.of_option Public_data.refined_influence_node_to_json node

    let get_dead_rules state =
      let state, rules = get_dead_rules state in
      state, Public_data.dead_rules_to_json rules

    let get_dead_agents state =
      let state, agents = get_dead_agents state in
      state, Public_data.json_of_dead_agents agents

    let get_separating_transitions state =
      let state, separating_transitions = get_separating_transitions state in
      state, Public_data.separating_transitions_to_json separating_transitions

    let get_constraints_list state = get_constraints_list_to_json state

    let get_errors_json state =
      let error = get_errors state in
      Exception_without_parameter.to_json error

    let to_json = Remanent_state.to_json
    let of_json = Remanent_state.of_json
  end
