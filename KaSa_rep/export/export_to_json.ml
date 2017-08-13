(**
  * export_to_KaSim.ml
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

    let get_local_influence_map
        ?accuracy_level:(accuracy_level=Public_data.Low)
        ?bwd  ?fwd  ~total
        ~origin
        state =
      let rule_id =
        try
          let origin = Public_data.influence_node_of_json origin in
          begin
            match origin with
            | Public_data.Rule a ->
              Ckappa_sig.rule_id_of_int (a.Public_data.rule_id)
            | Public_data.Var a ->
              Ckappa_sig.rule_id_of_int (a.Public_data.var_id)
          end
        with
        | _ -> Ckappa_sig.rule_id_of_int 0
      in     
      let state, influence_map =
        get_local_influence_map ~accuracy_level ?fwd ?bwd ~total
          rule_id state
      in
      state, Remanent_state.influence_map_to_json (accuracy_level,influence_map)

    let origin_of_influence_map state =
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let parameters = get_parameters state in
      let state, handler = get_handler state in
      let state, compil = get_c_compilation state in
      let error = get_errors state in
      let error, id_json =
        if nrules = 0 && nvars = 0 then
          Exception.warn parameters error __POS__ Exit  `Null
        else
          let error, id =
            convert_id
              parameters error handler compil (Ckappa_sig.rule_id_of_int 0)
          in
          error, Public_data.influence_node_to_json id
      in
      let state = set_errors error state in
      state, id_json

    let previous_node_in_influence_map state json =
      let node = Public_data.influence_node_of_json json in
      let parameters = get_parameters state in
      let error = get_errors state in
      let state, handler = get_handler state in
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let state, compil = get_c_compilation state in
      let id_int =
        match node with
        | Public_data.Rule a -> a.Public_data.rule_id
        | Public_data.Var a -> a.Public_data.var_id
      in
      let error, node =
        if id_int = 0 then
          convert_id parameters error handler compil
            (Ckappa_sig.rule_id_of_int (nrules+nvars))
        else
          convert_id parameters error handler compil
            (Ckappa_sig.rule_id_of_int (id_int-1))
      in
      let state = set_errors error state in
      let json = Public_data.influence_node_to_json node in
      state, json

    let next_node_in_influence_map state json =
      let node = Public_data.influence_node_of_json json in
      let parameters = get_parameters state in
      let error = get_errors state in
      let state, handler = get_handler state in
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let n = nrules+nvars-1 in
      let state, compil = get_c_compilation state in
      let id_int =
        match node with
        | Public_data.Rule a -> a.Public_data.rule_id
        | Public_data.Var a -> a.Public_data.var_id
      in
      if id_int = n then
        let state = set_errors error state in
        origin_of_influence_map state
      else
        let error, node =
          convert_id parameters error handler compil
            (Ckappa_sig.rule_id_of_int (id_int+1))
        in
        let json = Public_data.influence_node_to_json node in
        set_errors error state, json


    let get_dead_rules state =
      let state, rules = get_dead_rules state in
      state, Public_data.dead_rules_to_json rules

    let get_constraints_list state =
      get_constraints_list_to_json state

    let get_errors_json state =
      let error = get_errors state in
      Exception_without_parameter.to_json error

    let to_json = Remanent_state.to_json

    let of_json = Remanent_state.of_json
  end
