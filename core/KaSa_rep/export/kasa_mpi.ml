(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module A =
  (val Domain_selection.select_domain
         ~reachability_parameters:
           {
             Remanent_parameters_sig.views = true;
             Remanent_parameters_sig.site_across_bonds = true;
             Remanent_parameters_sig.parallel_bonds = true;
             Remanent_parameters_sig.dynamic_contact_map = true;
             Remanent_parameters_sig.counter_domain = Remanent_parameters_sig.Mi;
             Remanent_parameters_sig.counters = true;
           }
         ())

include Export_to_json.Export (A)

let gState =
  let compil = Ast.empty_compil in
  ref (init ~compil ())

let send_exception post ?id e =
  let head =
    match id with
    | None -> []
    | Some id -> [ "id", `Int id ]
  in
  let reply =
    `Assoc
      (head
      @ [
          "code", `String "ERROR";
          ( "data",
            Exception_without_parameter.to_json
              (Exception_without_parameter.add_uncaught_error
                 (Exception_without_parameter.build_uncaught_exception
                    ~file_name:"kasa_mpi" e)
                 Exception_without_parameter.empty_error_handler) );
        ])
  in
  post (Yojson.Basic.to_string reply)

let send_response post id x =
  let reply =
    if Exception_without_parameter.is_empty_error_handler (get_errors !gState)
    then
      `Assoc [ "id", `Int id; "code", `String "SUCCESS"; "data", x ]
    else
      `Assoc
        [
          "id", `Int id;
          "code", `String "ERROR";
          "data", Exception_without_parameter.to_json (get_errors !gState);
        ]
  in
  post (Yojson.Basic.to_string reply)

let unpack_request text =
  try
    match Yojson.Basic.from_string text with
    | `Assoc [ ("id", `Int id); ("data", data) ]
    | `Assoc [ ("data", data); ("id", `Int id) ] ->
      Some (id, data)
    | _ -> None
  with _ -> None

let on_message post text =
  match unpack_request text with
  | Some (id, `List [ `String "INIT"; compil ]) ->
    (try
       let compil = Ast.compil_of_json compil in
       let () = gState := init ~compil () in
       send_response post id `Null
     with e -> send_exception post ~id e)
  | Some (id, `List [ `String "CONTACT_MAP"; acc ]) ->
    let accuracy_level = Public_data.accuracy_of_json acc in
    let state, cm = get_contact_map ~accuracy_level !gState in
    let () = gState := state in
    send_response post id cm
  | Some (id, (`List [ `String "CONTACT_MAP" ] | `String "CONTACT_MAP")) ->
    let accuracy_level = Public_data.Low in
    let state, cm = get_contact_map ~accuracy_level !gState in
    let () = gState := state in
    send_response post id cm
  | Some (id, `List [ `String "INFLUENCE_MAP"; acc; fwd; bwd; total; origin ])
    ->
    let accuracy_level = Public_data.accuracy_of_json acc in
    let error_msg = "bad int" in
    let fwd = JsonUtil.to_option (JsonUtil.to_int ~error_msg) fwd in
    let bwd = JsonUtil.to_option (JsonUtil.to_int ~error_msg) bwd in
    let total = JsonUtil.to_int ~error_msg total in
    let origin =
      JsonUtil.to_option Public_data.short_influence_node_of_json origin
    in
    let state, im =
      get_local_influence_map ~accuracy_level ?fwd ?bwd ~total ?origin !gState
    in
    let () = gState := state in
    send_response post id im
  | Some (id, `List [ `String "INFLUENCE_MAP"; fwd; bwd; total; origin ]) ->
    let accuracy_level = Public_data.Low in
    let error_msg = "bad int" in
    let fwd = JsonUtil.to_option (JsonUtil.to_int ~error_msg) fwd in
    let bwd = JsonUtil.to_option (JsonUtil.to_int ~error_msg) bwd in
    let total = JsonUtil.to_int ~error_msg total in
    let origin =
      JsonUtil.to_option Public_data.short_influence_node_of_json origin
    in
    let state, im =
      get_local_influence_map ~accuracy_level ?fwd ?bwd ~total ?origin !gState
    in
    let () = gState := state in
    send_response post id im
  | Some (id, `List [ `String "INFLUENCE_MAP"; acc ]) ->
    let accuracy_level = Public_data.accuracy_of_json acc in
    let state, im = get_influence_map ~accuracy_level !gState in
    let () = gState := state in
    send_response post id im
  | Some (id, (`List [ `String "INFLUENCE_MAP" ] | `String "INFLUENCE_MAP")) ->
    let accuracy_level = Public_data.Low in
    let state, im = get_influence_map ~accuracy_level !gState in
    let () = gState := state in
    send_response post id im
  | Some
      ( id,
        ( `List [ `String "INFLUENCE_MAP_ORIGINAL_NODE" ]
        | `String "INFLUENCE_MAP_ORIGINAL_NODE" ) ) ->
    let state, im = origin_of_influence_map !gState in
    let () = gState := state in
    send_response post id im
  | Some (id, `List [ `String "INFLUENCE_MAP_NEXT_NODE"; origin ]) ->
    let origin =
      JsonUtil.to_option Public_data.short_influence_node_of_json origin
    in
    let state, im = next_node_in_influence_map !gState origin in
    let () = gState := state in
    send_response post id im
  | Some (id, `List [ `String "INFLUENCE_MAP_PREVIOUS_NODE"; origin ]) ->
    let origin =
      JsonUtil.to_option Public_data.short_influence_node_of_json origin
    in
    let state, im = previous_node_in_influence_map !gState origin in
    let () = gState := state in
    send_response post id im
  | Some (id, `List [ `String "INFLUENCE_MAP_ALL_NODES"; acc ]) ->
    let accuracy_level = Public_data.accuracy_of_json acc in
    let state, rules = get_all_nodes_of_influence_map ~accuracy_level !gState in
    let () = gState := state in
    send_response post id rules
  | Some
      ( id,
        ( `List [ `String "INFLUENCE_MAP_ALL_NODES" ]
        | `String "INFLUENCE_MAP_ALL_NODES" ) ) ->
    let accuracy_level = Public_data.Low in
    let state, rules = get_all_nodes_of_influence_map ~accuracy_level !gState in
    let () = gState := state in
    send_response post id rules
  | Some
      ( id,
        ( `List [ `String "INFLUENCE_MAP_NODES_LOCATION" ]
        | `String "INFLUENCE_MAP_NODES_LOCATION" ) ) ->
    let state, list = get_influence_map_nodes_location !gState in
    let () = gState := state in
    send_response post id list
  | Some (id, (`List [ `String "DEAD_RULES" ] | `String "DEAD_RULES")) ->
    let state, rules = get_dead_rules !gState in
    let () = gState := state in
    send_response post id rules
  | Some (id, (`List [ `String "DEAD_AGENTS" ] | `String "DEAD_AGENTS")) ->
    let state, agents = get_dead_agents !gState in
    let () = gState := state in
    send_response post id agents
  | Some
      ( id,
        ( `List [ `String "NON_WEAKLY_REVERSIBLE_TRANSITIONS" ]
        | `String "NON_WEAKLY_REVERSIBLE_TRANSITIONS" ) ) ->
    let state, transitions = get_separating_transitions !gState in
    let () = gState := state in
    send_response post id transitions
  | Some (id, (`List [ `String "CONSTRAINTS" ] | `String "CONSTRAINTS")) ->
    let state, out = get_constraints_list !gState in
    let () = gState := state in
    send_response post id out
  | Some (id, `List [ `String "POLYMERS"; acc_cm ]) ->
    let accuracy_level_cm = Public_data.accuracy_of_json acc_cm in
    let state, out =
      get_scc_decomposition ~accuracy_level_cm
        ~accuracy_level_scc:Public_data.Low !gState
    in
    let () = gState := state in
    send_response post id out
  | Some (id, `List [ `String "POLYMERS"; acc_cm; acc_scc ]) ->
    let accuracy_level_cm = Public_data.accuracy_of_json acc_cm in
    let accuracy_level_scc = Public_data.accuracy_of_json acc_scc in
    let state, out =
      get_scc_decomposition ~accuracy_level_cm ~accuracy_level_scc !gState
    in
    let () = gState := state in
    send_response post id out
  | Some (id, (`List [ `String "POLYMERS" ] | `String "POLYMERS")) ->
    let state, out =
      get_scc_decomposition ~accuracy_level_cm:Public_data.Low
        ~accuracy_level_scc:Public_data.Low !gState
    in
    let () = gState := state in
    send_response post id out
  | Some (id, x) ->
    send_exception post ~id
      (Yojson.Basic.Util.Type_error ("Invalid KaSa request", x))
  | None ->
    let message =
      "Not a valid { id : _int_, data : ... } JSON message: " ^ text
    in
    let reply =
      `Assoc
        [
          "code", `String "ERROR";
          ( "data",
            Exception_without_parameter.to_json
              (Exception_without_parameter.add_uncaught_error
                 (Exception_without_parameter.build_uncaught_exception
                    ~file_name:"kasa_mpi" ~message Exit)
                 Exception_without_parameter.empty_error_handler) );
        ]
    in
    post (Yojson.Basic.to_string reply)
