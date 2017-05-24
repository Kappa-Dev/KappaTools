(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module A =
  (val Domain_selection.select_domain
      ~reachability_parameters:{
        Remanent_parameters_sig.views = true;
        Remanent_parameters_sig.site_across_bonds = true;
        Remanent_parameters_sig.parallel_bonds = true;
        Remanent_parameters_sig.dynamic_contact_map = true;
      } ())

include Export.Export(A)

let gState =
  let compil = Ast.empty_compil in
  ref (init ~compil ~called_from:Remanent_parameters_sig.Server ())

let send_exception post e =
  let reply =
    `Assoc [
      "code", `String "ERROR";
      "data", `String (Printexc.to_string e);
    ] in
  post (Yojson.Basic.to_string reply)

let send_response post x =
  let reply =
    if false (*TODO: there_are_errors_in !gState*)
    then `Assoc [
        "code", `String "ERROR";
        "data", Exception_without_parameter.to_json (get_errors !gState)
      ]
    else  `Assoc [ "code", `String "SUCCESS"; "data", x ] in
  post (Yojson.Basic.to_string reply)

let on_message post text =
  match Yojson.Basic.from_string text with
  | `List [ `String "INIT"; compil ] ->
    (try
       let compil = Ast.compil_of_json compil in
       let () = gState :=
           init ~compil ~called_from:Remanent_parameters_sig.Server () in
       send_response post `Null
     with e -> send_exception post e)
  | `List [ `String "CONTACT_MAP"; acc ] ->
    let accuracy_level = Remanent_state.accuracy_of_json acc in
    let state, cm = get_contact_map ~accuracy_level !gState in
    let () = gState := state in
    send_response
      post (Remanent_state.contact_map_to_json (accuracy_level,cm))
  | `List [ `String "CONTACT_MAP" ] | `String "CONTACT_MAP" ->
    let accuracy_level = Remanent_state.Low in
    let state, cm = get_contact_map ~accuracy_level !gState in
    let () = gState := state in
    send_response
      post (Remanent_state.contact_map_to_json (accuracy_level,cm))
  | `List [ `String "INFLUENCE_MAP"; acc ] ->
    let accuracy_level = Remanent_state.accuracy_of_json acc in
    let state, im = get_influence_map ~accuracy_level !gState in
    let () = gState := state in
    send_response
      post (Remanent_state.influence_map_to_json (accuracy_level,im))
  | `List [ `String "INFLUENCE_MAP" ] | `String "INFLUENCE_MAP" ->
    let accuracy_level = Remanent_state.Low in
    let state, im = get_influence_map ~accuracy_level !gState in
    let () = gState := state in
    send_response
      post (Remanent_state.influence_map_to_json (accuracy_level,im))
  | `List [ `String "DEAD_RULES" ] | `String "DEAD_RULES" ->
    let state, rules = get_dead_rules !gState in
    let () = gState := state in
    send_response post (Remanent_state.dead_rules_to_json rules)
  | `List [ `String "CONSTRAINTS" ] | `String "CONSTRAINTS" ->
    let state, out = get_constraints_list_to_json !gState in
    let () = gState := state in
    send_response post out
  | x ->
    let reply =
      `Assoc [
        "code", `String "ERROR";
        "data", Exception_without_parameter.to_json
          (Exception_without_parameter.add_uncaught_error
             (Exception_without_parameter.build_uncaught_exception
                None None
                (Yojson.Basic.Util.Type_error("Invalid KaSa request",x)))
             Exception_without_parameter.empty_error_handler)
      ] in
    post (Yojson.Basic.to_string reply)
