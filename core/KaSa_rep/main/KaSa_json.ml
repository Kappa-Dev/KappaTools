(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Dec 09 2018>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let main () =
  let errors = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option errors in
  let module A =
    (val Domain_selection.select_domain
           ~reachability_parameters:
             (Remanent_parameters.get_reachability_analysis_parameters
                parameters)
           ())
  in
  let export_to_json =
    (module Export_to_json.Export (A) : Export_to_json.Type)
  in
  let module Export_to_json = (val export_to_json : Export_to_json.Type) in
  let state = Export_to_json.init () in
  let state, cm = Export_to_json.get_contact_map state in
  let _ = Public_data.contact_map_of_json cm in
  let state, im = Export_to_json.get_influence_map state in
  let _ = Public_data.influence_map_of_json im in
  let state, dr = Export_to_json.get_dead_rules state in
  let _ = Public_data.dead_rules_of_json dr in
  let state, constraints = Export_to_json.get_constraints_list state in
  let _ = Remanent_state.lemmas_list_of_json constraints in
  let errors = Export_to_json.get_errors state in
  let error_json = Exception_without_parameter.to_json errors in
  let json = Export_to_json.to_json state in
  let () =
    Printf.fprintf stdout "%s\n%s\n"
      (Yojson.Basic.to_string error_json)
      (Yojson.Basic.to_string json)
  in
  let _ = Export_to_json.of_json json in
  ()

let () = main ()
