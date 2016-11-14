(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Nov 14 2016>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let main () =
  let state = Export_to_KaSa.init () in
  let parameters = Export_to_KaSa.get_parameters state in
  let state =
    let bool, state  =
      if (Remanent_parameters.get_do_contact_map parameters)
      then
        match Remanent_parameters.get_contact_map_accuracy_level parameters
        with
        | Remanent_parameters_sig.None
        | Remanent_parameters_sig.Low ->
          true,
          Export_to_KaSa.output_contact_map
            ~accuracy_level:Remanent_state.Low state
        | Remanent_parameters_sig.Medium
        | Remanent_parameters_sig.High
        | Remanent_parameters_sig.Full -> false, state
      else false, state
    in
    if bool then state
    else
    if Remanent_parameters.get_trace parameters || Print_cckappa.trace
    then
      let state, c_compil = Export_to_KaSa.get_c_compilation state in
      let parameters' =
        Remanent_parameters.update_prefix  parameters "Compilation:" in
      let state = Export_to_KaSa.set_parameters parameters' state in
      let state = Export_to_KaSa.dump_c_compil state c_compil in
      let state = Export_to_KaSa.set_parameters parameters state in
      state
    else
      state
  in
  let state =
    if Remanent_parameters.get_do_influence_map parameters
    then
      Export_to_KaSa.output_influence_map
        ~accuracy_level:(match
                           Remanent_parameters.get_influence_map_accuracy_level parameters
                         with
                         | Remanent_parameters_sig.None
                         | Remanent_parameters_sig.Low ->
                           Remanent_state.Low
                         | Remanent_parameters_sig.Medium
                         | Remanent_parameters_sig.High
                         | Remanent_parameters_sig.Full -> Remanent_state.Medium)
        state
    else
      state
  in
  (*-----------------------------------------------------------------------*)
  let state, reachability_result_opt =
    if Remanent_parameters.get_do_reachability_analysis parameters
    then
      let state, output = Export_to_KaSa.get_reachability_analysis state in
      state, Some output
    else
      state, None
  in
  let state =
    if (Remanent_parameters.get_do_contact_map parameters)
    then
      match Remanent_parameters.get_contact_map_accuracy_level parameters
      with
      | Remanent_parameters_sig.Medium
      | Remanent_parameters_sig.High
      | Remanent_parameters_sig.Full ->
        Export_to_KaSa.output_contact_map
          ~accuracy_level:Remanent_state.Medium
          state
      | Remanent_parameters_sig.None
      | Remanent_parameters_sig.Low -> state
    else state
  in
  (*-----------------------------------------------------------------------*)
  (*Stochastic flow of information*)
  let state, stochastic_flow_opt =
    if Remanent_parameters.get_do_stochastic_flow_of_information parameters
    then
      let state, output = Export_to_KaSa.get_ctmc_flow state in
      state, Some output
    else
      state, None
  in
  let state, ode_flow_opt =
    if Remanent_parameters.get_do_ODE_flow_of_information parameters
    then
      let state, output = Export_to_KaSa.get_ode_flow state in
      state, Some output
    else
      state, None
  in
  (*-----------------------------------------------------------------------*)
  let _ = state, reachability_result_opt, stochastic_flow_opt, ode_flow_opt in
  (*Print constraint list*)
  let error = Export_to_KaSa.get_errors state in
  let error, l =
    match Export_to_KaSa.get_constraint_list state with
    | None -> Exception.warn parameters error __POS__ Exit []
    | Some l -> error, l
  in
  let state, handler = Export_to_KaSa.get_handler state in
  let error =
    Remanent_state.print_constraint_list_list
      (Remanent_parameters.get_logger parameters) parameters
      error
      handler
      l
  in
  (*print internal constraint list*)
  let error, l =
    match Export_to_KaSa.get_internal_constraint_list state with
    | None -> Exception.warn parameters error __POS__ Exit []
    | Some l -> error, l
  in
  let error =
    Remanent_state.print_internal_constraint_list_list
      (Remanent_parameters.get_logger parameters) parameters
      error
      handler
      l
  in
  let state = Export_to_KaSa.set_errors error state in
  let state, json = Export_to_KaSa.get_constraint_list_to_json state in
  (*print in the format of Json*)
  let _ = Printf.fprintf stdout "%s" (Yojson.Basic.to_string json) in
  let _ = Exception.print parameters (Export_to_KaSa.get_errors state) in
  ()

let () = main ()

let _ = Export_to_json.init
