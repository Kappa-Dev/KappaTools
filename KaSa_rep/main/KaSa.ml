(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Nov 21 2016>
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
      ~with_views_domain:(Remanent_parameters.get_view_analysis parameters)
      ~with_parallel_bonds_domain:(Remanent_parameters.get_parallel_bonds_analysis parameters)
      ~with_site_accross_bonds_domain:(Remanent_parameters.get_site_accross_bonds_analysis parameters)
      ()
        )
  in
  let export_to_kasa =
    (module Export_to_KaSa.Export(A) : Export_to_KaSa.Type) in
  let module Export_to_KaSa =
    (val export_to_kasa : Export_to_KaSa.Type)
  in
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
  let state =
    if Remanent_parameters.get_do_reachability_analysis parameters
    then
      if Remanent_parameters.get_trace parameters then
        Export_to_KaSa.output_constraints_list state
      else
        fst (Export_to_KaSa.get_reachability_analysis state)
      else
      state
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
  let _ = state, stochastic_flow_opt, ode_flow_opt in
  let _ = Exception.print parameters (Export_to_KaSa.get_errors state) in
  ()

let () = main ()
