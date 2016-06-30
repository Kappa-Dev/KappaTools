(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Jun 29 2016>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)


let main () =
  let state = Export.init  ~called_from:Remanent_parameters_sig.KaSa () in
  let parameters = Remanent_state.get_parameters state in
  let state =
    if (Remanent_parameters.get_do_contact_map parameters)
    then
      let state,_ =
        Export.get_internal_contact_map ~accuracy_level:Remanent_state.Low state
      in
      let state, handler = Export.get_handler state in
      let error = Remanent_state.get_errors state in
      let error = Print_handler.dot_of_contact_map parameters error handler in
      Remanent_state.set_errors error state
    else
      let state, c_compil = Export.get_c_compilation state in
      let state, handler = Export.get_handler state in
      let parameters = Remanent_state.get_parameters state in
      let parameters = Remanent_parameters.update_prefix  parameters "Compilation:" in
      let error = Remanent_state.get_errors state in
      let error =
        if Remanent_parameters.get_trace parameters || Print_cckappa.trace
        then Print_cckappa.print_compil parameters error handler c_compil
        else error
      in
      let state = Remanent_state.set_errors error state in
      state
  in
  let state =
    if Remanent_parameters.get_do_influence_map parameters
    then
      let state, influence_map =
        Export.get_internal_influence_map
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
      in
      let state, c_compil = Export.get_c_compilation state in
      let state, handler = Export.get_handler state in
      let error = Remanent_state.get_errors state in
      let error =
        Print_quarks.dot_of_influence_map parameters error handler c_compil influence_map
      in
      Remanent_state.set_errors error state
    else
      state
  in

  (*-----------------------------------------------------------------------*)
  let state, reachability_result_opt =
    if Remanent_parameters.get_do_reachability_analysis parameters
    then
      let state, output = Export.get_reachability_analysis state in
      state, Some output
    else
      state, None
  in

  (*-----------------------------------------------------------------------*)
  (*Stochastic flow of information*)
  let state, stochastic_flow_opt =
    if Remanent_parameters.get_do_stochastic_flow_of_information parameters
    then
      let state, output = Export.get_ctmc_flow state in
      state, Some output
    else
      state, None
  in
  let state, ode_flow_opt =
    if Remanent_parameters.get_do_ODE_flow_of_information parameters
    then
      let state, output = Export.get_ode_flow state in
      state, Some output
    else
      state, None
  in
  let _ = state, reachability_result_opt, stochastic_flow_opt, ode_flow_opt in
  let _ = Exception.print parameters (Remanent_state.get_errors state) in
  ()

let _ = main ()
