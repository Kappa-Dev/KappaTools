(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Aug 11 2017>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

(*module B =
  (val Domain_selection.select_domain
      ~reachability_parameters:{
        Remanent_parameters_sig.views = true;
        Remanent_parameters_sig.site_across_bonds = true;
        Remanent_parameters_sig.parallel_bonds = true;
        Remanent_parameters_sig.dynamic_contact_map = true;
      } ())

include Export.Export(B)

let gState =
  let compil = Ast.empty_compil in
  ref (init ~compil ~called_from:Remanent_parameters_sig.Server ())*)


let main () =
  let start_time = Sys.time () in
  let errors = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option errors in
  let module A =
    (val Domain_selection.select_domain
        ~reachability_parameters:
          (Remanent_parameters.get_reachability_analysis_parameters parameters)
        ())
  in
  let export_to_kasa =
    (module Export_to_KaSa.Export(A) : Export_to_KaSa.Type)
  in
  let module Export_to_KaSa =
    (val export_to_kasa : Export_to_KaSa.Type)
  in
  let state = Export_to_KaSa.init () in
  let state =
    if (Remanent_parameters.get_compute_symmetries parameters)
    then
      fst (Export_to_KaSa.get_env state)
    else
      state
  in
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
            ~accuracy_level:Public_data.Low state
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
  (*-----------------------------------------------------------------------*)
  (*WORK IN PROCESS:*)
  (*let gstate, string_contact_map = get_contact_map !gState in
  let () =
    Contact_map_scc.convert_contact_map_to_graph parameters errors
      string_contact_map
  in*)

  (*let _ =
    let state, output = Export_to_KaSa.get_graph_scc state in
    state, Some output
  in*)
  (*-----------------------------------------------------------------------*)
  (**)
  let state =
    if Remanent_parameters.get_do_influence_map parameters
    then
      let state =
        Export_to_KaSa.output_influence_map
          ~accuracy_level:
            (match
               Remanent_parameters.get_influence_map_accuracy_level parameters
             with
             | Remanent_parameters_sig.None
             | Remanent_parameters_sig.Low -> Public_data.Low
             | Remanent_parameters_sig.Medium -> Public_data.Medium
             | Remanent_parameters_sig.High
             | Remanent_parameters_sig.Full -> Public_data.High)
          state
      in
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
          ~accuracy_level:Public_data.Medium
          state
      | Remanent_parameters_sig.None
      | Remanent_parameters_sig.Low -> state
    else state
  in
  let state =
    if (Remanent_parameters.get_compute_symmetries parameters)
    then
    match Remanent_parameters.get_contact_map_accuracy_level parameters
    with
    | Remanent_parameters_sig.Medium
    | Remanent_parameters_sig.High
    | Remanent_parameters_sig.Full ->
      Export_to_KaSa.output_symmetries
        ~accuracy_level:Public_data.Medium
        state
    | Remanent_parameters_sig.None
    | Remanent_parameters_sig.Low ->
      Export_to_KaSa.output_symmetries
        ~accuracy_level:Public_data.Low
        state
    else
      state
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
  let _ = Exception.print parameters (Export_to_KaSa.get_errors state) in
  let () =
    if Remanent_parameters.get_print_efficiency parameters
    then
      let end_time = Sys.time () in
      let cpu_time = end_time -. start_time in
      let handler, dead_rules, separating_transitions =
        Export_to_KaSa.get_data state
      in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "CPU time: %g s." cpu_time
      in
      let () =
        match
          handler
        with
        | None -> ()
        | Some l ->
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "; rules: %i"
            l.Cckappa_sig.nrules
          in
      let () =
        match
          dead_rules
        with
        | None -> ()
        | Some l ->
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "; dead rules: %i"
            (List.length l)
      in
      let () =
        match
          separating_transitions
        with
        | None -> ()
        | Some l ->
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "; separating transitions: %i"
            (List.length l)
      in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "."
      in
      let () =
        Loggers.print_newline
          (Remanent_parameters.get_logger parameters)
      in
      ()
  in
  ()

let () = main ()
let _ = Local_influence_map.explore_influence_map
