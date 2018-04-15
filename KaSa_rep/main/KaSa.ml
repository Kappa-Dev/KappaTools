(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Apr 15 2018>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

open Mat_inter

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
  (*-----------------------------------------------------------------------*)
  (*WORK IN PROCESS:*)
  let state =
    if Remanent_parameters.get_do_scc parameters
    then
      let accuracy_level_cm =
        match
          Remanent_parameters.get_contact_map_accuracy_level parameters
        with
        | Remanent_parameters_sig.None -> Public_data.Low
        | Remanent_parameters_sig.Low -> Public_data.Low
        | Remanent_parameters_sig.Medium
        | Remanent_parameters_sig.High
        | Remanent_parameters_sig.Full -> Public_data.Full
      in
      let accuracy_level_scc =
        match
          Remanent_parameters.get_scc_accuracy_level parameters
        with
        | Remanent_parameters_sig.None
        | Remanent_parameters_sig.Low -> Public_data.Low
        | Remanent_parameters_sig.Medium
        | Remanent_parameters_sig.High
        | Remanent_parameters_sig.Full -> Public_data.High
      in
      let state =
        Export_to_KaSa.output_scc_decomposition
          ~accuracy_level_cm ~accuracy_level_scc state
      in
        state
    else
      state
  in
  (*--------------------------------------------------------------------*)
  let state =
    let bool, state  =
      if (Remanent_parameters.get_do_contact_map parameters)
      then
        match Remanent_parameters.get_contact_map_accuracy_level parameters
        with
        | Remanent_parameters_sig.None
        | Remanent_parameters_sig.Low ->
          true,
          Export_to_KaSa.output_internal_contact_map
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
  let error = Export_to_KaSa.get_errors state in
  let state = Export_to_KaSa.set_errors error state in



(*-----------------------------------------------------------------------*)
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
    || Remanent_parameters.get_compute_separating_transitions parameters
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
        Export_to_KaSa.output_internal_contact_map
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
  let state, _stochastic_flow_opt =
    if Remanent_parameters.get_do_stochastic_flow_of_information parameters
    then
      let state, output = Export_to_KaSa.get_ctmc_flow state in
      state, Some output
    else
      state, None
  in
  let state, _ode_flow_opt =
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
      let handler, dead_rules, separating_transitions, transition_system_length  =
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
          let json = Public_data.separating_transitions_to_json l in
          let l = Public_data.separating_transitions_of_json json in
          let nr,nt =
            List.fold_left
              (fun (nr,nt) (_,l) ->
                 nr+1,
                 nt+List.length l)
              (0,0) l
          in
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "; separating transitions: %i in %i rules ;"
            nt nr
      in
      let () =
        Loggers.print_newline
          (Remanent_parameters.get_logger parameters)
      in
      let _ =
        match
          transition_system_length
        with
        | None -> ()
        | Some l ->
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "transition system lengths: %a"
              (fun fmt ->
                 List.iter (Format.fprintf fmt "%i;")) l
          in
          let () =
            Loggers.print_newline
              (Remanent_parameters.get_logger parameters)
          in
          let sum,nbr,longest =
            List.fold_left
              (fun (sum,nbr,longest) i ->
                 sum+i,
                 succ nbr,
                 max longest i)
              (0,0,0) l
          in
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "Total: %i; Average: %i; Longest: %i"
              sum
              (int_of_float (ceil ((float_of_int sum)/.(float_of_int nbr))))
              longest
          in
          let () =
            Loggers.print_newline
              (Remanent_parameters.get_logger parameters)
          in
          ()
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
