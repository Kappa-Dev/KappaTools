(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <2016-02-14 10:20:44 feret>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)


module A =
  Analyzer.Make
    (Composite_domain.Make
       (Product.Product
          (Parallel_bonds.Domain)
          (Product.Product
             (Site_accross_bonds_domain.Domain)
             (Product.Product
                (Views_domain.Domain)
                (Product.Product
                   (Contact_map_domain.Domain)
                   (Product.Product
                      (Agents_domain.Domain)
                      (Rules_domain.Domain)))))))

let main () =
  let (state:Remanent_state.state) =
    Export.init  ~called_from:Remanent_parameters_sig.KaSa ()
  in
  let state,_ = Export.get_internal_contact_map ~accuracy_level:Remanent_state.Low state in
  let parameters = Remanent_state.get_parameters state in
  let state =
    match
      Remanent_parameters.get_influence_map_accuracy_level parameters
    with
    | Remanent_parameters_sig.None -> state
    | Remanent_parameters_sig.Low ->
      let state, _ = Export.get_internal_influence_map ~accuracy_level:Remanent_state.Low state in
      state
    | Remanent_parameters_sig.Medium
    | Remanent_parameters_sig.High
    | Remanent_parameters_sig.Full ->
      let state, _ = Export.get_internal_influence_map ~accuracy_level:Remanent_state.Medium state in
      state
  in
  let () =
    if (Remanent_parameters.get_trace parameters) || Print_quarks.trace
    then
      Export.dump_influence_map
        (  match
             Remanent_parameters.get_influence_map_accuracy_level parameters
           with
           | Remanent_parameters_sig.None
           | Remanent_parameters_sig.Low ->
             Remanent_state.Low
           | Remanent_parameters_sig.Medium
           | Remanent_parameters_sig.High
           | Remanent_parameters_sig.Full ->Remanent_state.Medium)
        state
  in
(*-----------------------------------------------------------------------*)
let c_compil =
  match Remanent_state.get_c_compil state with
    None -> assert false
  | Some c_compil -> c_compil
  in
  let state,handler = Export.get_handler state in
  let error = Remanent_state.get_errors state in
  let error, handler_bdu = Mvbdu_wrapper.Mvbdu.init parameters error in
  let log_info = Remanent_state.get_log_info state in
  let error, log_info, static_opt, dynamic_opt =
  if Remanent_parameters.get_do_reachability_analysis parameters
  then
    (*    (*covering classes*)
          let error, covering_classes =
          (*Remark: this parameter is a trick not to print covering classes twice*)
          if Remanent_parameters.get_do_site_dependencies parameters
          then
          let parameters_cv =
          Remanent_parameters.update_prefix
            parameters "Potential dependencies between sites:"
          in
          let _ =
          if (Remanent_parameters.get_trace parameters_cv)
          then
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters_cv)
                "Potential dependencies between sites:"
            in
            let () =
              Loggers.print_newline
                (Remanent_parameters.get_logger parameters_cv)
            in ()
          in
          let error, dep =
          Covering_classes_main.covering_classes
            parameters_cv error handler c_compil
          in error, Some dep
          else
          error, None
          in*)
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "Reachability analysis..." in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    let parameters_cv =
      Remanent_parameters.update_prefix parameters "" in
    let _ =
      if (Remanent_parameters.get_trace parameters_cv)
      then Loggers.fprintf (Remanent_parameters.get_logger parameters_cv) ""
    in
    let error, log_info, static, dynamic =
      A.main parameters log_info error handler_bdu c_compil handler
    in
    error, log_info, Some static, Some dynamic
  else
    error, log_info, None, None
in
(*-----------------------------------------------------------------------*)
(*Stochastic flow of information*)
let error, stochastic_flow =
  if Remanent_parameters.get_do_stochastic_flow_of_information parameters
  then
    let parameters_stoch = Remanent_parameters.update_prefix parameters "Stochastic flow of information:" in
    let _ =
      if Remanent_parameters.get_trace parameters
      then
        let () = Loggers.fprintf (Remanent_parameters.get_logger parameters_stoch) "Stochastic flow of information:" in
        let () = Loggers.print_newline (Remanent_parameters.get_logger parameters_stoch) in ()
    in
    let error, stochastic_flow =
      Stochastic_classes.stochastic_classes parameters_stoch error handler c_compil
    in error, Some stochastic_flow
  else error, None
in
(*ODE*)
let error,ode_flow =
  if Remanent_parameters.get_do_ODE_flow_of_information parameters
  then
    let parameters_ode = Remanent_parameters.update_prefix parameters "Flow of information in the ODE semantics:" in
    let _ =
      if (Remanent_parameters.get_trace parameters)
      then
        let () = Loggers.fprintf (Remanent_parameters.get_logger parameters_ode) "Flow of information in the ODE semantics:" in
        let () = Loggers.print_newline (Remanent_parameters.get_logger parameters_ode) in ()
    in
    let error, ode_fragmentation =
      Ode_fragmentation_main.ode_fragmentation parameters_ode error handler c_compil
    in error, Some ode_fragmentation
  else error, None
in
let _ = log_info, static_opt, dynamic_opt, stochastic_flow, ode_flow in
let _ = Exception.print parameters error in
()

let _ = main ()
