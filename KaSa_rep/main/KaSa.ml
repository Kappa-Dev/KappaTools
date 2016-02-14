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
	  (Views_domain.Domain)
	  (Product.Product
	     (Agents_domain.Domain)
	     (Rules_domain.Domain))))

module B =
  Analyzer.Make
    (Composite_domain.Make
       (Global_domain.Domain))

let main () =
  let error = Exception.empty_error_handler in
  let error,parameters,files  = Get_option.get_option error in
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_full_version parameters) in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_launched_when_and_where parameters) in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let compil =
    List.fold_left (KappaLexer.compile Format.std_formatter) Ast.empty_compil files in
  let parameters_compil = Remanent_parameters.update_call_stack parameters Preprocess.local_trace (Some "Prepreprocess.translate_compil") in
  let error,refined_compil = Prepreprocess.translate_compil parameters_compil error compil in
  let parameters_list_tokens = Remanent_parameters.update_call_stack parameters List_tokens.local_trace (Some "List_tokens.scan_compil") in
  let error,handler = List_tokens.scan_compil parameters_list_tokens error refined_compil in
  let parameters_sig = Remanent_parameters.update_prefix parameters "Signature:" in
  let error =
    if (Remanent_parameters.get_trace parameters_sig) || Print_handler.trace
    then Print_handler.print_handler parameters_sig error handler
    else
      error
  in
  let parameters_c_compil = Remanent_parameters.update_call_stack parameters Preprocess.local_trace (Some "Preprocess.translate_c_compil") in
  let _ = Format.printf "Compiling...@." in
  let error,handler,c_compil = Preprocess.translate_c_compil parameters_c_compil error handler refined_compil in
  let error =
    if Remanent_parameters.get_do_contact_map parameters
    then
      let _ = Format.printf "Generating the raw contact map...@." in
      Print_handler.dot_of_contact_map parameters error handler
    else error
  in
  let nrules = Handler.nrules parameters error handler in
  let parameters_compil = Remanent_parameters.update_prefix parameters "Compilation:" in
  let error =
    if (Remanent_parameters.get_trace parameters_compil) || Print_cckappa.trace
    then Print_cckappa.print_compil parameters_compil error handler c_compil
    else error
  in
   let error =
    if Remanent_parameters.get_do_influence_map parameters
    then
      let _ = Format.printf "Generating the raw influence map...@." in
      let parameters_quark = Remanent_parameters.update_call_stack parameters Quark.local_trace (Some "Quark.quarkify") in
      let parameters_quark = Remanent_parameters.update_prefix parameters_quark "Quarks:" in
      let error,quark_map = Quark.quarkify parameters_quark error  handler c_compil  in
      let parameters_quark = Remanent_parameters.update_prefix parameters "Quarks:" in
      let error =
	if (Remanent_parameters.get_trace parameters_quark) || Print_quarks.trace
	then Print_quarks.print_quarks parameters_quark error handler quark_map
	else error
      in
      let parameters_influence_map = Remanent_parameters.update_prefix parameters "Influence_map:" in
      let error,wake_up_map,inhibition_map = Influence_map.compute_influence_map parameters_influence_map error handler quark_map nrules in
      let error,wake_up_map,inhibition_map =
	match Remanent_parameters.get_influence_map_accuracy_level parameters_influence_map
	with
	| Remanent_parameters_sig.None | Remanent_parameters_sig.Low -> error,wake_up_map,inhibition_map
	| Remanent_parameters_sig.Medium | Remanent_parameters_sig.High | Remanent_parameters_sig.Full ->
	  let parameters_refine_influence_map = Remanent_parameters.update_prefix parameters "Refine_influence_map:" in
	  let _ = Format.printf "Refining the influence map...@." in
	  let error,wake_up_map = Algebraic_construction.filter_influence parameters_refine_influence_map error handler c_compil wake_up_map true in
	  let error,inhibition_map = Algebraic_construction.filter_influence parameters error handler c_compil inhibition_map false in
	  error,wake_up_map,inhibition_map
      in
      let error =
	if (Remanent_parameters.get_trace parameters_influence_map) || Print_quarks.trace
	then Print_quarks.print_wake_up_map parameters_influence_map error handler c_compil Handler.print_rule_txt Handler.print_var_txt Handler.get_label_of_rule_txt Handler.get_label_of_var_txt Handler.print_labels_txt "\n" wake_up_map
	else error
      in
      let error =
	if (Remanent_parameters.get_trace parameters_influence_map) || Print_quarks.trace
	then Print_quarks.print_inhibition_map parameters_influence_map error handler c_compil Handler.print_rule_txt Handler.print_var_txt Handler.get_label_of_rule_txt Handler.get_label_of_var_txt Handler.print_labels_txt "\n" inhibition_map
	else error
      in
      let error = Print_quarks.dot_of_influence_map parameters_influence_map error handler c_compil (wake_up_map,inhibition_map) in
      error
    else
      error
  in
  (*covering classes*)
  let error, covering_classes =
    (*Remark: this parameter is a trick not to print covering classes twice*)
    if Remanent_parameters.get_do_site_dependencies parameters
    then
      let parameters_cv =
	Remanent_parameters.update_prefix parameters "Potential dependencies between sites:" in
      let _ =
	if (Remanent_parameters.get_trace parameters_cv)
	then
	  let () = Loggers.fprintf (Remanent_parameters.get_logger parameters_cv) "Potential dependencies between sites:" in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters_cv) in ()
      in
      let error, dep = Covering_classes_main.covering_classes parameters_cv error handler c_compil
      in error, Some dep
    else
      error, None
  in
  (*-----------------------------------------------------------------------*)
  (*BDU of fixpoint iteration function*)
  let error,handler_bdu_opt, bdu_analysis =
    if Remanent_parameters.get_do_reachability_analysis parameters
    then
      let _ = Format.printf "Reachability analysis...@." in
      let parameters_cv =
	Remanent_parameters.update_prefix parameters "" in
      let _ =
	if (Remanent_parameters.get_trace parameters_cv)
	then Loggers.fprintf (Remanent_parameters.get_logger parameters_cv) ""
      in
       let error, handler_bdu, dep =
	 Bdu_analysis_main.bdu_main parameters_cv  error handler c_compil
       in error, Some handler_bdu, Some dep
    else
      error, None, None
  in
  (*-----------------------------------------------------------------------*)
  (*call analyzer in module*)
  let error, handler_bdu =
    match handler_bdu_opt with
    | None -> Mvbdu_wrapper.Mvbdu.init parameters error
    | Some handler_bdu -> error, handler_bdu
  in
  let error, static_opt, dynamic_opt =
    if Remanent_parameters.get_do_reachability_analysis_module parameters
    then
      let _ = Format.printf "\nReachability analysis...@." in
      let parameters_cv =
        Remanent_parameters.update_prefix parameters "" in
      let _ =
        if (Remanent_parameters.get_trace parameters_cv)
        then Loggers.fprintf (Remanent_parameters.get_logger parameters_cv) ""
      in
      let error, static, dynamic =
        A.main parameters error handler_bdu c_compil handler
      in
      error, Some static, Some dynamic
    else
      error, None, None
  in
  (*-----------------------------------------------------------------------*)
  (*Stochastic flow of information*)
  let error, stochastic_flow =
    if Remanent_parameters.get_do_stochastic_flow_of_information parameters
    then
      let parameters_stoch = Remanent_parameters.update_prefix parameters "Stochastic flow of information:" in
      let _ =
        if Remanent_parameters.get_trace parameters
        then Loggers.fprintf (Remanent_parameters.get_logger parameters_stoch) "Stochastic flow of information:\n"
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
	then Loggers.fprintf (Remanent_parameters.get_logger parameters_ode) "Flow of information in the ODE semantics:\n"
      in
      let error, ode_fragmentation =
        Ode_fragmentation_main.ode_fragmentation parameters_ode error handler c_compil
      in error, Some ode_fragmentation
    else error, None
  in
  let _ = Exception.print parameters error in
  ()

let _ = main ()

