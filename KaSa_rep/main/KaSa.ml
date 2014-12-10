(**
  * main.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: December, the 18th of 2010 
  * Last modification: December, the 9th of 2014
  * * 
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let main () =
  let error = Exception.empty_error_handler in    
  let error,parameters,files  = Get_option.get_option error in 
  let compil =
    let _ = Ast.init_compil() in 
    let _ =  
      List.iter (fun fic -> 
        let _ = KappaLexer.compile fic in 
        ())
      files 
    in 
    !Ast.result
  in 
  let parameters_compil = Remanent_parameters.update_call_stack parameters Preprocess.local_trace (Some "Prepreprocess.translate_compil") in 
  let error,refined_compil = Prepreprocess.translate_compil parameters_compil error compil in 
  let parameters_list_tokens = Remanent_parameters.update_call_stack parameters List_tokens.local_trace (Some "List_tokens.scan_compil") in 
  let error,handler = List_tokens.scan_compil parameters_list_tokens error refined_compil in 
  let parameters_sig = Remanent_parameters.update_prefix parameters "Signature:" in 
  let error = 
    if parameters_sig.Remanent_parameters_sig.trace || Print_handler.trace 
    then Print_handler.print_handler parameters_sig error handler 
    else 
      error
  in 
  let parameters_c_compil = Remanent_parameters.update_call_stack parameters Preprocess.local_trace (Some "Preprocess.translate_c_compil") in 
  let error,handler,c_compil = Preprocess.translate_c_compil parameters_c_compil error handler refined_compil in 
  let error = Print_handler.dot_of_contact_map parameters error handler in 
  let nrules = Handler.nrules parameters error handler in 
  let parameters_compil = Remanent_parameters.update_prefix parameters "Compilation:" in 
  let error = 
    if parameters_compil.Remanent_parameters_sig.trace || Print_cckappa.trace 
    then Print_cckappa.print_compil parameters_compil error handler c_compil 
    else error
  in 
  let parameters_quark = Remanent_parameters.update_call_stack parameters Quark.local_trace (Some "Quark.quarkify") in 
  let parameters_quark = Remanent_parameters.update_prefix parameters_quark "Quarks:" in 
  let error,quark_map = Quark.quarkify parameters_quark error  handler c_compil  in 
  let parameters_quark = Remanent_parameters.update_prefix parameters "Quarks:" in 
  let error = 
    if parameters_quark.Remanent_parameters_sig.trace || Print_quarks.trace 
    then Print_quarks.print_quarks parameters_quark error handler quark_map  
    else error 
  in 
  let parameters_influence_map = Remanent_parameters.update_prefix parameters "Influence_map:" in 
  let error,wake_up_map,inhibition_map = Influence_map.compute_influence_map parameters_influence_map error handler quark_map nrules in 
  let error = 
    if parameters_influence_map.Remanent_parameters_sig.trace || Print_quarks.trace 
    then Print_quarks.print_wake_up_map parameters_influence_map error handler c_compil Handler.print_rule_txt Handler.print_var_txt Handler.print_labels_txt "\n" wake_up_map  
    else error
  in 
  let error = 
    if parameters_influence_map.Remanent_parameters_sig.trace || Print_quarks.trace 
    then Print_quarks.print_inhibition_map parameters_influence_map error handler c_compil Handler.print_rule_txt Handler.print_var_txt Handler.print_labels_txt "\n" inhibition_map  
    else error 
  in 
  let error = Print_quarks.dot_of_influence_map parameters_influence_map error handler c_compil (wake_up_map,inhibition_map) in 
  let _ = Exception.print parameters error  in 
  () 

let _ = main () 

