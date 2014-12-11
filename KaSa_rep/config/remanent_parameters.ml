 (**
  * parameters.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 2010, the 19th of December
  * Last modification: 2014, the 9th of December
  * * 
  * Configuration parameters which are passed through functions computation

  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

let get_symbols () = 
  {
    Remanent_parameters_sig.bound = "!" ;
    Remanent_parameters_sig.at = "@" ;
    Remanent_parameters_sig.link_to_any = "?" ;
    Remanent_parameters_sig.link_to_some = "!" ; 
    Remanent_parameters_sig.agent_open = "(" ;
    Remanent_parameters_sig.agent_close = ")" ;
    Remanent_parameters_sig.agent_sep_comma = "," ; 
    Remanent_parameters_sig.agent_sep_plus = "+" ;
    Remanent_parameters_sig.agent_sep_dot = "." ; 
    Remanent_parameters_sig.site_sep_comma = "," ; 
    Remanent_parameters_sig.ghost_agent = "0" ; 
    Remanent_parameters_sig.internal = "~" ; 
    Remanent_parameters_sig.uni_arrow = "->" ;
    Remanent_parameters_sig.rev_arrow = "<-" ;
    Remanent_parameters_sig.bi_arrow = "<->" ; 
    Remanent_parameters_sig.uni_arrow_nopoly = "-!->"   
    }
  
let get_influence_map () = 
  {
    Remanent_parameters_sig.im_file = !Config.influence_map_file ; 
    Remanent_parameters_sig.rule_shape = !Config.rule_shape ;
    Remanent_parameters_sig.rule_color = !Config.rule_color ; 
    Remanent_parameters_sig.variable_shape = !Config.variable_shape ;
    Remanent_parameters_sig.variable_color = !Config.variable_color ;
    Remanent_parameters_sig.wake_up_color = !Config.wake_up_color ;
    Remanent_parameters_sig.inhibition_color = !Config.inhibition_color ;
    Remanent_parameters_sig.wake_up_arrow = !Config.wake_up_arrow ; 
    Remanent_parameters_sig.inhibition_arrow = !Config.inhibition_arrow ;
    Remanent_parameters_sig.prompt_full_var_def = !Config.prompt_full_var_def ; 
    Remanent_parameters_sig.prompt_full_rule_def = !Config.prompt_full_rule_def 
}
  
let get_contact_map () = 
   {
    Remanent_parameters_sig.cm_file = !Config.contact_map_file ;
    Remanent_parameters_sig.binding_site_shape = !Config.binding_site_shape ;
    Remanent_parameters_sig.binding_site_color = !Config.binding_site_color ;
    Remanent_parameters_sig.internal_site_shape = !Config.internal_site_shape ;
    Remanent_parameters_sig.internal_site_color = !Config.internal_site_color ; 
    Remanent_parameters_sig.agent_shape_array = !Config.agent_shape_array ; 
    Remanent_parameters_sig.agent_color_array = !Config.agent_color_array ;
    Remanent_parameters_sig.agent_shape_def = !Config.agent_shape_def ;
    Remanent_parameters_sig.agent_color_def = !Config.agent_color_def ;
    Remanent_parameters_sig.link_color = !Config.link_color ;
    Remanent_parameters_sig.influence_color = !Config.influence_color ;
    Remanent_parameters_sig.influence_arrow = !Config.influence_arrow ;
   }
  
let get_parameters () = 
  {
    Remanent_parameters_sig.file = !Config.file ;
    Remanent_parameters_sig.symbols = get_symbols () ; 
    Remanent_parameters_sig.influence_map_output = get_influence_map () ;
    Remanent_parameters_sig.contact_map_output = get_contact_map () ;
    Remanent_parameters_sig.unsafe = !Config.unsafe ;
    Remanent_parameters_sig.trace  = !Config.trace ;
    Remanent_parameters_sig.dump_error_as_soon_as_they_occur = !Config.dump_error_as_soon_as_they_occur;
    Remanent_parameters_sig.log    = !Config.log ;
    Remanent_parameters_sig.formatter = !Config.formatter ;
    Remanent_parameters_sig.prefix = "" ;
    Remanent_parameters_sig.call_stack = []; 
    Remanent_parameters_sig.link_mode = !Config.link_mode ;
    Remanent_parameters_sig.kasa_state = Remanent_state_signature.empty_engine_state;    
  } 

  
let update_prefix parameters suffix = 
  {parameters with Remanent_parameters_sig.prefix = parameters.Remanent_parameters_sig.prefix^suffix}
  
let update_call_stack parameters bool name = 
  let rep_bool = parameters.Remanent_parameters_sig.trace || bool in 
    match name,parameters.Remanent_parameters_sig.trace=bool  with
      | None,true -> parameters 
      | None,false -> {parameters with Remanent_parameters_sig.trace = rep_bool}
      | Some x,true -> {parameters with Remanent_parameters_sig.call_stack = x::parameters.Remanent_parameters_sig.call_stack}
      | Some x,false -> {parameters with Remanent_parameters_sig.trace = rep_bool ; call_stack = x::parameters.Remanent_parameters_sig.call_stack}
        

let open_file parameters error = 
  let error,channel = 
    match parameters.Remanent_parameters_sig.file
    with 
      | None -> error,stdout
      | Some a -> error,open_out a 
  in 
    error,{parameters with Remanent_parameters_sig.log = channel}

let close_file parameters error =
  let channel = parameters.Remanent_parameters_sig.log in 
  if channel = stdout 
  then 
    error,parameters
  else
    let _ = close_out channel in 
    error, {parameters with Remanent_parameters_sig.log = !Config.log}
    
let open_influence_map_file parameters error = 
  let error,channel = 
    match parameters.Remanent_parameters_sig.influence_map_output.Remanent_parameters_sig.im_file
    with 
      | None -> error,stdout
      | Some a -> error,open_out a 
  in 
    error,
    {parameters with Remanent_parameters_sig.log = channel}
       
 let open_contact_map_file parameters error = 
  let error,channel = 
    match parameters.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.cm_file
    with 
      | None -> error,stdout
      | Some a -> error,open_out a 
  in 
    error,
    {parameters with Remanent_parameters_sig.log = channel}
