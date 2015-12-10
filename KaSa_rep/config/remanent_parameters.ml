 (**
  * parameters.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 2010, the 19th of December
  * Last modification: Time-stamp: <2015-11-27 09:31:50 feret>
  * * 
  * Configuration parameters which are passed through functions computation

  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

let compose f g = fun x -> f (g x)

let fetch_accuracy_level r =
  match
    !r
  with
  | "None" -> Remanent_parameters_sig.None 
  | "Low" -> Remanent_parameters_sig.Low
  | "Medium" -> Remanent_parameters_sig.Medium
  | "High" -> Remanent_parameters_sig.High
  | _ ->
     let _ = Printf.fprintf stderr "This is not an accuracy level !!!" in raise Exit 
			     
let get_symbols () = 
  {
    Remanent_parameters_sig.bound = "!" ;
    Remanent_parameters_sig.at = "@" ;
    Remanent_parameters_sig.link_to_any = "?" ;
    Remanent_parameters_sig.link_to_some = "!_" ; 
    Remanent_parameters_sig.agent_open = "(" ;
    Remanent_parameters_sig.agent_close = ")" ;
    Remanent_parameters_sig.agent_sep_comma = "," ; 
    Remanent_parameters_sig.agent_sep_plus = "+" ;
    Remanent_parameters_sig.agent_sep_dot = "." ; 
    Remanent_parameters_sig.btype_sep = ".";
    Remanent_parameters_sig.site_sep_comma = "," ; 
    Remanent_parameters_sig.ghost_agent = "Ghost" ; 
    Remanent_parameters_sig.show_ghost = false ; 
    Remanent_parameters_sig.internal = "~" ; 
    Remanent_parameters_sig.uni_arrow = "->" ;
    Remanent_parameters_sig.rev_arrow = "<-" ;
    Remanent_parameters_sig.bi_arrow = "<->" ; 
    Remanent_parameters_sig.uni_arrow_nopoly = "-!->"   
    }

  
let get_influence_map () = 
  {
    Remanent_parameters_sig.im_file = 
      (match !Config.influence_map_file 
      with 
      | "" -> None 
      | x -> Some x) ;
	
    Remanent_parameters_sig.im_directory =
      (match !Config.output_directory 
      with 
      | "" -> Some ""
      | x -> Some (x^"/")) ; 
	
    Remanent_parameters_sig.rule_shape = !Config.rule_shape ;
    Remanent_parameters_sig.rule_color = !Config.rule_color ; 
    Remanent_parameters_sig.variable_shape = !Config.variable_shape ;
    Remanent_parameters_sig.variable_color = !Config.variable_color ;
    Remanent_parameters_sig.wake_up_color = !Config.wake_up_color ;
    Remanent_parameters_sig.inhibition_color = !Config.inhibition_color ;
    Remanent_parameters_sig.wake_up_arrow = !Config.wake_up_arrow ; 
    Remanent_parameters_sig.inhibition_arrow = !Config.inhibition_arrow ;
    Remanent_parameters_sig.prompt_full_var_def = !Config.prompt_full_var_def ; 
    Remanent_parameters_sig.prompt_full_rule_def = !Config.prompt_full_rule_def ;
    Remanent_parameters_sig.make_labels_compatible = 
      List.fold_left
	(fun map (a,l) -> Remanent_parameters_sig.CharMap.add a l map)
	Remanent_parameters_sig.CharMap.empty 
	!Config.make_labels_compatible_with_dot
}
  
let get_contact_map () = 
   {
     Remanent_parameters_sig.cm_file = 
       (match !Config.contact_map_file 
	with 
	| "" -> None 
	| x -> Some x) ;
     
     Remanent_parameters_sig.cm_directory =
       (match !Config.output_directory 
	with 
	| "" -> Some ""
      | x -> Some (x^"/")) ; 
    Remanent_parameters_sig.pure_contact = !Config.pure_contact ;  
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
  { Remanent_parameters_sig.marshalisable_parameters = 
      {
	Remanent_parameters_sig.do_contact_map = !Config.do_contact_map ; 
	Remanent_parameters_sig.do_influence_map = !Config.do_influence_map ;
	Remanent_parameters_sig.do_ODE_flow_of_information = !Config.do_ODE_flow_of_information ; 
	Remanent_parameters_sig.do_stochastic_flow_of_information = !Config.do_stochastic_flow_of_information ; 
	Remanent_parameters_sig.do_site_dependencies = !Config.do_site_dependencies ;
	Remanent_parameters_sig.do_reachability_analysis = !Config.do_reachability_analysis ;
	Remanent_parameters_sig.file = !Config.file ;
	Remanent_parameters_sig.symbols = get_symbols () ; 
	Remanent_parameters_sig.influence_map_output = get_influence_map () ;
	Remanent_parameters_sig.contact_map_output = get_contact_map () ;
	Remanent_parameters_sig.unsafe = !Config.unsafe ;
	Remanent_parameters_sig.trace  = !Config.trace ;
	Remanent_parameters_sig.dump_error_as_soon_as_they_occur = !Config.dump_error_as_soon_as_they_occur;
	Remanent_parameters_sig.prefix = "" ;
	Remanent_parameters_sig.call_stack = []; 
	Remanent_parameters_sig.link_mode = !Config.link_mode ;
	Remanent_parameters_sig.kasa_state = Remanent_state_signature.empty_engine_state ;
	Remanent_parameters_sig.launching_date = Unix.localtime (Unix.gettimeofday ()) ;
	Remanent_parameters_sig.time_shift=(
	  let x = Unix.gettimeofday () in
	  (Unix.localtime x).Unix.tm_hour - (Unix.gmtime x).Unix.tm_hour)  ;
	Remanent_parameters_sig.hostname=begin try Unix.gethostname () with Failure _ -> "javascript" end;
	Remanent_parameters_sig.command_line=Sys.argv;
	Remanent_parameters_sig.short_version=Version.version_string;
	Remanent_parameters_sig.version=Version.version_kasa_full_name;
	Remanent_parameters_sig.tk_interface=Tk_version.tk;
	Remanent_parameters_sig.influence_map_accuracy_level = fetch_accuracy_level Config.influence_map_accuracy_level ;
	Remanent_parameters_sig.contact_map_accuracy_level = fetch_accuracy_level Config.contact_map_accuracy_level ;
	Remanent_parameters_sig.view_accuracy_level = fetch_accuracy_level Config.view_accuracy_level ; 
      } ;
    Remanent_parameters_sig.log    = !Config.log ;
    Remanent_parameters_sig.formatter = !Config.formatter ;    
  }

let dummy_parameters = get_parameters ()
    
let get_btype_sep_symbol_1         symbol = symbol.Remanent_parameters_sig.btype_sep 
let get_bound_symbol_1             symbol = symbol.Remanent_parameters_sig.bound 
let get_at_symbol_1                symbol = symbol.Remanent_parameters_sig.at
let get_link_to_any_symbol_1       symbol = symbol.Remanent_parameters_sig.link_to_any
let get_link_to_some_symbol_1      symbol = symbol.Remanent_parameters_sig.link_to_some 
let get_agent_open_symbol_1        symbol = symbol.Remanent_parameters_sig.agent_open
let get_agent_close_symbol_1       symbol = symbol.Remanent_parameters_sig.agent_close
let get_agent_sep_comma_symbol_1   symbol = symbol.Remanent_parameters_sig.agent_sep_comma 
let get_agent_sep_plus_symbol_1    symbol = symbol.Remanent_parameters_sig.agent_sep_plus
let get_agent_sep_dot_symbol_1     symbol = symbol.Remanent_parameters_sig.agent_sep_dot
let get_site_sep_comma_symbol_1    symbol = symbol.Remanent_parameters_sig.site_sep_comma 
let get_ghost_agent_symbol_1       symbol = symbol.Remanent_parameters_sig.ghost_agent
let get_do_we_show_ghost_1         symbol = symbol.Remanent_parameters_sig.show_ghost
let get_internal_symbol_1          symbol = symbol.Remanent_parameters_sig.internal
let get_uni_arrow_symbol_1         symbol = symbol.Remanent_parameters_sig.uni_arrow
let get_rev_arrow_symbol_1         symbol = symbol.Remanent_parameters_sig.rev_arrow
let get_bi_arrow_symbol_1          symbol = symbol.Remanent_parameters_sig.bi_arrow
let get_uni_arrow_no_poly_symbol_1 symbol = symbol.Remanent_parameters_sig.uni_arrow_nopoly 

let get_im_file_1              influence = influence.Remanent_parameters_sig.im_file 
let get_im_directory_1         influence = influence.Remanent_parameters_sig.im_directory 
let get_rule_shape_1           influence = influence.Remanent_parameters_sig.rule_shape 
let get_rule_color_1           influence = influence.Remanent_parameters_sig.rule_color
let get_variable_shape_1       influence = influence.Remanent_parameters_sig.variable_shape
let get_variable_color_1       influence = influence.Remanent_parameters_sig.variable_color
let get_wake_up_color_1        influence = influence.Remanent_parameters_sig.wake_up_color
let get_inhibition_color_1     influence = influence.Remanent_parameters_sig.inhibition_color
let get_wake_up_arrow_1        influence = influence.Remanent_parameters_sig.wake_up_arrow
let get_inhibition_arrow_1     influence = influence.Remanent_parameters_sig.inhibition_arrow
let get_prompt_full_var_def_1  influence = influence.Remanent_parameters_sig.prompt_full_var_def 
let get_prompt_full_rule_def_1 influence = influence.Remanent_parameters_sig.prompt_full_rule_def
let get_make_labels_compatible_1 influence = influence.Remanent_parameters_sig.make_labels_compatible 

let get_pure_contact_1        cm = cm.Remanent_parameters_sig.pure_contact
let get_cm_file_1             cm = cm.Remanent_parameters_sig.cm_file 
let get_cm_directory_1        cm = cm.Remanent_parameters_sig.cm_directory 
let get_binding_site_shape_1  cm = cm.Remanent_parameters_sig.binding_site_shape 
let get_binding_site_color_1  cm = cm.Remanent_parameters_sig.binding_site_color 
let get_internal_site_shape_1 cm = cm.Remanent_parameters_sig.internal_site_shape 
let get_internal_site_color_1 cm = cm.Remanent_parameters_sig.internal_site_color 
let get_agent_shape_array_1   cm = cm.Remanent_parameters_sig.agent_shape_array
let get_agent_color_array_1   cm = cm.Remanent_parameters_sig.agent_color_array
let get_agent_shape_def_1     cm = cm.Remanent_parameters_sig.agent_shape_def
let get_agent_color_def_1     cm = cm.Remanent_parameters_sig.agent_color_def 
let get_link_color_1          cm = cm.Remanent_parameters_sig.link_color 
let get_influence_color_1     cm = cm.Remanent_parameters_sig.influence_color 
let get_influence_arrow_1     cm = cm.Remanent_parameters_sig.influence_arrow 


let get_symbols_1                          marshalisable = marshalisable.Remanent_parameters_sig.symbols
let get_file_1                             marshalisable = marshalisable.Remanent_parameters_sig.file 
let get_influence_map_1                    marshalisable = marshalisable.Remanent_parameters_sig.influence_map_output 
let get_contact_map_1                      marshalisable = marshalisable.Remanent_parameters_sig.contact_map_output
let get_unsafe_1                           marshalisable = marshalisable.Remanent_parameters_sig.unsafe 
let get_trace_1                            marshalisable = marshalisable.Remanent_parameters_sig.trace 
let get_dump_error_as_soon_as_they_occur_1 marshalisable = marshalisable.Remanent_parameters_sig.dump_error_as_soon_as_they_occur
let get_prefix_1                           marshalisable = marshalisable.Remanent_parameters_sig.prefix
let get_call_stack_1                       marshalisable = marshalisable.Remanent_parameters_sig.call_stack 
let get_link_mode_1                        marshalisable = marshalisable.Remanent_parameters_sig.link_mode 
let get_kasa_state_1                       marshalisable = marshalisable.Remanent_parameters_sig.kasa_state 
let get_do_contact_map_1                   marshalisable = marshalisable.Remanent_parameters_sig.do_contact_map 
let get_do_influence_map_1                 marshalisable = marshalisable.Remanent_parameters_sig.do_influence_map 
let get_do_ODE_flow_of_information_1       marshalisable = marshalisable.Remanent_parameters_sig.do_ODE_flow_of_information
let get_do_stochastic_flow_of_information_1  marshalisable = marshalisable.Remanent_parameters_sig.do_stochastic_flow_of_information 
let get_do_site_dependencies_1             marshalisable = marshalisable.Remanent_parameters_sig.do_site_dependencies
let get_do_reachability_analysis_1        marshalisable = marshalisable.Remanent_parameters_sig.do_reachability_analysis
let get_influence_map_accuracy_level_1     marshalisable = marshalisable.Remanent_parameters_sig.influence_map_accuracy_level
let get_contact_map_accuracy_level_1      marshalisable = marshalisable.Remanent_parameters_sig.contact_map_accuracy_level
let get_view_accuracy_level_1             marshalisable = marshalisable.Remanent_parameters_sig.view_accuracy_level
let get_launching_date_1                             marshalisable =
  let t = marshalisable.Remanent_parameters_sig.launching_date in
  let gmt = marshalisable.Remanent_parameters_sig.time_shift in 
  Printf.sprintf "Analysis launched at %04i/%02i/%02i %02i:%02i:%02i (GMT%+i)"
		   (t.Unix.tm_year+1900)
		   (t.Unix.tm_mon+1)
		   (t.Unix.tm_mday)
		   t.Unix.tm_hour
		   t.Unix.tm_min t.Unix.tm_sec gmt
let get_short_version_1                              marshalisable =
  marshalisable.Remanent_parameters_sig.version 
let get_full_version_1                               marshalisable =
  Printf.sprintf "%s (with%s Tk interface)" marshalisable.Remanent_parameters_sig.version (if marshalisable.Remanent_parameters_sig.tk_interface then "" else "out")
let get_launched_where_1                             marshalisable =
      marshalisable.Remanent_parameters_sig.hostname
let get_command_line_1                               marshalisable =
  marshalisable.Remanent_parameters_sig.command_line
    
let get_marshalisable parameter = parameter.Remanent_parameters_sig.marshalisable_parameters
let get_log parameter = parameter.Remanent_parameters_sig.log   
let get_formatter parameter = parameter.Remanent_parameters_sig.formatter 
let set_formatter parameter logger = {parameter with Remanent_parameters_sig.formatter = logger}

let upgrade_from_marshal_field f = compose f get_marshalisable 

let get_command_line = upgrade_from_marshal_field get_command_line_1 
let get_short_version = upgrade_from_marshal_field get_short_version_1
let get_launched_where = upgrade_from_marshal_field get_launched_where_1
let get_full_version = upgrade_from_marshal_field get_full_version_1
let get_launching_date = upgrade_from_marshal_field get_launching_date_1
let get_launched_when_and_where parameters = Printf.sprintf "%s on %s" (get_launching_date parameters) (get_launched_where parameters)
let get_do_contact_map = upgrade_from_marshal_field get_do_contact_map_1
let get_do_influence_map = upgrade_from_marshal_field get_do_influence_map_1
let get_do_ODE_flow_of_information = upgrade_from_marshal_field get_do_ODE_flow_of_information_1
let get_do_stochastic_flow_of_information = upgrade_from_marshal_field get_do_stochastic_flow_of_information_1
let get_do_site_dependencies = upgrade_from_marshal_field get_do_site_dependencies_1
let get_do_reachability_analysis = upgrade_from_marshal_field get_do_reachability_analysis_1

let get_symbols = upgrade_from_marshal_field get_symbols_1 
let get_file = upgrade_from_marshal_field get_file_1 
let get_influence_map = upgrade_from_marshal_field get_influence_map_1
let get_contact_map = upgrade_from_marshal_field get_contact_map_1
let get_unsafe = upgrade_from_marshal_field get_unsafe_1
let get_trace = upgrade_from_marshal_field get_trace_1
let get_dump_error_as_soon_as_they_occur = upgrade_from_marshal_field get_dump_error_as_soon_as_they_occur_1
let get_prefix = upgrade_from_marshal_field get_prefix_1
let get_call_stack = upgrade_from_marshal_field get_call_stack_1
let get_link_mode = upgrade_from_marshal_field get_link_mode_1
let get_contact_map_accuracy_level = upgrade_from_marshal_field get_contact_map_accuracy_level_1
let get_influence_map_accuracy_level = upgrade_from_marshal_field get_influence_map_accuracy_level_1
let get_view_accuracy_level = upgrade_from_marshal_field get_view_accuracy_level_1
								    
let upgrade_from_influence_map_field f = compose f get_influence_map
let upgrade_from_contact_map_field f = compose f get_contact_map
let upgrade_from_symbols_field f = compose f get_symbols

let get_btype_sep_symbol = upgrade_from_symbols_field get_btype_sep_symbol_1
let get_bound_symbol = upgrade_from_symbols_field get_bound_symbol_1 
let get_at_symbol = upgrade_from_symbols_field get_at_symbol_1   
let get_link_to_any_symbol = upgrade_from_symbols_field get_link_to_any_symbol_1
let get_link_to_some_symbol = upgrade_from_symbols_field get_link_to_some_symbol_1
let get_agent_open_symbol = upgrade_from_symbols_field get_agent_open_symbol_1
let get_agent_close_symbol = upgrade_from_symbols_field get_agent_close_symbol_1
let get_agent_sep_comma_symbol = upgrade_from_symbols_field get_agent_sep_comma_symbol_1
let get_agent_sep_plus_symbol = upgrade_from_symbols_field get_agent_sep_plus_symbol_1
let get_agent_sep_dot_symbol = upgrade_from_symbols_field get_agent_sep_dot_symbol_1
let get_site_sep_comma_symbol = upgrade_from_symbols_field get_site_sep_comma_symbol_1
let get_ghost_agent_symbol = upgrade_from_symbols_field get_ghost_agent_symbol_1
let get_do_we_show_ghost = upgrade_from_symbols_field get_do_we_show_ghost_1
let get_internal_symbol = upgrade_from_symbols_field get_internal_symbol_1
let get_uni_arrow_symbol = upgrade_from_symbols_field get_uni_arrow_symbol_1
let get_rev_arrow_symbol = upgrade_from_symbols_field get_rev_arrow_symbol_1
let get_bi_arrow_symbol = upgrade_from_symbols_field get_bi_arrow_symbol_1
let get_uni_arrow_no_poly_symbol = upgrade_from_symbols_field get_uni_arrow_no_poly_symbol_1

let get_im_file = upgrade_from_influence_map_field get_im_file_1      
let get_im_directory = upgrade_from_influence_map_field get_im_directory_1
let get_rule_shape = upgrade_from_influence_map_field get_rule_shape_1
let get_rule_color = upgrade_from_influence_map_field get_rule_color_1
let get_variable_shape = upgrade_from_influence_map_field get_variable_shape_1
let get_variable_color = upgrade_from_influence_map_field get_variable_color_1
let get_wake_up_color = upgrade_from_influence_map_field get_wake_up_color_1
let get_inhibition_color = upgrade_from_influence_map_field get_inhibition_color_1
let get_wake_up_arrow = upgrade_from_influence_map_field get_wake_up_arrow_1
let get_inhibition_arrow = upgrade_from_influence_map_field get_inhibition_arrow_1
let get_prompt_full_var_def = upgrade_from_influence_map_field get_prompt_full_var_def_1
let get_prompt_full_rule_def = upgrade_from_influence_map_field get_prompt_full_rule_def_1
let get_make_labels_compatible_with_dot = upgrade_from_influence_map_field get_make_labels_compatible_1 

let get_pure_contact = upgrade_from_contact_map_field get_pure_contact_1
let get_cm_file = upgrade_from_contact_map_field get_cm_file_1 
let get_cm_directory = upgrade_from_contact_map_field get_cm_directory_1
let get_binding_site_shape = upgrade_from_contact_map_field get_binding_site_shape_1
let get_binding_site_color = upgrade_from_contact_map_field get_binding_site_color_1
let get_internal_site_shape = upgrade_from_contact_map_field get_internal_site_shape_1
let get_internal_site_color = upgrade_from_contact_map_field get_internal_site_color_1
let get_agent_shape_array = upgrade_from_contact_map_field get_agent_shape_array_1
let get_agent_color_array = upgrade_from_contact_map_field get_agent_color_array_1
let get_agent_shape_def = upgrade_from_contact_map_field get_agent_shape_def_1
let get_agent_color_def = upgrade_from_contact_map_field get_agent_color_def_1
let get_link_color = upgrade_from_contact_map_field get_link_color_1
let get_influence_color = upgrade_from_contact_map_field get_influence_color_1
let get_influence_arrow = upgrade_from_contact_map_field get_influence_arrow_1

let set_prefix_1 marshalisable prefix = 
  {marshalisable with Remanent_parameters_sig.prefix = prefix}
let set_call_stack_1 marshalisable call_stack = 
  {marshalisable with Remanent_parameters_sig.call_stack = call_stack}
let set_trace_1 marshalisable bool = 
  {marshalisable with Remanent_parameters_sig.trace = bool}

let upgrade_to_marshalisable f parameters prefix = 
  {parameters with 
    Remanent_parameters_sig.marshalisable_parameters = 
      f (get_marshalisable parameters) prefix}

let set_prefix = upgrade_to_marshalisable set_prefix_1
let set_call_stack = upgrade_to_marshalisable set_call_stack_1
let set_trace = upgrade_to_marshalisable set_trace_1

let update_prefix parameters suffix = 
  set_prefix parameters ((get_prefix parameters)^suffix)
let update_call_stack parameters bool name = 
  let rep_bool = get_trace parameters || bool in 
    match name,get_trace parameters=bool  with
      | None,true -> parameters 
      | None,false -> set_trace parameters rep_bool
      | Some x,true -> set_call_stack parameters (x::(get_call_stack parameters))
      | Some x,false -> 
	set_call_stack 
	  (set_trace parameters rep_bool)
	  (x::(get_call_stack parameters))

let open_file parameters error = 
  let error,channel = 
    match get_file parameters
    with 
      | None -> error,stdout
      | Some a -> error,open_out a 
  in 
  error,{parameters with Remanent_parameters_sig.log = channel}

let close_file parameters error =
  let channel = get_log parameters in 
  if channel = stdout 
  then 
    error,parameters
  else
    let _ = close_out channel in 
    error, {parameters with Remanent_parameters_sig.log = !Config.log}
    
let open_influence_map_file parameters error = 
  let error,channel = 
    match get_im_file parameters,get_im_directory parameters 
    with 
      | None,_  -> error,stdout
      | Some a,None -> error,open_out a
      | Some a,Some d -> error,open_out (d^a)
  in 
    error,
    {parameters with Remanent_parameters_sig.log = channel}
       
 let open_contact_map_file parameters error = 
  let error,channel = 
    match get_cm_file parameters,get_cm_directory parameters 
    with 
      | None,_ -> error,stdout
      | Some a,None -> error,open_out a 
      | Some a,Some d -> error,open_out (d^a)
  in 
    error,
    {parameters with Remanent_parameters_sig.log = channel}

 let persistent_mode = false
			 
 let lexical_analysis_of_tested_only_patterns_is_required_by_the_influence_map parameter =
   (get_influence_map_accuracy_level parameter = Remanent_parameters_sig.Medium) && 
     (get_do_influence_map parameter)
 let lexical_analysis_of_tested_only_patterns_is_required_by_the_persistent_mode  parameter =
   persistent_mode
 let lexical_analysis_of_tested_only_patterns_is_required_by_the_contact_map parameter =
   (get_do_contact_map parameter) 
   && (match get_contact_map_accuracy_level parameter with
   | Remanent_parameters_sig.Medium | Remanent_parameters_sig.Low -> true
   | Remanent_parameters_sig.None | Remanent_parameters_sig.High -> false)
								      
 let lexical_analysis_of_tested_only_patterns parameter =
   lexical_analysis_of_tested_only_patterns_is_required_by_the_persistent_mode  parameter 
   || lexical_analysis_of_tested_only_patterns_is_required_by_the_contact_map parameter
   || lexical_analysis_of_tested_only_patterns_is_required_by_the_influence_map parameter


