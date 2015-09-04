 (**
  * config.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 08/03/2010
  * Last modification: Time-stamp: <2015-04-17 20:52:07 feret>
  * * 
  * Some parameters
  * references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2010,2011,2012,2013,2014,2015 
  * Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

let date="<2015.01.23"
let date_commit=Git_commit_info.git_commit_date 
let version = "4.01" 

let output_directory = ref "" 


let unsafe = ref true 
let trace = ref false
let dump_error_as_soon_as_they_occur = ref false 
let log = ref stdout 
let formatter = ref Format.std_formatter
let file = ref (None:string option) 
let link_mode = ref Remanent_parameters_sig.Bound_indices


(** influence map *)
let do_influence_map = ref true
let rule_shape = ref "box"
let rule_color = ref "lightskyblue"
let variable_shape = ref "ellipse"
let variable_color = ref "palegreen3"
let wake_up_color = ref "green"
let inhibition_color = ref "red"
let wake_up_arrow = ref "normal" 
let inhibition_arrow = ref "tee"
let influence_map_file = ref "influence.dot"
let prompt_full_var_def = ref false 
let prompt_full_rule_def = ref false 
let make_labels_compatible_with_dot = 
  ref 
    [ 
      '\"', ['\\';'\"'];
      '\\', ['\\';'\\']
    ]


(** contact map*)
let do_contact_map = ref true
let contact_map_file = ref "contact.dot"
let binding_site_shape = ref "circle"
let binding_site_color = ref "yellow"
let internal_site_shape = ref "ellipse"
let internal_site_color = ref "green"
let agent_shape_array = ref ([||]:string option array)
let agent_color_array = ref ([||]:string option array)
let agent_shape_def = ref "rectangle"
let agent_color_def = ref "red"
let link_color = ref "black"
let influence_color = ref "red"
let influence_arrow = ref "normal" 

(**flow of information: internal; external flow*)
let do_ODE_flow_of_information = ref false
let do_stochastic_flow_of_information = ref false
(*covering classes, side effects, etc.*)
let do_site_dependencies = ref false
(*fixpoint iteration in bdu structure*)
let do_iteration_dependencies = ref false
