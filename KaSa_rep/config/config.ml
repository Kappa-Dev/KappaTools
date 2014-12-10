 (**
  * config.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 08/03/2010
  * Last modification: 09/12/2014
  * * 
  * Some parameters
  * references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2010,2011,2012,2013,2014 
  * Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

let version = "1.04"
let date = "2014/12/10"
let unsafe = ref true 
let trace = ref false
let dump_error_as_soon_as_they_occur = ref false 
let log = ref stdout 
let formatter = ref Format.std_formatter
let file = ref (None:string option) 
let link_mode = ref Remanent_parameters_sig.Bound_indices
  
(** influence map *)
let rule_shape = ref "box"
let rule_color = ref "lightskyblue"
let variable_shape = ref "ellipse"
let variable_color = ref "palegreen3"
let wake_up_color = ref "green"
let inhibition_color = ref "red"
let wake_up_arrow = ref "normal" 
let inhibition_arrow = ref "tee"
let influence_map_file = ref (Some "influence.dot")
let prompt_full_var_def = ref false 
let prompt_full_rule_def = ref false 

(** contact map*)
let contact_map_file = ref (Some "contact.dot")
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
let influence_arrow = ref "normal" ;
   
