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
  * Copyright 2010 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

type link_mode = Bound_indices | Site_address | Bound_type 
  
type symbol_table = 
  {
   bound : string ;
   at : string ;
   link_to_any : string ;
   link_to_some : string ;
   agent_open : string ;
   agent_close : string ; 
   site_sep_comma : string ;
   agent_sep_comma : string ;
   agent_sep_dot : string ; 
   agent_sep_plus : string ; 
   ghost_agent : string ;
   internal : string ;
   uni_arrow : string ;
   rev_arrow : string ; 
   bi_arrow : string ;
   uni_arrow_nopoly : string ;
    }

type influence_map_output =
  {
    im_file : string option ; 
    rule_shape : string ; 
    rule_color : string ;
    variable_shape : string ;
    variable_color : string ;
    wake_up_color : string ; 
    inhibition_color : string ;
    wake_up_arrow : string ;
    inhibition_arrow : string ;
    prompt_full_var_def: bool ; 
    prompt_full_rule_def: bool 
  }
  
type contact_map_output = 
  {
    cm_file : string option ;
    binding_site_shape : string ;
    binding_site_color : string ; 
    internal_site_shape : string ; 
    internal_site_color : string ; 
    agent_shape_array : string option array ;
    agent_color_array : string option array ; 
    agent_shape_def : string ; 
    agent_color_def : string ;
    link_color : string ;
    influence_color : string ; 
    influence_arrow : string ;
  }

type parameters = 
 { 
   unsafe : bool ;
   trace  : bool ;
   dump_error_as_soon_as_they_occur : bool ;
   log    : out_channel ;
   formatter : Format.formatter ; 
   file : string option ; 
   prefix : string ; 
   call_stack : string list;
   link_mode : link_mode ;
   symbols : symbol_table ; 
   influence_map_output : influence_map_output ;
   contact_map_output : contact_map_output ;
   kasa_state : Remanent_state_signature.engine_state 
 } 


    
