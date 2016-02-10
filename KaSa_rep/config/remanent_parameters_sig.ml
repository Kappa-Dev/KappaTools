 (**
  * parameters.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: 2010, the 19th of December
  * Last modification: Time-stamp: <2016-01-22 14:31:36 feret>
  * *
  * Configuration parameters which are passed through functions computation
  *
  * Copyright 2010,2011,2012,2013,2014,2015
  * Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

module CharMap = Mods.CharMap

type called_from = KaSa | KaSim | Internalised | JS
type accuracy_level = None | Low | Medium | High | Full
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
   btype_sep : string ;
   agent_sep_comma : string ;
   agent_sep_dot : string ;
   agent_sep_plus : string ;
   ghost_agent : string ;
   show_ghost : bool ;
   internal : string ;
   uni_arrow : string ;
   rev_arrow : string ;
   bi_arrow : string ;
   uni_arrow_nopoly : string ;
    }

type influence_map_output =
  {
    im_directory : string option ;
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
    prompt_full_rule_def: bool ;
    make_labels_compatible: char list CharMap.t
  }

type contact_map_output =
  {
    cm_directory : string option ;
    cm_file : string option ;
    pure_contact : bool ;
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

type reachability_map_output =
  {
    dump_reachability_analysis_result : bool;
    dump_reachability_analysis_covering_classes : bool;
    dump_reachability_analysis_iteration : bool;
    dump_reachability_analysis_static : bool;
    dump_reachability_analysis_dynamic : bool;
    dump_reachability_analysis_diff : bool;
    dump_reachability_analysis_wl : bool;
    hide_one_d_relations_from_cartesian_decomposition : bool;
    smash_relations : bool;
    use_natural_language : bool;
  }

type marshalisable_parameters =
 {
   unsafe : bool ;
   trace  : bool ;
   do_contact_map : bool ;
   do_influence_map : bool ;
   do_ODE_flow_of_information : bool ;
   do_stochastic_flow_of_information : bool ;
   do_site_dependencies : bool ;
   dump_site_dependencies : bool ;
   do_reachability_analysis : bool ;
   do_reachability_analysis_module : bool ;
   called_from : called_from;
   dump_error_as_soon_as_they_occur : bool ;
   file : string option ;
   prefix : string ;
   call_stack : string list;
   link_mode : link_mode ;
   symbols : symbol_table ;
   influence_map_output : influence_map_output ;
   contact_map_output : contact_map_output ;
   reachability_map_output : reachability_map_output;
   influence_map_accuracy_level: accuracy_level ;
   contact_map_accuracy_level: accuracy_level ;
   view_accuracy_level: accuracy_level ;
   kasa_state : Remanent_state_signature.engine_state ;
   launching_date: Unix.tm ;
   time_shift: int ;
   hostname: string ;
   command_line: string array ;
   version: string ;
   short_version: string ;
   tk_interface: bool ;
   html_mode: bool ;
  }

type parameters =
  {
    logger: Loggers.t;
    profiler: Loggers.t;
    compression_status: Loggers.t;
    marshalisable_parameters : marshalisable_parameters;
  }


