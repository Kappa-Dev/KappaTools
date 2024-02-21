(**
  * parameters.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: 2010, the 19th of December
  * Last modification:  Time-stamp: <Jan 07 2020>
  * *
  * Configuration parameters which are passed through functions computation
  *
  * Copyright 2010,2011,2012,2013,2014,2015
  * Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

module CharMap = Mods.CharMap

type called_from = KaSa | KaSim | Internalised | Server
type accuracy_level = None | Low | Medium | High | Full
type link_mode = Bound_indices | Site_address | Bound_type
type graph_format = DOT | HTML | DIM | GEPHI
type reachability_output = Raw | Natural_language | Kappa
type counters_domain = Mi | Non_relational | Abstract_multiset | Octagons

type rate_convention =
  | No_correction
  | Divide_by_nbr_of_autos_in_lhs
  | Biochemist
  | Common

type influence_map_output = {
  im_directory: string option;
  im_file: string option;
  im_format: graph_format;
  rule_shape: Graph_loggers_sig.shape;
  rule_color: Graph_loggers_sig.color;
  variable_shape: Graph_loggers_sig.shape;
  variable_color: Graph_loggers_sig.color;
  wake_up_color: Graph_loggers_sig.color;
  inhibition_color: Graph_loggers_sig.color;
  wake_up_arrow: Graph_loggers_sig.headkind;
  inhibition_arrow: Graph_loggers_sig.headkind;
  prompt_full_var_def: bool;
  prompt_full_rule_def: bool;
  make_labels_compatible: char list CharMap.t;
}

type contact_map_output = {
  cm_directory: string option;
  cm_file: string option;
  cm_format: graph_format;
  pure_contact: bool;
  binding_site_shape: Graph_loggers_sig.shape;
  binding_site_color: Graph_loggers_sig.color;
  internal_site_shape: Graph_loggers_sig.shape;
  internal_site_color: Graph_loggers_sig.color;
  counter_site_shape: Graph_loggers_sig.shape;
  counter_site_color: Graph_loggers_sig.color;
  agent_shape_array: Graph_loggers_sig.shape option array;
  agent_color_array: Graph_loggers_sig.color option array;
  agent_shape_def: Graph_loggers_sig.shape;
  agent_color_def: Graph_loggers_sig.color;
  link_color: Graph_loggers_sig.color;
  influence_color: Graph_loggers_sig.color;
  influence_arrow: Graph_loggers_sig.headkind;
}

type reachability_map_output = {
  dump_reachability_analysis_result: bool;
  dump_reachability_analysis_covering_classes: bool;
  dump_reachability_analysis_iteration: bool;
  dump_reachability_analysis_static: bool;
  dump_reachability_analysis_dynamic: bool;
  dump_reachability_analysis_diff: bool;
  dump_reachability_analysis_wl: bool;
  hide_one_d_relations_from_cartesian_decomposition: bool;
  smash_relations: bool;
  compute_local_traces: bool;
  show_rule_names_in_local_traces: bool;
  format_for_local_traces: graph_format;
  use_macrotransitions_in_local_traces: bool;
  add_singular_macrostates: bool;
  add_singular_microstates: bool;
  ignore_trivial_losanges: bool;
  use_natural_language: reachability_output;
  trace_prefix: string;
  trace_directory: string;
  compute_separating_transitions: bool;
  hide_reverse_rule_without_label_from_dead_rules: bool;
}

type reachability_parameters = {
  views: bool;
  site_across_bonds: bool;
  parallel_bonds: bool;
  dynamic_contact_map: bool;
  counters: bool;
  counter_domain: counters_domain;
}

type backdoors = {
  backdoor_nbr_of_scc: bool;
  backdoor_average_size_of_scc: bool;
  backdoor_nbr_of_influences: bool;
  backdoor_nbr_of_constraints: bool;
  backdoor_nbr_of_nr_constraints: bool;
  backdoor_nbr_of_dead_rules: bool;
  backdoor_nbr_of_rules: bool;
  backdoor_nbr_of_non_weakly_reversible_transitions: bool;
  backdoor_nbr_of_rules_with_non_weakly_reversible_transitions: bool;
  backdoor_timing: bool;
  backdoor_file: string;
  backdoor_directory: string;
}

type marshalisable_parameters = {
  syntax_version: Ast.syntax_version;
  unsafe: bool;
  trace: bool;
  do_contact_map: bool;
  do_scc: bool;
  do_influence_map: bool;
  do_ODE_flow_of_information: bool;
  do_stochastic_flow_of_information: bool;
  do_site_dependencies: bool;
  do_symmetries_analysis: bool;
  rate_convention: rate_convention;
  dump_site_dependencies: bool;
  do_reachability_analysis: bool;
  called_from: called_from;
  dump_error_as_soon_as_they_occur: bool;
  file: string option;
  prefix: string;
  call_stack: string list;
  link_mode: link_mode;
  symbols: Symbol_table.symbol_table;
  influence_map_output: influence_map_output;
  contact_map_output: contact_map_output;
  reachability_analysis_parameters: reachability_parameters;
  reachability_map_output: reachability_map_output;
  influence_map_accuracy_level: accuracy_level;
  contact_map_accuracy_level: accuracy_level;
  scc_accuracy_level: accuracy_level;
  view_accuracy_level: accuracy_level;
  kasa_state: Remanent_state_signature.engine_state;
  launching_date: Unix.tm;
  time_shift: int;
  hostname: string;
  command_line: string array;
  version: string;
  short_version: string;
  tk_interface: bool;
  html_mode: bool;
  empty_hashtbl_size: int;
  backdoors: backdoors;
}

type parameters = {
  logger_backdoor: Loggers.t;
  logger: Loggers.t;
  logger_err: Loggers.t;
  profiler: Loggers.t;
  compression_status: Loggers.t;
  print_efficiency: bool;
  save_error_list: Exception_without_parameter.method_handler -> unit;
  save_progress_bar: bool * int * int * int -> unit;
  reset_progress_bar: unit -> unit;
  save_current_phase_title: string -> unit;
  reset_current_phase_title: unit -> unit;
  marshalisable_parameters: marshalisable_parameters;
}
