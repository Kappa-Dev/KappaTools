(**
  * parameters.ml
  * openkappa
  * Jérôme Feret, project Antique, INRIA Paris
  *
  * Creation: 2010, the 19th of December
  * Last modification: Time-stamp: <Jan 08 2020>
  * *
  * Configuration parameters which are passed through functions computation

  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)

val open_out : string -> string -> out_channel
val ext_format : Remanent_parameters_sig.graph_format -> string

val get_parameters :
  ?html_mode:bool ->
  called_from:Remanent_parameters_sig.called_from ->
  unit ->
  Remanent_parameters_sig.parameters

val dummy_parameters :
  called_from:Remanent_parameters_sig.called_from ->
  unit ->
  Remanent_parameters_sig.parameters

val get_called_from :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.called_from

val get_logger : Remanent_parameters_sig.parameters -> Loggers.t
val get_logger_err : Remanent_parameters_sig.parameters -> Loggers.t
val get_logger_backdoor : Remanent_parameters_sig.parameters -> Loggers.t
val get_command_line : Remanent_parameters_sig.parameters -> string array
val get_short_version : Remanent_parameters_sig.parameters -> string
val get_full_version : Remanent_parameters_sig.parameters -> string
val get_launched_when_and_where : Remanent_parameters_sig.parameters -> string

val get_syntax_version :
  Remanent_parameters_sig.parameters -> Ast.syntax_version

val get_file : Remanent_parameters_sig.parameters -> string option
val get_do_contact_map : Remanent_parameters_sig.parameters -> bool
val get_do_scc : Remanent_parameters_sig.parameters -> bool
val get_do_influence_map : Remanent_parameters_sig.parameters -> bool
val get_do_ODE_flow_of_information : Remanent_parameters_sig.parameters -> bool
val get_do_reachability_analysis : Remanent_parameters_sig.parameters -> bool

val get_do_stochastic_flow_of_information :
  Remanent_parameters_sig.parameters -> bool

val get_do_site_dependencies : Remanent_parameters_sig.parameters -> bool
val get_dump_site_dependencies : Remanent_parameters_sig.parameters -> bool
val get_unsafe : Remanent_parameters_sig.parameters -> bool

val get_dump_error_as_soon_as_they_occur :
  Remanent_parameters_sig.parameters -> bool

val get_trace : Remanent_parameters_sig.parameters -> bool
val get_prefix : Remanent_parameters_sig.parameters -> string

val set_logger :
  Remanent_parameters_sig.parameters ->
  Loggers.t ->
  Remanent_parameters_sig.parameters

val set_prefix :
  Remanent_parameters_sig.parameters ->
  string ->
  Remanent_parameters_sig.parameters

val get_link_mode :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.link_mode

val get_view_accuracy_level :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.accuracy_level

val get_influence_map_accuracy_level :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.accuracy_level

val get_contact_map_accuracy_level :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.accuracy_level

val get_cm_format :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.graph_format

val get_scc_accuracy_level :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.accuracy_level

val get_btype_sep_symbol : Remanent_parameters_sig.parameters -> string
(** Kappa pretty-printing *)

val get_bound_symbol : Remanent_parameters_sig.parameters -> string
val get_open_binding_state : Remanent_parameters_sig.parameters -> string
val get_close_binding_state : Remanent_parameters_sig.parameters -> string
val get_missing_binding_state : Remanent_parameters_sig.parameters -> string
val get_at_symbol : Remanent_parameters_sig.parameters -> string
val get_link_to_any : Remanent_parameters_sig.parameters -> string
val get_link_to_some : Remanent_parameters_sig.parameters -> string
val get_agent_open_symbol : Remanent_parameters_sig.parameters -> string
val get_agent_close_symbol : Remanent_parameters_sig.parameters -> string
val get_agent_sep_comma_symbol : Remanent_parameters_sig.parameters -> string
val get_agent_sep_plus_symbol : Remanent_parameters_sig.parameters -> string
val get_agent_sep_dot_symbol : Remanent_parameters_sig.parameters -> string
val get_site_sep_comma_symbol : Remanent_parameters_sig.parameters -> string
val get_ghost_agent_symbol : Remanent_parameters_sig.parameters -> string
val get_do_we_show_ghost : Remanent_parameters_sig.parameters -> bool
val get_internal_state_symbol : Remanent_parameters_sig.parameters -> string
val get_open_internal_state : Remanent_parameters_sig.parameters -> string
val get_close_internal_state : Remanent_parameters_sig.parameters -> string
val get_free_symbol : Remanent_parameters_sig.parameters -> string
val get_uni_arrow_symbol : Remanent_parameters_sig.parameters -> string
val get_rev_arrow_symbol : Remanent_parameters_sig.parameters -> string
val get_rev_arrow_no_poly_symbol : Remanent_parameters_sig.parameters -> string
val get_uni_arrow_no_poly_symbol : Remanent_parameters_sig.parameters -> string
val get_bi_arrow_symbol : Remanent_parameters_sig.parameters -> string
val get_bi_arrow_no_poly_symbol : Remanent_parameters_sig.parameters -> string
val get_open_counter_state : Remanent_parameters_sig.parameters -> string
val get_open_counterceq : Remanent_parameters_sig.parameters -> string
val get_open_countercvar : Remanent_parameters_sig.parameters -> string
val get_open_countercgte : Remanent_parameters_sig.parameters -> string
val get_open_counterval : Remanent_parameters_sig.parameters -> string
val get_close_counter_state : Remanent_parameters_sig.parameters -> string
val get_close_counterceq : Remanent_parameters_sig.parameters -> string
val get_close_countercvar : Remanent_parameters_sig.parameters -> string
val get_close_countercgte : Remanent_parameters_sig.parameters -> string
val get_close_counterval : Remanent_parameters_sig.parameters -> string
val get_counterceq_symbol : Remanent_parameters_sig.parameters -> string
val get_countercvar_symbol : Remanent_parameters_sig.parameters -> string
val get_countercgte_symbol : Remanent_parameters_sig.parameters -> string
val get_open_counterdelta : Remanent_parameters_sig.parameters -> string
val get_close_counterdelta : Remanent_parameters_sig.parameters -> string
val get_counterdeltaplus_symbol : Remanent_parameters_sig.parameters -> string
val get_counterdeltaminus_symbol : Remanent_parameters_sig.parameters -> string
val get_counterval_symbol : Remanent_parameters_sig.parameters -> string

val get_rule_shape :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape
(** influence map *)

val get_rule_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_link_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_variable_shape :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape

val get_variable_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_influence_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_wake_up_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_inhibition_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_wake_up_arrow :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.headkind

val get_inhibition_arrow :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.headkind

val get_influence_arrow :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.headkind

val get_prompt_full_var_def : Remanent_parameters_sig.parameters -> bool
val get_prompt_full_rule_def : Remanent_parameters_sig.parameters -> bool

val get_make_labels_compatible_with_dot :
  Remanent_parameters_sig.parameters ->
  char list Remanent_parameters_sig.CharMap.t

val get_pure_contact : Remanent_parameters_sig.parameters -> bool
(** contact map *)

val get_binding_site_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_binding_site_shape :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape

val get_internal_site_shape :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape

val get_internal_site_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_counter_site_shape :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape

val get_counter_site_color :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_agent_shape_array :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape option array

val get_agent_color_array :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color option array

val get_agent_shape_def :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.shape

val get_agent_color_def :
  Remanent_parameters_sig.parameters -> Graph_loggers_sig.color

val get_dump_reachability_analysis_static :
  Remanent_parameters_sig.parameters -> bool
(** reachability analysis *)

val get_dump_reachability_analysis_dynamic :
  Remanent_parameters_sig.parameters -> bool

val get_dump_reachability_analysis_result :
  Remanent_parameters_sig.parameters -> bool

val get_dump_reachability_analysis_iteration :
  Remanent_parameters_sig.parameters -> bool

val get_dump_reachability_analysis_diff :
  Remanent_parameters_sig.parameters -> bool

val get_dump_reachability_analysis_wl :
  Remanent_parameters_sig.parameters -> bool

(*+ view analysis *)
val get_post_processing : Remanent_parameters_sig.parameters -> bool

val get_backend_mode :
  Remanent_parameters_sig.parameters ->
  Remanent_parameters_sig.reachability_output

val get_hide_one_d_relations_from_cartesian_decomposition :
  Remanent_parameters_sig.parameters -> bool

val get_smash_relations : Remanent_parameters_sig.parameters -> bool
val get_view_analysis : Remanent_parameters_sig.parameters -> bool
val get_site_across_bonds_analysis : Remanent_parameters_sig.parameters -> bool
val get_parallel_bonds_analysis : Remanent_parameters_sig.parameters -> bool
val get_dynamic_contact_map : Remanent_parameters_sig.parameters -> bool

val get_view_analysis_1 :
  Remanent_parameters_sig.reachability_parameters -> bool

val get_counters_analysis : Remanent_parameters_sig.parameters -> bool

val get_counters_domain :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.counters_domain

val get_counters_analysis_1 :
  Remanent_parameters_sig.reachability_parameters -> bool

val get_counters_domain_1 :
  Remanent_parameters_sig.reachability_parameters ->
  Remanent_parameters_sig.counters_domain

val get_reachability_analysis_parameters :
  Remanent_parameters_sig.parameters ->
  Remanent_parameters_sig.reachability_parameters

val get_site_across_bonds_analysis_1 :
  Remanent_parameters_sig.reachability_parameters -> bool

val get_parallel_bonds_analysis_1 :
  Remanent_parameters_sig.reachability_parameters -> bool

val get_dynamic_contact_map_1 :
  Remanent_parameters_sig.reachability_parameters -> bool

val get_reachability_parameters :
  unit -> Remanent_parameters_sig.reachability_parameters

val get_hide_reverse_rule_without_label_from_dead_rules :
  Remanent_parameters_sig.parameters -> bool

val get_local_trace_format :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.graph_format
(** local traces *)

val get_compute_local_traces : Remanent_parameters_sig.parameters -> bool

val get_show_rule_names_in_local_traces :
  Remanent_parameters_sig.parameters -> bool

val get_use_macrotransitions_in_local_traces :
  Remanent_parameters_sig.parameters -> bool

val set_use_macrotransitions_in_local_traces :
  Remanent_parameters_sig.parameters ->
  bool ->
  Remanent_parameters_sig.parameters

val get_ignore_local_losanges : Remanent_parameters_sig.parameters -> bool
val get_add_singular_macrostates : Remanent_parameters_sig.parameters -> bool
val get_add_singular_microstates : Remanent_parameters_sig.parameters -> bool
val get_local_trace_prefix : Remanent_parameters_sig.parameters -> string
val get_local_trace_directory : Remanent_parameters_sig.parameters -> string

val get_compute_separating_transitions :
  Remanent_parameters_sig.parameters -> bool

val set_compute_separating_transitions :
  Remanent_parameters_sig.parameters ->
  bool ->
  Remanent_parameters_sig.parameters

val get_compute_symmetries : Remanent_parameters_sig.parameters -> bool

val get_rate_convention :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.rate_convention

val set_trace :
  Remanent_parameters_sig.parameters ->
  bool ->
  Remanent_parameters_sig.parameters

val update_prefix :
  Remanent_parameters_sig.parameters ->
  string ->
  Remanent_parameters_sig.parameters

val update_call_stack :
  Remanent_parameters_sig.parameters ->
  bool ->
  string option ->
  Remanent_parameters_sig.parameters

val get_print_efficiency : Remanent_parameters_sig.parameters -> bool

val set_print_efficiency :
  Remanent_parameters_sig.parameters ->
  bool ->
  Remanent_parameters_sig.parameters

val open_influence_map_file :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.parameters

val open_contact_map_file :
  Remanent_parameters_sig.parameters -> Remanent_parameters_sig.parameters

val lexical_analysis_of_tested_only_patterns :
  Remanent_parameters_sig.parameters -> bool

val get_profiler : Remanent_parameters_sig.parameters -> Loggers.t

val get_compression_status_logger :
  Remanent_parameters_sig.parameters -> Loggers.t

val save_error_list :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  unit

val save_progress_bar :
  Remanent_parameters_sig.parameters -> bool * int * int * int -> unit

val reset_progress_bar : Remanent_parameters_sig.parameters -> unit -> unit

val save_current_phase_title :
  Remanent_parameters_sig.parameters -> string -> unit

val reset_current_phase_title :
  Remanent_parameters_sig.parameters -> unit -> unit

val get_empty_hashtbl_size : Remanent_parameters_sig.parameters -> int

val get_open_int_interval_inclusive_symbol :
  Remanent_parameters_sig.parameters -> string

val get_open_int_interval_exclusive_symbol :
  Remanent_parameters_sig.parameters -> string

val get_open_int_interval_infinity_symbol :
  Remanent_parameters_sig.parameters -> string

val get_close_int_interval_inclusive_symbol :
  Remanent_parameters_sig.parameters -> string

val get_close_int_interval_exclusive_symbol :
  Remanent_parameters_sig.parameters -> string

val get_close_int_interval_infinity_symbol :
  Remanent_parameters_sig.parameters -> string

val get_plus_infinity_symbol : Remanent_parameters_sig.parameters -> string
val get_minus_infinity_symbol : Remanent_parameters_sig.parameters -> string

val get_int_interval_separator_symbol :
  Remanent_parameters_sig.parameters -> string

val get_backdoor_nbr_of_scc : Remanent_parameters_sig.parameters -> bool

val get_backdoor_average_size_of_scc :
  Remanent_parameters_sig.parameters -> bool

val get_backdoor_nbr_of_constraints : Remanent_parameters_sig.parameters -> bool

val get_backdoor_nbr_of_nr_constraints :
  Remanent_parameters_sig.parameters -> bool

val get_backdoor_nbr_of_influences : Remanent_parameters_sig.parameters -> bool
val get_backdoor_nbr_of_dead_rules : Remanent_parameters_sig.parameters -> bool
val get_backdoor_nbr_of_rules : Remanent_parameters_sig.parameters -> bool

val get_backdoor_nbr_of_non_weakly_reversible_transitions :
  Remanent_parameters_sig.parameters -> bool

val get_backdoor_nbr_of_rules_with_non_weakly_reversible_transitions :
  Remanent_parameters_sig.parameters -> bool

val get_backdoor_timing : Remanent_parameters_sig.parameters -> bool
val get_backdoor_file : Remanent_parameters_sig.parameters -> string
val get_backdoor_directory : Remanent_parameters_sig.parameters -> string

val get_kasa_state :
  Remanent_parameters_sig.parameters -> Remanent_state_signature.engine_state
