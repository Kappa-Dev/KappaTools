(**
  * config.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: 08/03/2010
  * Last modification: Time-stamp: <Nov 28 2018>
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

let date = "<2015.01.23"
let version = "4.01"
let output_directory = ref "output"
let output_cm_directory = ref "output"
let output_im_directory = ref "output"
let output_local_trace_directory = ref "output"
let unsafe = ref true
let trace = ref false
let syntax_version = ref "V4"
let dump_error_as_soon_as_they_occur = ref false
let log = ref stdout
let formatter = ref Format.std_formatter
let file = ref (None : string option)
let link_mode = ref Remanent_parameters_sig.Bound_indices

(** influence map *)
let do_influence_map = ref true

let rule_shape = ref Graph_loggers_sig.Rect

let rule_color =
  ref Graph_loggers_sig.LightSkyBlue (*"#87ceeb" (* light sky blue *)*)

let variable_shape = ref Graph_loggers_sig.Ellipse

let variable_color =
  ref Graph_loggers_sig.PaleGreen (* "#98fb98" (*Pale green*)*)

let center_color = ref Graph_loggers_sig.Red
let wake_up_color = ref Graph_loggers_sig.Green (*"#00ff00" (*Green *)*)
let inhibition_color = ref Graph_loggers_sig.Red (*"#ff0000" (*red*)*)
let wake_up_arrow = ref Graph_loggers_sig.Normal
let inhibition_arrow = ref Graph_loggers_sig.Tee
let influence_map_file = ref "influence"
let influence_map_format = ref "DOT"
let prompt_full_var_def = ref false
let prompt_full_rule_def = ref false

let make_labels_compatible_with_dot =
  ref [ '\"', [ '\\'; '\"' ]; '\\', [ '\\'; '\\' ] ]

(** contact map*)
let do_contact_map = ref true

let do_scc = ref false
let pure_contact = ref false
let contact_map_file = ref "contact"
let contact_map_format = ref "DOT"
let binding_site_shape = ref Graph_loggers_sig.Circle
let binding_site_color = ref Graph_loggers_sig.Yellow
let internal_site_shape = ref Graph_loggers_sig.Ellipse
let internal_site_color = ref Graph_loggers_sig.Green
let counter_site_shape = ref Graph_loggers_sig.House
let counter_site_color = ref Graph_loggers_sig.Grey
let agent_shape_array = ref ([||] : Graph_loggers_sig.shape option array)
let agent_color_array = ref ([||] : Graph_loggers_sig.color option array)
let agent_shape_def = ref Graph_loggers_sig.Rect
let agent_color_def = ref Graph_loggers_sig.Blue
let link_color = ref Graph_loggers_sig.Black
let influence_color = ref Graph_loggers_sig.Red
let influence_arrow = ref Graph_loggers_sig.Normal

(**flow of information: internal; external flow*)
let do_ODE_flow_of_information = ref false

let do_stochastic_flow_of_information = ref false

(*covering classes: this parameter does not matter if it is true/false*)
let do_site_dependencies = ref false

(*set to true if one wants to print covering classes*)
let dump_site_dependencies = ref false

(*REMARK: one needs to set do_reachability_analysis to true first to be
  able to active different output *)
let do_reachability_analysis = ref true
let verbosity_level_for_reachability_analysis = ref "Low"
let dump_reachability_analysis_result = ref true
let dump_reachability_analysis_covering_classes = ref false
let dump_reachability_analysis_iteration = ref false
let dump_reachability_analysis_static = ref false
let dump_reachability_analysis_dynamic = ref false
let dump_reachability_analysis_diff = ref false
let dump_reachability_analysis_wl = ref false
let hide_reverse_rule_without_label_from_dead_rules = ref true
let hide_one_d_relations_from_cartesian_decomposition = ref true
let smash_relations = ref true
let use_natural_language = ref "kappa"
let compute_local_traces = ref false
let show_rule_names_in_local_traces = ref true
let use_macrotransitions_in_local_traces = ref false
let add_singular_macrostates = ref false
let add_singular_microstates = ref false
let do_not_compress_trivial_losanges = ref false
let local_trace_prefix = ref "Agent_trace_"
let local_trace_format = ref "DOT"
let compute_separating_transitions = ref false

(** accuracy *)

let with_views_analysis = ref true
let with_site_across_bonds_analysis = ref true
let with_parallel_bonds_analysis = ref true
let with_dynamic_contact_map = ref "dynamic"
let with_counters_analysis = ref true
let counter_analysis_domain = ref "mi"
let view_accuracy_level = ref "High"
let influence_map_accuracy_level = ref "Direct"
let contact_map_accuracy_level = ref "Low"
let scc_accuracy_level = ref "High"

(* Symmetries *)
let do_symmetries = ref false
let rate_convention = ref "biochemist"

(* Backdoors - stats for benchmarking *)
let print_efficiency = ref false
let backdoor_nbr_of_scc = ref false
let backdoor_average_size_of_scc = ref false
let backdoor_nbr_of_constraints = ref false
let backdoor_nbr_of_nr_constraints = ref false
let backdoor_nbr_of_influences = ref false
let backdoor_nbr_of_rules = ref false
let backdoor_nbr_of_dead_rules = ref false
let backdoor_nbr_of_rules_with_non_weakly_reversible_transitions = ref false
let backdoor_nbr_of_non_weakly_reversible_transitions = ref false
let backdoor_timing = ref false
let backdoor_file = ref "benchmark.tex"
let backdoor_directory = ref ""
