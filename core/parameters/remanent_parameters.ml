(**
  * parameters.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
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

let add_extension_if_not_already_mentioned a ext =
  let size_a = String.length a in
  let size_ext = String.length ext in
  try
    if size_a < size_ext || String.sub a (size_a - size_ext) size_ext = ext then
      a
    else
      a ^ ext
  with _ -> a ^ ext

let open_out a ext =
  (* it would be easier with OCaml 3.04 *)
  let a = add_extension_if_not_already_mentioned a ext in
  let d = Filename.dirname a in
  let () =
    try
      if not (Sys.is_directory d) then (
        Format.eprintf "'%s' is not a directory@." d;
        exit 1
      )
    with Sys_error _ -> Kappa_files.mk_dir_r d
  in
  open_out a

let open_append a ext =
  let a = add_extension_if_not_already_mentioned a ext in
  let d = Filename.dirname a in
  let () =
    try
      if not (Sys.is_directory d) then (
        Format.eprintf "'%s' is not a directory@." d;
        exit 1
      )
    with Sys_error _ -> Kappa_files.mk_dir_r d
  in
  try open_out_gen [ Open_append ] 511 a with _ -> open_out a ext

let compose f g x = f (g x)

let ext_format x =
  match x with
  | Remanent_parameters_sig.DOT -> ".dot"
  | Remanent_parameters_sig.HTML -> ".html"
  | Remanent_parameters_sig.DIM -> ".dim.json"
  | Remanent_parameters_sig.GEPHI -> ".gexf"

let fetch_level_gen s r =
  match Tools.lowercase !r with
  | "mute" | "none" -> Remanent_parameters_sig.None
  | "low" -> Remanent_parameters_sig.Low
  | "medium" -> Remanent_parameters_sig.Medium
  | "high" -> Remanent_parameters_sig.High
  | "complete" | "full" -> Remanent_parameters_sig.Full
  | x ->
    let () = Printf.eprintf "%s: %s is not a valid level !!!" s x in
    raise Exit

let fetch_graph_format f =
  match Tools.lowercase !f with
  | "dot" -> Remanent_parameters_sig.DOT
  | "html" -> Remanent_parameters_sig.HTML
  | "dim" -> Remanent_parameters_sig.DIM
  | "gephi" -> Remanent_parameters_sig.GEPHI
  | x ->
    let () = Printf.eprintf "%s is not a valid graph format !!!" x in
    raise Exit

let fetch_accuracy_level r = fetch_level_gen "an accuracy" r
let fetch_verbosity_level r = fetch_level_gen "a verbosity" r

let fetch_rate_convention f =
  match Tools.lowercase !f with
  | "kasim" -> Remanent_parameters_sig.No_correction
  | "divide_by_nbr_of_autos_in_lhs" ->
    Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs
  | "biochemist" -> Remanent_parameters_sig.Biochemist
  | x ->
    let () = Printf.eprintf "%s is not a valid rate convention !!!" x in
    raise Exit

let get_syntax_version () =
  match !Config.syntax_version with
  | "4" | "v4" | "V4" -> Ast.V4
  | "3" | "v3" | "V3" -> Ast.V3
  | _ -> failwith "Syntax version should be either V3 or V4"

let get_symbols () =
  match get_syntax_version () with
  | Ast.V3 -> Symbol_table.symbol_table_V3
  | Ast.V4 -> Symbol_table.symbol_table_V4

let get_influence_map () =
  {
    Remanent_parameters_sig.im_format =
      fetch_graph_format Config.influence_map_format;
    Remanent_parameters_sig.im_file =
      (match !Config.influence_map_file with
      | "" -> None
      | x -> Some x);
    Remanent_parameters_sig.im_directory =
      (match !Config.output_im_directory with
      | "" -> Some ""
      | x -> Some (x ^ "/"));
    Remanent_parameters_sig.rule_shape = !Config.rule_shape;
    Remanent_parameters_sig.rule_color = !Config.rule_color;
    Remanent_parameters_sig.variable_shape = !Config.variable_shape;
    Remanent_parameters_sig.variable_color = !Config.variable_color;
    Remanent_parameters_sig.wake_up_color = !Config.wake_up_color;
    Remanent_parameters_sig.inhibition_color = !Config.inhibition_color;
    Remanent_parameters_sig.wake_up_arrow = !Config.wake_up_arrow;
    Remanent_parameters_sig.inhibition_arrow = !Config.inhibition_arrow;
    Remanent_parameters_sig.prompt_full_var_def = !Config.prompt_full_var_def;
    Remanent_parameters_sig.prompt_full_rule_def = !Config.prompt_full_rule_def;
    Remanent_parameters_sig.make_labels_compatible =
      List.fold_left
        (fun map (a, l) -> Remanent_parameters_sig.CharMap.add a l map)
        Remanent_parameters_sig.CharMap.empty
        !Config.make_labels_compatible_with_dot;
  }

let get_contact_map () =
  {
    Remanent_parameters_sig.cm_format =
      fetch_graph_format Config.contact_map_format;
    Remanent_parameters_sig.cm_file =
      (match !Config.contact_map_file with
      | "" -> None
      | x -> Some x);
    Remanent_parameters_sig.cm_directory =
      (match !Config.output_cm_directory with
      | "" -> Some ""
      | x -> Some (x ^ "/"));
    Remanent_parameters_sig.pure_contact = !Config.pure_contact;
    Remanent_parameters_sig.binding_site_shape = !Config.binding_site_shape;
    Remanent_parameters_sig.binding_site_color = !Config.binding_site_color;
    Remanent_parameters_sig.internal_site_shape = !Config.internal_site_shape;
    Remanent_parameters_sig.internal_site_color = !Config.internal_site_color;
    Remanent_parameters_sig.counter_site_shape = !Config.counter_site_shape;
    Remanent_parameters_sig.counter_site_color = !Config.counter_site_color;
    Remanent_parameters_sig.agent_shape_array = !Config.agent_shape_array;
    Remanent_parameters_sig.agent_color_array = !Config.agent_color_array;
    Remanent_parameters_sig.agent_shape_def = !Config.agent_shape_def;
    Remanent_parameters_sig.agent_color_def = !Config.agent_color_def;
    Remanent_parameters_sig.link_color = !Config.link_color;
    Remanent_parameters_sig.influence_color = !Config.influence_color;
    Remanent_parameters_sig.influence_arrow = !Config.influence_arrow;
  }

let reachability_map_0 =
  {
    Remanent_parameters_sig.dump_reachability_analysis_result = false;
    Remanent_parameters_sig.dump_reachability_analysis_iteration = false;
    Remanent_parameters_sig.dump_reachability_analysis_diff = false;
    Remanent_parameters_sig.dump_reachability_analysis_wl = false;
    Remanent_parameters_sig.dump_reachability_analysis_covering_classes = false;
    Remanent_parameters_sig.dump_reachability_analysis_static = false;
    Remanent_parameters_sig.dump_reachability_analysis_dynamic = false;
    Remanent_parameters_sig.hide_one_d_relations_from_cartesian_decomposition =
      false;
    Remanent_parameters_sig.compute_local_traces = false;
    Remanent_parameters_sig.compute_separating_transitions = false;
    Remanent_parameters_sig.show_rule_names_in_local_traces = false;
    Remanent_parameters_sig.use_macrotransitions_in_local_traces = false;
    Remanent_parameters_sig.ignore_trivial_losanges = false;
    Remanent_parameters_sig.add_singular_macrostates = false;
    Remanent_parameters_sig.add_singular_microstates = false;
    Remanent_parameters_sig.smash_relations = false;
    Remanent_parameters_sig.hide_reverse_rule_without_label_from_dead_rules =
      true;
    Remanent_parameters_sig.use_natural_language = Remanent_parameters_sig.Kappa;
    Remanent_parameters_sig.format_for_local_traces =
      Remanent_parameters_sig.DOT;
    Remanent_parameters_sig.trace_prefix = "Agent_trace_";
    Remanent_parameters_sig.trace_directory =
      (match !Config.output_local_trace_directory with
      | "" -> ""
      | x -> x ^ "/");
  }

let reachability_map_1 =
  {
    reachability_map_0 with
    Remanent_parameters_sig.dump_reachability_analysis_result = true;
  }

let reachability_map_2 =
  {
    reachability_map_1 with
    Remanent_parameters_sig.dump_reachability_analysis_iteration = true;
  }

let reachability_map_3 =
  {
    reachability_map_2 with
    Remanent_parameters_sig.dump_reachability_analysis_diff = true;
  }

let reachability_map_4 =
  {
    reachability_map_3 with
    Remanent_parameters_sig.dump_reachability_analysis_wl = true;
  }

let add_debugging_parameters_to_reachability_map reachability =
  let trace = !Config.trace in
  let reachability =
    {
      reachability with
      Remanent_parameters_sig.hide_reverse_rule_without_label_from_dead_rules =
        !Config.hide_reverse_rule_without_label_from_dead_rules;
      Remanent_parameters_sig.hide_one_d_relations_from_cartesian_decomposition =
        !Config.hide_one_d_relations_from_cartesian_decomposition;
      Remanent_parameters_sig.smash_relations = !Config.smash_relations;
      Remanent_parameters_sig.use_natural_language =
        (match !Config.use_natural_language with
        | "raw" | "RAW" | "Raw" -> Remanent_parameters_sig.Raw
        | "kappa" | "KAPPA" | "Kappa" -> Remanent_parameters_sig.Kappa
        | "English" | "ENGLISH" | "english" ->
          Remanent_parameters_sig.Natural_language
        | _ -> Remanent_parameters_sig.Kappa);
      Remanent_parameters_sig.compute_local_traces =
        !Config.compute_local_traces;
      Remanent_parameters_sig.compute_separating_transitions =
        !Config.compute_separating_transitions;
      Remanent_parameters_sig.ignore_trivial_losanges =
        !Config.do_not_compress_trivial_losanges;
      Remanent_parameters_sig.add_singular_macrostates =
        !Config.add_singular_macrostates;
      Remanent_parameters_sig.add_singular_microstates =
        !Config.add_singular_microstates;
      Remanent_parameters_sig.show_rule_names_in_local_traces =
        !Config.show_rule_names_in_local_traces;
      Remanent_parameters_sig.use_macrotransitions_in_local_traces =
        !Config.use_macrotransitions_in_local_traces;
      Remanent_parameters_sig.format_for_local_traces =
        fetch_graph_format Config.local_trace_format;
      Remanent_parameters_sig.trace_prefix = !Config.local_trace_prefix;
      Remanent_parameters_sig.trace_directory =
        (match !Config.output_local_trace_directory with
        | "" -> ""
        | x -> x ^ "/");
    }
  in
  if trace then
    {
      reachability with
      Remanent_parameters_sig.dump_reachability_analysis_covering_classes =
        !Config.dump_reachability_analysis_covering_classes;
      Remanent_parameters_sig.dump_reachability_analysis_static =
        !Config.dump_reachability_analysis_static;
      Remanent_parameters_sig.dump_reachability_analysis_dynamic =
        !Config.dump_reachability_analysis_dynamic;
    }
  else
    reachability

let get_reachability_map () =
  add_debugging_parameters_to_reachability_map
    (match
       fetch_verbosity_level Config.verbosity_level_for_reachability_analysis
     with
    | Remanent_parameters_sig.None -> reachability_map_0
    | Remanent_parameters_sig.Low -> reachability_map_1
    | Remanent_parameters_sig.Medium -> reachability_map_2
    | Remanent_parameters_sig.High -> reachability_map_3
    | Remanent_parameters_sig.Full -> reachability_map_4)

let get_reachability_parameters () =
  {
    Remanent_parameters_sig.views = !Config.with_views_analysis;
    Remanent_parameters_sig.site_across_bonds =
      !Config.with_site_across_bonds_analysis;
    Remanent_parameters_sig.parallel_bonds =
      !Config.with_parallel_bonds_analysis;
    Remanent_parameters_sig.counters = !Config.with_counters_analysis;
    Remanent_parameters_sig.dynamic_contact_map =
      (match Tools.lowercase !Config.with_dynamic_contact_map with
      | "dynamic" -> true
      | "static" -> false
      | _ -> true);
    Remanent_parameters_sig.counter_domain =
      (match Tools.lowercase !Config.counter_analysis_domain with
      | "mi" -> Remanent_parameters_sig.Mi
      | "octagon" | "oct" | "octo" -> Remanent_parameters_sig.Octagons
      | "abstract-multiset" | "am" | "abstract_multiset" ->
        Remanent_parameters_sig.Abstract_multiset
      | "non-relational" | "nr" | "non-rel" | "non_rel" | "non_relational" ->
        Remanent_parameters_sig.Non_relational
      | _ -> Remanent_parameters_sig.Mi);
  }

let open_tasks_profiling =
  let cache = ref None in
  fun () ->
    match !cache with
    | None ->
      let channel = Kappa_files.open_tasks_profiling () in
      let () = cache := Some channel in
      channel
    | Some channel -> channel

let fetch_backdoors () =
  {
    Remanent_parameters_sig.backdoor_nbr_of_scc = !Config.backdoor_nbr_of_scc;
    Remanent_parameters_sig.backdoor_average_size_of_scc =
      !Config.backdoor_average_size_of_scc;
    Remanent_parameters_sig.backdoor_nbr_of_constraints =
      !Config.backdoor_nbr_of_constraints;
    Remanent_parameters_sig.backdoor_nbr_of_nr_constraints =
      !Config.backdoor_nbr_of_nr_constraints;
    Remanent_parameters_sig.backdoor_nbr_of_influences =
      !Config.backdoor_nbr_of_influences;
    Remanent_parameters_sig.backdoor_nbr_of_dead_rules =
      !Config.backdoor_nbr_of_dead_rules;
    Remanent_parameters_sig.backdoor_nbr_of_rules =
      !Config.backdoor_nbr_of_rules;
    Remanent_parameters_sig.backdoor_nbr_of_non_weakly_reversible_transitions =
      !Config.backdoor_nbr_of_non_weakly_reversible_transitions;
    Remanent_parameters_sig
    .backdoor_nbr_of_rules_with_non_weakly_reversible_transitions =
      !Config.backdoor_nbr_of_rules_with_non_weakly_reversible_transitions;
    Remanent_parameters_sig.backdoor_timing = !Config.backdoor_timing;
    Remanent_parameters_sig.backdoor_file = !Config.backdoor_file;
    Remanent_parameters_sig.backdoor_directory = !Config.backdoor_directory;
  }

let get_parameters ?(html_mode = true) ~called_from () =
  let channel, channel_err, channel_backdoor, html_mode, command =
    match called_from with
    | Remanent_parameters_sig.Server ->
      None, None, None, false || html_mode, [| "KaSa"; "(Interractive mode)" |]
    | Remanent_parameters_sig.Internalised ->
      ( Some stdout,
        Some Format.err_formatter,
        Some stdout,
        false || html_mode,
        Sys.argv )
    | Remanent_parameters_sig.KaSim ->
      Some (open_tasks_profiling ()), None, None, false || html_mode, Sys.argv
    | Remanent_parameters_sig.KaSa ->
      ( (match
           !Config.output_directory, "profiling", ".html"
           (*temporary, to do: provide a parameterisable filename*)
         with
        | _, "", _ -> Some stdout
        | "", a, ext -> Some (open_out a ext)
        | a, b, ext -> Some (open_out (a ^ "/" ^ b) ext)),
        Some Format.err_formatter,
        (match
           ( !Config.backdoor_nbr_of_rules
             || !Config.backdoor_nbr_of_scc
             || !Config.backdoor_average_size_of_scc
             || !Config.backdoor_nbr_of_dead_rules
             || !Config.backdoor_nbr_of_rules
             || !Config.backdoor_nbr_of_non_weakly_reversible_transitions
             || !Config
                 .backdoor_nbr_of_rules_with_non_weakly_reversible_transitions
             || !Config.backdoor_nbr_of_non_weakly_reversible_transitions
             || !Config.backdoor_nbr_of_constraints
             || !Config.backdoor_nbr_of_nr_constraints
             || !Config.backdoor_timing
             || !Config.backdoor_nbr_of_influences,
             !Config.backdoor_directory,
             !Config.backdoor_file,
             ".tex" )
         with
        | false, _, _, _ -> None
        | _, _, "", _ -> Some stdout
        | _, "", a, ext -> Some (open_append a ext)
        | _, a, b, ext -> Some (open_append (a ^ "/" ^ b) ext)),
        false || html_mode,
        Sys.argv )
  in
  {
    Remanent_parameters_sig.marshalisable_parameters =
      {
        Remanent_parameters_sig.syntax_version = get_syntax_version ();
        Remanent_parameters_sig.do_contact_map = !Config.do_contact_map;
        Remanent_parameters_sig.do_scc = !Config.do_scc;
        Remanent_parameters_sig.do_influence_map = !Config.do_influence_map;
        Remanent_parameters_sig.do_ODE_flow_of_information =
          !Config.do_ODE_flow_of_information;
        Remanent_parameters_sig.do_stochastic_flow_of_information =
          !Config.do_stochastic_flow_of_information;
        Remanent_parameters_sig.do_site_dependencies =
          !Config.do_site_dependencies;
        Remanent_parameters_sig.do_symmetries_analysis = !Config.do_symmetries;
        Remanent_parameters_sig.rate_convention =
          fetch_rate_convention Config.rate_convention;
        Remanent_parameters_sig.dump_site_dependencies =
          !Config.dump_site_dependencies;
        (*different reachability output*)
        Remanent_parameters_sig.do_reachability_analysis =
          !Config.do_reachability_analysis;
        (*GET SYMBOLS*)
        Remanent_parameters_sig.file = !Config.file;
        Remanent_parameters_sig.symbols = get_symbols ();
        Remanent_parameters_sig.influence_map_output = get_influence_map ();
        Remanent_parameters_sig.contact_map_output = get_contact_map ();
        Remanent_parameters_sig.reachability_map_output =
          get_reachability_map ();
        Remanent_parameters_sig.reachability_analysis_parameters =
          get_reachability_parameters ();
        Remanent_parameters_sig.unsafe = !Config.unsafe;
        Remanent_parameters_sig.trace = !Config.trace;
        Remanent_parameters_sig.dump_error_as_soon_as_they_occur =
          !Config.dump_error_as_soon_as_they_occur;
        Remanent_parameters_sig.prefix = "";
        Remanent_parameters_sig.call_stack = [];
        Remanent_parameters_sig.link_mode = !Config.link_mode;
        Remanent_parameters_sig.kasa_state =
          Remanent_state_signature.empty_engine_state;
        Remanent_parameters_sig.launching_date =
          Unix.localtime (Unix.gettimeofday ());
        Remanent_parameters_sig.time_shift =
          (let x = Unix.gettimeofday () in
           (Unix.localtime x).Unix.tm_hour - (Unix.gmtime x).Unix.tm_hour);
        Remanent_parameters_sig.hostname =
          (try Unix.gethostname () with _ -> "javascript");
        Remanent_parameters_sig.command_line = command;
        Remanent_parameters_sig.short_version = Version.version_string;
        Remanent_parameters_sig.version = Version.version_kasa_full_name;
        Remanent_parameters_sig.tk_interface = !Version.tk_is_initialized;
        Remanent_parameters_sig.influence_map_accuracy_level =
          (match Tools.lowercase !Config.influence_map_accuracy_level with
          | "indirect" -> Remanent_parameters_sig.Low
          | "direct" -> Remanent_parameters_sig.Medium
          | "realisable" | "realizable" -> Remanent_parameters_sig.High
          | _ -> fetch_accuracy_level Config.influence_map_accuracy_level);
        Remanent_parameters_sig.contact_map_accuracy_level =
          fetch_accuracy_level Config.contact_map_accuracy_level;
        Remanent_parameters_sig.scc_accuracy_level =
          fetch_accuracy_level Config.scc_accuracy_level;
        Remanent_parameters_sig.view_accuracy_level =
          fetch_accuracy_level Config.view_accuracy_level;
        Remanent_parameters_sig.called_from;
        Remanent_parameters_sig.html_mode;
        Remanent_parameters_sig.empty_hashtbl_size = 1;
        Remanent_parameters_sig.backdoors = fetch_backdoors ();
      };
    Remanent_parameters_sig.save_error_list = (fun _ -> ());
    Remanent_parameters_sig.save_progress_bar = (fun _ -> ());
    Remanent_parameters_sig.reset_progress_bar = (fun _ -> ());
    Remanent_parameters_sig.save_current_phase_title = (fun _ -> ());
    Remanent_parameters_sig.reset_current_phase_title = (fun _ -> ());
    Remanent_parameters_sig.logger =
      (match channel with
      | None -> Loggers.dummy_txt_logger
      | Some _ -> Loggers.open_logger_from_formatter !Config.formatter);
    Remanent_parameters_sig.logger_err =
      (match channel_err with
      | None -> Loggers.dummy_txt_logger
      | Some fmt -> Loggers.open_logger_from_formatter fmt);
    Remanent_parameters_sig.logger_backdoor =
      (match channel_backdoor with
      | None -> Loggers.dummy_txt_logger
      | Some channel -> Loggers.open_logger_from_channel channel);
    Remanent_parameters_sig.compression_status = Loggers.dummy_txt_logger;
    Remanent_parameters_sig.print_efficiency = !Config.print_efficiency;
    Remanent_parameters_sig.profiler =
      (match channel with
      | None -> Loggers.dummy_txt_logger
      | Some a -> Loggers.open_logger_from_channel ~mode:Loggers.HTML_Tabular a);
  }

let dummy_parameters ~called_from =
  let cache = ref None in
  fun () ->
    match !cache with
    | None ->
      let p = get_parameters ~called_from () in
      let () = cache := Some p in
      p
    | Some p -> p

let get_bound_symbol_1 symbol = symbol.Symbol_table.bound
let get_open_binding_state_1 symbol = symbol.Symbol_table.open_binding_state
let get_close_binding_state_1 symbol = symbol.Symbol_table.close_binding_state

let get_missing_binding_state_1 symbol =
  symbol.Symbol_table.missing_binding_state

let get_link_to_any_1 symbol = symbol.Symbol_table.link_to_any
let get_link_to_some_1 symbol = symbol.Symbol_table.link_to_some

let get_internal_state_symbol_1 symbol =
  symbol.Symbol_table.internal_state_symbol

let get_open_internal_state_1 symbol = symbol.Symbol_table.open_internal_state
let get_close_internal_state_1 symbol = symbol.Symbol_table.close_internal_state
let get_free_1 symbol = symbol.Symbol_table.free
let get_at_symbol_1 symbol = symbol.Symbol_table.at
let get_agent_open_symbol_1 symbol = symbol.Symbol_table.agent_open
let get_agent_close_symbol_1 symbol = symbol.Symbol_table.agent_close

let get_agent_sep_comma_symbol_1 symbol =
  fst symbol.Symbol_table.agent_sep_comma

let get_agent_sep_plus_symbol_1 symbol = fst symbol.Symbol_table.agent_sep_plus
let get_agent_sep_dot_symbol_1 symbol = fst symbol.Symbol_table.agent_sep_dot
let get_btype_sep_symbol_1 symbol = symbol.Symbol_table.btype_sep
let get_site_sep_comma_symbol_1 symbol = fst symbol.Symbol_table.site_sep
let get_ghost_agent_symbol_1 symbol = symbol.Symbol_table.ghost_agent
let get_do_we_show_ghost_1 symbol = symbol.Symbol_table.show_ghost
let get_uni_arrow_symbol_1 symbol = symbol.Symbol_table.uni_arrow
let get_rev_arrow_symbol_1 symbol = symbol.Symbol_table.rev_arrow
let get_bi_arrow_symbol_1 symbol = symbol.Symbol_table.bi_arrow
let get_bi_arrow_no_poly_symbol_1 symbol = symbol.Symbol_table.bi_arrow_nopoly
let get_uni_arrow_no_poly_symbol_1 symbol = symbol.Symbol_table.uni_arrow_nopoly
let get_rev_arrow_no_poly_symbol_1 symbol = symbol.Symbol_table.rev_arrow_nopoly

let get_open_int_interval_inclusive_symbol_1 symbol =
  symbol.Symbol_table.open_int_interval_inclusive

let get_open_int_interval_exclusive_symbol_1 symbol =
  symbol.Symbol_table.open_int_interval_exclusive

let get_open_int_interval_infinity_symbol_1 symbol =
  symbol.Symbol_table.open_int_interval_infinity

let get_close_int_interval_inclusive_symbol_1 symbol =
  symbol.Symbol_table.close_int_interval_inclusive

let get_close_int_interval_exclusive_symbol_1 symbol =
  symbol.Symbol_table.close_int_interval_exclusive

let get_close_int_interval_infinity_symbol_1 symbol =
  symbol.Symbol_table.close_int_interval_infinity

let get_plus_infinity_symbol_1 symbol = symbol.Symbol_table.plus_infinity
let get_minus_infinity_symbol_1 symbol = symbol.Symbol_table.minus_infinity

let get_int_interval_separator_symbol_1 symbol =
  symbol.Symbol_table.int_interval_separator

let get_open_counter_state_1 symbol = symbol.Symbol_table.open_counter_state
let get_open_counterceq_1 symbol = symbol.Symbol_table.open_counterceq
let get_open_countercvar_1 symbol = symbol.Symbol_table.open_countercvar
let get_open_countercgte_1 symbol = symbol.Symbol_table.open_countercgte
let get_open_counterdelta_1 symbol = symbol.Symbol_table.open_counterdelta
let get_open_counterval_1 symbol = symbol.Symbol_table.open_counterval
let get_close_counter_state_1 symbol = symbol.Symbol_table.close_counter_state
let get_close_counterceq_1 symbol = symbol.Symbol_table.close_counterceq
let get_close_countercvar_1 symbol = symbol.Symbol_table.close_countercvar
let get_close_countercgte_1 symbol = symbol.Symbol_table.close_countercgte
let get_close_counterdelta_1 symbol = symbol.Symbol_table.close_counterdelta
let get_close_counterval_1 symbol = symbol.Symbol_table.close_counterval
let get_counterceq_symbol_1 symbol = symbol.Symbol_table.counterceq_symbol
let get_countercvar_symbol_1 symbol = symbol.Symbol_table.countercvar_symbol
let get_countercgte_symbol_1 symbol = symbol.Symbol_table.countercgte_symbol

let get_counterdeltaplus_symbol_1 symbol =
  symbol.Symbol_table.counterdeltaplus_symbol

let get_counterdeltaminus_symbol_1 symbol =
  symbol.Symbol_table.counterdeltaminus_symbol

let get_counterval_symbol_1 symbol = symbol.Symbol_table.counterval_symbol

(*Influence*)
let get_im_format_1 influence = influence.Remanent_parameters_sig.im_format
let get_im_file_1 influence = influence.Remanent_parameters_sig.im_file

let get_im_directory_1 influence =
  influence.Remanent_parameters_sig.im_directory

let get_rule_shape_1 influence = influence.Remanent_parameters_sig.rule_shape
let get_rule_color_1 influence = influence.Remanent_parameters_sig.rule_color

let get_variable_shape_1 influence =
  influence.Remanent_parameters_sig.variable_shape

let get_variable_color_1 influence =
  influence.Remanent_parameters_sig.variable_color

let get_wake_up_color_1 influence =
  influence.Remanent_parameters_sig.wake_up_color

let get_inhibition_color_1 influence =
  influence.Remanent_parameters_sig.inhibition_color

let get_wake_up_arrow_1 influence =
  influence.Remanent_parameters_sig.wake_up_arrow

let get_inhibition_arrow_1 influence =
  influence.Remanent_parameters_sig.inhibition_arrow

let get_prompt_full_var_def_1 influence =
  influence.Remanent_parameters_sig.prompt_full_var_def

let get_prompt_full_rule_def_1 influence =
  influence.Remanent_parameters_sig.prompt_full_rule_def

let get_make_labels_compatible_1 influence =
  influence.Remanent_parameters_sig.make_labels_compatible

let get_pure_contact_1 cm = cm.Remanent_parameters_sig.pure_contact
let get_cm_format_1 cm = cm.Remanent_parameters_sig.cm_format
let get_cm_file_1 cm = cm.Remanent_parameters_sig.cm_file
let get_cm_directory_1 cm = cm.Remanent_parameters_sig.cm_directory
let get_binding_site_shape_1 cm = cm.Remanent_parameters_sig.binding_site_shape
let get_binding_site_color_1 cm = cm.Remanent_parameters_sig.binding_site_color

let get_internal_site_shape_1 cm =
  cm.Remanent_parameters_sig.internal_site_shape

let get_internal_site_color_1 cm =
  cm.Remanent_parameters_sig.internal_site_color

let get_counter_site_shape_1 cm = cm.Remanent_parameters_sig.counter_site_shape
let get_counter_site_color_1 cm = cm.Remanent_parameters_sig.counter_site_color
let get_agent_shape_array_1 cm = cm.Remanent_parameters_sig.agent_shape_array
let get_agent_color_array_1 cm = cm.Remanent_parameters_sig.agent_color_array
let get_agent_shape_def_1 cm = cm.Remanent_parameters_sig.agent_shape_def
let get_agent_color_def_1 cm = cm.Remanent_parameters_sig.agent_color_def
let get_link_color_1 cm = cm.Remanent_parameters_sig.link_color
let get_influence_color_1 cm = cm.Remanent_parameters_sig.influence_color
let get_influence_arrow_1 cm = cm.Remanent_parameters_sig.influence_arrow

(*add reachability*)

let get_dump_reachability_analysis_result_1 r =
  r.Remanent_parameters_sig.dump_reachability_analysis_result

let get_dump_reachability_analysis_iteration_1 r =
  r.Remanent_parameters_sig.dump_reachability_analysis_iteration

let get_dump_reachability_analysis_static_1 r =
  r.Remanent_parameters_sig.dump_reachability_analysis_static

let get_dump_reachability_analysis_dynamic_1 r =
  r.Remanent_parameters_sig.dump_reachability_analysis_dynamic

let get_dump_reachability_analysis_diff_1 r =
  r.Remanent_parameters_sig.dump_reachability_analysis_diff

let get_dump_reachability_analysis_wl_1 r =
  r.Remanent_parameters_sig.dump_reachability_analysis_wl

let get_smash_relations_1 r = r.Remanent_parameters_sig.smash_relations

let get_hide_one_d_relations_from_cartesian_decomposition_1 r =
  r.Remanent_parameters_sig.hide_one_d_relations_from_cartesian_decomposition

let get_hide_reverse_rule_without_label_from_dead_rules_1 r =
  r.Remanent_parameters_sig.hide_reverse_rule_without_label_from_dead_rules

let get_post_processing_1 r =
  match r.Remanent_parameters_sig.use_natural_language with
  | Remanent_parameters_sig.Raw -> false
  | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Natural_language ->
    true

let get_backend_mode_1 r = r.Remanent_parameters_sig.use_natural_language

let get_local_trace_format_1 r =
  r.Remanent_parameters_sig.format_for_local_traces

let get_compute_local_traces_1 r =
  r.Remanent_parameters_sig.compute_local_traces

let get_compute_separating_transitions_1 r =
  r.Remanent_parameters_sig.compute_separating_transitions

let set_compute_separating_transitions_1 r b =
  { r with Remanent_parameters_sig.compute_separating_transitions = b }

let set_use_macrotransition_in_local_traces_1 r b =
  { r with Remanent_parameters_sig.use_macrotransitions_in_local_traces = b }

let get_ignore_trivial_losanges_1 r =
  r.Remanent_parameters_sig.ignore_trivial_losanges

let get_show_rule_names_in_local_traces_1 r =
  r.Remanent_parameters_sig.show_rule_names_in_local_traces

let get_use_macrotransitions_in_local_traces_1 r =
  r.Remanent_parameters_sig.use_macrotransitions_in_local_traces

let get_add_singular_macrostates_1 r =
  r.Remanent_parameters_sig.add_singular_macrostates

let get_add_singular_microstates_1 r =
  r.Remanent_parameters_sig.add_singular_microstates

let get_local_trace_prefix_1 r = r.Remanent_parameters_sig.trace_prefix
let get_local_trace_directory_1 r = r.Remanent_parameters_sig.trace_directory
let get_view_analysis_1 r = r.Remanent_parameters_sig.views

let get_site_across_bonds_analysis_1 r =
  r.Remanent_parameters_sig.site_across_bonds

let get_parallel_bonds_analysis_1 r = r.Remanent_parameters_sig.parallel_bonds
let get_dynamic_contact_map_1 r = r.Remanent_parameters_sig.dynamic_contact_map
let get_counters_analysis_1 r = r.Remanent_parameters_sig.counters
let get_counters_domain_1 r = r.Remanent_parameters_sig.counter_domain

let get_compute_symmetries_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_symmetries_analysis

let get_rate_convention_1 marshalisable =
  marshalisable.Remanent_parameters_sig.rate_convention

let get_empty_hashtbl_size_1 marshalisable =
  marshalisable.Remanent_parameters_sig.empty_hashtbl_size

let get_symbols_1 marshalisable = marshalisable.Remanent_parameters_sig.symbols
let get_file_1 marshalisable = marshalisable.Remanent_parameters_sig.file

let get_influence_map_1 marshalisable =
  marshalisable.Remanent_parameters_sig.influence_map_output

let get_contact_map_1 marshalisable =
  marshalisable.Remanent_parameters_sig.contact_map_output

(*add reachability*)
let get_reachability_map_1 marshalisable =
  marshalisable.Remanent_parameters_sig.reachability_map_output

let get_reachability_analysis_parameters_1 marshalisable =
  marshalisable.Remanent_parameters_sig.reachability_analysis_parameters

let get_unsafe_1 marshalisable = marshalisable.Remanent_parameters_sig.unsafe
let get_trace_1 marshalisable = marshalisable.Remanent_parameters_sig.trace

let get_dump_error_as_soon_as_they_occur_1 marshalisable =
  marshalisable.Remanent_parameters_sig.dump_error_as_soon_as_they_occur

let get_prefix_1 marshalisable = marshalisable.Remanent_parameters_sig.prefix

let get_call_stack_1 marshalisable =
  marshalisable.Remanent_parameters_sig.call_stack

let get_link_mode_1 marshalisable =
  marshalisable.Remanent_parameters_sig.link_mode

let get_kasa_state_1 marshalisable =
  marshalisable.Remanent_parameters_sig.kasa_state

let get_do_contact_map_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_contact_map

let get_syntax_version_1 marshalisable =
  marshalisable.Remanent_parameters_sig.syntax_version

let get_do_scc_1 marshalisable = marshalisable.Remanent_parameters_sig.do_scc

let get_do_influence_map_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_influence_map

let get_do_ODE_flow_of_information_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_ODE_flow_of_information

let get_do_stochastic_flow_of_information_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_stochastic_flow_of_information

let get_do_site_dependencies_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_site_dependencies

let get_dump_site_dependencies_1 marshalisable =
  marshalisable.Remanent_parameters_sig.dump_site_dependencies

(*reachability different output*)
let get_do_reachability_analysis_1 marshalisable =
  marshalisable.Remanent_parameters_sig.do_reachability_analysis

(**)
let get_influence_map_accuracy_level_1 marshalisable =
  marshalisable.Remanent_parameters_sig.influence_map_accuracy_level

let get_contact_map_accuracy_level_1 marshalisable =
  marshalisable.Remanent_parameters_sig.contact_map_accuracy_level

let get_scc_accuracy_level_1 marshalisable =
  marshalisable.Remanent_parameters_sig.scc_accuracy_level

let get_view_accuracy_level_1 marshalisable =
  marshalisable.Remanent_parameters_sig.view_accuracy_level

let get_launching_date_1 marshalisable =
  let t = marshalisable.Remanent_parameters_sig.launching_date in
  let gmt = marshalisable.Remanent_parameters_sig.time_shift in
  Printf.sprintf "Analysis launched at %04i/%02i/%02i %02i:%02i:%02i (GMT%+i)"
    (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour
    t.Unix.tm_min t.Unix.tm_sec gmt

let get_short_version_1 marshalisable =
  marshalisable.Remanent_parameters_sig.version

let get_full_version_1 marshalisable =
  Printf.sprintf "%s (with%s Tk interface)"
    marshalisable.Remanent_parameters_sig.version
    (if marshalisable.Remanent_parameters_sig.tk_interface then
       ""
     else
       "out")

let get_launched_where_1 marshalisable =
  marshalisable.Remanent_parameters_sig.hostname

let get_command_line_1 marshalisable =
  marshalisable.Remanent_parameters_sig.command_line

let get_marshalisable parameter =
  parameter.Remanent_parameters_sig.marshalisable_parameters

let get_logger parameter = parameter.Remanent_parameters_sig.logger
let get_logger_err parameter = parameter.Remanent_parameters_sig.logger_err

let get_logger_backdoor parameter =
  parameter.Remanent_parameters_sig.logger_backdoor

(*let get_formatter parameter = parameter.Remanent_parameters_sig.formatter
  let set_formatter parameter logger = {parameter with Remanent_parameters_sig.formatter = logger}*)

let upgrade_from_marshal_field f = compose f get_marshalisable
let get_command_line = upgrade_from_marshal_field get_command_line_1
let get_short_version = upgrade_from_marshal_field get_short_version_1
let get_launched_where = upgrade_from_marshal_field get_launched_where_1
let get_full_version = upgrade_from_marshal_field get_full_version_1
let get_launching_date = upgrade_from_marshal_field get_launching_date_1

let get_launched_when_and_where parameters =
  Printf.sprintf "%s on %s"
    (get_launching_date parameters)
    (get_launched_where parameters)

let get_do_contact_map = upgrade_from_marshal_field get_do_contact_map_1
let get_syntax_version = upgrade_from_marshal_field get_syntax_version_1
let get_do_scc = upgrade_from_marshal_field get_do_scc_1
let get_do_influence_map = upgrade_from_marshal_field get_do_influence_map_1

let get_do_ODE_flow_of_information =
  upgrade_from_marshal_field get_do_ODE_flow_of_information_1

let get_do_stochastic_flow_of_information =
  upgrade_from_marshal_field get_do_stochastic_flow_of_information_1

let get_do_site_dependencies =
  upgrade_from_marshal_field get_do_site_dependencies_1

let get_dump_site_dependencies =
  upgrade_from_marshal_field get_dump_site_dependencies_1
(*reachability analysis in different output*)

(**)
let get_symbols = upgrade_from_marshal_field get_symbols_1
let get_file = upgrade_from_marshal_field get_file_1
let get_compute_symmetries = upgrade_from_marshal_field get_compute_symmetries_1
let get_rate_convention = upgrade_from_marshal_field get_rate_convention_1
let get_influence_map = upgrade_from_marshal_field get_influence_map_1
let get_contact_map = upgrade_from_marshal_field get_contact_map_1

(*add reachability*)
let get_reachability_map = upgrade_from_marshal_field get_reachability_map_1

let get_reachability_analysis_parameters =
  upgrade_from_marshal_field get_reachability_analysis_parameters_1

let get_unsafe = upgrade_from_marshal_field get_unsafe_1
let get_trace = upgrade_from_marshal_field get_trace_1

let get_dump_error_as_soon_as_they_occur =
  upgrade_from_marshal_field get_dump_error_as_soon_as_they_occur_1

let get_prefix = upgrade_from_marshal_field get_prefix_1
let get_call_stack = upgrade_from_marshal_field get_call_stack_1
let get_link_mode = upgrade_from_marshal_field get_link_mode_1

let get_contact_map_accuracy_level =
  upgrade_from_marshal_field get_contact_map_accuracy_level_1

let get_scc_accuracy_level = upgrade_from_marshal_field get_scc_accuracy_level_1

let get_influence_map_accuracy_level =
  upgrade_from_marshal_field get_influence_map_accuracy_level_1

let get_view_accuracy_level =
  upgrade_from_marshal_field get_view_accuracy_level_1

let get_empty_hashtbl_size = upgrade_from_marshal_field get_empty_hashtbl_size_1
let upgrade_from_influence_map_field f = compose f get_influence_map
let upgrade_from_contact_map_field f = compose f get_contact_map
let upgrade_from_symbols_field f = compose f get_symbols

(*add reachability*)
let upgrade_from_reachability_map_field f = compose f get_reachability_map

let upgrade_from_reachability_analysis_parameters_field f =
  compose f get_reachability_analysis_parameters

(*symbols*)
let get_bound_symbol = upgrade_from_symbols_field get_bound_symbol_1
let get_open_binding_state = upgrade_from_symbols_field get_open_binding_state_1

let get_close_binding_state =
  upgrade_from_symbols_field get_close_binding_state_1

let get_missing_binding_state =
  upgrade_from_symbols_field get_missing_binding_state_1

let get_internal_state_symbol =
  upgrade_from_symbols_field get_internal_state_symbol_1

let get_open_internal_state =
  upgrade_from_symbols_field get_open_internal_state_1

let get_close_internal_state =
  upgrade_from_symbols_field get_close_internal_state_1

let get_link_to_any = upgrade_from_symbols_field get_link_to_any_1
let get_link_to_some = upgrade_from_symbols_field get_link_to_some_1
let get_free_symbol = upgrade_from_symbols_field get_free_1
let get_at_symbol = upgrade_from_symbols_field get_at_symbol_1
let get_agent_open_symbol = upgrade_from_symbols_field get_agent_open_symbol_1
let get_agent_close_symbol = upgrade_from_symbols_field get_agent_close_symbol_1

let get_agent_sep_comma_symbol =
  upgrade_from_symbols_field get_agent_sep_comma_symbol_1

let get_agent_sep_plus_symbol =
  upgrade_from_symbols_field get_agent_sep_plus_symbol_1

let get_agent_sep_dot_symbol =
  upgrade_from_symbols_field get_agent_sep_dot_symbol_1

let get_btype_sep_symbol = upgrade_from_symbols_field get_btype_sep_symbol_1

let get_site_sep_comma_symbol =
  upgrade_from_symbols_field get_site_sep_comma_symbol_1

let get_ghost_agent_symbol = upgrade_from_symbols_field get_ghost_agent_symbol_1
let get_do_we_show_ghost = upgrade_from_symbols_field get_do_we_show_ghost_1
let get_uni_arrow_symbol = upgrade_from_symbols_field get_uni_arrow_symbol_1
let get_rev_arrow_symbol = upgrade_from_symbols_field get_rev_arrow_symbol_1
let get_bi_arrow_symbol = upgrade_from_symbols_field get_bi_arrow_symbol_1

let get_bi_arrow_no_poly_symbol =
  upgrade_from_symbols_field get_bi_arrow_no_poly_symbol_1

let get_rev_arrow_no_poly_symbol =
  upgrade_from_symbols_field get_rev_arrow_no_poly_symbol_1

let get_uni_arrow_no_poly_symbol =
  upgrade_from_symbols_field get_uni_arrow_no_poly_symbol_1

let get_open_int_interval_inclusive_symbol =
  upgrade_from_symbols_field get_open_int_interval_inclusive_symbol_1

let get_open_int_interval_exclusive_symbol =
  upgrade_from_symbols_field get_open_int_interval_exclusive_symbol_1

let get_open_int_interval_infinity_symbol =
  upgrade_from_symbols_field get_open_int_interval_infinity_symbol_1

let get_close_int_interval_inclusive_symbol =
  upgrade_from_symbols_field get_close_int_interval_inclusive_symbol_1

let get_close_int_interval_exclusive_symbol =
  upgrade_from_symbols_field get_close_int_interval_exclusive_symbol_1

let get_close_int_interval_infinity_symbol =
  upgrade_from_symbols_field get_close_int_interval_infinity_symbol_1

let get_plus_infinity_symbol =
  upgrade_from_symbols_field get_plus_infinity_symbol_1

let get_minus_infinity_symbol =
  upgrade_from_symbols_field get_minus_infinity_symbol_1

let get_int_interval_separator_symbol =
  upgrade_from_symbols_field get_int_interval_separator_symbol_1

let get_open_counter_state = upgrade_from_symbols_field get_open_counter_state_1
let get_open_counterceq = upgrade_from_symbols_field get_open_counterceq_1
let get_open_countercgte = upgrade_from_symbols_field get_open_countercgte_1
let get_open_countercvar = upgrade_from_symbols_field get_open_countercvar_1
let get_open_counterdelta = upgrade_from_symbols_field get_open_counterdelta_1
let get_open_counterval = upgrade_from_symbols_field get_open_counterval_1

let get_close_counter_state =
  upgrade_from_symbols_field get_close_counter_state_1

let get_close_counterceq = upgrade_from_symbols_field get_close_counterceq_1
let get_close_countercgte = upgrade_from_symbols_field get_close_countercgte_1
let get_close_countercvar = upgrade_from_symbols_field get_close_countercvar_1
let get_close_counterdelta = upgrade_from_symbols_field get_close_counterdelta_1
let get_close_counterval = upgrade_from_symbols_field get_close_counterval_1
let get_counterceq_symbol = upgrade_from_symbols_field get_counterceq_symbol_1
let get_countercgte_symbol = upgrade_from_symbols_field get_countercgte_symbol_1
let get_countercvar_symbol = upgrade_from_symbols_field get_countercvar_symbol_1

let get_counterdeltaplus_symbol =
  upgrade_from_symbols_field get_counterdeltaplus_symbol_1

let get_counterdeltaminus_symbol =
  upgrade_from_symbols_field get_counterdeltaminus_symbol_1

let get_counterval_symbol = upgrade_from_symbols_field get_counterval_symbol_1
let get_im_format = upgrade_from_influence_map_field get_im_format_1
let get_im_file = upgrade_from_influence_map_field get_im_file_1
let get_im_directory = upgrade_from_influence_map_field get_im_directory_1
let get_rule_shape = upgrade_from_influence_map_field get_rule_shape_1
let get_rule_color = upgrade_from_influence_map_field get_rule_color_1
let get_variable_shape = upgrade_from_influence_map_field get_variable_shape_1
let get_variable_color = upgrade_from_influence_map_field get_variable_color_1
let get_wake_up_color = upgrade_from_influence_map_field get_wake_up_color_1

let get_inhibition_color =
  upgrade_from_influence_map_field get_inhibition_color_1

let get_wake_up_arrow = upgrade_from_influence_map_field get_wake_up_arrow_1

let get_inhibition_arrow =
  upgrade_from_influence_map_field get_inhibition_arrow_1

let get_prompt_full_var_def =
  upgrade_from_influence_map_field get_prompt_full_var_def_1

let get_prompt_full_rule_def =
  upgrade_from_influence_map_field get_prompt_full_rule_def_1

let get_make_labels_compatible_with_dot =
  upgrade_from_influence_map_field get_make_labels_compatible_1

let get_pure_contact = upgrade_from_contact_map_field get_pure_contact_1
let get_cm_format = upgrade_from_contact_map_field get_cm_format_1
let get_cm_file = upgrade_from_contact_map_field get_cm_file_1
let get_cm_directory = upgrade_from_contact_map_field get_cm_directory_1

let get_binding_site_shape =
  upgrade_from_contact_map_field get_binding_site_shape_1

let get_binding_site_color =
  upgrade_from_contact_map_field get_binding_site_color_1

let get_internal_site_shape =
  upgrade_from_contact_map_field get_internal_site_shape_1

let get_internal_site_color =
  upgrade_from_contact_map_field get_internal_site_color_1

let get_counter_site_shape =
  upgrade_from_contact_map_field get_counter_site_shape_1

let get_counter_site_color =
  upgrade_from_contact_map_field get_counter_site_color_1

let get_agent_shape_array =
  upgrade_from_contact_map_field get_agent_shape_array_1

let get_agent_color_array =
  upgrade_from_contact_map_field get_agent_color_array_1

let get_agent_shape_def = upgrade_from_contact_map_field get_agent_shape_def_1
let get_agent_color_def = upgrade_from_contact_map_field get_agent_color_def_1
let get_link_color = upgrade_from_contact_map_field get_link_color_1
let get_influence_color = upgrade_from_contact_map_field get_influence_color_1
let get_influence_arrow = upgrade_from_contact_map_field get_influence_arrow_1

(*add reachablity*)

let get_dump_reachability_analysis_result =
  upgrade_from_reachability_map_field get_dump_reachability_analysis_result_1

let get_dump_reachability_analysis_iteration =
  upgrade_from_reachability_map_field get_dump_reachability_analysis_iteration_1

let get_dump_reachability_analysis_static =
  upgrade_from_reachability_map_field get_dump_reachability_analysis_static_1

let get_dump_reachability_analysis_dynamic =
  upgrade_from_reachability_map_field get_dump_reachability_analysis_dynamic_1

let get_dump_reachability_analysis_diff =
  upgrade_from_reachability_map_field get_dump_reachability_analysis_diff_1

let get_dump_reachability_analysis_wl =
  upgrade_from_reachability_map_field get_dump_reachability_analysis_wl_1

let get_post_processing =
  upgrade_from_reachability_map_field get_post_processing_1

let get_backend_mode = upgrade_from_reachability_map_field get_backend_mode_1

let get_hide_one_d_relations_from_cartesian_decomposition =
  upgrade_from_reachability_map_field
    get_hide_one_d_relations_from_cartesian_decomposition_1

let get_hide_reverse_rule_without_label_from_dead_rules =
  upgrade_from_reachability_map_field
    get_hide_reverse_rule_without_label_from_dead_rules_1

let get_smash_relations =
  upgrade_from_reachability_map_field get_smash_relations_1

let get_local_trace_format =
  upgrade_from_reachability_map_field get_local_trace_format_1

let get_compute_local_traces =
  upgrade_from_reachability_map_field get_compute_local_traces_1

let get_compute_separating_transitions =
  upgrade_from_reachability_map_field get_compute_separating_transitions_1

let set_compute_separating_transitions_2 r b =
  {
    r with
    Remanent_parameters_sig.reachability_map_output =
      set_compute_separating_transitions_1
        r.Remanent_parameters_sig.reachability_map_output b;
  }

let set_use_macrotransition_in_local_traces_2 r b =
  {
    r with
    Remanent_parameters_sig.reachability_map_output =
      set_use_macrotransition_in_local_traces_1
        r.Remanent_parameters_sig.reachability_map_output b;
  }

let set_compute_separating_transitions r b =
  {
    r with
    Remanent_parameters_sig.marshalisable_parameters =
      set_compute_separating_transitions_2
        r.Remanent_parameters_sig.marshalisable_parameters b;
  }

let set_use_macrotransitions_in_local_traces r b =
  {
    r with
    Remanent_parameters_sig.marshalisable_parameters =
      set_use_macrotransition_in_local_traces_2
        r.Remanent_parameters_sig.marshalisable_parameters b;
  }

let get_show_rule_names_in_local_traces =
  upgrade_from_reachability_map_field get_show_rule_names_in_local_traces_1

let get_use_macrotransitions_in_local_traces =
  upgrade_from_reachability_map_field get_use_macrotransitions_in_local_traces_1

let get_ignore_local_losanges =
  upgrade_from_reachability_map_field get_ignore_trivial_losanges_1

let get_add_singular_macrostates =
  upgrade_from_reachability_map_field get_add_singular_macrostates_1

let get_add_singular_microstates =
  upgrade_from_reachability_map_field get_add_singular_microstates_1

let get_local_trace_prefix =
  upgrade_from_reachability_map_field get_local_trace_prefix_1

let get_local_trace_directory =
  upgrade_from_reachability_map_field get_local_trace_directory_1

let get_view_analysis =
  upgrade_from_reachability_analysis_parameters_field get_view_analysis_1

let get_parallel_bonds_analysis =
  upgrade_from_reachability_analysis_parameters_field
    get_parallel_bonds_analysis_1

let get_site_across_bonds_analysis =
  upgrade_from_reachability_analysis_parameters_field
    get_site_across_bonds_analysis_1

let get_dynamic_contact_map =
  upgrade_from_reachability_analysis_parameters_field get_dynamic_contact_map_1

let get_counters_analysis =
  upgrade_from_reachability_analysis_parameters_field get_counters_analysis_1

let get_counters_domain =
  upgrade_from_reachability_analysis_parameters_field get_counters_domain_1

let get_do_reachability_analysis p =
  upgrade_from_marshal_field get_do_reachability_analysis_1 p
  || get_compute_local_traces p

let set_prefix_1 marshalisable prefix =
  { marshalisable with Remanent_parameters_sig.prefix }

let set_call_stack_1 marshalisable call_stack =
  { marshalisable with Remanent_parameters_sig.call_stack }

let set_trace_1 marshalisable bool =
  { marshalisable with Remanent_parameters_sig.trace = bool }

let upgrade_to_marshalisable f parameters prefix =
  {
    parameters with
    Remanent_parameters_sig.marshalisable_parameters =
      f (get_marshalisable parameters) prefix;
  }

let set_prefix = upgrade_to_marshalisable set_prefix_1
let set_call_stack = upgrade_to_marshalisable set_call_stack_1
let set_trace = upgrade_to_marshalisable set_trace_1

let update_prefix parameters suffix =
  set_prefix parameters (get_prefix parameters ^ suffix)

let update_call_stack parameters bool name =
  let rep_bool = get_trace parameters || bool in
  match name, get_trace parameters = bool with
  | None, true -> parameters
  | None, false -> set_trace parameters rep_bool
  | Some x, true -> set_call_stack parameters (x :: get_call_stack parameters)
  | Some x, false ->
    set_call_stack
      (set_trace parameters rep_bool)
      (x :: get_call_stack parameters)

let open_influence_map_file parameters =
  let channel =
    match
      ( get_im_file parameters,
        get_im_directory parameters,
        ext_format (get_im_format parameters) )
    with
    | None, _, _ -> stdout
    | Some a, None, ext -> open_out a ext
    | Some a, Some d, ext -> open_out (d ^ a) ext
  in
  let format =
    match get_im_format parameters with
    | Remanent_parameters_sig.DOT -> Loggers.DOT
    | Remanent_parameters_sig.HTML -> Loggers.HTML_Graph
    | Remanent_parameters_sig.DIM -> Loggers.Matrix
    | Remanent_parameters_sig.GEPHI -> Loggers.GEPHI
  in
  let logger = Loggers.open_logger_from_channel ~mode:format channel in
  { parameters with Remanent_parameters_sig.logger }

let open_contact_map_file parameters =
  let channel =
    match
      ( get_cm_file parameters,
        get_cm_directory parameters,
        ext_format (get_cm_format parameters) )
    with
    | None, _, _ -> stdout
    | Some a, None, ext -> open_out a ext
    | Some a, Some d, ext -> open_out (d ^ a) ext
  in
  {
    parameters with
    Remanent_parameters_sig.logger = Loggers.open_logger_from_channel channel;
  }

let persistent_mode = false

let lexical_analysis_of_tested_only_patterns_is_required_by_the_influence_map
    parameter =
  (match get_influence_map_accuracy_level parameter with
  | Remanent_parameters_sig.Full | Remanent_parameters_sig.Medium
  | Remanent_parameters_sig.High | Remanent_parameters_sig.Low ->
    true
  | Remanent_parameters_sig.None -> false)
  && get_do_influence_map parameter

let lexical_analysis_of_tested_only_patterns_is_required_by_the_persistent_mode
    _ =
  persistent_mode

let lexical_analysis_of_tested_only_patterns_is_required_by_the_contact_map
    parameter =
  get_do_contact_map parameter
  &&
  match get_contact_map_accuracy_level parameter with
  | Remanent_parameters_sig.Medium | Remanent_parameters_sig.Low -> true
  | Remanent_parameters_sig.None | Remanent_parameters_sig.High
  | Remanent_parameters_sig.Full ->
    false

let lexical_analysis_of_tested_only_patterns parameter =
  lexical_analysis_of_tested_only_patterns_is_required_by_the_persistent_mode
    parameter
  || lexical_analysis_of_tested_only_patterns_is_required_by_the_contact_map
       parameter
  || lexical_analysis_of_tested_only_patterns_is_required_by_the_influence_map
       parameter

let get_called_from parameter =
  parameter.Remanent_parameters_sig.marshalisable_parameters
    .Remanent_parameters_sig.called_from

let get_backdoor_nbr_of_scc_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_nbr_of_scc

let get_backdoor_average_size_of_scc_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_average_size_of_scc

let get_backdoor_nbr_of_influences_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_nbr_of_influences

let get_backdoor_nbr_of_constraints_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_nbr_of_constraints

let get_backdoor_nbr_of_nr_constraints_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_nbr_of_nr_constraints

let get_backdoor_nbr_of_dead_rules_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_nbr_of_dead_rules

let get_backdoor_nbr_of_rules_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_nbr_of_rules

let get_backdoor_nbr_of_rules_with_non_weakly_reversible_transitions_1 backdoors
    =
  backdoors
    .Remanent_parameters_sig
     .backdoor_nbr_of_rules_with_non_weakly_reversible_transitions

let get_backdoor_nbr_of_non_weakly_reversible_transitions_1 backdoors =
  backdoors
    .Remanent_parameters_sig.backdoor_nbr_of_non_weakly_reversible_transitions

let get_backdoor_timing_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_timing

let get_backdoor_file_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_file

let get_backdoor_directory_1 backdoors =
  backdoors.Remanent_parameters_sig.backdoor_directory

let get_backdoors marshalisable =
  marshalisable.Remanent_parameters_sig.backdoors

let get_backdoor_nbr_of_scc_2 = compose get_backdoor_nbr_of_scc_1 get_backdoors

let get_backdoor_average_size_of_scc_2 =
  compose get_backdoor_average_size_of_scc_1 get_backdoors

let get_backdoor_nbr_of_constraints_2 =
  compose get_backdoor_nbr_of_constraints_1 get_backdoors

let get_backdoor_nbr_of_nr_constraints_2 =
  compose get_backdoor_nbr_of_nr_constraints_1 get_backdoors

let get_backdoor_nbr_of_influences_2 =
  compose get_backdoor_nbr_of_influences_1 get_backdoors

let get_backdoor_nbr_of_dead_rules_2 =
  compose get_backdoor_nbr_of_dead_rules_1 get_backdoors

let get_backdoor_nbr_of_rules_2 =
  compose get_backdoor_nbr_of_rules_1 get_backdoors

let get_backdoor_nbr_of_non_weakly_reversible_transitions_2 =
  compose get_backdoor_nbr_of_non_weakly_reversible_transitions_1 get_backdoors

let get_backdoor_nbr_of_rules_with_non_weakly_reversible_transitions_2 =
  compose get_backdoor_nbr_of_rules_with_non_weakly_reversible_transitions_1
    get_backdoors

let get_backdoor_timing_2 = compose get_backdoor_timing_1 get_backdoors
let get_backdoor_file_2 = compose get_backdoor_file_1 get_backdoors
let get_backdoor_directory_2 = compose get_backdoor_directory_1 get_backdoors

let get_backdoor_nbr_of_constraints =
  compose get_backdoor_nbr_of_constraints_2 get_marshalisable

let get_backdoor_nbr_of_nr_constraints =
  compose get_backdoor_nbr_of_nr_constraints_2 get_marshalisable

let get_backdoor_nbr_of_scc =
  compose get_backdoor_nbr_of_scc_2 get_marshalisable

let get_backdoor_average_size_of_scc =
  compose get_backdoor_average_size_of_scc_2 get_marshalisable

let get_backdoor_nbr_of_influences =
  compose get_backdoor_nbr_of_influences_2 get_marshalisable

let get_backdoor_nbr_of_rules =
  compose get_backdoor_nbr_of_rules_2 get_marshalisable

let get_backdoor_nbr_of_dead_rules =
  compose get_backdoor_nbr_of_dead_rules_2 get_marshalisable

let get_backdoor_nbr_of_rules_with_non_weakly_reversible_transitions =
  compose get_backdoor_nbr_of_rules_with_non_weakly_reversible_transitions_2
    get_marshalisable

let get_backdoor_nbr_of_non_weakly_reversible_transitions =
  compose get_backdoor_nbr_of_non_weakly_reversible_transitions_2
    get_marshalisable

let get_backdoor_timing = compose get_backdoor_timing_2 get_marshalisable
let get_backdoor_file = compose get_backdoor_file_2 get_marshalisable
let get_backdoor_directory = compose get_backdoor_directory_2 get_marshalisable
let get_profiler parameter = parameter.Remanent_parameters_sig.profiler

let get_compression_status_logger parameter =
  parameter.Remanent_parameters_sig.compression_status

let get_kasa_state = compose get_kasa_state_1 get_marshalisable

let set_print_efficiency parameter bool =
  { parameter with Remanent_parameters_sig.print_efficiency = bool }

let get_print_efficiency parameter =
  parameter.Remanent_parameters_sig.print_efficiency

let set_logger parameter logger =
  { parameter with Remanent_parameters_sig.logger }

let save_error_list parameter error =
  parameter.Remanent_parameters_sig.save_error_list error

let save_progress_bar parameter bar =
  parameter.Remanent_parameters_sig.save_progress_bar bar

let reset_progress_bar parameter =
  parameter.Remanent_parameters_sig.reset_progress_bar

let save_current_phase_title parameter =
  parameter.Remanent_parameters_sig.save_current_phase_title

let reset_current_phase_title parameter =
  parameter.Remanent_parameters_sig.reset_current_phase_title
