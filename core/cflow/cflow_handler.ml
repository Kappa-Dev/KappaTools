(**
  * cflow_handler.ml
  *
  * Creation:                      <2013-08-02 feret>
  * Last modification: Time-stamp: <2016-02-19 14:30:40 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Diderot, CNRS
  *
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Cflow_handler = sig
  type sort_algo_for_stories
  type compression_mode

  val get_causal_trace : compression_mode -> bool
  val get_causal_trace_only : compression_mode -> bool
  val get_weak_compression : compression_mode -> bool
  val get_strong_compression : compression_mode -> bool

  type parameter = {
    cache_size: int option;
    current_compression_mode: Story_json.current_compression_mode option;
    compression_mode: compression_mode;
    priorities_weak: Priority.priorities;
    priorities_strong: Priority.priorities;
    priorities_causal: Priority.priorities;
    compute_all_stories: bool;
    sort_algo_for_stories: sort_algo_for_stories;
    logger_err: Loggers.t;
    logger_profiling: Loggers.t;
    logger_out: Loggers.t;
    logger_server: Loggers.t;
    json_buffer:
      StoryProfiling.StoryStats.log_info Story_json.message Fifo.t ref option;
    log_step: bool;
    debug_mode: bool;
    logger_step: Loggers.t;
    kasa: Remanent_parameters_sig.parameters;
    always_disambiguate_initial_states: bool;
    bound_on_itteration_number: int option;
    time_independent: bool;
    blacklist_events: bool;
    server: bool;
    is_server_channel_on: bool;
    dump: string -> unit;
  }
  (** a struct which contains parameterizable options *)

  val get_current_compression_mode :
    parameter -> Story_json.current_compression_mode option

  type handler = {
    (*handler to interpret abstract values*)
    env: Model.t;
    rule_name_cache: string array;
    agent_name_cache: string array;
    steps_by_column:
      (int * Predicate_maps.predicate_value * bool) list
      Predicate_maps.QPredicateMap.t;
  }

  type 'a zeroary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'a

  type ('a, 'b) unary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'b

  type ('a, 'b, 'c) binary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'c

  type ('a, 'b, 'c, 'd) ternary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'd

  type ('a, 'b, 'c, 'd, 'e) quaternary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    'd ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'e

  val do_not_bound_itterations : parameter -> parameter
  val set_itteration_bound : parameter -> int -> parameter
  val get_bound_on_itteration_number : parameter -> int option
  val set_first_story_per_obs : parameter -> parameter
  val set_all_stories_per_obs : parameter -> parameter

  val build_parameter :
    called_from:Remanent_parameters_sig.called_from ->
    ?send_message:(string -> unit) ->
    none:bool ->
    weak:bool ->
    strong:bool ->
    unit ->
    parameter

  val string_of_exn : exn -> string option
  val is_server_mode : parameter -> bool
  val set_compression_weak : parameter -> parameter
  val set_compression_strong : parameter -> parameter
  val set_compression_none : parameter -> parameter
  val get_priorities : parameter -> Priority.priorities option
  val get_all_stories_per_obs : parameter -> bool
  val set_log_step : parameter -> bool -> parameter
  val get_log_step : parameter -> bool
  val set_debugging_mode : parameter -> bool -> parameter
  val get_debugging_mode : parameter -> bool
  val get_profiling_logger : parameter -> Loggers.t
  val get_server_channel : parameter -> Loggers.t
  val shut_down_server_channel : parameter -> parameter
  val is_server_channel_on : parameter -> bool
  val get_logger : parameter -> Loggers.t
  val set_logger : parameter -> Loggers.t -> parameter
  val get_out_channel : parameter -> Loggers.t
  val set_out_channel : parameter -> Loggers.t -> parameter
  val get_debugging_channel : parameter -> Loggers.t
  val set_debugging_channel : parameter -> Loggers.t -> parameter
  val get_kasa_parameters : parameter -> Remanent_parameters_sig.parameters

  val set_kasa_parameters :
    Remanent_parameters_sig.parameters -> parameter -> parameter

  val do_we_use_bucket_sort : parameter -> bool
  val use_bucket_sort : parameter -> parameter
  val use_fusion_sort : parameter -> parameter
  val always_disambiguate : parameter -> bool
  val set_always_disambiguate : parameter -> bool -> parameter
  val init_handler : Model.t -> handler
  val string_of_rule_id : handler -> int -> string
  val string_of_agent_id : handler -> int -> string

  val get_predicate_map :
    handler ->
    (int * Predicate_maps.predicate_value * bool) list
    Predicate_maps.QPredicateMap.t

  val get_is_time_independent : parameter -> bool
  val get_blacklist_events : parameter -> bool
  val save_current_phase_title : parameter -> string -> unit
  val reset_current_phase_title : parameter -> unit
  val save_progress_bar : parameter -> bool * int * int * int -> unit
  val reset_progress_bar : parameter -> unit
  val set_save_current_phase_title : parameter -> (string -> unit) -> parameter
  val set_reset_current_phase_title : parameter -> (unit -> unit) -> parameter

  val set_save_progress_bar :
    parameter -> (bool * int * int * int -> unit) -> parameter

  val set_reset_progress_bar : parameter -> (unit -> unit) -> parameter

  val save_error_log :
    parameter -> Exception_without_parameter.method_handler -> unit

  val set_save_error_log :
    parameter ->
    (Exception_without_parameter.method_handler -> unit) ->
    parameter

  val push_json :
    parameter -> StoryProfiling.StoryStats.log_info Story_json.message -> unit

  val pop_json :
    parameter -> StoryProfiling.StoryStats.log_info Story_json.message option
end

module Cflow_handler : Cflow_handler = struct
  type sort_algo_for_stories = Bucket | Fusion

  type compression_mode = {
    causal_trace: bool;
    weak_compression: bool;
    strong_compression: bool;
  }

  let get_causal_trace x = x.causal_trace
  let get_causal_trace_only x = not (x.weak_compression || x.strong_compression)
  let get_weak_compression x = x.weak_compression
  let get_strong_compression x = x.strong_compression

  type parameter = {
    cache_size: int option;
    current_compression_mode: Story_json.current_compression_mode option;
    compression_mode: compression_mode;
    priorities_weak: Priority.priorities;
    priorities_strong: Priority.priorities;
    priorities_causal: Priority.priorities;
    compute_all_stories: bool;
    sort_algo_for_stories: sort_algo_for_stories;
    logger_err: Loggers.t;
    logger_profiling: Loggers.t;
    logger_out: Loggers.t;
    logger_server: Loggers.t;
    json_buffer:
      StoryProfiling.StoryStats.log_info Story_json.message Fifo.t ref option;
    log_step: bool;
    debug_mode: bool;
    logger_step: Loggers.t;
    kasa: Remanent_parameters_sig.parameters;
    always_disambiguate_initial_states: bool;
    bound_on_itteration_number: int option;
    time_independent: bool;
    blacklist_events: bool;
    server: bool;
    is_server_channel_on: bool;
    dump: string -> unit;
  }

  let get_current_compression_mode parameter =
    parameter.current_compression_mode

  let build_parameter ~called_from
      ?(send_message =
        fun x ->
          Loggers.fprintf Loggers.dummy_txt_logger "%s" x;
          Loggers.print_newline Loggers.dummy_txt_logger) ~none ~weak ~strong ()
      =
    let ( server,
          out_server,
          out_channel,
          out_channel_err,
          out_channel_profiling,
          log_step_channel ) =
      match called_from with
      | Remanent_parameters_sig.Server ->
        ( true,
          Loggers.open_logger_from_channel ~mode:Loggers.Json stdout,
          Loggers.open_infinite_buffer ~mode:Loggers.HTML (),
          Loggers.open_infinite_buffer ~mode:Loggers.HTML (),
          Loggers.open_circular_buffer ~mode:Loggers.HTML (),
          Loggers.open_circular_buffer ~mode:Loggers.HTML_Tabular () )
      | Remanent_parameters_sig.KaSa | Remanent_parameters_sig.KaSim
      | Remanent_parameters_sig.Internalised ->
        let channel = Kappa_files.open_branch_and_cut_engine_profiling () in
        ( false,
          Loggers.dummy_txt_logger,
          Loggers.open_logger_from_formatter Format.err_formatter,
          Loggers.open_logger_from_formatter Format.err_formatter,
          Loggers.open_logger_from_formatter
            (Format.formatter_of_out_channel channel),
          Loggers.open_logger_from_formatter Format.std_formatter )
    in
    {
      server;
      is_server_channel_on = server;
      current_compression_mode = None;
      priorities_weak = Priority.weak;
      priorities_strong = Priority.strong;
      priorities_causal = Priority.causal;
      compute_all_stories = false;
      sort_algo_for_stories = Bucket;
      logger_server = out_server;
      logger_out = out_channel;
      logger_err = out_channel_err;
      logger_profiling = out_channel_profiling;
      json_buffer = None;
      compression_mode =
        {
          causal_trace = none;
          weak_compression = weak;
          strong_compression = strong;
        };
      cache_size = Parameter.get_cache_size ();
      debug_mode = false;
      log_step = true;
      logger_step = log_step_channel;
      kasa = Remanent_parameters.get_parameters ~called_from ();
      always_disambiguate_initial_states = true;
      bound_on_itteration_number = None;
      time_independent = !Parameter.time_independent;
      blacklist_events = !Parameter.blacklist_events;
      dump = send_message;
    }

  let set_compression_weak p =
    { p with current_compression_mode = Some Story_json.Weak }

  let set_compression_strong p =
    { p with current_compression_mode = Some Story_json.Strong }

  let set_compression_none p =
    { p with current_compression_mode = Some Story_json.Causal }

  type handler = {
    env: Model.t;
    rule_name_cache: string array;
    agent_name_cache: string array;
    steps_by_column:
      (int * Predicate_maps.predicate_value * bool) list
      Predicate_maps.QPredicateMap.t;
  }

  type 'a zeroary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'a

  type ('a, 'b) unary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'b

  type ('a, 'b, 'c) binary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'c

  type ('a, 'b, 'c, 'd) ternary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'd

  type ('a, 'b, 'c, 'd, 'e) quaternary =
    parameter ->
    handler ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    'd ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'e

  let init_handler env =
    let n_rules = Model.nb_rules env in
    let rule_name_cache = Array.make n_rules "" in
    let () =
      Model.fold_rules
        (fun x () r ->
          rule_name_cache.(x) <-
            Format.asprintf "%a"
              (Model.print_ast_rule ~noCounters:false ~env)
              r.Primitives.syntactic_rule)
        () env
    in
    let n_agents = Signature.size (Model.signatures env) in
    let agent_name_cache =
      Array.init n_agents (fun x ->
          Format.asprintf "%a" (Model.print_agent ~env) x)
    in
    let steps_by_column = Predicate_maps.QPredicateMap.empty 0 in
    { env; rule_name_cache; agent_name_cache; steps_by_column }

  let string_of_exn _x = Some ""

  let get_priorities parameter =
    match parameter.current_compression_mode with
    | None -> None
    | Some Story_json.Weak -> Some parameter.priorities_weak
    | Some Story_json.Strong -> Some parameter.priorities_strong
    | Some Story_json.Causal -> Some parameter.priorities_causal

  let set_first_story_per_obs parameter =
    { parameter with compute_all_stories = false }

  let set_all_stories_per_obs parameter =
    { parameter with compute_all_stories = true }

  let get_all_stories_per_obs parameter = parameter.compute_all_stories
  let get_debugging_mode parameter = parameter.debug_mode
  let set_debugging_mode parameter bool = { parameter with debug_mode = bool }
  let get_log_step parameter = parameter.log_step
  let set_log_step parameter bool = { parameter with log_step = bool }
  let get_logger parameter = parameter.logger_step
  let set_logger parameter fmt = { parameter with logger_step = fmt }
  let get_out_channel parameter = parameter.logger_out
  let set_out_channel parameter fmt = { parameter with logger_out = fmt }
  let get_debugging_channel parameter = parameter.logger_err
  let set_debugging_channel parameter fmt = { parameter with logger_err = fmt }
  let get_kasa_parameters parameter = parameter.kasa

  let set_kasa_parameters parameter parameter' =
    { parameter' with kasa = parameter }

  let do_we_use_bucket_sort parameter = parameter.sort_algo_for_stories = Bucket

  let use_bucket_sort parameter =
    { parameter with sort_algo_for_stories = Bucket }

  let use_fusion_sort parameter =
    { parameter with sort_algo_for_stories = Fusion }

  let always_disambiguate parameter =
    parameter.always_disambiguate_initial_states

  let set_always_disambiguate parameter bool =
    { parameter with always_disambiguate_initial_states = bool }

  let do_not_bound_itterations parameter =
    { parameter with bound_on_itteration_number = None }

  let set_itteration_bound parameter int =
    { parameter with bound_on_itteration_number = Some int }

  let get_bound_on_itteration_number parameter =
    parameter.bound_on_itteration_number

  let get_profiling_logger parameter = parameter.logger_profiling
  let string_of_rule_id handler i = handler.rule_name_cache.(i)
  let string_of_agent_id handler i = handler.agent_name_cache.(i)
  let get_predicate_map handler = handler.steps_by_column
  let get_is_time_independent parameter = parameter.time_independent
  let get_blacklist_events parameter = parameter.blacklist_events
  let is_server_mode parameter = parameter.server
  let get_server_channel parameter = parameter.logger_server

  let shut_down_server_channel parameter =
    {
      parameter with
      logger_server = Loggers.dummy_txt_logger;
      is_server_channel_on = false;
    }

  let is_server_channel_on parameter = parameter.is_server_channel_on

  let save_current_phase_title parameter x =
    parameter.kasa.Remanent_parameters_sig.save_current_phase_title x

  let dump_json parameter message =
    if is_server_mode parameter then
      parameter.dump
        (Yojson.Basic.to_string (Story_json.message_to_json message))

  let save_progress_bar parameter x =
    let b, i, _j, n_stories = x in
    let () =
      dump_json parameter
        (Story_json.Progress
           {
             Story_json.bool =
               (if b then
                  "true"
                else
                  "false");
             Story_json.current = i;
             Story_json.total = n_stories;
           })
    in
    parameter.kasa.Remanent_parameters_sig.save_progress_bar x

  let reset_progress_bar parameter =
    parameter.kasa.Remanent_parameters_sig.reset_progress_bar ()

  let reset_current_phase_title parameter =
    parameter.kasa.Remanent_parameters_sig.reset_current_phase_title ()

  let set_save_current_phase_title parameter f =
    {
      parameter with
      kasa =
        {
          parameter.kasa with
          Remanent_parameters_sig.save_current_phase_title = f;
        };
    }

  let set_reset_current_phase_title parameter f =
    {
      parameter with
      kasa =
        {
          parameter.kasa with
          Remanent_parameters_sig.reset_current_phase_title = f;
        };
    }

  let set_save_progress_bar parameter f =
    {
      parameter with
      kasa =
        { parameter.kasa with Remanent_parameters_sig.save_progress_bar = f };
    }

  let set_reset_progress_bar parameter f =
    {
      parameter with
      kasa =
        { parameter.kasa with Remanent_parameters_sig.reset_progress_bar = f };
    }

  let save_error_log parameter x =
    parameter.kasa.Remanent_parameters_sig.save_error_list x

  let set_save_error_log parameter f =
    {
      parameter with
      kasa = { parameter.kasa with Remanent_parameters_sig.save_error_list = f };
    }

  let push_json parameter json =
    match parameter.json_buffer with
    | None -> dump_json parameter json
    | Some ref -> ref := Fifo.push json !ref

  let pop_json parameter =
    match parameter.json_buffer with
    | None -> None
    | Some ref ->
      let fifo, elt_opt = Fifo.pop !ref in
      let () = ref := fifo in
      elt_opt
end
