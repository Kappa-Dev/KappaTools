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
  (*  val dump_json: parameter -> Yojson.Basic.t -> unit*)

  val push_json :
    parameter -> StoryProfiling.StoryStats.log_info Story_json.message -> unit

  val pop_json :
    parameter -> StoryProfiling.StoryStats.log_info Story_json.message option
end

module Cflow_handler : Cflow_handler
