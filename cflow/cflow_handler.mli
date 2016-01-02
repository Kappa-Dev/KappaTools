module type Cflow_handler =
  sig
    (** a struct which contains parameterizable options *)
    type parameter  =
        {
          cache_size : int option ;
          current_compression_mode: Parameter.current_compression_mode option;
          compression_mode : Parameter.compression_mode ;
          priorities_weak: Priority.priorities ;
          priorities_strong : Priority.priorities ;
          priorities_causal : Priority.priorities ;
	  compute_all_stories : bool ; 
	  sort_algo_for_stories: Parameter.sort_algo_for_stories;	  
	  out_channel_err : Format.formatter ;
          out_channel_profiling : Format.formatter ;
          out_channel : Format.formatter ;
	  log_step : bool ;
	  debug_mode : bool ;
	  log_step_channel: Format.formatter ; 
	  kasa: Remanent_parameters_sig.parameters ;
	  always_disambiguate_initial_states : bool  ;
	  bound_on_itteration_number: int option ;
	  time_independent: bool ;
	} 

    type handler =   (*handler to interpret abstract values*)
        {
          env: Environment.t ;
	  rule_name_cache: string array;
	  agent_name_cache: string array;
	    steps_by_column:  (int * Predicate_maps.predicate_value * bool) list Predicate_maps.QPredicateMap.t ;
	}

    type 'a zeroary = parameter -> handler -> StoryProfiling.StoryStats.log_info -> Exception.method_handler -> Exception.method_handler * StoryProfiling.StoryStats.log_info * 'a
    type ('a,'b) unary  = parameter -> handler -> StoryProfiling.StoryStats.log_info -> Exception.method_handler -> 'a -> Exception.method_handler * StoryProfiling.StoryStats.log_info * 'b
    type ('a,'b,'c) binary  = parameter -> handler -> StoryProfiling.StoryStats.log_info -> Exception.method_handler -> 'a -> 'b -> Exception.method_handler * StoryProfiling.StoryStats.log_info * 'c
    type ('a,'b,'c,'d) ternary  = parameter -> handler -> StoryProfiling.StoryStats.log_info -> Exception.method_handler -> 'a -> 'b -> 'c ->  Exception.method_handler * StoryProfiling.StoryStats.log_info * 'd
    type ('a,'b,'c,'d,'e) quaternary  = parameter -> handler -> StoryProfiling.StoryStats.log_info -> Exception.method_handler -> 'a -> 'b -> 'c -> 'd -> Exception.method_handler * StoryProfiling.StoryStats.log_info * 'e	  
    val do_not_bound_itterations: parameter -> parameter 									 
    val set_itteration_bound: parameter -> int -> parameter 
    val get_bound_on_itteration_number: parameter -> int option 
    val set_first_story_per_obs: parameter -> parameter  
    val set_all_stories_per_obs: parameter -> parameter 
    val build_parameter: unit -> parameter
    val string_of_exn: exn -> string option
    val set_compression_weak: parameter -> parameter
    val set_compression_strong: parameter -> parameter
    val set_compression_none: parameter -> parameter
    val get_priorities: parameter -> Priority.priorities option
    val get_all_stories_per_obs: parameter -> bool
    val set_log_step: parameter -> bool -> parameter
    val get_log_step: parameter -> bool
    val set_debugging_mode: parameter -> bool -> parameter
    val get_debugging_mode: parameter -> bool
    val get_profiling_logger: parameter -> Format.formatter 
    val get_logger: parameter -> Format.formatter
    val set_logger: parameter -> Format.formatter -> parameter
    val get_out_channel: parameter -> Format.formatter
    val set_out_channel: parameter -> Format.formatter -> parameter
    val get_debugging_channel: parameter -> Format.formatter
    val set_debugging_channel: parameter -> Format.formatter -> parameter
    val get_kasa_parameters: parameter -> Remanent_parameters_sig.parameters 
    val set_kasa_parameters: Remanent_parameters_sig.parameters -> parameter -> parameter 
    val do_we_use_bucket_sort: parameter -> bool
    val use_bucket_sort: parameter -> parameter
    val use_fusion_sort: parameter -> parameter
    val always_disambiguate: parameter -> bool 
    val set_always_disambiguate: parameter -> bool -> parameter 
    val init_handler: Environment.t -> handler
    val string_of_rule_id: handler -> int -> string
    val string_of_agent_id: handler -> int -> string
    val get_predicate_map: handler -> (int * Predicate_maps.predicate_value * bool) list Predicate_maps.QPredicateMap.t
    val get_is_time_independent: parameter -> bool
  end

module Cflow_handler:Cflow_handler
