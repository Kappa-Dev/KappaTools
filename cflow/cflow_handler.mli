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
        } 

    type handler =   (*handler to interpret abstract values*)
        {
          env: Environment.t ;
        }

    type 'a with_handler = parameter -> handler -> Exception.method_handler -> 'a

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
 
  end

module Cflow_handler:Cflow_handler
