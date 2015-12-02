module type Cflow_handler =
  sig
    type parameter  =
        {
          cache_size : int option ;
          current_compression_mode: Parameter.current_compression_mode option;
          compression_mode : Parameter.compression_mode ;
          priorities_weak: Priority.priorities ;
          priorities_strong : Priority.priorities ;
          priorities_causal : Priority.priorities ;
	  compute_all_stories : bool ; 
	  out_channel_err : Format.formatter ;
          out_channel_profiling : Format.formatter ;
          out_channel : Format.formatter ;
	  log_step : bool ;
	  debug_mode : bool ;
	  log_step_channel: Format.formatter ; 
	  kasa: Remanent_parameters_sig.parameters
        } (*a struct which contains parameterizable options*)
    type error
    type error_channel = error list (*a list which contains the errors so far*)
    type handler =   (*handler to interpret abstract values*)
        {
          env: Environment.t ;
        }
    type 'a with_handler = parameter -> handler -> error_channel -> 'a

    val set_first_story_per_obs: parameter -> parameter  
    val set_all_stories_per_obs: parameter -> parameter 
    val build_parameter: unit -> parameter
    val string_of_exn: exn -> string option
    val error_init: error_channel
    val create_error:
      (string option -> string option -> string option -> string option -> string option -> exn -> error_channel * error) with_handler
    val add_error: (error -> error_channel) with_handler
    val raise_error:  (error -> 'a -> error_channel * 'a) with_handler
    val dump_error: (error -> unit) with_handler
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
end

module Cflow_handler:Cflow_handler
