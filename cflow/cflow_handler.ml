(**
  * cflow_handler.ml
  *
  * Creation:                      <2013-08-02 feret>
  * Last modification: Time-stamp: <2015-12-02 11:21:41 feret>
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


module type Cflow_handler =
  sig
    (**a struct which contains parameterizable options*)
    type parameter  = (*try to hide type definition *)
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
	  log_step_channel : Format.formatter ;
	  kasa : Remanent_parameters_sig.parameters ;
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


module Cflow_handler =
  (struct
    type parameter =
        {
          cache_size : int option ;
          current_compression_mode: Parameter.current_compression_mode option;
          compression_mode : Parameter.compression_mode ;
          priorities_weak: Priority.priorities ;
          priorities_strong : Priority.priorities ;
          priorities_causal: Priority.priorities ;
	  compute_all_stories : bool ; 
	  sort_algo_for_stories: Parameter.sort_algo_for_stories;	  
	  out_channel_err : Format.formatter;
          out_channel_profiling: Format.formatter;
          out_channel : Format.formatter;
	  log_step: bool ;
	  debug_mode: bool ;
	  log_step_channel : Format.formatter ;
	  kasa : Remanent_parameters_sig.parameters ;
	  always_disambiguate_initial_states : bool  ;
	  bound_on_itteration_number: int option ;
	}

    let build_parameter () =
      let channel = Kappa_files.open_profiling () in
      {
        current_compression_mode = None ;
        priorities_weak = Priority.weak ;
        priorities_strong = Priority.strong2 ;
        priorities_causal = Priority.causal ;
	compute_all_stories = false ; 
	sort_algo_for_stories = Parameter.Bucket;
	out_channel = Format.err_formatter ;
        out_channel_err = Format.err_formatter ;
        out_channel_profiling = Format.formatter_of_out_channel channel ;
        compression_mode = Parameter.get_compression_mode () ;
        cache_size = Parameter.get_cache_size () ;
	debug_mode = false ;
	log_step = true ;
	log_step_channel = Format.std_formatter ; 
	kasa = Remanent_parameters.get_parameters () ;
	always_disambiguate_initial_states = true ;
	bound_on_itteration_number = None ;
      }

    let set_compression_weak p =
      {p with current_compression_mode = Some Parameter.Weak}
    let set_compression_strong p =
      {p with current_compression_mode = Some Parameter.Strong}
    let set_compression_none p =
      {p with current_compression_mode = Some Parameter.Causal}


   
    type handler =
        {
          env: Environment.t ;
        }

    type 'a with_handler = parameter -> handler -> Exception.method_handler -> 'a

   
    let string_of_exn x = Some ""

    let get_priorities parameter =
      match parameter.current_compression_mode with
      | None -> None
      | Some Parameter.Weak -> Some parameter.priorities_weak
      | Some Parameter.Strong -> Some parameter.priorities_strong
      | Some Parameter.Causal -> Some parameter.priorities_causal

   let set_first_story_per_obs parameter =
     {
       parameter
     with compute_all_stories = false }
      
   let set_all_stories_per_obs parameter =
      {
	parameter
      with compute_all_stories = true }

   let get_all_stories_per_obs parameter = parameter.compute_all_stories 

   let get_debugging_mode parameter = parameter.debug_mode

   let set_debugging_mode parameter bool= {parameter with debug_mode = bool }

   let get_log_step parameter = parameter.log_step
   let set_log_step parameter bool = {parameter with log_step = bool}

   let get_logger parameter = parameter.log_step_channel
   let set_logger parameter fmt = {parameter with log_step_channel = fmt}
   let get_out_channel parameter = parameter.out_channel
   let set_out_channel parameter fmt = {parameter with out_channel = fmt}				  
   let get_debugging_channel parameter = parameter.out_channel_err
   let set_debugging_channel parameter fmt = {parameter with out_channel_err = fmt }

   let get_kasa_parameters parameter = parameter.kasa 
   let set_kasa_parameters parameter parameter' = {parameter' with kasa = parameter}

   let do_we_use_bucket_sort parameter = parameter.sort_algo_for_stories == Parameter.Bucket
   let use_bucket_sort parameter = {parameter with sort_algo_for_stories = Parameter.Bucket}
   let use_fusion_sort parameter = {parameter with sort_algo_for_stories = Parameter.Fusion}

   let always_disambiguate parameter = parameter.always_disambiguate_initial_states
   let set_always_disambiguate parameter  bool = { parameter with always_disambiguate_initial_states = bool}
						   
   let do_not_bound_itterations parameter = {parameter with bound_on_itteration_number = None}
   let set_itteration_bound parameter int = {parameter with bound_on_itteration_number = Some int}				      
   let get_bound_on_itteration_number parameter = parameter.bound_on_itteration_number
						    
    end:Cflow_handler)
    
