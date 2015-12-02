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
    type parameter  = (*try to hide type definition *)
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
	  log_step_channel : Format.formatter ;
	  kasa : Remanent_parameters_sig.parameters ;
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
	  out_channel_err : Format.formatter;
          out_channel_profiling: Format.formatter;
          out_channel : Format.formatter;
	  log_step: bool ;
	  debug_mode: bool ;
	  log_step_channel : Format.formatter ;
	  kasa : Remanent_parameters_sig.parameters 
        }

    let build_parameter () =
      let channel = Kappa_files.open_profiling () in
      {
        current_compression_mode = None ;
        priorities_weak = Priority.weak ;
        priorities_strong = Priority.strong2 ;
        priorities_causal = Priority.causal ;
	compute_all_stories = false ; 
        out_channel = Format.err_formatter ;
        out_channel_err = Format.err_formatter ;
        out_channel_profiling = Format.formatter_of_out_channel channel ;
        compression_mode = Parameter.get_compression_mode () ;
        cache_size = Parameter.get_cache_size () ;
	debug_mode = false ;
	log_step = true ;
	log_step_channel = Format.std_formatter ; 
	kasa = Remanent_parameters.get_parameters () 
      }

    let set_compression_weak p =
      {p with current_compression_mode = Some Parameter.Weak}
    let set_compression_strong p =
      {p with current_compression_mode = Some Parameter.Strong}
    let set_compression_none p =
      {p with current_compression_mode = Some Parameter.Causal}


    type error =
        {
          caml_file: string option;
          caml_module: string option;
          caml_function: string option;
          caml_line: string option;
          error_message: string option;
          exn: exn
        }

    type error_channel = error list
    type handler =
        {
          env: Environment.t ;
        }

    type 'a with_handler = parameter -> handler -> error_channel -> 'a

    let error_init = []
    let create_error parameter handler error file modu fun_name line message exn =
      error,
      {
        caml_file=file;
        caml_function=fun_name;
        caml_module=modu;
        caml_line=line;
        error_message=message;
        exn=exn
      }

    let add_error parameter handler error_list error =
      error::error_list

    let string_of_exn x = Some ""

    let dump_error parameter handler error_list error  =
      let log = parameter.out_channel_err in
      let f prefix suffix mes =
        match mes with
        | None -> ()
        | Some mes ->
           Format.fprintf log "%s%s%s" prefix mes suffix
      in
      let g s = Format.fprintf log "%s" s in
      let _ = g "Internal Error\n" in
      let _ = f "File: " "\n" error.caml_file in
      let _ = f "Line: " "\n" error.caml_line in
      let _ = f "message: " "\n" error.error_message in
      let _ = f "function: " "\n" error.caml_function in
      let _ = f "module: " "\n" error.caml_module in
      let _ = f "error " "has been raised\n%!" (string_of_exn error.exn) in
      ()

    let raise_error parameter handler error_list error def =
      if Parameter.interrupt_on_exception
      then
        let () = dump_error parameter handler error_list error in
        raise error.exn
      else
        let error_list = add_error parameter handler error_list error in
        error_list,def

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
end:Cflow_handler)
    
