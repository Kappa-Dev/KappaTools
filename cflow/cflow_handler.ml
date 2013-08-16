(**
  * cflow_handler.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 29/08/2011
  * Last modification: 02/08/2013
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)


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
            out_channel_err : out_channel ;
            out_channel_profiling : out_channel ;
            out_channel : out_channel 
          } (*a struct which contains parameterizable options*)  
    type error 
    type error_channel = error list    (*a list which contains the errors so far*)
    type handler =   (*handler to interpret abstract values*)
          {
            state: State.implicit_state ;
            env: Environment.t ;
          }
    type 'a with_handler = parameter -> handler -> error_channel -> 'a

    val build_parameter: unit -> parameter 
    val string_of_exn: exn -> string option  
    val error_init: error_channel 
    val create_error: (string option -> string option -> string option -> string option -> string option -> exn -> error_channel * error) with_handler
    val add_error: (error -> error_channel) with_handler  
    val raise_error:  (error -> 'a -> error_channel * 'a) with_handler  
    val dump_error: (error -> unit) with_handler
    val set_compression_weak: parameter -> parameter 
    val set_compression_strong: parameter -> parameter 
    val set_compression_none: parameter -> parameter 
    val get_priorities: parameter -> Priority.priorities option 
end


module Cflow_handler = 
  (
    struct 
      type parameter = 
          { 
            cache_size : int option ;
            current_compression_mode: Parameter.current_compression_mode option;
            compression_mode : Parameter.compression_mode ;
            priorities_weak: Priority.priorities ;
            priorities_strong : Priority.priorities ;
            priorities_causal: Priority.priorities ;
            out_channel_err : out_channel ;
            out_channel_profiling: out_channel ;
            out_channel : out_channel 
          }

      let build_parameter () = 
        let channel = open_out !Parameter.profilingName in 
        {
          current_compression_mode = None ; 
          priorities_weak = Priority.weak ; 
          priorities_strong = Priority.strong2 ; 
          priorities_causal = Priority.causal ;
          out_channel = stderr ; 
          out_channel_err = stderr ; 
          out_channel_profiling = channel ;
          compression_mode = Parameter.get_compression_mode () ; 
          cache_size = Parameter.get_cache_size () ;
        }

      let set_compression_weak p = {p with current_compression_mode = Some Parameter.Weak}
      let set_compression_strong p = {p with current_compression_mode = Some Parameter.Strong}
      let set_compression_none p = {p with current_compression_mode = Some Parameter.Causal}


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
            state: State.implicit_state ;
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
          match mes
          with 
            | None -> ()
            | Some mes ->
              Printf.fprintf log "%s%s%s" prefix mes suffix 
        in 
        let g s = Printf.fprintf log "%s" s in 
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
          let _ = dump_error parameter handler error_list error in 
          raise error.exn
        else
          let error_list = add_error parameter handler error_list error in 
            error_list,def 

      let get_priorities parameter = 
        match 
          parameter.current_compression_mode 
        with 
        | None -> None 
        | Some Parameter.Weak -> Some parameter.priorities_weak
        | Some Parameter.Strong -> Some parameter.priorities_strong 
        | Some Parameter.Causal -> Some parameter.priorities_causal
    end:Cflow_handler
  )
