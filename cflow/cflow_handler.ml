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
  * Last modification: 21/03/2012
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
            compression_mode : Parameter.compression_mode ;
            out_channel_err : out_channel ;
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
    val create_error: (string option -> string option -> string option -> string option -> string option -> exn -> error_channel * error) with_handler
    val add_error: (error -> error_channel) with_handler  
    val raise_error:  (error -> 'a -> error_channel * 'a) with_handler  
    val dump_error: (error -> unit) with_handler
end


module Cflow_handler = 
  (
    struct 
      type parameter = 
          { 
            compression_mode : Parameter.compression_mode ;
            out_channel_err : out_channel ;
            out_channel : out_channel 
          }

      let build_parameter () = 
        {
          out_channel = stderr ; 
          out_channel_err = stderr ; 
          compression_mode = Parameter.get_compression_mode () 
        }

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
    end:Cflow_handler
  )
