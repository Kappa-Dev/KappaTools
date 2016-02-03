(**
  * analyzer_sig.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 30th of January
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche 
  * en Informatique et en Automatique.  
  * All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(* Before properly achieving separation of concepts. We introduce one
   monolithic domain that collect everything (as in the previous analyzer).*)

module Domain =
struct

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type domain_static_information =
    Bdu_analysis_type.bdu_analysis_static * Bdu_analysis_type.bdu_build

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;	   
      domain_static_information : domain_static_information
    }

  let get_compilation_information static = 
    Analyzer_headers.get_compilation_information static

  let get_parameter static = Analyzer_headers.get_parameter static

  (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)

  type fixpoint = Mvbdu_wrapper.Mvbdu.handler *
    Mvbdu_wrapper.Mvbdu.mvbdu Bdu_analysis_type.Map_bdu_update.Map.t
    
  type dead_rule_array = bool array

  type result_of_analysis =
    Exception.method_handler * fixpoint * dead_rule_array
      
  type domain_dynamic_information =
    Bdu_analysis_type.bdu_analysis_dynamic * result_of_analysis

  type dynamic_information =
    {
      global_dynamic_information : Analyzer_headers.global_dynamic_information;
      mvbdu_handler              : Mvbdu_wrapper.Mvbdu.handler;
      domain_dynamic_information : domain_dynamic_information
      }

    (* explain how to extract the handler for kappa expressions from a
       value of type static_information *)

    let get_kappa_handler static =
      let parameter = get_parameter static in
      let error = Exception.empty_error_handler in
      let error, parameters, files = Get_option.get_option error in
      let compil = 
        List.fold_left (KappaLexer.compile Format.std_formatter) Ast.empty_compil files 
      in
      let parameters_compil = Remanent_parameters.update_call_stack parameter
        Preprocess.local_trace (Some "Prepreprocess.translate_compil")
      in
      let error, refined_compil = 
        Prepreprocess.translate_compil parameters_compil error compil 
      in
      let parameters_list_tokens = Remanent_parameters.update_call_stack
        parameter List_tokens.local_trace (Some "List_tokens.scan_compil") 
      in
      let error, handler_kappa =
        List_tokens.scan_compil parameters_list_tokens error refined_compil
      in
      error, handler_kappa

    (* explain how to extract the handler for mvbdu *)

    let get_mvbdu_handler dynamic = ()

    (* explain how to overwritte the previous handler *)

    let set_mvbdu_handler handler dynamic = dynamic

    let initialize static dynamic error =
      let parameter = get_parameter static in
      let compilation_result = get_compilation_information static in
      let error, init_bdu_build =
        Bdu_analysis_main.init_bdu_build parameter error
      in
      let init_domain_static_information =
        Bdu_analysis_main.init_bdu_analysis_static, init_bdu_build
      in
      let error, init_global_static_information, init_global_dynamic_information =
        Analyzer_headers.initialize_global_information
          parameter error
          compilation_result
      in
      error,
      {
	global_static_information = init_global_static_information;
	domain_static_information = init_domain_static_information
      }
			       
    type 'a zeroary =
      static_information
      -> dynamic_information
      -> Exception.method_handler
      -> Exception.method_handler * dynamic_information * 'a

    type ('a, 'b) unary =
      static_information
      -> dynamic_information 
      -> Exception.method_handler
      -> 'a 
      -> Exception.method_handler * dynamic_information * 'b

    type ('a, 'b, 'c) binary =
      static_information
      -> dynamic_information 
      -> Exception.method_handler 
      -> 'a 
      -> 'b 
      -> Exception.method_handler * dynamic_information * 'c

    let add_initial_state static dynamic error state =
      error, dynamic, []

    let is_enabled static dynamic error r_id =
      error, dynamic, None

    let apply_rule static dynamic error r_id precondition =
      error, dynamic, []
		      
    let apply_event_list static dynamic error event_list =
      error, dynamic, []

    let export static dynamic error kasa_state =
      error, dynamic, kasa_state
		      
    let print static dynamic error loggers =
      error, dynamic, ()

  end
    
