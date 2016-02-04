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

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;	   
      domain_static_information : Bdu_analysis_type.bdu_analysis_static
    }

  (*--------------------------------------------------------------------*)
  (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)
  
  type dynamic_information =
    {
      global_dynamic_information : Analyzer_headers.global_dynamic_information;
      mvbdu_handler              : Mvbdu_wrapper.Mvbdu.handler;
      dead_rule                  : bool array;
      fixpoint_result            : Mvbdu_wrapper.Mvbdu.handler * Mvbdu_wrapper.Mvbdu.mvbdu
        Bdu_analysis_type.Map_bdu_update.Map.t;
      domain_dynamic_information : Bdu_analysis_type.bdu_analysis_dynamic
      }

  (*--------------------------------------------------------------------*)
  (** explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)
      
  let get_compilation_information global_static =
    Analyzer_headers.get_compilation_information global_static

  let get_kappa_handler global_static =
    let compilation_result = get_compilation_information global_static in
    let kappa_handler = compilation_result.Analyzer_headers.kappa_handler in
    kappa_handler
      
  let get_parameter global_static = Analyzer_headers.get_parameter global_static

  let get_cc_code compilation_result = compilation_result.Analyzer_headers.cc_code

  let get_bdu_common_static global_static =
    Analyzer_headers.get_common_static global_static

  (**[get_common_static static] returns information about parameter,
     kappa_handler, and compiled*)

  let get_common_static global_static =
    let parameter = get_parameter global_static in
    let kappa_handler = get_kappa_handler global_static in
    let compilation_result = get_compilation_information global_static in
    let compiled = get_cc_code compilation_result in
    parameter, kappa_handler, compiled

  (*--------------------------------------------------------------------*)
  (* explain how to extract the handler for mvbdu *)

  let get_mvbdu_handler dynamic = dynamic.mvbdu_handler

  (* explain how to overwritte the previous handler *)

  let set_mvbdu_handler handler dynamic = 
    {
      dynamic with mvbdu_handler = handler
    }
      
  (*--------------------------------------------------------------------*)
  (** intialization function of global static & dynamic information of this
      domain*)

  let initialize_domain_static_information (static:static_information) error =
    let parameter = get_parameter static.global_static_information in
    let error, init_bdu_analysis_static =
      Bdu_analysis_main.init_bdu_analysis_static parameter error
    in
    let compilation_result = get_compilation_information static.global_static_information in
    let error, init_global_static_information, _ =
      Analyzer_headers.initialize_global_information parameter
        error compilation_result
    in
    error,
    {
      global_static_information = init_global_static_information;
      domain_static_information = init_bdu_analysis_static
    }

  let initialize_domain_dynamic_information static dynamic error =
    let parameter = get_parameter static.global_static_information in
    let kappa_handler = get_kappa_handler static.global_static_information in
    let error, handler_bdu = Boolean_mvbdu.init_remanent parameter error in
    let nrules = Handler.nrules parameter error kappa_handler in
    let init_dead_rule_array = Array.make nrules false in
    let init_fixpoint =
      handler_bdu, Bdu_analysis_type.Map_bdu_update.Map.empty
    in
    let error, init_bdu_analysis_dynamic = 
      Bdu_analysis_main.init_bdu_analysis_dynamic parameter error
    in
    error,
    {
      dynamic with
        mvbdu_handler   = handler_bdu;
        dead_rule       = init_dead_rule_array;
        fixpoint_result = init_fixpoint;
        domain_dynamic_information = init_bdu_analysis_dynamic
    }

  let initialize static dynamic error =
    let error, domain_static_information =
      initialize_domain_static_information static error
    in
    let error, domain_dynamic_information =
      initialize_domain_dynamic_information static dynamic error
    in
    error, domain_static_information, domain_dynamic_information
      
  (*--------------------------------------------------------------------*)

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

  (*----------------------------------------------------------------------*)
  (*Instantiate of functions that store the static and dynamic information
    accordingly from the previous analyzer *)

  (*--------------------------------------------------------------------*)
  (** [add_initial_state static dynamic error state] takes an initial state
      and returns the information of the dynamic and a list of event*)

  let add_initial_state static dynamic error state =
    error, dynamic, []

  (**[get_bdu_false/true] from dynamic*)

  let get_mvbdu_false static dynamic error =
    let parameter = get_parameter static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_false =
      Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler_bdu error
    in
    error, handler_bdu, bdu_false

  (** the initial build for mvbdu_true*)
  let get_mvbdu_true static dynamic error =
    let parameter = get_parameter static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_true =
      Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler_bdu error
    in
    error, handler_bdu, bdu_true

  (** [get_scan_rule_set static] *)
  let get_scan_rule_set (static: static_information) dynamic error =
    let parameter, kappa_handler, compiled = 
      get_common_static static.global_static_information 
    in
    let error, handler_bdu = Boolean_mvbdu.init_remanent parameter error in
    let error, (handler_bdu, result) =
      Bdu_analysis_main.scan_rule_set
        parameter
        handler_bdu
        error
        kappa_handler
        compiled
        compiled.Cckappa_sig.rules
    in
    let static_information =
      {
        static with
          domain_static_information = result.Bdu_analysis_type.store_bdu_analysis_static
      }
    in
    let dynamic_information =
      {
        dynamic with
          mvbdu_handler = handler_bdu;
          domain_dynamic_information = result.Bdu_analysis_type.store_bdu_analysis_dynamic
      }
    in
    error, static_information, dynamic_information
      
  (*let is_enabled' static dynamic error rule_id =
    let parameter = get_parameter static in
    let error, handler_bdu, bdu_false = get_mvbdu_false static dynamic error in
    let error, (handler_bdu, bdu_proj_views) =
      get_bdu_proj_views static error rule_id
    in
    let error, bool, handler_bdu, store_bdu_update_map =
      get_store_bdu_fixpoint_init_map static dynamic error covering_classes
    in
    let error, handler_bdu, is_enable =
      Bdu_fixpoint_iteration.is_enable
        parameter
        handler_bdu
        error
        bdu_false
        rule_id
        bdu_proj_views
        store_bdu_update_map
    in
    if is_enable
    then
    error, dynamic
    else*)
      
  (*update by setting handler_bdu*)
  let is_enabled static dynamic error rule_id =
    error, dynamic, None

  let apply_rule static dynamic error r_id precondition =
    error, dynamic, []
      
  let apply_event_list static dynamic error event_list =
    error, dynamic, []

  (*--------------------------------------------------------------------*)
  (** [add_initial_state static dynamic error state] takes an initial state
      and returns the information of the dynamic and a list of event*)
      
  let add_initial_state static dynamic error state =
    error, dynamic, []

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state
      
  let print static dynamic error loggers =
    error, dynamic, ()

end

(**A functor that takes a module Analyzer.Analyzer, and implement a
   function: main, export, and print*)
