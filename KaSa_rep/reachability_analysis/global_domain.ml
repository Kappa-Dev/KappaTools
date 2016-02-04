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

  (*--------------------------------------------------------------------*)
  (** explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)
      
  let get_compilation_information static = 
    Analyzer_headers.get_compilation_information static

  let get_kappa_handler static =
    let compilation_result = get_compilation_information static in
    let kappa_handler = compilation_result.Analyzer_headers.kappa_handler in
    kappa_handler
      
  let get_parameter static = Analyzer_headers.get_parameter static

  let get_cc_code compilation_result = compilation_result.Analyzer_headers.cc_code

  (**[get_common_static static] returns information about parameter,
     kappa_handler, and compiled*)

  let get_common_static static =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compilation_result = get_compilation_information static in
    let compiled = get_cc_code compilation_result in
    parameter, kappa_handler, compiled

  (*--------------------------------------------------------------------*)
  (* explain how to extract the handler for mvbdu *)

  let get_mvbdu_handler dynamic = dynamic.mvbdu_handler

  (* explain how to overwritte the previous handler *)

  let set_mvbdu_handler handler dynamic = 
    {
      dynamic with mvbdu_handler = handler (*CHECK ME*)
    }
      
  (*--------------------------------------------------------------------*)
  (** intialization function of global static & dynamic information of this
      domain*)

  let initialize_domain_static_information (static:static_information) error =
    let parameter = get_parameter static.global_static_information in
    let error, init_bdu_analysis_static =
      Bdu_analysis_main.init_bdu_analysis_static parameter error
    in
    error,
    {
      static with
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
    let init_result_of_analysis =
      error, init_fixpoint, init_dead_rule_array
    in
    let error, init_bdu_analysis_dynamic = 
      Bdu_analysis_main.init_bdu_analysis_dynamic parameter error
    in
    let init_domain_dynamic_information =
      init_bdu_analysis_dynamic, init_result_of_analysis
    in
    error,
    {
      dynamic with
        mvbdu_handler = handler_bdu;
        domain_dynamic_information = init_domain_dynamic_information;
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

  (*let get_bdu_analysis_static static error rule_id rule covering_classes =
    let parameter, kappa_handler, compiled = get_common_static static in
    let error, bdu_analysis_static =
      Bdu_analysis_main.scan_rule_static
        parameter
        error
        kappa_handler
        rule_id
        rule
        rule
        covering_classes
        store
    in
    error, bdu_analysis_static*)

  (*let get_bdu_build static error rule_id covering_classes =
    let parameter, kappa_handler, compiled = get_common_static static in
    let error, bdu_build =
      Bdu_analysis_main.scan_rule_bdu_build
        parameter
        handler_bdu
        error
        rule_id
        rule
        compiled
        covering_classes
        store_potential_side_effects
        store
    in
    error, bdu_build*)

  (** [get_domain_static_information static] *)
  (*let get_domain_static_information static =
  in
  *)

  (**[bdu_main static error]*)
  (*let get_bdu_main static error =
    let parameter, kappa_handler, compiled = get_common_static static in
    let error, handler_bdu, result =
      Bdu_analysis_main.bdu_main
        parameter
        error
        kappa_handler
        store_covering_classes
        compiled
    in
    error, handler_bdu, result*)

  (** get the implementation of dynamic *)
  (*let get_bdu_analysis_dynamic static dynamic error store =
    let parameter, kappa_handler, compiled = get_common_static static in
    (*get result of domain_static_information*)
    let domain_static_information =
      
    in
    let bdu_analysis_dynamic =
      Bdu_analysis_main.scan_rule_dynamic
        parameter
        error
        kappa_handler
        rule_id
        rule
        compiled
        store_test_modification_map
        store_covering_classes_id
        store_side_effects
        store_potential_side_effects
        covering_classes
        store
    in
    error, bdu_analysis_dynamic*)

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
  let get_scan_rule_set static error covering_classes =
    let parameter, kappa_handler, compiled = get_common_static static  in
    let error, handler_bdu = Boolean_mvbdu.init_remanent parameter error in
    let error, (handler_bdu, result) =
      Bdu_analysis_main.scan_rule_set
        parameter
        handler_bdu
        error
        kappa_handler
        compiled
        covering_classes
        compiled.Cckappa_sig.rules
    in
    error, (handler_bdu, result)

  let get_store_remanent_triple static error covering_classes =
    let error, (handler_bdu, result) =
      get_scan_rule_set static error covering_classes
    in
    error, 
    (handler_bdu,
     result.Bdu_analysis_type.store_bdu_analysis_static.Bdu_analysis_type.store_remanent_triple)

  let get_store_bdu_init_restriction_map static error covering_classes =
    let error, (handler_bdu, result) =
      get_scan_rule_set static error covering_classes
    in
    error,
    (handler_bdu,
     result.Bdu_analysis_type.store_bdu_analysis_static.Bdu_analysis_type.store_bdu_init_restriction_map)

  (**[get_store_proj_bdu_views] from a result of static information,
     returns a function point to a field that store the static information
     of views after projection*)
  let get_store_proj_bdu_views static error covering_classes =
    let error, (handler_bdu, result) =
      get_scan_rule_set static error covering_classes
    in
    error, 
    (handler_bdu, 
     result.Bdu_analysis_type.store_bdu_analysis_static.Bdu_analysis_type.store_proj_bdu_views)


  let get_bdu_proj_views static error covering_classes rule_id =
    let error, (handler_bdu, store_proj_bdu_views) =
      get_store_proj_bdu_views static error covering_classes
    in
    error,
    (handler_bdu, 
     Bdu_fixpoint_iteration.collect_bdu_proj_views error rule_id store_proj_bdu_views)

  let get_store_bdu_fixpoint_init_map static dynamic error bdu_false covering_classes =
    let error, (handler_bdu, result) =
      get_scan_rule_set static error covering_classes
    in
    let parameter, kappa_handler, _ = get_common_static static in
    let error, (handler_bdu, site_corresponce_in_covering_classes) =
      get_store_remanent_triple static error covering_classes
    in
    let error, (handler_bdu, store_bdu_init_restriction_map) =
      get_store_bdu_init_restriction_map static error covering_classes
    in
    let error, bool, handler_bdu, store_bdu_fixpoint_init_map =
      Bdu_fixpoint_iteration.store_bdu_fixpoint_init_map
        parameter
        handler_bdu
        error
        kappa_handler
        bdu_false
        site_corresponce_in_covering_classes
        store_bdu_init_restriction_map
    in
    let dynamic_information = set_mvbdu_handler handler_bdu dynamic in
    error, bool,
    (*{
      static with
        domain_static_information =
        result.Bdu_analysis_type.store_bdu_build.site_coressponce_in_covering_classes
    },*) dynamic_information
    
    (*error, bool, handler_bdu, store_bdu_fixpoint_init_map*)

  (*let is_enabled' static dynamic error rule_id bdu_false covering_classes =
    let parameter = get_parameter static in
    (*let error, handler_bdu, bdu_false = get_mvbdu_false static dynamic error in*)
    let error, (handler_bdu, bdu_proj_views) =
      get_bdu_proj_views static error covering_classes rule_id
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
    else*)
      
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
