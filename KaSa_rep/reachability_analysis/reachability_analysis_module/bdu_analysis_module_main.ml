(**
  * bdu_analysis_main.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 19th of Januaray
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(*******************************************************************************)
(*type abstraction*)

type bdu_analysis =
  {
    store_bdu_analysis_static  : Bdu_analysis_static_type.bdu_analysis_static;
    store_bdu_analysis_dynamic : Bdu_analysis_dynamic_type.bdu_analysis_dynamic;
    store_bdu_build            : Bdu_build_type.bdu_build
  }

(*******************************************************************************)
(*RULE*)

let scan_rule parameter error handler_bdu (handler_kappa: Cckappa_sig.kappa_handler)
    (rule_id: int) (rule: Cckappa_sig.rule) (compiled: Cckappa_sig.compil)
    covering_classes store_result =
  let error, store_bdu_analysis_static =
    Bdu_analysis_static_module.Bdu_analysis_Static.scan_rule_static
      parameter
      error
      handler_kappa
      rule_id
      rule
      covering_classes
      store_result.store_bdu_analysis_static          
  in
  (*-------------------------------------------------------------------------------*)
  let error, store_bdu_analysis_dynamic =
    Bdu_analysis_dynamic_module.Bdu_analysis_Dynamic.scan_rule_dynamic
      parameter
      error
      handler_kappa
      rule_id
      rule
      compiled
      store_bdu_analysis_static.Bdu_analysis_static_type.store_test_modif_map
      store_bdu_analysis_static.Bdu_analysis_static_type.store_covering_classes_id
      store_bdu_analysis_static.Bdu_analysis_static_type.store_potential_side_effects
      covering_classes
      store_result.store_bdu_analysis_dynamic
  in
  (*-------------------------------------------------------------------------------*)
  let error, handler_bdu, store_bdu_build =
    Bdu_build_module.Bdu_Build.scan_rule_bdu_build
      parameter
      handler_bdu
      error
      rule_id
      rule
      compiled
      covering_classes
      store_bdu_analysis_static.Bdu_analysis_static_type.store_potential_side_effects
      store_result.store_bdu_build
  in
  error, 
  (handler_bdu, 
   {
     store_bdu_analysis_static  = store_bdu_analysis_static;
     store_bdu_analysis_dynamic = store_bdu_analysis_dynamic;
     store_bdu_build            = store_bdu_build
   })

(*******************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler_bdu handler_kappa compiled 
    store_covering_classes = 
  let error, init_bdu_analysis_static =
    Bdu_analysis_static_module.Bdu_analysis_Static.init_bdu_analysis_static error
  in
  let error, init_bdu_analysis_dynamic =
    Bdu_analysis_dynamic_module.Bdu_analysis_Dynamic.init_bdu_analysis_dynamic error
  in
  let error, init_bdu_build =
    Bdu_build_module.Bdu_Build.init_bdu_build parameter error
  in
  let error, init_bdu =
    error, 
    {
      store_bdu_analysis_static = init_bdu_analysis_static;
      store_bdu_analysis_dynamic = init_bdu_analysis_dynamic;
      store_bdu_build            = init_bdu_build;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, (handler_bdu, store_results) =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule (handler_bdu, store_result) ->
        scan_rule
          parameter
          error
          handler_bdu
	  handler_kappa
          rule_id
          rule.Cckappa_sig.e_rule_c_rule
          compiled
          store_covering_classes
          store_result
      ) compiled.Cckappa_sig.rules (handler_bdu, init_bdu)
  in
  error, (handler_bdu, store_results)

(*******************************************************************************)
(*MAIN*)
    
let bdu_main parameter error handler_kappa store_covering_classes compiled =
  let error, handler_bdu = Boolean_mvbdu.init_remanent parameter error in
  let error, (handler_bdu, result) =
    scan_rule_set
      parameter
      error
      handler_bdu
      handler_kappa 
      compiled
      store_covering_classes
  in
  (* Static information before fixpoint computation *)
  (*let error = 
    if  (Remanent_parameters.get_trace parameter) || trace
    then Print_bdu_analysis.print_result parameter error handler_kappa compiled result
    else error
  in*)
  (*--------------------------------------------------------------------*)
  (*discover dead rule; an initial array is false everywhere*)
  (*let nrules = Handler.nrules parameter error handler_kappa in
  let init_dead_rule_array = Array.make nrules false in
  (*-------------------------------------------------------------------------------*)
  (*fixpoint computation: no rule in particular, we should start with rule
    with no lhs and those induced by initial states to remove *)
  let error, (handler_bdu, store_bdu_fixpoint, dead_rule_array) =
    Bdu_fixpoint_iteration.collect_bdu_fixpoint_map
      parameter
      handler_bdu
      error
      handler_kappa
      compiled
      result.store_bdu_build.store_remanent_triple
      result.store_bdu_build.store_wl_creation
      result.store_bdu_build.store_proj_bdu_creation_restriction_map
      result.store_bdu_build.store_modif_list_restriction_map
      result.store_bdu_build.store_proj_bdu_test_restriction_map
      result.store_bdu_build.store_proj_bdu_potential_restriction_map
      result.store_bdu_build.store_bdu_test_restriction_map
      result.store_bdu_build.store_proj_bdu_views
      result.store_bdu_analysis_dynamic.store_covering_classes_modification_update_full
      result.store_bdu_build.store_bdu_init_restriction_map
      init_dead_rule_array
  in
  (*let error, handler_bdu =
    if  Remanent_parameters.get_dump_reachability_analysis_result parameter
    then
      (*Print a list of rules that is dead*)
      let error =
        print_result_dead_rule parameter error handler_kappa compiled dead_rule_array
      in
      Print_bdu_analysis.print_result_fixpoint 
        parameter
        handler_bdu 
        error 
        handler_kappa
	result.store_bdu_build.store_remanent_triple
	store_bdu_fixpoint
    else error, handler_bdu*)
  in*)
  error, handler_bdu, result
