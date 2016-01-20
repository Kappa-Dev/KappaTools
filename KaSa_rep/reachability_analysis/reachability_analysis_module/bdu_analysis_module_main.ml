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
      store_bdu_analysis_static.store_test_modif_map
      store_bdu_analysis_static.store_covering_classes_id
      store_bdu_analysis_static.store_potential_side_effects
      covering_classes
      store_result.store_bdu_analysis_dynamic
  in
  error, 
  (handler_bdu, 
   {
     store_bdu_analysis_static  = store_bdu_analysis_static;
     store_bdu_analysis_dynamic = store_bdu_analysis_dynamic;
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
  let error, init_bdu =
    error, 
    {
      store_bdu_analysis_static = init_bdu_analysis_static;
      store_bdu_analysis_dynamic = init_bdu_analysis_dynamic
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
  error, handler_bdu, result
