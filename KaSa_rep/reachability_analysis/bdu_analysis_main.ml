(**
  * bdu_analysis_main.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of September
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Int_storage
open Fifo
open Bdu_build
open Bdu_analysis_type
open Bdu_creation
open Print_bdu_analysis
open Bdu_side_effects
open Bdu_modification_sites
open Bdu_contact_map
open Bdu_update
open Bdu_working_list
open Bdu_fixpoint_iteration

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*static analysis*)

let scan_rule_static parameter error handler rule_id rule covering_classes
    store_result =
  (*------------------------------------------------------------------------------*)
  (*static information of covering classes: from sites -> covering_class id list*)
  let error, store_covering_classes_id =
    site_covering_classes
      parameter
      error
      covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*side effects*)
  let error, store_side_effects =
    collect_side_effects
      parameter
      error
      handler
      rule_id 
      rule.actions.half_break
      rule.actions.remove
      store_result.store_side_effects
  in 
  (*------------------------------------------------------------------------------*)
  (*modification sites*)
  let error, store_creation_sites =
    collect_creation_sites
      parameter
      error
      rule_id
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_creation_sites
  in
  let error, store_modification_sites_without_creation =
    collect_modification_sites_without_creation
      parameter
      error
      rule_id
      rule.diff_direct
      store_creation_sites
      store_result.store_modification_sites_without_creation
  in
  let error, store_modification_sites =
    collect_modification_sites
      parameter
      error
      rule_id
      rule.diff_direct
      store_result.store_modification_sites
  in
  (*test*)
  let error, store_test_sites =
    collect_test_sites
      parameter
      error
      rule_id
      rule.rule_lhs.views
      store_result.store_test_sites
  in
  (*test and modification*)
  let error, store_test_modification_sites =
    collect_test_modification_sites
      parameter
      error
      store_modification_sites
      store_test_sites
  in
  (*test and modification without creation*)
  let error, store_test_modification_without_creation =
    collect_test_modification_without_creation
      parameter
      error
      store_modification_sites_without_creation
      store_test_sites
  in
  error, 
  {
    store_covering_classes_id                  = store_covering_classes_id;
    store_side_effects                         = store_side_effects;
    store_creation_sites                       = store_creation_sites;
    store_modification_sites_without_creation  = 
      store_modification_sites_without_creation;
    store_modification_sites                   = store_modification_sites;
    store_test_sites                           = store_test_sites;
    store_test_modification_sites              = store_test_modification_sites;
    store_test_modification_without_creation   = 
      store_test_modification_without_creation;
  }

(************************************************************************************)
(*dynamic analysis*)

let scan_rule_dynamic parameter error handler rule_id rule 
    store_test_modification_without_creation
    store_covering_classes_id
    store_side_effects
    store_result =
  (*------------------------------------------------------------------------------*)
  (*contact map*)
  let error, store_contact_map =
    compute_contact_map
      parameter
      error
      handler
      rule
  in
  (*-------------------------------------------------------------------------------*)
  (*return a mapping of covering classes to a list of rules that has [modified and test]
    sites*)
  let error, store_covering_classes_modification_update =
    store_covering_classes_modification_update
      parameter
      error
      store_test_modification_without_creation
      store_covering_classes_id
  in
  (*------------------------------------------------------------------------------*)
  (*update function*)
  let error, store_update =
    store_binding_update
      parameter
      error
      store_covering_classes_modification_update
      store_side_effects
      store_contact_map
  in
  (*------------------------------------------------------------------------------*)
  (*adding rule_id inside a working list*)
  let error, store_wl_update =
    collect_wl_update
      parameter
      error
      store_update
  in
  (*rule_id of creation inside working list*)
  let error, store_wl_creation =
    collect_wl_creation
      parameter
      error
      rule_id
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_wl_creation
  in
  (*rule_id of both update function and creation inside working list*)
  let error, store_wl_creation_update =
    collect_wl_creation_update
      parameter
      error
      store_wl_creation
      store_wl_update
  in
  error, 
  {
    store_contact_map                          = store_contact_map;
    store_covering_classes_modification_update =
      store_covering_classes_modification_update;
    store_update                               = store_update;
    store_wl_update                            = store_wl_update;
    store_wl_creation                          = store_wl_creation;
    store_wl_creation_update                   = store_wl_creation_update;
  }

(************************************************************************************)
(*rule bdu build*)

let scan_rule_bdu_build parameter error rule covering_classes 
    covering_class_set store_result =
  let error, store_remanent_test =
    collect_remanent_test
      parameter
      error
      rule
      covering_classes
      store_result.store_remanent_test
  in
  let error, store_test_restriction =
    collect_test_restriction
      parameter
      error
      rule
      store_remanent_test
      store_result.store_test_restriction
  in
  error, 
  {
    store_remanent_test    = store_remanent_test;
    store_test_restriction = store_test_restriction
  }

(************************************************************************************)
(*rule*)

let scan_rule parameter error handler rule_id rule store_covering_classes
    compiled store_result =
  let covering_classes, covering_class_set = store_covering_classes in
  let error, store_bdu_analysis_static =
    scan_rule_static 
      parameter
      error 
      handler
      rule_id 
      rule 
      covering_classes
      store_result.store_bdu_analysis_static
  in
  let error, store_bdu_analysis_dynamic =
    scan_rule_dynamic
      parameter
      error
      handler
      rule_id
      rule 
      store_bdu_analysis_static.store_test_modification_without_creation
      store_bdu_analysis_static.store_covering_classes_id
      store_bdu_analysis_static.store_side_effects
      store_result.store_bdu_analysis_dynamic
  in
  let error, store_bdu_build =
    scan_rule_bdu_build
      parameter
      error
      rule
      covering_classes
      covering_class_set
      store_result.store_bdu_build
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_bdu_analysis_static  = store_bdu_analysis_static;
    store_bdu_analysis_dynamic = store_bdu_analysis_dynamic;
    store_bdu_build            = store_bdu_build
  }
 
(************************************************************************************)
(*intitial state of static analysis*)

let init_bdu_analysis_static =
  let init_covering_classes_id     = Int2Map_CV.empty_map in
  let init_half_break              = Int2Map_HalfBreak_effect.empty_map  in
  let init_remove                  = Int2Map_Remove_effect.empty_map  in
  let init_creation                = Int2Map_Modif.empty_map in
  let init_modif_without_creation  = Int2Map_Modif.empty_map in
  let init_modification            = Int2Map_Modif.empty_map in
  let init_test                    = Int2Map_Modif.empty_map in
  let init_test_modification       = Int2Map_Modif.empty_map in
  let init_test_modification_without_creation = Int2Map_Modif.empty_map in
  let init_bdu_analysis_static =
    {
      store_covering_classes_id                 = init_covering_classes_id;
      store_side_effects                        = (init_half_break, init_remove);
      store_creation_sites                      = init_creation;
      store_modification_sites_without_creation = init_modif_without_creation;
      store_modification_sites                  = init_modification;
      store_test_sites                          = init_test;
      store_test_modification_sites             = init_test_modification;
      store_test_modification_without_creation  =
        init_test_modification_without_creation;
    }
  in
  init_bdu_analysis_static
      
(************************************************************************************)
(*intitial state of dynamic analysis*)

let init_bdu_analysis_dynamic parameter error =
  let init_contact_map             = Int2Map_CM_state.empty_map in
  let init_cv_modification         = Int2Map_CV_Modif.empty_map in
  let init_store_hb                = Int2Map_CV_Modif.empty_map in
  let init_store_remove            = Int2Map_CV_Modif.empty_map in
  let init_store_hb_remove         = Int2Map_CV_Modif.empty_map in
  let init_store_update_aux        = Int2Map_CV_Modif.empty_map in
  let error, init_wl_update          = AgentMap.create parameter error 0 in
  let error, init_wl_creation        = AgentMap.create parameter error 0 in
  let error, init_wl_creation_update = AgentMap.create parameter error 0 in
  let init_bdu_analysis_dynamic =
    {
      store_contact_map                          = init_contact_map;
      store_covering_classes_modification_update = init_cv_modification;
      store_update                               =
        (init_store_hb,
         init_store_remove,
         init_store_hb_remove,
         init_store_update_aux);
      store_wl_update                            = init_wl_update;
      store_wl_creation                          = init_wl_creation;
      store_wl_creation_update                   = init_wl_creation_update;
    }
  in
  error, init_bdu_analysis_dynamic

(************************************************************************************)
(*init of bdu build*)

let init_bdu_build parameter error =
  let error, init_remanent_test    = AgentMap.create parameter error 0 in
  let error, init_test_restriction = AgentMap.create parameter error 0 in
  let init_restriction_bdu_test =
    {
      store_remanent_test    = init_remanent_test;
      store_test_restriction = init_test_restriction
    }
  in
  error, init_restriction_bdu_test

(************************************************************************************)
(*rules*)

let scan_rule_set parameter error handler store_covering_classes compiled rules =
  let error, init_bdu_analysis_dynamic = init_bdu_analysis_dynamic parameter error in
  let error, init_bdu_build            = init_bdu_build parameter error in
  let init_bdu =
    {
      store_bdu_analysis_static  = init_bdu_analysis_static;
      store_bdu_analysis_dynamic = init_bdu_analysis_dynamic;
      store_bdu_build            = init_bdu_build;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_results =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        let error, result =
          scan_rule
            parameter
            error
            handler
            rule_id
            rule.e_rule_c_rule
            store_covering_classes
            compiled
            store_result
        in
        error, result
      ) rules init_bdu
  in
  error, store_results

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler store_covering_classes cc_compil =
  let error, result =
    scan_rule_set
      parameter
      error 
      handler
      store_covering_classes
      cc_compil 
      cc_compil.rules 
  in
  let error =
    if  (Remanent_parameters.get_trace parameter) || trace
    then print_result parameter error result
    else error
  in
  error, result
