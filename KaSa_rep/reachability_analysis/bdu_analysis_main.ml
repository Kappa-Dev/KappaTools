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
open Bdu_fixpoint_iteration

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler rule_id rule covering_classes compiled store_result =
  (*------------------------------------------------------------------------------*)
  (*list of rules has a creation action *)
  let error, store_creation_rule =
    collect_rule_creation
      parameter
      error
      handler
      rule
      rule_id
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_creation_rule
  in
  (*------------------------------------------------------------------------------*)
  (*bdu structure of creation rules-this function uses for testing the
    result of the list of creation rules in the working list.*)
  let error, store_creation =
    collect_creation
      parameter
      error
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_creation
  in
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
  let error, store_modification_sites =
    collect_modification_sites
      parameter
      error
      rule_id
      rule.diff_direct
      store_result.store_modification_sites
  in
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
  (*return a mapping of covering classes to a list of rules that has modified sites*)
  let error, store_covering_classes_modification_update =
    store_covering_classes_modification_update
      parameter
      error
      store_modification_sites
      store_covering_classes_id
  in
  (*------------------------------------------------------------------------------*)
  (*TEST*)
  let error, store_update =
    store_binding_update
      parameter
      error
      store_covering_classes_modification_update
      store_side_effects
      store_contact_map
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation                             = store_creation;
    store_creation_rule                        = store_creation_rule;
    (*static information*)
    store_covering_classes_id                  = store_covering_classes_id;
    store_side_effects                         = store_side_effects;
    store_modification_sites                   = store_modification_sites;
    (*dynamic information*)
    store_contact_map                          = store_contact_map;
    store_covering_classes_modification_update =
      store_covering_classes_modification_update;
    store_update                               = store_update;
  }
 
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler covering_classes compiled rules =
  let error, init_creation         = AgentMap.create parameter error 0 in
  let error, init_creation_rule    = AgentMap.create parameter error 0 in
  (*static information*)
  let init_covering_classes_id     = Int2Map_CV.empty_map in
  let init_half_break              = Int2Map_HalfBreak_effect.empty_map  in
  let init_remove                  = Int2Map_Remove_effect.empty_map  in
  let init_modification            = Int2Map_Modif.empty_map in
  (*dynamic information*)
  let init_contact_map             = Int2Map_CM_state.empty_map in
  let init_cv_modification         = Int2Map_CV_Modif.empty_map in
  (*update function*)
  let init_store_hb                = Int2Map_CV_Modif.empty_map in
  let init_store_remove            = Int2Map_CV_Modif.empty_map in
  let init_store_hb_remove         = Int2Map_CV_Modif.empty_map in
  let init_store_update_aux        = Int2Map_CV_Modif.empty_map in
  let init_bdu =
    {
      store_creation            = init_creation;
      store_creation_rule       = init_creation_rule;
      (*static information*)
      store_covering_classes_id = init_covering_classes_id;
      store_side_effects        = (init_half_break, init_remove);
      store_modification_sites  = init_modification;
      (*dynamic information*)
      store_contact_map         = init_contact_map;
      store_covering_classes_modification_update = init_cv_modification;
      store_update =
        (init_store_hb,
         init_store_remove,
         init_store_hb_remove,
         init_store_update_aux
        )
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
            covering_classes
            compiled
            store_result
        in
        error, result
      ) rules init_bdu
  in
  error, store_results

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler covering_classes cc_compil =
  let error, result =
    scan_rule_set parameter error handler covering_classes cc_compil cc_compil.rules 
  in
  let error =
    if  (Remanent_parameters.get_trace parameter) || trace
    then print_result parameter error result
    else error
  in
  error, result
