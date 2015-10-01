(**
  * bdu_analysis_main.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler rule_id rule covering_classes compiled store_result =
  (*------------------------------------------------------------------------------*)
  (*List of rules has a creation action *)
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
  (*-------------------------------------------------------------------------------*)
  (*return a mapping of covering classes to a list of rules that has modified sites*)
  let error, store_covering_classes_modified_sites =
    covering_classes_modified_sites
      parameter
      error
      covering_classes
      store_modification_sites
      store_result.store_covering_classes_modified_sites
  in
  (*------------------------------------------------------------------------------*)
  (*TEST*)
  (*let error, wl =
    fixpoint
      parameter
      error
      store_creation_rule
      store_result.store_wl
  in*)
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation       = store_creation;
    store_creation_rule  = store_creation_rule;
    store_side_effects   = store_side_effects;
    store_modification_sites = store_modification_sites;
    store_covering_classes_modified_sites = store_covering_classes_modified_sites
  }
 
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler covering_classes compiled rules =
  let error, init_creation      = AgentMap.create parameter error 0 in
  let error, init_creation_rule = AgentMap.create parameter error 0 in
  let error, init_half_break    = AgentMap.create parameter error 0 in
  let error, init_remove1       = AgentMap.create parameter error 0 in
  let error, init_remove2       = AgentMap.create parameter error 0 in
  let error, init_modification  = AgentMap.create parameter error 0 in
  let error, init_cv_modified   = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_creation      = init_creation;
      store_creation_rule = init_creation_rule;
      store_side_effects  = init_half_break, (init_remove1, init_remove2);
      store_modification_sites = init_modification;
      store_covering_classes_modified_sites = init_cv_modified
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_results =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
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
  (*let parameter = Remanent_parameters.update_prefix parameter "agent_type/rule_id_" in*)
  let error, result =
    scan_rule_set parameter error handler covering_classes cc_compil cc_compil.rules 
  in
  let _ = print_result parameter error result in
  error, result
