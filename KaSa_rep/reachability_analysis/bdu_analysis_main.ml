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
open Bdu_analysis_type
open Print_bdu_analysis
open Bdu_side_effects
open Bdu_modification_sites
open Bdu_contact_map
open Bdu_update
open Bdu_working_list
open Bdu_build
open Bdu_fixpoint_iteration

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*global static information*)

let scan_rule_common parameter error handler_kappa rule_id rule store_result =
  (*------------------------------------------------------------------------------*)
  (*side effects*)
  let error, store_side_effects =
    Bdu_side_effects.collect_side_effects
      parameter
      error
      handler_kappa
      rule_id 
      rule.actions.half_break
      rule.actions.remove
      store_result.store_side_effects
  in 
  (*------------------------------------------------------------------------------*)
  (*potential partner side effects*)
  let error, store_potential_side_effects =
    Bdu_side_effects.collect_potential_side_effects
      parameter
      error
      handler_kappa
      rule_id
      rule.actions.half_break
      rule.actions.remove
      store_result.store_potential_side_effects
  in
  error,
  {
    store_side_effects = store_side_effects;
    store_potential_side_effects = store_potential_side_effects
  }

(************************************************************************************)
(**static views information*)

let scan_rule_pre_static parameter error rule_id rule handler_bdu store_result =
  (*-------------------------------------------------------------------------------*)
  (*update of the views due to modification with agent_id*)
  let error, store_modification_sites =
    Bdu_modification_sites.collect_modification_sites
      parameter
      error
      rule_id
      rule.diff_direct
      store_result.store_modification_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations of the views that are tested with agent_id*)
  let error, store_test_sites =
    Bdu_modification_sites.collect_test_sites
      parameter
      error
      rule_id
      rule.rule_lhs.views
      store_result.store_test_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations and update of the views that are tested and modification with agent_id*)
  let error, store_test_modification_sites =
    Bdu_modification_sites.collect_test_modification_sites
      parameter
      error
      store_modification_sites
      store_test_sites
      store_result.store_test_modification_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*update of the views due to modification without agent_id*)
  let error, store_modif_map =
    Bdu_modification_sites.collect_modif_map
      parameter
      error
      store_modification_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations of the views that are tested without agent_id*)
  let error, store_test_map =
    Bdu_modification_sites.collect_test_map
      parameter
      error
      store_test_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations and update of the views that are tested and modification
    without agent_id*)
  let error, store_test_modif_map =
    Bdu_modification_sites.collect_test_modif_map
      parameter
      error
      store_test_modification_sites
  in
  error, handler_bdu,
  {
    store_modification_sites      = store_modification_sites;
    store_test_sites              = store_test_sites;
    store_test_modification_sites = store_test_modification_sites;
    store_modif_map               = store_modif_map;
    store_test_map                = store_test_map;
    store_test_modif_map          = store_test_modif_map;
  }

(************************************************************************************)

let scan_rule_static parameter error handler_kappa handler_bdu rule_id rule
    store_potential_side_effects compil store_result =
  (*------------------------------------------------------------------------------*)
  let error, store_covering_classes =
    Covering_classes_main.covering_classes
      parameter
      error
      handler_kappa
      compil
  in
  (*------------------------------------------------------------------------------*)
  (*static information of covering classes: from sites -> covering_class id list*)
  let error, store_covering_classes_id =
    Bdu_modification_sites.site_covering_classes
      parameter
      error
      store_covering_classes
  in
  (*-------------------------------------------------------------------------------*)
  let error, store_remanent_triple =
    (* JF: it should be computed only once, not for each rule *)
    Bdu_build.collect_remanent_triple
      parameter
      error
      store_covering_classes
      store_result.store_remanent_triple
  in
  (*------------------------------------------------------------------------------*)
  (*working list*)
  let error, store_wl_creation =
    Bdu_working_list.collect_wl_creation
      parameter
      error
      rule_id
      rule
      store_result.store_wl_creation
  in
  (*-------------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_creation_restriction_map =
    Bdu_build.collect_proj_bdu_creation_restriction_map
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_result.store_proj_bdu_creation_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let error, (handler_bdu, store_bdu_init_restriction_map) =
    (* JF: it should be computed only once, not for each rule *)
    Bdu_build.collect_bdu_init_restriction_map
      parameter
      handler_bdu
      error
      compil
      store_remanent_triple
      store_result.store_bdu_init_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let error, (handler_bdu, store_modif_list_restriction_map) =
    Bdu_build.collect_modif_list_restriction_map
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_result.store_modif_list_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_potential_restriction_map =
    Bdu_build.collect_proj_bdu_potential_restriction_map
      parameter
      handler_bdu
      error
      store_remanent_triple
      store_potential_side_effects
      store_result.store_proj_bdu_potential_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_test_restriction =
    Bdu_build.collect_proj_bdu_test_restriction
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_result.store_proj_bdu_test_restriction
  in
  (*-------------------------------------------------------------------------------*)
  error, handler_bdu,
  {
    store_covering_classes = store_covering_classes;
    store_covering_classes_id = store_covering_classes_id;
    store_remanent_triple     = store_remanent_triple;
    store_wl_creation         = store_wl_creation;
    store_proj_bdu_creation_restriction_map = store_proj_bdu_creation_restriction_map;
    store_bdu_init_restriction_map          = store_bdu_init_restriction_map;
    store_modif_list_restriction_map        = store_modif_list_restriction_map;
    store_proj_bdu_potential_restriction_map = store_proj_bdu_potential_restriction_map;
    store_proj_bdu_test_restriction          = store_proj_bdu_test_restriction;

  }

(************************************************************************************)
(*dynamic views information*)

let scan_rule_dynamic parameter error handler rule_id rule compiled
    store_test_modification_map
    covering_classes
    store_covering_classes_id
    store_side_effects
    store_potential_side_effects
    store_result =
  (*------------------------------------------------------------------------------*)
  (*contact map dynamic*)
  let error, store_contact_map_full =
    Bdu_contact_map.compute_contact_map_full
      parameter
      error
      handler
      rule
  in
  (*------------------------------------------------------------------------------*)
  (*syntactic contact map*)
  let error, store_syn_contact_map_full =
    Bdu_contact_map.compute_syn_contact_map_full
      parameter
      error
      rule
      compiled
      store_result.store_syn_contact_map_full
  in
  (*-------------------------------------------------------------------------------*)
  (*final update function*)
  let error, store_covering_classes_modification_update_full =
    Bdu_update.store_covering_classes_modification_update_full
      parameter
      error
      store_test_modification_map
      store_potential_side_effects
      store_covering_classes_id
      covering_classes
      store_result.store_covering_classes_modification_update_full
  in
  (*-------------------------------------------------------------------------------*)
  error, 
  {
    store_contact_map_full                     = store_contact_map_full;
    store_syn_contact_map_full                 = store_syn_contact_map_full;
    store_covering_classes_modification_update_full = 
      store_covering_classes_modification_update_full;
  }

(************************************************************************************)
(*rule*)

let scan_rule parameter handler_bdu error handler_kappa rule_id rule compiled 
    store_result =
  (*-------------------------------------------------------------------------------*)
  let error, store_bdu_common_static =
    scan_rule_common
      parameter
      error
      handler_kappa
      rule_id
      rule
      store_result.store_bdu_common_static
  in
  (*-------------------------------------------------------------------------------*)
  let error, handler_bdu, store_pre_static =
    scan_rule_pre_static
      parameter
      error
      rule_id
      rule
      handler_bdu
      store_result.store_pre_static      
  in
  (*-------------------------------------------------------------------------------*)
  let error, handler_bdu, store_bdu_analysis_static =
    scan_rule_static 
      parameter
      error 
      handler_kappa
      handler_bdu
      rule_id
      rule 
      store_bdu_common_static.store_potential_side_effects
      compiled
      store_result.store_bdu_analysis_static
  in
  (*-------------------------------------------------------------------------------*)
  let error, store_bdu_analysis_dynamic =
    scan_rule_dynamic
      parameter
      error
      handler_kappa
      rule_id
      rule
      compiled
      store_pre_static.store_test_modif_map
      store_bdu_analysis_static.store_covering_classes
      store_bdu_analysis_static.store_covering_classes_id
      store_bdu_common_static.store_side_effects
      store_bdu_common_static.store_potential_side_effects
      store_result.store_bdu_analysis_dynamic
  in
  error, (handler_bdu, 
  {
    store_bdu_common_static    = store_bdu_common_static;
    store_pre_static           = store_pre_static;
    store_bdu_analysis_static  = store_bdu_analysis_static;
    store_bdu_analysis_dynamic = store_bdu_analysis_dynamic;
  })
 
(************************************************************************************)
(*intitial state of global static information*)

let init_bdu_common_static =
  let init_half_break          = Int2Map_HalfBreak_effect.Map.empty  in
  let init_remove              = Int2Map_Remove_effect.Map.empty  in
  let init_potential_free      = Int2Map_potential_effect.Map.empty in
  let init_potential_bind      = Int2Map_potential_effect.Map.empty in
  let init_common_static =
    {
      store_side_effects            = (init_half_break, init_remove);
      store_potential_side_effects  = (init_potential_free, init_potential_bind); 
    }
  in
  init_common_static

let init_pre_static =
  let init_modification        = Int2Map_Modif.Map.empty in
  let init_test                = Int2Map_Modif.Map.empty in
  let init_test_modification   = Int2Map_Modif.Map.empty in
  let init_modif_map           = Int2Map_Test_Modif.Map.empty in
  let init_test_map            = Int2Map_Test_Modif.Map.empty in
  let init_test_modif_map      = Int2Map_Test_Modif.Map.empty in
  let init_pre_static =
    {
      store_modification_sites      = init_modification;
      store_test_sites              = init_test;
      store_test_modification_sites = init_test_modification;
      store_modif_map               = init_modif_map;
      store_test_map                = init_test_map;
      store_test_modif_map          = init_test_modif_map;
    }
  in init_pre_static

(**initial of static views information*)

let init_bdu_analysis_static parameter error =
  let error, init_covering_classes =
    Covering_classes_type.AgentMap.create parameter error 0 
  in
  let init_covering_classes_id = Int2Map_CV.Map.empty in
  let error, init_remanent_triple            = AgentMap.create parameter error 0 in
  let init_wl_creation                       = IntWL.empty in
  let init_proj_bdu_creation_restriction_map = Map_final_creation_bdu.Map.empty in
  let init_bdu_init_restriction_map          = Map_init_bdu.Map.empty in
  let init_modif_list_restriction_map        = Map_modif_list.Map.empty in
  let init_proj_bdu_potential_restriction_map  = Map_final_potential_bdu.Map.empty in
  let init_proj_bdu_test_restriction           = Map_rule_id_views.Map.empty in
  let init_bdu_analysis_static =
    {
      store_covering_classes = init_covering_classes;
      store_covering_classes_id = init_covering_classes_id;
      store_remanent_triple     = init_remanent_triple;
      store_wl_creation         = init_wl_creation;
      store_proj_bdu_creation_restriction_map = init_proj_bdu_creation_restriction_map;
      store_bdu_init_restriction_map          = init_bdu_init_restriction_map;
      store_modif_list_restriction_map        = init_modif_list_restriction_map;
      store_proj_bdu_potential_restriction_map = init_proj_bdu_potential_restriction_map;
      store_proj_bdu_test_restriction = init_proj_bdu_test_restriction;
    }
  in
  error, init_bdu_analysis_static
      
(************************************************************************************)
(*intitial state of dynamic analysis*)

let init_bdu_analysis_dynamic parameter error =
  let init_contact_map_full = Int2Map_CM_state.Map.empty in
  let init_syn_contact_map  = Int2Map_CM_Syntactic.Map.empty in
  let init_cv_modification_full = Int2Map_CV_Modif.Map.empty in
  let init_bdu_analysis_dynamic =
    {
      store_contact_map_full     = init_contact_map_full;
      store_syn_contact_map_full = init_syn_contact_map;      
      store_covering_classes_modification_update_full  = init_cv_modification_full;
    }
  in
  error, init_bdu_analysis_dynamic

(************************************************************************************)
(*rules*)

let scan_rule_set parameter handler_bdu error handler_kappa compiled rules =
  let error, init_bdu_analysis_static  = init_bdu_analysis_static parameter error in
  let error, init_bdu_analysis_dynamic = init_bdu_analysis_dynamic parameter error in
  let init_bdu =
    {
      store_bdu_common_static    = init_bdu_common_static;
      store_pre_static           = init_pre_static;
      store_bdu_analysis_static  = init_bdu_analysis_static;
      store_bdu_analysis_dynamic = init_bdu_analysis_dynamic;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, (handler_bdu, store_results) =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule (handler_bdu,store_result) ->
       scan_rule
            parameter
	    handler_bdu 
            error
	    handler_kappa
            rule_id
            rule.e_rule_c_rule
            compiled
            store_result
      ) rules (handler_bdu,init_bdu)
  in
  error, (handler_bdu,store_results)

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler_kappa compiled =
  let error,handler_bdu = Boolean_mvbdu.init_remanent parameter error in
  let error, (handler_bdu, result) =
    scan_rule_set
      parameter
      handler_bdu
      error 
      handler_kappa 
      compiled
      compiled.rules 
  in
  (*-------------------------------------------------------------------------------*)
  (* Static information before fixpoint computation *)
  let error = 
    if  (Remanent_parameters.get_trace parameter) || trace
    then Print_bdu_analysis.print_result parameter error handler_kappa compiled result
    else error
  in
  (*--------------------------------------------------------------------*)
  (*discover dead rule; an initial array is false everywhere*)
  let nrules = Handler.nrules parameter error handler_kappa in
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
      result.store_bdu_analysis_static.store_remanent_triple
      result.store_bdu_analysis_static.store_wl_creation
      result.store_bdu_analysis_static.store_proj_bdu_creation_restriction_map
      result.store_bdu_analysis_static.store_modif_list_restriction_map
      result.store_bdu_analysis_static.store_proj_bdu_potential_restriction_map
      result.store_bdu_analysis_static.store_proj_bdu_test_restriction
      result.store_bdu_analysis_dynamic.store_covering_classes_modification_update_full
      result.store_bdu_analysis_static.store_bdu_init_restriction_map
      init_dead_rule_array
  in
  (*-----------------------------------------------------------------------*)
  let error, handler_bdu =
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
	result.store_bdu_analysis_static.store_remanent_triple
	store_bdu_fixpoint
    else error, handler_bdu
  in
  error, handler_bdu, result
