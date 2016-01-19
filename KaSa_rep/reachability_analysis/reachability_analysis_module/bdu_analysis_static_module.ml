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

open Bdu_analysis_static_type

(*******************************************************************************)
(*signature of module*)

module type Bdu_analysis_Static =
sig
  
  type handler = 
    (Boolean_mvbdu.memo_tables,
     Boolean_mvbdu.mvbdu_dic,
     Boolean_mvbdu.association_list_dic,
     Boolean_mvbdu.variables_list_dic,
     bool, int) Memo_sig.handler

  val init_bdu_analysis_static : Exception.method_handler
    -> Exception.method_handler * bdu_analysis_static

  val site_covering_classes : Remanent_parameters_sig.parameters
    -> Exception.method_handler 
    -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
    -> Exception.method_handler * 
    (int list * int list) Int2Map_CV.Map.t
      
  val collect_side_effects : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Cckappa_sig.kappa_handler
    -> int
    -> Cckappa_sig.rule
    -> half_break_action * remove_action
    -> Exception.method_handler * (half_break_action * remove_action)

  val collect_potential_side_effects: Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Cckappa_sig.kappa_handler
    -> int
    -> Cckappa_sig.rule
    -> potential_partner_free * potential_partner_bind
    -> Exception.method_handler * (potential_partner_free * potential_partner_bind)

  val collect_modification_sites : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> int
    -> Cckappa_sig.rule
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> Exception.method_handler * 
    (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t

  val collect_test_sites : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> int
    -> Cckappa_sig.rule
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> Exception.method_handler * 
    (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t

  val collect_test_modification_sites : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> Exception.method_handler *
    (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t

  val collect_modif_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> Exception.method_handler *
    (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t

  val collect_test_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> Exception.method_handler *
    (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t
      
  val collect_test_modif_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Modif.Map.t
    -> Exception.method_handler *
    (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t      

  val scan_rule_static : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Cckappa_sig.kappa_handler
    -> int
    -> Cckappa_sig.rule
    -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
    -> bdu_analysis_static
    -> Exception.method_handler * bdu_analysis_static

end

(*******************************************************************************)
(*struture of module*)

module Bdu_analysis_Static =
  (struct
      
    type handler = 
      (Boolean_mvbdu.memo_tables,
       Boolean_mvbdu.mvbdu_dic,
       Boolean_mvbdu.association_list_dic,
       Boolean_mvbdu.variables_list_dic,
       bool, int) Memo_sig.handler

    let init_bdu_analysis_static error =
      let init_covering_classes_id = Int2Map_CV.Map.empty in
      let init_half_break          = Int2Map_HalfBreak_effect.Map.empty  in
      let init_remove              = Int2Map_Remove_effect.Map.empty  in
      let init_potential_free      = Int2Map_potential_effect.Map.empty in
      let init_potential_bind      = Int2Map_potential_effect.Map.empty in
      let init_modification        = Int2Map_Modif.Map.empty in
      let init_test                = Int2Map_Modif.Map.empty in
      let init_test_modification   = Int2Map_Modif.Map.empty in
      let init_modif_map           = Int2Map_Test_Modif.Map.empty in
      let init_test_map            = Int2Map_Test_Modif.Map.empty in
      let init_test_modif_map      = Int2Map_Test_Modif.Map.empty in
      error, 
      {
        store_covering_classes_id    = init_covering_classes_id;
        store_side_effects           = (init_half_break, init_remove);
        store_potential_side_effects = (init_potential_free, init_potential_bind);
        store_modification_sites     = init_modification;
        store_test_sites             = init_test;
        store_test_modification_sites = init_test_modification;
        store_modif_map               = init_modif_map;
        store_test_map                = init_test_map;
        store_test_modif_map          = init_test_modif_map;
      }

    let site_covering_classes parameter error covering_classes =
      Bdu_analysis_static_operations.site_covering_classes parameter error covering_classes
        
    let collect_side_effects parameter error handler_kappa rule_id rule store_result =
      Bdu_analysis_static_operations.collect_side_effects parameter error
        handler_kappa rule_id rule store_result
        
    let collect_potential_side_effects parameter error handler_kappa rule_id rule
        store_result =
      Bdu_analysis_static_operations.collect_potential_side_effects
        parameter error handler_kappa rule_id rule store_result

    let collect_modification_sites parameter error rule_id rule store_result =
      Bdu_analysis_static_operations.collect_modification_sites
        parameter error rule_id rule store_result

    let collect_test_sites parameter error rule_id rule store_result =
      Bdu_analysis_static_operations.collect_test_sites
        parameter error rule_id rule store_result

    let collect_test_modification_sites parameter error store_modification_sites
        store_test_sites store_result =
      Bdu_analysis_static_operations.collect_test_modification_sites
        parameter error store_modification_sites store_test_sites store_result

    let collect_modif_map parameter error store_modification_sites =
      Bdu_analysis_static_operations.collect_modif_map
        parameter error store_modification_sites
        
    let collect_test_map parameter error store_test_sites =
      Bdu_analysis_static_operations.collect_test_map
        parameter error store_test_sites

    let collect_test_modif_map parameter error store_test_modification_sites =
      Bdu_analysis_static_operations.collect_test_modif_map
        parameter error store_test_modification_sites

    let scan_rule_static parameter error handler_kappa rule_id rule covering_classes
        store_result=
      let error, store_covering_classes_id = 
        site_covering_classes
          parameter
          error
          covering_classes
      in
      let error, store_side_effects =
        collect_side_effects
          parameter
          error
          handler_kappa
          rule_id
          rule
          store_result.store_side_effects
      in
      let error, store_potential_side_effects =
        collect_potential_side_effects
          parameter
          error
          handler_kappa
          rule_id
          rule
          store_result.store_potential_side_effects
      in
      let error, store_modification_sites =
        collect_modification_sites
          parameter
          error
          rule_id
          rule
          store_result.store_modification_sites
      in
      let error, store_test_sites =
        collect_test_sites
          parameter
          error
          rule_id
          rule
          store_result.store_test_sites
      in
      let error, store_test_modification_sites =
        collect_test_modification_sites
          parameter
          error
          store_modification_sites
          store_test_sites
          store_result.store_test_modification_sites
      in
      let error, store_modif_map =
        collect_modif_map
          parameter
          error
          store_modification_sites
      in
      let error, store_test_map =
        collect_test_map
          parameter
          error
          store_test_sites
      in
      let error, store_test_modif_map =
        collect_test_modif_map
          parameter
          error
          store_test_modification_sites
      in
      error, 
      {
        store_covering_classes_id    = store_covering_classes_id;
        store_side_effects           = store_side_effects;
        store_potential_side_effects = store_potential_side_effects;
        store_modification_sites     = store_modification_sites;
        store_test_sites             = store_test_sites;
        store_test_modification_sites = store_test_modification_sites;
        store_modif_map               = store_modif_map;
        store_test_map                = store_test_map;
        store_test_modif_map          = store_test_modif_map;
      }

   end:Bdu_analysis_Static)
