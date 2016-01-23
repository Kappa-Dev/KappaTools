(**
  * bdu_analysis_dynamic_module.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 20th of Januaray
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
open Bdu_analysis_dynamic_type

(*******************************************************************************)
(*signature of module*)

module type Bdu_analysis_Dynamic =
  sig

    val init_bdu_analysis_dynamic : Exception.method_handler
      -> Exception.method_handler * bdu_analysis_dynamic

    val compute_contact_map_full : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Exception.method_handler * Set_triple.Set.t Int2Map_CM_state.Map.t

    val compute_syn_contact_map_full : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.rule
      -> Cckappa_sig.compil
      -> Set_triple.Set.t Int2Map_CM_Syntactic.Map.t
      -> Exception.method_handler * Set_triple.Set.t Int2Map_CM_Syntactic.Map.t

    val collect_covering_classes_modification_update : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t
      -> (int list * int list) Int2Map_CV.Map.t
      -> Exception.method_handler *
      (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t

    val collect_covering_classes_modification_side_effects:
      Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t
      -> potential_partner_free * potential_partner_bind
      -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> Exception.method_handler * (int list * Cckappa_sig.Site_map_and_set.Set.t)
      Int2Map_CV_Modif.Map.t

    val collect_covering_classes_modification_update_full :
      Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> Exception.method_handler * (int list * Cckappa_sig.Site_map_and_set.Set.t)
      Int2Map_CV_Modif.Map.t

    val scan_rule_dynamic : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> int
      -> Cckappa_sig.rule
      -> Cckappa_sig.compil
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t
      -> (int list * int list) Int2Map_CV.Map.t
      -> potential_partner_free * potential_partner_bind
      -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
      -> bdu_analysis_dynamic
      -> Exception.method_handler * bdu_analysis_dynamic

    val print_contact_map_full : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Set_triple.Set.t Int2Map_CM_state.Map.t
      -> unit

    val print_syn_contact_map_full : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Set_triple.Set.t Int2Map_CM_Syntactic.Map.t
      -> unit

    val print_covering_classes_modification_update : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Cckappa_sig.compil
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> unit

    val print_covering_classes_modification_side_effects : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Cckappa_sig.compil
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> unit

    val print_covering_classes_modification_update_full : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Cckappa_sig.compil
      -> (int list * Cckappa_sig.Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t
      -> unit

    val print_bdu_analysis_dynamic : Remanent_parameters_sig.parameters
      -> Exception.method_handler
      -> Cckappa_sig.kappa_handler
      -> Cckappa_sig.compil
      -> bdu_analysis_dynamic
      -> unit

  end

(*******************************************************************************)
(*struture of module*)

module Bdu_analysis_Dynamic =
  (struct

    let init_bdu_analysis_dynamic error =
      let init_contact_map_full = Int2Map_CM_state.Map.empty in
      let init_syn_contact_map  = Int2Map_CM_Syntactic.Map.empty in
      let init_cv_modification  = Int2Map_CV_Modif.Map.empty in
      let init_cv_modification_side_effects  = Int2Map_CV_Modif.Map.empty in
      let init_cv_modification_full          = Int2Map_CV_Modif.Map.empty in
      let init_bdu_analysis_dynamic =
        {
          store_contact_map_full     = init_contact_map_full;
          store_syn_contact_map_full = init_syn_contact_map;
          store_covering_classes_modification_update       =
            init_cv_modification;
          store_covering_classes_modification_side_effects =
            init_cv_modification_side_effects;
          store_covering_classes_modification_update_full  =
            init_cv_modification_full;
        }
      in
      error, init_bdu_analysis_dynamic

    let compute_contact_map_full parameter error handler_kappa =
      Bdu_analysis_dynamic_operations.compute_contact_map_full parameter error handler_kappa

    let compute_syn_contact_map_full parameter error rule compiled store_result =
      Bdu_analysis_dynamic_operations.compute_syn_contact_map_full
        parameter error rule compiled store_result

    let collect_covering_classes_modification_update parameter error
        store_test_modification_map store_covering_classes_id =
      Bdu_analysis_dynamic_operations.collect_covering_classes_modification_update
        parameter error store_test_modification_map store_covering_classes_id

    let collect_covering_classes_modification_side_effects parameter error
        store_test_modification_map store_potential_side_effects covering_classes
        store_result =
      Bdu_analysis_dynamic_operations.collect_covering_classes_modification_side_effects
        parameter error store_test_modification_map store_potential_side_effects
        covering_classes store_result

    let collect_covering_classes_modification_update_full parameter error
        store_covering_classes_modification_update
        store_covering_classes_modification_side_effects store_result =
      Bdu_analysis_dynamic_operations.collect_covering_classes_modification_update_full
        parameter error store_covering_classes_modification_update
        store_covering_classes_modification_side_effects store_result

    let scan_rule_dynamic parameter error handler_kappa rule_id rule compiled
        store_test_modification_map store_covering_classes_id
        store_potential_side_effects covering_classes store_result =
      (*------------------------------------------------------------------------------*)
      (*contact map dynamic*)
      let error, store_contact_map_full =
        compute_contact_map_full
          parameter
          error
          handler_kappa
      in
      (*------------------------------------------------------------------------------*)
      (*syntactic contact map*)
      let error, store_syn_contact_map_full =
        compute_syn_contact_map_full
          parameter
          error
          rule
          compiled
          store_result.store_syn_contact_map_full
      in
      (*-------------------------------------------------------------------------------*)
      (*return a mapping of covering classes to a list of rules that has [modified and test]
        sites*)
      let error, store_covering_classes_modification_update =
        collect_covering_classes_modification_update
          parameter
          error
          store_test_modification_map
          store_covering_classes_id
      in
      (*-------------------------------------------------------------------------------*)
      (*update(c) in the case when discover side effects*)
      let error, store_covering_classes_modification_side_effects =
        collect_covering_classes_modification_side_effects
          parameter
          error
          store_test_modification_map
          store_potential_side_effects
          covering_classes
          store_result.store_covering_classes_modification_side_effects
      in
      (*-------------------------------------------------------------------------------*)
      (*final update function*)
      let error, store_covering_classes_modification_update_full =
        collect_covering_classes_modification_update_full
          parameter
          error
          store_covering_classes_modification_update
          store_covering_classes_modification_side_effects
          store_result.store_covering_classes_modification_update_full
      in
      (*-------------------------------------------------------------------------------*)
      error,
      {
        store_contact_map_full                     = store_contact_map_full;
        store_syn_contact_map_full                 = store_syn_contact_map_full;
        store_covering_classes_modification_update =
          store_covering_classes_modification_update;
        store_covering_classes_modification_side_effects =
          store_covering_classes_modification_side_effects;
        store_covering_classes_modification_update_full =
          store_covering_classes_modification_update_full;
      }

    (*PRINT SECTION*)

    let print_contact_map_full parameter error handler_kappa result =
      Bdu_analysis_dynamic_operations.print_contact_map_full parameter error
        handler_kappa result

    let print_syn_contact_map_full parameter error handler_kappa result =
      Bdu_analysis_dynamic_operations.print_syn_contact_map_full parameter error
        handler_kappa result

    let print_covering_classes_modification_update parameter error handler_kappa
        compiled result =
      Bdu_analysis_dynamic_operations.print_covering_classes_modification_update
        parameter error handler_kappa compiled result

    let print_covering_classes_modification_side_effects parameter error handler_kappa
        compiled result =
      Bdu_analysis_dynamic_operations.print_covering_classes_modification_side_effects
        parameter error handler_kappa compiled result

    let print_covering_classes_modification_update_full parameter error handler_kappa
        compiled result =
      Bdu_analysis_dynamic_operations.print_covering_classes_modification_update_full
        parameter error handler_kappa compiled result

    let print_bdu_analysis_dynamic parameter error handler_kappa compiled result =
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "============================================================";
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "* BDU Analysis:";
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "============================================================";
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "** Dynamic information:";
        Loggers.print_newline (Remanent_parameters.get_logger parameter);
	(*------------------------------------------------------------------------------*)
        let () =
          print_contact_map_full
            parameter
            error
            handler_kappa
            result.store_contact_map_full
        in
        (*------------------------------------------------------------------------------*)
        let () =
          print_syn_contact_map_full
            parameter
            error
            handler_kappa
            result.store_syn_contact_map_full
        in
        (*------------------------------------------------------------------------------*)
        let () =
          print_covering_classes_modification_update
            parameter
            error
            handler_kappa
            compiled
            result.store_covering_classes_modification_update
        in
        (*------------------------------------------------------------------------------*)
        let () =
          print_covering_classes_modification_side_effects
            parameter
            error
            handler_kappa
            compiled
            result.store_covering_classes_modification_side_effects
        in
        (*------------------------------------------------------------------------------*)
        let () =
          print_covering_classes_modification_update_full
            parameter
            error
            handler_kappa
            compiled
            result.store_covering_classes_modification_update_full
        in
        ()
      in
      ()

   end:Bdu_analysis_Dynamic)
