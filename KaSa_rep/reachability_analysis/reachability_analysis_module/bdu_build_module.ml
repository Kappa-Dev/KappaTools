(**
  * bdu_analysis_main.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 20th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Bdu_build_type

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)

let trace = false

(*******************************************************************************)
(*signatures of module*)

module type Bdu_Build =
sig

  val init_bdu_build : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Exception.method_handler * bdu_build

  val collect_remanent_triple : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
    -> ((int * int list * Cckappa_sig.Site_map_and_set.Set.t) list) AgentMap.t
    -> Exception.method_handler *
    ((int * int list * Cckappa_sig.Site_map_and_set.Set.t) list) AgentMap.t

  val collect_wl_creation : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> int
    -> Cckappa_sig.rule
    -> wl_int
    -> Exception.method_handler * wl_int

  val collect_bdu_test_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> int
    -> Cckappa_sig.rule
    -> ((int * int list * Cckappa_sig.Site_map_and_set.Set.t) list) AgentMap.t
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t
    -> Exception.method_handler *
    (Mvbdu_wrapper.Mvbdu.handler * Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t)

  val collect_proj_bdu_test_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t
    -> (Exception.method_handler * Mvbdu_wrapper.Mvbdu.handler) *
    Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_id_test_bdu.Map.t Map_final_test_bdu.Map.t

  val collect_bdu_creation_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> int
    -> Cckappa_sig.rule
    -> ((int * Cckappa_sig.Site_map_and_set.Map.elt list *
           Cckappa_sig.Site_map_and_set.Set.t) list) AgentMap.t
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t
    -> Exception.method_handler *
    (Mvbdu_wrapper.Mvbdu.handler * Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t)

  val collect_proj_bdu_creation_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t
    -> (Exception.method_handler * Mvbdu_wrapper.Mvbdu.handler) *
    Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_type_creation_bdu.Map.t Map_final_creation_bdu.Map.t

  val collect_bdu_init_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> Cckappa_sig.compil
    -> ((int *  Cckappa_sig.Site_map_and_set.Map.elt list *
           Cckappa_sig.Site_map_and_set.Set.t) list) AgentMap.t
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_init_bdu.Map.t
    -> Exception.method_handler *
    (Mvbdu_wrapper.Mvbdu.handler * Mvbdu_wrapper.Mvbdu.mvbdu Map_init_bdu.Map.t)

  val collect_modif_list_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> int
    -> Cckappa_sig.rule
    -> ((int * Cckappa_sig.Site_map_and_set.Map.elt list *
           Cckappa_sig.Site_map_and_set.Set.t) list) AgentMap.t
    -> Mvbdu_wrapper.Mvbdu.hconsed_association_list Map_modif_list.Map.t
    -> Exception.method_handler *
    (Mvbdu_wrapper.Mvbdu.handler *
       Mvbdu_wrapper.Mvbdu.hconsed_association_list Map_modif_list.Map.t)

  val collect_bdu_potential_effect_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> (int * Cckappa_sig.Site_map_and_set.Map.elt list *
          Cckappa_sig.Site_map_and_set.Set.t) list AgentMap.t
    -> Bdu_analysis_static_type.potential_partner_free *
    Bdu_analysis_static_type.potential_partner_bind
    -> (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
    Map_potential_bdu.Map.t
    -> Exception.method_handler *
    (Mvbdu_wrapper.Mvbdu.handler *
       (Mvbdu_wrapper.Mvbdu.mvbdu *
          Mvbdu_wrapper.Mvbdu.hconsed_association_list)
            Map_potential_bdu.Map.t)

  val collect_proj_bdu_potential_restriction_map : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
      Map_potential_bdu.Map.t
    -> (Exception.method_handler * Mvbdu_wrapper.Mvbdu.handler) *
     (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
      Map_agent_type_potential_bdu.Map.t Map_final_potential_bdu.Map.t

  val collect_proj_bdu_views : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t
    -> (Exception.method_handler * Mvbdu_wrapper.Mvbdu.handler) *
    Mvbdu_wrapper.Mvbdu.mvbdu Map_triple_views.Map.t Map_rule_id_views.Map.t

  val scan_rule_bdu_build : Remanent_parameters_sig.parameters
    -> Mvbdu_wrapper.Mvbdu.handler
    -> Exception.method_handler
    -> Fifo.IntWL.WSet.elt
    -> Cckappa_sig.rule
    -> Cckappa_sig.compil
    -> Covering_classes_type.remanent Bdu_build_type.AgentMap.t
    -> Bdu_analysis_static_type.potential_partner_free *
    Bdu_analysis_static_type.potential_partner_bind
    -> bdu_build
    -> Exception.method_handler * Mvbdu_wrapper.Mvbdu.handler * bdu_build

  val print_remanent_triple : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (int * Cckappa_sig.Site_map_and_set.Map.elt list *
          Cckappa_sig.Site_map_and_set.Set.t) list AgentMap.t
    -> Exception.method_handler

  val print_wl_creation : Remanent_parameters_sig.parameters
    -> wl_int
    -> unit

  val print_bdu_test_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t
    -> unit

  val print_proj_bdu_test_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_id_test_bdu.Map.t Map_final_test_bdu.Map.t
    -> unit

  val print_bdu_creation_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t
    -> unit

  val print_proj_bdu_creation_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_type_creation_bdu.Map.t
    Map_final_creation_bdu.Map.t
      -> unit

  val print_bdu_init_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_init_bdu.Map.t
    -> unit

  val print_modif_list_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.hconsed_association_list Map_modif_list.Map.t
    -> unit

  val print_bdu_potential_effect_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
    Map_potential_bdu.Map.t
    -> unit

  val print_proj_bdu_potential_restriction_map : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
    Map_agent_type_potential_bdu.Map.t Map_final_potential_bdu.Map.t
    -> unit

  val print_proj_bdu_views : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Mvbdu_wrapper.Mvbdu.mvbdu Map_triple_views.Map.t Map_rule_id_views.Map.t
    -> unit
end

(*******************************************************************************)
(*structures of module*)

module Bdu_Build =
  (struct

    let init_bdu_build parameter error =
      let error, init_remanent_triple            = AgentMap.create parameter error 0 in
      let init_wl_creation                       = Fifo.IntWL.empty in
      let init_bdu_test_restriction_map          = Map_test_bdu.Map.empty in
      let init_proj_bdu_test_restriction_map     = Map_final_test_bdu.Map.empty in
      let init_bdu_creation_restriction_map      = Map_creation_bdu.Map.empty in
      let init_proj_bdu_creation_restriction_map = Map_final_creation_bdu.Map.empty in
      let init_bdu_init_restriction_map          = Map_init_bdu.Map.empty in
      let init_modif_list_restriction_map        = Map_modif_list.Map.empty in
      let init_bdu_potential_restriction_map       = Map_potential_bdu.Map.empty in
      let init_proj_bdu_potential_restriction_map  = Map_final_potential_bdu.Map.empty in
      let init_proj_bdu_views                      = Map_rule_id_views.Map.empty in
      let init_build =
        {
          store_remanent_triple                   = init_remanent_triple;
          store_wl_creation                       = init_wl_creation;
          store_bdu_test_restriction_map          = init_bdu_test_restriction_map;
          store_proj_bdu_test_restriction_map     = init_proj_bdu_test_restriction_map;
          store_bdu_creation_restriction_map      = init_bdu_creation_restriction_map;
          store_proj_bdu_creation_restriction_map = init_proj_bdu_creation_restriction_map;
          store_bdu_init_restriction_map          = init_bdu_init_restriction_map;
          store_modif_list_restriction_map        = init_modif_list_restriction_map;
          store_bdu_potential_effect_restriction_map = init_bdu_potential_restriction_map;
          store_proj_bdu_potential_restriction_map = init_proj_bdu_potential_restriction_map;
          store_proj_bdu_views                     = init_proj_bdu_views;
        }
      in
      error, init_build

    let collect_remanent_triple parameter error covering_classes store_result =
      Bdu_build_operations.collect_remanent_triple parameter error
        covering_classes store_result

    let collect_wl_creation parameter error rule_id rule store_result =
      Bdu_build_operations.collect_wl_creation parameter error rule_id rule store_result

    let collect_bdu_test_restriction_map parameter handler_bdu error rule_id rule
        store_remanent_triple store_result =
      Bdu_build_operations.collect_bdu_test_restriction_map parameter handler_bdu error
        rule_id rule store_remanent_triple store_result

    let collect_proj_bdu_test_restriction_map parameter handler_bdu error
          store_bdu_test_restriction_map =
      Bdu_build_operations.collect_proj_bdu_test_restriction_map parameter handler_bdu error
        store_bdu_test_restriction_map

    let collect_bdu_creation_restriction_map parameter handler_bdu error rule_id
        rule store_remanent_triple store_result =
      Bdu_build_operations.collect_bdu_creation_restriction_map parameter handler_bdu
        error rule_id rule store_remanent_triple store_result

    let collect_proj_bdu_creation_restriction_map parameter handler_bdu error
        store_bdu_creation_restriction_map =
      Bdu_build_operations.collect_proj_bdu_creation_restriction_map parameter handler_bdu
        error store_bdu_creation_restriction_map

    let collect_bdu_init_restriction_map parameter handler_bdu error compil
        store_remanent_triple store_result =
      Bdu_build_operations.collect_bdu_init_restriction_map parameter handler_bdu error
        compil store_remanent_triple store_result

    let collect_modif_list_restriction_map parameter handler_bdu error rule_id rule
          store_remanent_triple store_result =
      Bdu_build_operations.collect_modif_list_restriction_map parameter handler_bdu error
        rule_id rule store_remanent_triple store_result

    let collect_bdu_potential_effect_restriction_map parameter handler_bdu error
        store_remanent_triple store_potential_side_effects store_result =
      Bdu_build_operations.collect_bdu_potential_effect_restriction_map parameter
        handler_bdu error store_remanent_triple store_potential_side_effects store_result

    let collect_proj_bdu_potential_restriction_map parameter handler_bdu error
        store_bdu_potential_restriction_map =
      Bdu_build_operations.collect_proj_bdu_potential_restriction_map parameter
        handler_bdu error store_bdu_potential_restriction_map

    let collect_proj_bdu_views parameter handler_bdu error store_bdu_test_restriction_map =
      Bdu_build_operations.collect_proj_bdu_views parameter handler_bdu error
        store_bdu_test_restriction_map

    let scan_rule_bdu_build parameter handler_bdu error rule_id rule compil
        covering_classes store_potential_side_effects
        store_result =
      (*------------------------------------------------------------------------------*)
      let error, store_remanent_triple =
        (* JF: it should be computed only once, not for each rule *)
        collect_remanent_triple
          parameter
          error
          covering_classes
          store_result.store_remanent_triple
      in
      (*------------------------------------------------------------------------------*)
      (*working list*)
      let error, store_wl_creation =
        collect_wl_creation
          parameter
          error
          rule_id
          rule
          store_result.store_wl_creation
      in
      (*-------------------------------------------------------------------------------*)
      let error, (handler_bdu, store_bdu_test_restriction_map) =
        collect_bdu_test_restriction_map
          parameter
          handler_bdu
          error
          rule_id
          rule
          store_remanent_triple
          store_result.store_bdu_test_restriction_map
      in
      let (error, handler_bdu), store_proj_bdu_test_restriction_map =
        collect_proj_bdu_test_restriction_map
          parameter
          handler_bdu
          error
          store_bdu_test_restriction_map
      in
      (*-------------------------------------------------------------------------------*)
      let error, (handler_bdu, store_bdu_creation_restriction_map) =
        collect_bdu_creation_restriction_map
          parameter
          handler_bdu
          error
          rule_id
          rule
          store_remanent_triple
          store_result.store_bdu_creation_restriction_map
      in
      let (error, handler_bdu), store_proj_bdu_creation_restriction_map =
        collect_proj_bdu_creation_restriction_map
          parameter
          handler_bdu
          error
          store_bdu_creation_restriction_map
      in
      (*-------------------------------------------------------------------------------*)
      let error, (handler_bdu, store_bdu_init_restriction_map) =
        (* JF: it should be computed only once, not for each rule *)
        collect_bdu_init_restriction_map
          parameter
          handler_bdu
          error
          compil
          store_remanent_triple
          store_result.store_bdu_init_restriction_map
      in
      (*-------------------------------------------------------------------------------*)
      let error, (handler_bdu, store_modif_list_restriction_map) =
        collect_modif_list_restriction_map
          parameter
          handler_bdu
          error
          rule_id
          rule
          store_remanent_triple
          store_result.store_modif_list_restriction_map
      in
      (*-------------------------------------------------------------------------------*)
      let error, (handler_bdu, store_bdu_potential_effect_restriction_map) =
        collect_bdu_potential_effect_restriction_map
          parameter
          handler_bdu
          error
          store_remanent_triple
          store_potential_side_effects
          store_result.store_bdu_potential_effect_restriction_map
      in
      let (error, handler_bdu), store_proj_bdu_potential_restriction_map =
        collect_proj_bdu_potential_restriction_map
          parameter
          handler_bdu
          error
          store_bdu_potential_effect_restriction_map
      in
      (*-------------------------------------------------------------------------------*)
      let (error, handler_bdu), store_proj_bdu_views =
        collect_proj_bdu_views
          parameter
          handler_bdu
          error
          store_bdu_test_restriction_map
      in
      (*-------------------------------------------------------------------------------*)
      error, handler_bdu,
      {
        store_remanent_triple                   = store_remanent_triple;
        store_wl_creation                       = store_wl_creation;
        store_bdu_test_restriction_map          = store_bdu_test_restriction_map;
        store_proj_bdu_test_restriction_map     = store_proj_bdu_test_restriction_map;
        store_bdu_creation_restriction_map      = store_bdu_creation_restriction_map;
        store_proj_bdu_creation_restriction_map = store_proj_bdu_creation_restriction_map;
        store_bdu_init_restriction_map          = store_bdu_init_restriction_map;
        store_modif_list_restriction_map        = store_modif_list_restriction_map;
        store_bdu_potential_effect_restriction_map =
          store_bdu_potential_effect_restriction_map;
        store_proj_bdu_potential_restriction_map   =
          store_proj_bdu_potential_restriction_map;
        store_proj_bdu_views                       = store_proj_bdu_views;
      }

   (*PRINT SECTION*)

    let print_remanent_triple parameter error result =
      Bdu_build_operations.print_remanent_triple parameter error result

    let print_wl_creation parameter result =
      Bdu_build_operations.print_wl_creation parameter result

    let print_bdu_test_restriction_map parameter error result =
      Bdu_build_operations.print_bdu_test_restriction_map parameter error result

    let print_proj_bdu_test_restriction_map parameter error result =
      Bdu_build_operations.print_proj_bdu_test_restriction_map parameter error result

    let print_bdu_creation_restriction_map parameter error result =
      Bdu_build_operations.print_bdu_creation_restriction_map parameter error result

    let print_proj_bdu_creation_restriction_map parameter error result =
      Bdu_build_operations.print_proj_bdu_creation_restriction_map
        parameter error result

    let print_bdu_init_restriction_map parameter error result =
      Bdu_build_operations.print_bdu_init_restriction_map parameter error result

    let print_modif_list_restriction_map parameter error result =
      Bdu_build_operations.print_modif_list_restriction_map parameter error result

    let print_bdu_potential_effect_restriction_map parameter error result =
      Bdu_build_operations.print_bdu_potential_effect_restriction_map parameter error result

    let print_proj_bdu_potential_restriction_map parameter error result =
      Bdu_build_operations.print_proj_bdu_potential_restriction_map parameter error result

    let print_proj_bdu_views parameter error result =
      Bdu_build_operations.print_proj_bdu_views parameter error result

    let print_bdu_build parameter error result =
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------";
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "* Covering classes with new indexes:";
	Loggers.print_newline (Remanent_parameters.get_logger parameter);
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------";
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      let () =
        print_wl_creation
          parameter
          result.store_wl_creation
      in
      let () =
        print_proj_bdu_test_restriction_map
          parameter
          error
          result.store_proj_bdu_test_restriction_map
      in
      let () =
        print_proj_bdu_creation_restriction_map
          parameter
          error
          result.store_proj_bdu_creation_restriction_map
      in
      let () =
        print_modif_list_restriction_map
          parameter
          error
          result.store_modif_list_restriction_map
      in
      let () =
        print_proj_bdu_potential_restriction_map
          parameter
          error
          result.store_proj_bdu_potential_restriction_map
      in
      let () =
        print_proj_bdu_views
          parameter
          error
          result.store_proj_bdu_views
      in
      ()

   end:Bdu_Build)
