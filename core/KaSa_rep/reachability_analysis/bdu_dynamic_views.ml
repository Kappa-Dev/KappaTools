(**
   * bdu_dynamic_views.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 18th of Feburary
   * Last modification: Time-stamp: <Aug 17 2018>
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

type bdu_analysis_dynamic = {
  store_update:
    Ckappa_sig.Rule_map_and_set.Set.t
    Covering_classes_type.AgentCV_map_and_set.Map.t;
  (*FIXME: compute dual contact map here*)
  store_dual_contact_map:
    Ckappa_sig.AgentSiteState_map_and_set.Set.t
    Ckappa_sig.AgentSiteState_map_and_set.Map.t;
}

(************************************************************************)
(*implementation*)

let add_link parameters error (agent_type, cv_id) rule_id_set store_result =
  let error, old_set =
    match
      Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
        parameters error (agent_type, cv_id) store_result
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error', new_set =
    Ckappa_sig.Rule_map_and_set.Set.union parameters error rule_id_set old_set
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error, store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite parameters
      error (agent_type, cv_id) new_set store_result
  in
  error, store_result

(**************************************************************************)

let store_covering_classes_modification_update_aux parameters error
    agent_type_cv site_type_cv cv_id store_test_modification_map store_result =
  (*----------------------------------------------------------------------*)
  let error, rule_id_set =
    match
      Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameters
        error
        (agent_type_cv, site_type_cv)
        store_test_modification_map
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, result =
    add_link parameters error (agent_type_cv, cv_id) rule_id_set store_result
  in
  (*----------------------------------------------------------------------*)
  (*map this map*)
  let store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.map (fun x -> x) result
  in
  error, store_result

(***************************************************************************)

let store_covering_classes_modification_update parameters error
    store_test_modification_map store_covering_classes_id =
  let error, store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.fold
      (fun (agent_type_cv, site_type_cv) l2 store_result ->
        List.fold_left
          (fun (error, store_current_result) cv_id ->
            let error, result =
              store_covering_classes_modification_update_aux parameters error
                agent_type_cv site_type_cv cv_id store_test_modification_map
                store_current_result
            in
            error, result)
          store_result l2
        (*REMARK: when it is folding inside a list, start with empty result,
          because the add_link function has already called the old result.*))
      store_covering_classes_id
      (error, Covering_classes_type.AgentCV_map_and_set.Map.empty)
  in
  let store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.map (fun x -> x) store_result
  in
  error, store_result

(**************************************************************************)
(*combine update(c) and update(c') of side effects together*)

(***************************************************************************)
(*update function added information of rule_id in side effects*)

let store_covering_classes_modification_side_effects parameters error
    store_test_modification_map store_potential_side_effects covering_classes
    store_result =
  (*------------------------------------------------------------------------*)
  let error, store_result =
    Ckappa_sig.AgentRule_map_and_set.Map.fold
      (fun (agent_type_partner, rule_id_effect) pair_list (error, store_result) ->
        List.fold_left
          (fun (error, store_result) (_, (site_type_partner, _state)) ->
            let error, rule_id_set =
              match
                Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                  parameters error
                  (agent_type_partner, site_type_partner)
                  store_test_modification_map
              with
              | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
              | error, Some s -> error, s
            in
            let error, new_rule_id_set =
              Ckappa_sig.Rule_map_and_set.Set.add_when_not_in parameters error
                rule_id_effect rule_id_set
            in
            let error, store_result =
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
                parameters error
                (fun parameters error _agent_type_cv remanent store_result ->
                  let cv_dic = remanent.Covering_classes_type.store_dic in
                  let error, store_result =
                    Covering_classes_type.Dictionary_of_List_sites.fold
                      (fun _list_of_site_type ((), ()) cv_id
                           (error, store_result) ->
                        (*get a set of rule_id in update(c)*)
                        let error, store_result =
                          add_link parameters error
                            (agent_type_partner, cv_id)
                            new_rule_id_set store_result
                        in
                        error, store_result)
                      cv_dic (error, store_result)
                  in
                  error, store_result)
                covering_classes store_result
            in
            error, store_result)
          (error, store_result) pair_list)
      store_potential_side_effects (error, store_result)
  in
  error, store_result

(**************************************************************************)

let store_update parameters log_info error store_test_modification_map
    store_potential_side_effects store_covering_classes_id covering_classes
    store_result =
  let error, log_info =
    StoryProfiling.StoryStats.add_event parameters error
      StoryProfiling.Regular_influences None log_info
  in
  let error, store_update_modification =
    store_covering_classes_modification_update parameters error
      store_test_modification_map store_covering_classes_id
  in
  let error, log_info =
    StoryProfiling.StoryStats.close_event parameters error
      StoryProfiling.Regular_influences None log_info
  in
  let init_cv_modification_side_effects =
    Covering_classes_type.AgentCV_map_and_set.Map.empty
  in
  let error, log_info =
    StoryProfiling.StoryStats.add_event parameters error
      StoryProfiling.Side_effects_influences None log_info
  in
  let error, store_update_with_side_effects =
    store_covering_classes_modification_side_effects parameters error
      store_test_modification_map store_potential_side_effects covering_classes
      init_cv_modification_side_effects
  in
  let error, log_info =
    StoryProfiling.StoryStats.close_event parameters error
      StoryProfiling.Side_effects_influences None log_info
  in
  let error, log_info =
    StoryProfiling.StoryStats.add_event parameters error
      StoryProfiling.Merge_influences None log_info
  in
  (*-------------------------------------------------------------------*)
  (*fold 2 map*)
  let error, store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.fold2 parameters error
      (*exists in 'a t*)
        (fun parameters error (agent_type, cv_id) rule_id_set store_result ->
        let error, store_result =
          add_link parameters error (agent_type, cv_id) rule_id_set store_result
        in
        error, store_result)
      (*exists in 'b t*)
        (fun parameters error (agent_type, cv_id) rule_id_set store_result ->
        let error, store_result =
          add_link parameters error (agent_type, cv_id) rule_id_set store_result
        in
        error, store_result)
      (*both*)
        (fun parameters error (agent_type, cv_id) s1 s2 store_result ->
        let error, union_set =
          Ckappa_sig.Rule_map_and_set.Set.union parameters error s1 s2
        in
        let error, store_result =
          add_link parameters error (agent_type, cv_id) union_set store_result
        in
        error, store_result)
      store_update_modification store_update_with_side_effects store_result
  in
  let error, log_info =
    StoryProfiling.StoryStats.close_event parameters error
      StoryProfiling.Merge_influences None log_info
  in
  error, log_info, store_result

(**************************************************************************)
(*compute dual contact map*)

(*compute dual in contact map to be used to check the bond in the pattern
  (lhs)*)

let collect_dual_map parameters error handler store_result =
  let error, store_result =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .fold parameters error
      (fun parameters error (agent_type, (site_type, state))
           (agent_type', site_type', state') store_result ->
        let error, old_set =
          match
            Ckappa_sig.AgentSiteState_map_and_set.Map.find_option_without_logs
              parameters error
              (agent_type, site_type, state)
              store_result
          with
          | error, None ->
            error, Ckappa_sig.AgentSiteState_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error', set =
          Ckappa_sig.AgentSiteState_map_and_set.Set.add_when_not_in parameters
            error
            (agent_type', site_type', state')
            Ckappa_sig.AgentSiteState_map_and_set.Set.empty
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error', new_set =
          Ckappa_sig.AgentSiteState_map_and_set.Set.union parameters error set
            old_set
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error, store_result =
          Ckappa_sig.AgentSiteState_map_and_set.Map.add_or_overwrite parameters
            error
            (agent_type, site_type, state)
            new_set store_result
        in
        error, store_result)
      handler.Cckappa_sig.dual store_result
  in
  error, store_result

(****************************************************************************)

let scan_rule_dynamic parameters log_info error _compiled kappa_handler
    handler_bdu covering_classes store_covering_classes_id
    store_potential_side_effects store_test_modif_map store_result =
  let error, log_info, store_update =
    store_update parameters log_info error store_test_modif_map
      store_potential_side_effects store_covering_classes_id covering_classes
      store_result.store_update
  in
  let error, store_dual_contact_map =
    collect_dual_map parameters error kappa_handler
      store_result.store_dual_contact_map
  in
  error, handler_bdu, log_info, { store_update; store_dual_contact_map }

(**************************************************************************)

let init_bdu_analysis_dynamic =
  let init_bdu_analysis_dynamic =
    {
      store_update = Covering_classes_type.AgentCV_map_and_set.Map.empty;
      store_dual_contact_map = Ckappa_sig.AgentSiteState_map_and_set.Map.empty;
    }
  in
  init_bdu_analysis_dynamic

(**************************************************************************)
(*rules*)

let scan_rule_set_dynamic parameters log_info error compiled kappa_handler
    handler_bdu store_test_modif_map covering_classes store_covering_classes_id
    store_potential_side_effects =
  let error, handler_bdu, log_info, store_result =
    scan_rule_dynamic parameters log_info error compiled kappa_handler
      handler_bdu covering_classes store_covering_classes_id
      store_potential_side_effects store_test_modif_map
      init_bdu_analysis_dynamic
  in
  error, (handler_bdu, log_info, store_result)
