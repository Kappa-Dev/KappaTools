(**
   * bdu_dynamic_views.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 18th of Feburary
   * Last modification:
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_fixpoint_iteration") message exn
    (fun () -> default)

let local_trace = true

type bdu_analysis_dynamic =
  {
    store_update : Ckappa_sig.Rule_map_and_set.Set.t Covering_classes_type.AgentCV_map_and_set.Map.t;
    (*FIXME: compute dual contact map here*)
    store_dual_contact_map :
      Ckappa_sig.AgentSiteState_map_and_set.Set.t Ckappa_sig.AgentSiteState_map_and_set.Map.t
  }

(************************************************************************************)
(*implementation*)

let add_link parameter error (agent_type, cv_id) rule_id_set store_result =
  let error, old_set =
    match Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
      parameter error
      (agent_type, cv_id) store_result
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error', new_set =
    Ckappa_sig.Rule_map_and_set.Set.union parameter error rule_id_set old_set
  in
  let error =
    Exception.check warn parameter error error' (Some "line 46") Exit
  in
  let error, store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite
      parameter error (agent_type, cv_id)
      new_set
      store_result
  in
  error, store_result

(************************************************************************************)

let store_covering_classes_modification_update_aux parameter error agent_type_cv
    site_type_cv cv_id store_test_modification_map store_result =
  (*-------------------------------------------------------------------------------*)
  let error, rule_id_set =
    match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
      parameter error
      (agent_type_cv, site_type_cv)
      store_test_modification_map
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, result =
    add_link parameter error (agent_type_cv, cv_id) rule_id_set store_result
  in
  (*-------------------------------------------------------------------------------*)
  (*map this map*)
  let store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.map (fun x -> x) result
  in
  error, store_result

(************************************************************************************)

let store_covering_classes_modification_update parameter error
    store_test_modification_map
    store_covering_classes_id =
  let error, store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.fold
      (fun (agent_type_cv, site_type_cv) l2 store_result ->
        List.fold_left (fun (error, store_current_result) cv_id ->
          let error, result =
            store_covering_classes_modification_update_aux
              parameter
              error
              agent_type_cv
              site_type_cv
              cv_id
              store_test_modification_map
              store_current_result
          in
          error, result
        ) store_result l2
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) store_covering_classes_id
      (error,
       Covering_classes_type.AgentCV_map_and_set.Map.empty)
  in
  let store_result =
    Covering_classes_type.AgentCV_map_and_set.Map.map
      (fun x -> x) store_result
  in
  error, store_result

(************************************************************************************)
(*combine update(c) and update(c') of side effects together*)

(************************************************************************************)
(*update function added information of rule_id in side effects*)

let store_covering_classes_modification_side_effects parameter error
    store_test_modification_map
    store_potential_side_effects
    covering_classes
    store_result =
  (*-------------------------------------------------------------------------------*)
  let _, store_potential_side_effects_bind = store_potential_side_effects in
  let error, store_result =
    Ckappa_sig.AgentRule_map_and_set.Map.fold
      (fun (agent_type_partner, rule_id_effect) pair_list (error, store_result) ->
        List.fold_left (fun (error, store_result) (site_type_partner, state) ->
          let error, store_result =
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
              (fun parameter error agent_type_cv remanent store_result ->
                let cv_dic = remanent.Covering_classes_type.store_dic in
                let error, store_result =
                  Covering_classes_type.Dictionary_of_List_sites.fold
                    (fun list_of_site_type ((), ()) cv_id (error, store_result) ->
                      (*get a set of rule_id in update(c)*)
                      let error, rule_id_set =
                        match
                          Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                            parameter error
                            (agent_type_partner, site_type_partner)
                            store_test_modification_map
                        with
                        | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
                        | error, Some s -> error, s
                      in
                      let error, new_rule_id_set =
                        Ckappa_sig.Rule_map_and_set.Set.add_when_not_in
                          parameter
                          error
                          rule_id_effect
                          rule_id_set
                      in
                      let error, store_result =
                        add_link parameter error
                          (agent_type_partner, cv_id) new_rule_id_set store_result
                      in
                      error, store_result
                    ) cv_dic (error, store_result)
                in
                error, store_result
              ) covering_classes store_result
          in
          error, store_result
        ) (error, store_result) pair_list
      ) store_potential_side_effects_bind (error, store_result)
  in
  error, store_result

(************************************************************************************)

let store_update parameter error store_test_modification_map store_potential_side_effects
    store_covering_classes_id covering_classes store_result =
  let error, store_update_modification =
    store_covering_classes_modification_update
      parameter
      error
      store_test_modification_map
      store_covering_classes_id
  in
  let init_cv_modification_side_effects  =
    Covering_classes_type.AgentCV_map_and_set.Map.empty in
  let error, store_update_with_side_effects =
    store_covering_classes_modification_side_effects
      parameter
      error
      store_test_modification_map
      store_potential_side_effects
      covering_classes
      init_cv_modification_side_effects
  in
  (*---------------------------------------------------------------------------*)
  (*fold 2 map*)
  Covering_classes_type.AgentCV_map_and_set.Map.fold2
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) rule_id_set store_result ->
      let error, store_result =
        add_link parameter error (agent_type, cv_id) rule_id_set store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) rule_id_set store_result ->
      let error, store_result =
        add_link parameter error (agent_type, cv_id) rule_id_set store_result
      in
      error, store_result
    )
    (*both*)
    (fun parameter error (agent_type, cv_id) s1 s2 store_result ->
      let error, union_set =
        Ckappa_sig.Rule_map_and_set.Set.union parameter error s1 s2
      in
      let error, store_result =
        add_link parameter error (agent_type, cv_id) union_set store_result
      in
      error, store_result
    )
    store_update_modification
    store_update_with_side_effects
    store_result

(************************************************************************************)
(*compute dual contact map*)

(*compute dual in contact map to be used to check the bond in the pattern
  (lhs)*)

let collect_dual_map parameter error handler store_result =
  let error, store_result =
    Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent_type, (site_type, state))
        (agent_type', site_type', state') store_result ->
          let error, old_set =
            match Ckappa_sig.AgentSiteState_map_and_set.Map.find_option_without_logs parameter error
              (agent_type, site_type, state) store_result
            with
            | error, None -> error, Ckappa_sig.AgentSiteState_map_and_set.Set.empty
            | error, Some s -> error, s
          in
          let error', set =
            Ckappa_sig.AgentSiteState_map_and_set.Set.add_when_not_in parameter error
              (agent_type', site_type', state')
              Ckappa_sig.AgentSiteState_map_and_set.Set.empty
          in
          let error = Exception.check warn parameter error error' (Some "line 1109") Exit
          in
          let error', new_set =
            Ckappa_sig.AgentSiteState_map_and_set.Set.union parameter error set old_set
          in
          let error = Exception.check warn parameter error error' (Some "line 1115") Exit
          in
          let error, store_result =
            Ckappa_sig.AgentSiteState_map_and_set.Map.add_or_overwrite
              parameter
              error
              (agent_type, site_type, state)
              new_set
              store_result
          in
          error, store_result
      ) handler.Cckappa_sig.dual store_result
  in
  error, store_result


(************************************************************************************)

let scan_rule_dynamic parameter log_info error rule_id rule compiled
    kappa_handler
    handler_bdu
    covering_classes
    store_covering_classes_id
    store_potential_side_effects
    store_pre_static
    store_result
    =
    let error, log_info = StoryProfiling.StoryStats.add_event parameter error
        (StoryProfiling.Scan_rule_dynamic (Ckappa_sig.int_of_rule_id rule_id))
        None log_info
    in
    let error, store_update =
      store_update
        parameter
        error
        store_pre_static.Bdu_static_views.store_test_modif_map
        store_potential_side_effects
        store_covering_classes_id
        covering_classes
        store_result.store_update
    in
    let error, store_dual_contact_map =
      collect_dual_map
        parameter
        error
        kappa_handler
        store_result.store_dual_contact_map
    in
    let error, log_info = StoryProfiling.StoryStats.close_event parameter error
        (StoryProfiling.Scan_rule_dynamic (Ckappa_sig.int_of_rule_id rule_id))
        None log_info
    in
    error, handler_bdu, log_info,
    {
      store_update = store_update;
      store_dual_contact_map = store_dual_contact_map
    }

(************************************************************************************)

let init_bdu_analysis_dynamic =
  let init_bdu_analysis_dynamic =
    {
      store_update  = Covering_classes_type.AgentCV_map_and_set.Map.empty;
      store_dual_contact_map = Ckappa_sig.AgentSiteState_map_and_set.Map.empty
    }
  in
  init_bdu_analysis_dynamic

(************************************************************************************)
(*rules*)

let scan_rule_set_dynamic
    parameter
    log_info
    error
    compiled
    kappa_handler
    handler_bdu
    store_pre_static
    covering_classes
    store_covering_classes_id
    store_potential_side_effects =
  let error, (handler_bdu, log_info, store_result) =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule (handler_bdu, log_info, store_result) ->
        let error, handler_bdu, log_info, store_result =
          scan_rule_dynamic
            parameter
            log_info
            error
            rule_id
            rule.Cckappa_sig.e_rule_c_rule
            compiled
            kappa_handler
            handler_bdu
            covering_classes
            store_covering_classes_id
            store_potential_side_effects
            store_pre_static
            store_result
        in
        error, (handler_bdu, log_info, store_result)
      ) compiled.Cckappa_sig.rules (handler_bdu, log_info, init_bdu_analysis_dynamic)
  in
  error, (handler_bdu, log_info, store_result)
