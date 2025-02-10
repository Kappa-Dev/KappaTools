(**
   * bdu_static_views.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 18th of Feburary
   * Last modification: Time-stamp: <Aug 21 2018>
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

(***************************************************************************)
(*TYPE of pattern*)
(***************************************************************************)

type bdu_analysis_static_pattern = {
  (*pattern*)
  store_proj_bdu_test_restriction_pattern:
    (Covering_classes_type.cv_id
    * Ckappa_sig.c_state Cckappa_sig.interval
      Ckappa_sig.GuardSite_map_and_set.Map.t)
    list;
}

(***************************************************************************)
(*initial values of pattern *)
(***************************************************************************)

let init_bdu_analysis_static_pattern =
  { store_proj_bdu_test_restriction_pattern = [] }

(***************************************************************************)
(*TYPE of BDU static*)
(***************************************************************************)

type bdu_analysis_static = {
  store_proj_bdu_creation_restriction_map:
    Ckappa_sig.Views_bdu.mvbdu Covering_classes_type.AgentCV_setmap.Map.t
    Ckappa_sig.Rule_setmap.Map.t;
  store_modif_list_restriction_map:
    Ckappa_sig.Views_bdu.hconsed_association_list
    Covering_classes_type.AgentsRuleCV_map_and_set.Map.t;
  store_proj_bdu_potential_restriction_map:
    (Ckappa_sig.Views_bdu.mvbdu * Ckappa_sig.Views_bdu.hconsed_association_list)
    Covering_classes_type.AgentSiteCV_setmap.Map.t
    Ckappa_sig.Rule_setmap.Map.t;
  store_proj_bdu_test_restriction:
    Ckappa_sig.Views_bdu.mvbdu Covering_classes_type.AgentsCV_setmap.Map.t
    Ckappa_sig.Rule_setmap.Map.t;
  store_guard_restriction_bdu:
    Ckappa_sig.Views_bdu.mvbdu Covering_classes_type.AgentCV_setmap.Map.t;
  site_to_renamed_site_list:
    (Covering_classes_type.cv_id * Ckappa_sig.c_guard_p_then_site) list
    Ckappa_sig
    .Agent_type_guard_or_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .t;
}

(***************************************************************************)
(*initial values of BDU static*)
(***************************************************************************)

let init_bdu_analysis_static parameters error =
  let error, init_site_to_renamed_site_list =
    Ckappa_sig
    .Agent_type_guard_or_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .create parameters error (0, 0)
  in
  let init_bdu_analysis_static =
    {
      store_proj_bdu_creation_restriction_map = Ckappa_sig.Rule_setmap.Map.empty;
      store_modif_list_restriction_map =
        Covering_classes_type.AgentsRuleCV_map_and_set.Map.empty;
      store_proj_bdu_potential_restriction_map =
        Ckappa_sig.Rule_setmap.Map.empty;
      store_proj_bdu_test_restriction = Ckappa_sig.Rule_setmap.Map.empty;
      store_guard_restriction_bdu =
        Covering_classes_type.AgentCV_setmap.Map.empty;
      site_to_renamed_site_list = init_site_to_renamed_site_list;
    }
  in
  error, init_bdu_analysis_static

let get_bdu_guard store_guard_bdu agent_type cv_id bdu_true =
  match
    Covering_classes_type.AgentCV_setmap.Map.find_option (agent_type, cv_id)
      store_guard_bdu
  with
  | None -> bdu_true
  | Some guard_bdu -> guard_bdu

let get_bdu_guard_original_names guard_mvbdus rule_id parameters handler error =
  match Ckappa_sig.Rule_setmap.Map.find_option rule_id guard_mvbdus with
  | None -> Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  | Some bdu -> error, handler, bdu

(******************************************************************)
(*implementation of bdu_analysis_static*)
(******************************************************************)
(*creation rules*)

let get_bdu_map_and_set error bdu_false (agent_type, rule_id, cv_id)
    store_result =
  let error, bdu_value =
    match
      Covering_classes_type.AgentRuleCV_setmap.Map.find_option
        (agent_type, rule_id, cv_id)
        store_result
    with
    | None -> error, bdu_false
    (*default value when there is no creation in this rule*)
    | Some bdu -> error, bdu
  in
  error, bdu_value

let add_dependency_triple_bdu parameters handler error
    (agent_type, rule_id, cv_id) bdu store_result guard_bdu restriction_bdu =
  (*add guard information*)
  let error, handler, bdu =
    Common_static.mvbdu_and_for_guards parameters handler error bdu guard_bdu
  in
  let error, handler, bdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false parameters handler error
  in
  let error, old_bdu =
    get_bdu_map_and_set error bdu_false
      (agent_type, rule_id, cv_id)
      store_result
  in
  (* In the case when the agent is created twice, we take the union *)
  let error, handler, bdu_new =
    Common_static.mvbdu_or_for_guards parameters handler error old_bdu bdu
      restriction_bdu
  in
  let store_result =
    Covering_classes_type.AgentRuleCV_setmap.Map.add
      (agent_type, rule_id, cv_id)
      bdu_new store_result
  in
  error, handler, store_result

let add_dependency_site parameters map_new_index_forward site state
    (error, store_result) =
  let error, site' =
    match
      Ckappa_sig.SiteOrGuard_map_and_set.Map.find_option parameters error site
        map_new_index_forward
    with
    | error, None ->
      Exception.warn parameters error __POS__ Exit
        Ckappa_sig.dummy_site_or_guard_name
    | error, Some s -> error, s
  in
  Ckappa_sig.GuardSite_map_and_set.Map.add parameters error site' state
    store_result

let to_site_or_guard_map parameters error site_map =
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun site value (error, new_set) ->
      Ckappa_sig.SiteOrGuard_map_and_set.Map.add parameters error
        (Ckappa_sig.Site site) value new_set)
    site_map
    (error, Ckappa_sig.SiteOrGuard_map_and_set.Map.empty)

let get_pair_cv_map_with_missing_association_creation parameters error agent
    triple_list =
  List.fold_left
    (fun (error, current_list) (cv_id, list, set) ->
      let error, (map_new_index_forward, _) =
        Common_map.new_index_pair_map parameters error list
      in
      let error, agent_interface =
        to_site_or_guard_map parameters error agent.Cckappa_sig.agent_interface
      in
      (*----------------------------------------------------*)
      let error', map_res =
        try
          Ckappa_sig.SiteOrGuard_map_and_set.Map
          .fold_restriction_with_missing_associations parameters error
            (fun site port (error, m) ->
              match
                ( port.Cckappa_sig.site_state.Cckappa_sig.min,
                  port.Cckappa_sig.site_state.Cckappa_sig.max )
              with
              | Some a, Some b when a = b ->
                let error, m =
                  add_dependency_site parameters map_new_index_forward site a
                    (error, m)
                in
                error, m
              | Some _, Some _ | None, _ | _, None -> raise Exit)
            (fun site (error, m) ->
              match site with
              | Ckappa_sig.Site _ ->
                let error, m =
                  add_dependency_site parameters map_new_index_forward site
                    Ckappa_sig.dummy_state_index (error, m)
                in
                error, m
              | Ckappa_sig.Guard_p _ -> error, m)
            set agent_interface Ckappa_sig.GuardSite_map_and_set.Map.empty
        with Exit ->
          Exception.warn parameters error __POS__ Exit
            Ckappa_sig.GuardSite_map_and_set.Map.empty
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, (cv_id, map_res) :: current_list)
    (error, []) triple_list

let collect_bdu_creation_restriction_map parameters handler error rule_id rule
    store_remanent_triple store_result current_guard_bdu guard_restriction_bdu =
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_type' triple_list (handler, store_result) ->
      List.fold_left
        (fun (error, (handler, store_result)) (agent_id, agent_type) ->
          let error, agent =
            Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
              parameters error agent_id
              rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
          in
          match agent with
          | Some (Cckappa_sig.Unknown_agent _)
          | Some (Cckappa_sig.Dead_agent _)
          | None ->
            Exception.warn parameters error __POS__ Exit (handler, store_result)
          | Some Cckappa_sig.Ghost -> error, (handler, store_result)
          | Some (Cckappa_sig.Agent agent) ->
            if agent_type' = agent_type then (
              (*-----------------------------------------------------------*)
              (*get map restriction from covering classes*)
              let error, get_pair_list =
                get_pair_cv_map_with_missing_association_creation parameters
                  error agent triple_list
              in
              (*----------------------------------------------------------*)
              (*fold a list and get a pair of site and state and rule_id*)
              let error, handler, store_result =
                List.fold_left
                  (fun (error, handler, store_result) (cv_id, map_res) ->
                    let pair_list =
                      Ckappa_sig.GuardSite_map_and_set.Map.fold
                        (fun site' state current_list ->
                          (site', state) :: current_list)
                        map_res []
                    in
                    let error, handler, bdu_creation =
                      Ckappa_sig.Views_bdu
                      .mvbdu_of_reverse_sorted_association_list parameters
                        handler error pair_list
                    in
                    let guard_bdu =
                      get_bdu_guard current_guard_bdu agent_type cv_id bdu_true
                    in
                    let restriction_bdu =
                      get_bdu_guard guard_restriction_bdu agent_type cv_id
                        bdu_true
                    in
                    let error, handler, store_result =
                      add_dependency_triple_bdu parameters handler error
                        (agent_type, rule_id, cv_id)
                        bdu_creation store_result guard_bdu restriction_bdu
                    in
                    error, handler, store_result)
                  (error, handler, store_result)
                  get_pair_list
              in
              error, (handler, store_result)
            ) else
              error, (handler, store_result))
        (error, (handler, store_result))
        rule.Cckappa_sig.actions.Cckappa_sig.creation)
    store_remanent_triple (handler, store_result)

(*projection with rule_id*)

let collect_proj_bdu_creation_restriction_map parameters handler_bdu error
    rule_id rule store_remanent_triple store_result current_guard_bdu
    guard_restriction_bdu =
  let store_init_bdu_creation_restriction_map =
    Covering_classes_type.AgentRuleCV_setmap.Map.empty
  in
  let error, (handler_bdu, store_bdu_creation_restriction_map) =
    collect_bdu_creation_restriction_map
      (* collect should work directly on the partitioned map (store_result) *)
      parameters handler_bdu error rule_id rule store_remanent_triple
      store_init_bdu_creation_restriction_map current_guard_bdu
      guard_restriction_bdu
  in
  let error, handler_bdu, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler_bdu error
  in
  let (error, handler_bdu), store_result' =
    Covering_classes_type.Project2bdu_creation.proj2_monadic parameters
      (error, handler_bdu)
      (fun (_agent_type, rule_id, _cv_id) -> rule_id)
      (fun (agent_type, _rule_id, cv_id) -> agent_type, cv_id)
      bdu_true
      (fun parameters (error, handler_bdu) bdu bdu' ->
        let error, handler_bdu, bdu_union =
          Common_static.mvbdu_and_for_guards parameters handler_bdu error bdu
            bdu'
        in
        (error, handler_bdu), bdu_union)
      store_bdu_creation_restriction_map
  in
  let store_result =
    Ckappa_sig.Rule_setmap.Map.fold Ckappa_sig.Rule_setmap.Map.add store_result'
      store_result
  in
  (error, handler_bdu), store_result

(**************************************************************************)
(*modification rule with creation rules*)

let get_pair_cv_map_with_restriction_modification parameters error agent
    triple_list =
  List.fold_left
    (fun (error, current_list) (cv_id, list, set) ->
      (*-----------------------------------------------------------*)
      (*new index for site type in covering class*)
      let error, (map_new_index_forward, _) =
        Common_map.new_index_pair_map parameters error list
      in
      let error, agent_interface =
        to_site_or_guard_map parameters error agent.Cckappa_sig.agent_interface
      in
      (*-----------------------------------------------------------*)
      let error, map_res =
        Ckappa_sig.SiteOrGuard_map_and_set.Map.fold_restriction parameters error
          (fun site port (error, store_result) ->
            let state = port.Cckappa_sig.site_state.Cckappa_sig.min in
            let error, () =
              if state = port.Cckappa_sig.site_state.Cckappa_sig.max then
                error, ()
              else
                Exception.warn parameters error __POS__ Exit ()
            in
            let error, site' =
              Ckappa_sig.SiteOrGuard_map_and_set.Map.find_default_without_logs
                parameters error Ckappa_sig.dummy_site_or_guard_name site
                map_new_index_forward
            in
            let error, map_res =
              Ckappa_sig.GuardSite_map_and_set.Map.add parameters error site'
                state store_result
            in
            error, map_res)
          set agent_interface Ckappa_sig.GuardSite_map_and_set.Map.empty
      in
      error, (cv_id, map_res) :: current_list)
    (error, []) triple_list

let collect_modif_list_restriction_map parameters handler error rule_id rule
    (*store_new_index_pair_map*)
      store_remanent_triple store_result =
  let add_link error (agent_id, agent_type, rule_id, cv_id) list_a store_result
      =
    (*the association must be unique *)
    let error, result_map =
      Covering_classes_type.AgentsRuleCV_map_and_set.Map.add_or_overwrite
        parameters error
        (agent_id, agent_type, rule_id, cv_id)
        list_a store_result
    in
    error, result_map
  in
  (*let (map_new_index_forward, _) = store_new_index_pair_map in*)
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_id agent_modif (handler, store_result) ->
      if
        Ckappa_sig.Site_map_and_set.Map.is_empty
          agent_modif.Cckappa_sig.agent_interface
      then
        error, (handler, store_result)
      else (
        let agent_type = agent_modif.Cckappa_sig.agent_name in
        let error, triple_list =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
              parameters error agent_type store_remanent_triple
          with
          | error, None -> Exception.warn parameters error __POS__ Exit []
          | error, Some x -> error, x
        in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
          get_pair_cv_map_with_restriction_modification parameters error
            agent_modif triple_list
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, handler, store_result =
          List.fold_left
            (fun (error, handler, store_result) (cv_id, map_res) ->
              if Ckappa_sig.GuardSite_map_and_set.Map.is_empty map_res then
                error, handler, store_result
              else (
                (*get a list of pair (site, state) in a map of new indexes
                  of site.*)
                let error, pair_list =
                  Ckappa_sig.GuardSite_map_and_set.Map.fold
                    (fun site' state (error, current_list) ->
                      match state with
                      | Some state ->
                        let pair_list = (site', state) :: current_list in
                        error, pair_list
                      | None ->
                        Exception.warn parameters error __POS__ Exit
                          current_list)
                    map_res (error, [])
                in
                (*-------------------------------------------------------*)
                (*build list_a*)
                let error, handler, list_a =
                  Ckappa_sig.Views_bdu.build_association_list parameters handler
                    error pair_list
                in
                let error, store_result =
                  add_link error
                    (agent_id, agent_type, rule_id, cv_id)
                    list_a store_result
                in
                error, handler, store_result
              ))
            (error, handler, store_result)
            get_pair_list
        in
        error, (handler, store_result)
      ))
    rule.Cckappa_sig.diff_direct (handler, store_result)

(**************************************************************************)
(*build bdu for potential side effects*)

let get_triple_map parameters error pair_list triple_list =
  List.fold_left
    (fun (error, current_list) (cv_id, list, set) ->
      (*-------------------------------------------------------*)
      (*get new indexes for sites*)
      let error, (map_new_index_forward, _) =
        Common_map.new_index_pair_map parameters error list
      in
      (*-----------------------------------------------------*)
      let error', map_res =
        List.fold_left
          (fun (error, map_res) (_, (site, state)) ->
            if
              Ckappa_sig.SiteOrGuard_map_and_set.Set.mem (Ckappa_sig.Site site)
                set
            then (
              let error, site' =
                Ckappa_sig.SiteOrGuard_map_and_set.Map.find_default_without_logs
                  parameters error Ckappa_sig.dummy_site_or_guard_name
                  (Ckappa_sig.Site site) map_new_index_forward
              in
              let error, old =
                Ckappa_sig.GuardSite_map_and_set.Map.find_default_without_logs
                  parameters error [] site' map_res
              in
              let error, map_res =
                Ckappa_sig.GuardSite_map_and_set.Map.add_or_overwrite parameters
                  error site' (state :: old) map_res
              in
              error, map_res
            ) else
              error, map_res)
          (error, Ckappa_sig.GuardSite_map_and_set.Map.empty)
          pair_list
      in
      (*------------------------------------------------------*)
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      ( error,
        Ckappa_sig.GuardSite_map_and_set.Map.fold
          (fun site' list_state list -> (cv_id, site', list_state) :: list)
          map_res current_list ))
    (error, []) triple_list

let store_bdu_potential_restriction_map_aux parameters handler error
    (*store_new_index_pair_map*) store_remanent_triple
    store_potential_side_effects store_result store_guard_bdu
    guard_restriction_bdu =
  let error, handler, bdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false parameters handler error
  in
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  (*-----------------------------------------------------------------*)
  let add_link handler error (agent_type, new_site_type, rule_id, cv_id) bdu
      store_result =
    (*build a list_a*)
    let error, handler, list =
      Ckappa_sig.Views_bdu.build_reverse_sorted_association_list parameters
        handler error
        [ new_site_type, Ckappa_sig.dummy_state_index ]
      (*state is 0*)
    in
    let result_map =
      Covering_classes_type.AgentSiteRuleCV_setmap.Map.add
        (agent_type, new_site_type, rule_id, cv_id)
        (bdu, list) store_result
    in
    error, handler, result_map
  in
  (*let (map_new_index_forward, _) = store_new_index_pair_map in*)
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_type' triple_list (handler, store_result) ->
      (*map of potential partner side_effect with site is bond*)
      Ckappa_sig.AgentRule_map_and_set.Map.fold
        (fun (agent_type, rule_id) pair_list (error, (handler, store_result)) ->
          if agent_type' = agent_type then (
            let error, get_triple_list =
              get_triple_map parameters error pair_list triple_list
            in
            (*---------------------------------------------------------*)
            let error, handler, store_result =
              List.fold_left
                (fun (error, handler, store_result) (cv_id, site', map_res) ->
                  let error, handler, bdu =
                    List.fold_left
                      (fun (error, handler, bdu) state ->
                        (*-----------------------------------------------*)
                        (*build bdu_potential side effects*)
                        let error, handler, bdu_potential_effect =
                          Ckappa_sig.Views_bdu
                          .mvbdu_of_reverse_sorted_association_list parameters
                            handler error
                            [ site', state ]
                        in
                        let guard_bdu =
                          get_bdu_guard store_guard_bdu agent_type cv_id
                            bdu_true
                        in
                        let restriction_bdu =
                          get_bdu_guard guard_restriction_bdu agent_type cv_id
                            bdu_true
                        in
                        let error, handler, bdu_potential_effect_with_guards =
                          Common_static.mvbdu_and_for_guards parameters handler
                            error guard_bdu bdu_potential_effect
                        in
                        (*union of bdu and bdu effect*)
                        let error, handler, bdu =
                          Common_static.mvbdu_or_for_guards parameters handler
                            error bdu bdu_potential_effect_with_guards
                            restriction_bdu
                        in
                        error, handler, bdu)
                      (error, handler, bdu_false)
                      map_res
                  in
                  let error, handler, store_result =
                    add_link handler error
                      (agent_type, site', rule_id, cv_id)
                      bdu store_result
                  in
                  error, handler, store_result)
                (error, handler, store_result)
                get_triple_list
            in
            error, (handler, store_result)
          ) else
            error, (handler, store_result))
        store_potential_side_effects
        (error, (handler, store_result)))
    store_remanent_triple (handler, store_result)

(*************************************************************************)
(*build bdu_potential in the case of binding*)

let store_bdu_potential_effect_restriction_map parameters handler error
    (*store_new_index_pair_map*) store_remanent_triple
    store_potential_side_effects store_result store_guard_bdu
    guard_restriction_bdu =
  let error', (handler, store_result) =
    store_bdu_potential_restriction_map_aux parameters handler
      error (*store_new_index_pair_map*)
      store_remanent_triple store_potential_side_effects store_result
      store_guard_bdu guard_restriction_bdu
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  error, (handler, store_result)

let collect_site_to_renamed_site_list parameters error store_remanent_triple
    nr_guard_params output =
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_type' triple_list output ->
      List.fold_left
        (fun (error, output) (cv_id, list, _) ->
          let rec aux error site list output =
            match list with
            | [] -> error, output
            | h :: t ->
              let h =
                Ckappa_sig.guard_p_then_site_of_site_or_guard_p h
                  nr_guard_params
              in
              let key = agent_type', h in
              let error, old =
                match
                  Ckappa_sig
                  .Agent_type_guard_or_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                  .unsafe_get parameters error key output
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let new_list = (cv_id, site) :: old in
              let error, output =
                Ckappa_sig
                .Agent_type_guard_or_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                .set parameters error key new_list output
              in
              let site' = Ckappa_sig.next_guard_or_site_name site in
              aux error site' t output
          in
          aux error
            (Ckappa_sig.guard_p_then_site_of_site Ckappa_sig.dummy_site_name_1
               nr_guard_params)
            list output)
        (error, output) triple_list)
    store_remanent_triple output

(**************************************************************************)
(*projection with rule_id*)

let collect_proj_bdu_potential_restriction_map parameters handler error
    (*store_new_index_pair_map*) store_remanent_triple
    store_potential_side_effects store_result store_guard_bdu
    guard_restriction_bdu =
  let store_init_bdu_potential_restriction_map =
    Covering_classes_type.AgentSiteRuleCV_setmap.Map.empty
  in
  let error, (handler, store_bdu_potential_restriction_map) =
    (* this function should work directly on the partitioned map (store_result) *)
    store_bdu_potential_effect_restriction_map parameters handler
      error (*store_new_index_pair_map*)
      store_remanent_triple store_potential_side_effects
      store_init_bdu_potential_restriction_map store_guard_bdu
      guard_restriction_bdu
  in
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  (*an empty hconsed list*)
  let error, handler, empty =
    Ckappa_sig.Views_bdu.build_reverse_sorted_association_list parameters
      handler error []
  in
  let (error, handler), store_result' =
    Covering_classes_type.Project2bdu_potential.proj2_monadic parameters
      (error, handler)
      (fun (_agent_type, _new_site_name, rule_id, _cv_id) -> rule_id)
      (fun (agent_type, new_site_name, _rule_id, cv_id) ->
        agent_type, new_site_name, cv_id)
      (bdu_true, empty)
      (fun _ (error, handler) _ pair' -> (error, handler), pair')
      store_bdu_potential_restriction_map
  in
  let store_result =
    Ckappa_sig.Rule_setmap.Map.fold Ckappa_sig.Rule_setmap.Map.add store_result'
      store_result
  in
  (error, handler), store_result

(**************************************************************************)

let get_pair_cv_map_with_restriction_views parameters error agent triple_list =
  List.fold_left
    (fun (error, current_list) (cv_id, list, set) ->
      (*----------------------------------------------------------*)
      (*new index for site type in covering class*)
      let error, (map_new_index_forward, _) =
        Common_map.new_index_pair_map parameters error list
      in
      let error, agent_interface =
        to_site_or_guard_map parameters error agent.Cckappa_sig.agent_interface
      in
      (*----------------------------------------------------------*)
      let error', map_res =
        Ckappa_sig.SiteOrGuard_map_and_set.Map.fold_restriction parameters error
          (fun site port (error, store_result) ->
            let state = port.Cckappa_sig.site_state in
            let error, site' =
              Ckappa_sig.SiteOrGuard_map_and_set.Map.find_default parameters
                error Ckappa_sig.dummy_site_or_guard_name site
                map_new_index_forward
            in
            let error, map_res =
              Ckappa_sig.GuardSite_map_and_set.Map.add parameters error site'
                state store_result
            in
            error, map_res)
          set agent_interface Ckappa_sig.GuardSite_map_and_set.Map.empty
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, (cv_id, map_res) :: current_list)
    (error, []) triple_list

let collect_bdu_test_restriction_map parameters handler error rule_id rule
    (*store_new_index_pair_map*) store_remanent_triple store_result
    store_guard_bdu =
  let error, handler, bdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false parameters handler error
  in
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  (*let (map_new_index_forward, _) = store_new_index_pair_map in*)
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_id agent (handler, store_result) ->
      match agent with
      | Cckappa_sig.Unknown_agent _
      (* Unfortunatly, we can do nothing with undefined agents in the views
         domain *)
      (* They will be handled with properly in the agents domain *)
      | Cckappa_sig.Ghost ->
        error, (handler, store_result)
      | Cckappa_sig.Dead_agent (agent, _, _, _) ->
        let agent_type = agent.Cckappa_sig.agent_name in
        let error, triple_list =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_remanent_triple
          with
          | error, None -> error, []
          | error, Some x -> error, x
        in
        let error, store_result =
          List.fold_left
            (fun (error, store_result) (cv_id, _, _) ->
              ( error,
                Covering_classes_type.AgentsRuleCV_setmap.Map.add
                  (agent_id, agent_type, rule_id, cv_id)
                  bdu_false store_result ))
            (error, store_result) triple_list
        in
        error, (handler, store_result)
      | Cckappa_sig.Agent agent ->
        let agent_type = agent.Cckappa_sig.agent_name in
        let error, triple_list =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_remanent_triple
          with
          | error, None -> error, []
          | error, Some x -> error, x
        in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
          get_pair_cv_map_with_restriction_views parameters error agent
            triple_list
        in
        (*-----------------------------------------------------------------*)
        let error, handler, store_result =
          List.fold_left
            (fun (error, handler, store_result) (cv_id, map_res) ->
              if Ckappa_sig.GuardSite_map_and_set.Map.is_empty map_res then
                error, handler, store_result
              else (
                let error, pair_list =
                  Ckappa_sig.GuardSite_map_and_set.Map.fold
                    (fun site' state (error, current_list) ->
                      let pair_list =
                        (site', (state.Cckappa_sig.min, state.Cckappa_sig.max))
                        :: current_list
                      in
                      error, pair_list)
                    map_res (error, [])
                in
                (*build bdu_test*)
                let error, handler, bdu_test =
                  Ckappa_sig.Views_bdu.mvbdu_of_reverse_sorted_range_list
                    parameters handler error pair_list
                in
                let guard_bdu =
                  get_bdu_guard store_guard_bdu agent_type cv_id bdu_true
                in
                let error, handler, bdu_test_with_guards =
                  Common_static.mvbdu_and_for_guards parameters handler error
                    guard_bdu bdu_test
                in
                let error, store_result =
                  ( error,
                    Covering_classes_type.AgentsRuleCV_setmap.Map.add
                      (agent_id, agent_type, rule_id, cv_id)
                      bdu_test_with_guards store_result )
                in
                error, handler, store_result
              ))
            (error, handler, store_result)
            get_pair_list
        in
        error, (handler, store_result))
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views (handler, store_result)

(***************************************************************************)

let collect_proj_bdu_test_restriction parameters handler_kappa error rule_id
    rule (*store_new_index_pair_map*) store_remanent_triple store_result
    store_guard_bdu =
  let store_init_bdu_test_restriction_map =
    Covering_classes_type.AgentsRuleCV_setmap.Map.empty
  in
  let error, (handler, store_bdu_test_restriction_map) =
    (* collect should work directly on the partitioned map (store_result) *)
    collect_bdu_test_restriction_map parameters handler_kappa error rule_id
      rule (*store_new_index_pair_map*)
      store_remanent_triple store_init_bdu_test_restriction_map store_guard_bdu
  in
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  let (error, handler), store_result' =
    Covering_classes_type.Project2_bdu_views.proj2_monadic parameters
      (error, handler)
      (fun (_agent_id, _agent_type, rule_id, _cv_id) -> rule_id)
      (fun (agent_id, agent_type, _rule_id, cv_id) ->
        agent_id, agent_type, cv_id)
      bdu_true
      (fun parameters (error, handler) bdu bdu' ->
        let error, handler, bdu_union =
          Common_static.mvbdu_and_for_guards parameters handler error bdu bdu'
        in
        (error, handler), bdu_union)
      store_bdu_test_restriction_map
  in
  let store_result =
    Ckappa_sig.Rule_setmap.Map.fold Ckappa_sig.Rule_setmap.Map.add store_result'
      store_result
  in
  (error, handler), store_result

(***************************************************************************)

let rename_guards_in_mvbdu_to_cv_indexing parameters error bdu bdu_handler
    nr_guard_parameters map1 =
  let guard_parameter_list =
    Ckappa_sig.get_list_of_guard_parameters nr_guard_parameters
  in
  let error, guard_parameter_renaming_list =
    List.fold_left
      (fun (error, renaming_list) g ->
        let guard_parameter = Ckappa_sig.guard_p_then_site_of_guard g in
        let error, renamed =
          Ckappa_sig.GuardPOrSite_nearly_Inf_Int_storage_Imperatif.get
            parameters error guard_parameter map1
        in
        match renamed with
        | None ->
          Exception.warn parameters error __POS__ Exit
            ((guard_parameter, guard_parameter) :: renaming_list)
        | Some renamed -> error, (guard_parameter, renamed) :: renaming_list)
      (error, []) guard_parameter_list
  in
  (* rename bdu to original index *)
  let error, handler, renaming_list =
    Ckappa_sig.Views_bdu.build_renaming_list parameters bdu_handler error
      guard_parameter_renaming_list
  in
  let error, handler, bdu_guard_renamed =
    Ckappa_sig.Views_bdu.mvbdu_rename parameters handler error bdu renaming_list
  in
  error, handler, bdu_guard_renamed

let collect_bdu_by_agent_name_cv_id parameters handler_bdu error
    site_correspondence nr_guard_p f =
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent site_correspondence (handler_bdu, map_guard_bdu) ->
      let error, (handler_bdu, map_guard_bdu) =
        Covering_classes_type.Cv_id_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error cv_id (map1, _) (handler_bdu, map_guard_bdu) ->
            let error, handler_bdu, bdu = f parameters handler_bdu error in
            let error, handler_bdu, renamed_bdu =
              rename_guards_in_mvbdu_to_cv_indexing parameters error bdu
                handler_bdu nr_guard_p map1
            in
            ( error,
              ( handler_bdu,
                Covering_classes_type.AgentCV_setmap.Map.add (agent, cv_id)
                  renamed_bdu map_guard_bdu ) ))
          site_correspondence
          (handler_bdu, map_guard_bdu)
      in
      error, (handler_bdu, map_guard_bdu))
    site_correspondence
    (handler_bdu, Covering_classes_type.AgentCV_setmap.Map.empty)

let collect_guard_restriction_bdu parameters handler_bdu error
    site_correspondence nr_guard_p restriction_bdu =
  collect_bdu_by_agent_name_cv_id parameters handler_bdu error
    site_correspondence nr_guard_p (fun _ handler_bdu error ->
      error, handler_bdu, restriction_bdu)

let collect_guard_bdu parameters handler_bdu error rule_id site_correspondence
    nr_guard_p guard_mvbdus =
  collect_bdu_by_agent_name_cv_id parameters handler_bdu error
    site_correspondence nr_guard_p
    (get_bdu_guard_original_names guard_mvbdus rule_id)

(***************************************************************************)

(*Pattern*)

let collect_proj_bdu_test_restriction_pattern parameters error
    (pattern : Cckappa_sig.mixture)
    (*store_new_index_pair_map*)
      store_remanent_triple store_result =
  (*let (map_new_index_forward, _) = store_new_index_pair_map in*)
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error _agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost
        | Cckappa_sig.Dead_agent (_, _, _, _) ->
          error, store_result (*CHECK ME: SHOULD I RAISE A WARNING HERE?*)
        (*Exception.warn parameters error __POS__
          ~message:"Dead_agent"
          Exit store_result*)
        (*----------------------------------------------------------*)
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, triple_list =
            match
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
              .unsafe_get parameters error agent_type store_remanent_triple
            with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let error, get_pair_list =
            get_pair_cv_map_with_restriction_views parameters error agent
              triple_list
          in
          error, get_pair_list)
      pattern.Cckappa_sig.views store_result
  in
  error, store_result

(***************************************************************************)

let scan_rule_static parameters log_info error handler_bdu
    (rule_id : Ckappa_sig.c_rule_id) rule
    (*store_new_index_pair_map*)
      store_remanent_triple store_potential_side_effects _compil store_result
    site_correspondence nr_guard_p guard_mvbdus guard_restriction_bdu =
  (*-----------------------------------------------------------------------*)
  (*pre_static*)
  let error, log_info =
    StoryProfiling.StoryStats.add_event parameters error
      (StoryProfiling.Scan_rule_static (Ckappa_sig.int_of_rule_id rule_id))
      None log_info
  in
  (*------------------------------------------------------------------------*)
  let error, (handler_bdu, current_guard_bdu) =
    collect_guard_bdu parameters handler_bdu error rule_id site_correspondence
      nr_guard_p guard_mvbdus
  in
  (*------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_creation_restriction_map =
    collect_proj_bdu_creation_restriction_map parameters handler_bdu error
      rule_id rule (*store_new_index_pair_map*)
      store_remanent_triple store_result.store_proj_bdu_creation_restriction_map
      current_guard_bdu guard_restriction_bdu
  in
  (*-----------------------------------------------------------------------*)
  let error, (handler_bdu, store_modif_list_restriction_map) =
    collect_modif_list_restriction_map parameters handler_bdu error rule_id rule
      (*store_new_index_pair_map*)
      store_remanent_triple store_result.store_modif_list_restriction_map
  in
  (*-----------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_potential_restriction_map =
    collect_proj_bdu_potential_restriction_map parameters handler_bdu
      error (*store_new_index_pair_map*)
      store_remanent_triple store_potential_side_effects
      store_result.store_proj_bdu_potential_restriction_map current_guard_bdu
      guard_restriction_bdu
  in
  (*------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_test_restriction =
    collect_proj_bdu_test_restriction parameters handler_bdu error rule_id
      rule (*store_new_index_pair_map*)
      store_remanent_triple store_result.store_proj_bdu_test_restriction
      current_guard_bdu
  in
  (*------------------------------------------------------------------------*)
  let error, log_info =
    StoryProfiling.StoryStats.close_event parameters error
      (StoryProfiling.Scan_rule_static (Ckappa_sig.int_of_rule_id rule_id))
      None log_info
  in
  ( error,
    log_info,
    handler_bdu,
    {
      store_result with
      store_proj_bdu_creation_restriction_map;
      store_modif_list_restriction_map;
      store_proj_bdu_potential_restriction_map;
      store_proj_bdu_test_restriction;
    } )

(***************************************************************************)

let scan_rule_set parameters log_info handler_bdu error handler_kappa compiled
    store_potential_side_effects store_remanent_triple site_correspondence
    guard_mvbdus restriction_bdu =
  let error, init = init_bdu_analysis_static parameters error in
  let nr_guard_params = Handler.get_nr_guard_parameters handler_kappa in
  let error, (handler_bdu, store_guard_restriction_bdu) =
    collect_guard_restriction_bdu parameters handler_bdu error
      site_correspondence nr_guard_params restriction_bdu
  in
  let error, (handler_bdu, log_info, store_results) =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
      (fun parameters error rule_id rule (handler_bdu, log_info, store_result) ->
        let error, log_info, handler_bdu, store_result =
          scan_rule_static parameters log_info error handler_bdu rule_id
            rule.Cckappa_sig.e_rule_c_rule store_remanent_triple
            store_potential_side_effects compiled store_result
            site_correspondence nr_guard_params guard_mvbdus
            store_guard_restriction_bdu
        in
        error, (handler_bdu, log_info, store_result))
      compiled.Cckappa_sig.rules
      (handler_bdu, log_info, init)
  in
  let error, site_to_renamed_site_list =
    collect_site_to_renamed_site_list parameters error store_remanent_triple
      nr_guard_params store_results.site_to_renamed_site_list
  in
  ( error,
    ( handler_bdu,
      log_info,
      {
        store_results with
        site_to_renamed_site_list;
        store_guard_restriction_bdu;
      } ) )

(***************************************************************************)
(*PATTERN*)
(***************************************************************************)

let scan_rule_static_pattern parameters
    (*store_new_index_pair_map*)
      store_remanent_triple error rule store_result =
  let error, store_proj_bdu_test_restriction_pattern =
    collect_proj_bdu_test_restriction_pattern parameters error
      rule.Cckappa_sig.rule_lhs (*pattern*)
      (*store_new_index_pair_map*)
      store_remanent_triple store_result.store_proj_bdu_test_restriction_pattern
  in
  error, { store_proj_bdu_test_restriction_pattern }

let scan_rule_set_pattern parameters error
    (*store_new_index_pair_map*)
      store_remanent_triple compiled =
  let init = init_bdu_analysis_static_pattern in
  let error, store_results =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
      (fun parameters error _ rule store_result ->
        let error, store_result =
          scan_rule_static_pattern parameters
            (*store_new_index_pair_map*)
            store_remanent_triple error rule.Cckappa_sig.e_rule_c_rule
            store_result
        in
        error, store_result)
      compiled.Cckappa_sig.rules init
  in
  error, store_results
