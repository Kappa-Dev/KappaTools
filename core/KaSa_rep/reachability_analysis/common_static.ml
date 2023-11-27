(**
  * common_static_type.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
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

let trace = false

(***************************************************************************)
(*MODIFICATION*)
(***************************************************************************)

type modification_views = {
  store_modified_map:
    Ckappa_sig.AgentsSiteState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_project_modified_map:
    (*use in parallel domain*)
    Ckappa_sig.AgentSite_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
  store_modification_sites:
    Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentsSite_map_and_set.Map.t;
}

(***************************************************************************)
(*VIEWS-TEST: LHS-RHS*)
(***************************************************************************)

type test_views = {
  store_views_rhs:
    Ckappa_sig.pair_of_states Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_id_map_and_set.Map.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_views_lhs:
    Ckappa_sig.pair_of_states Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_id_map_and_set.Map.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_test_sites:
    Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentsSite_map_and_set.Map.t;
}

(***************************************************************************)
(*SIDE EFFECTS*)
(***************************************************************************)

type half_break_action =
  (int list * (Ckappa_sig.c_rule_id * Ckappa_sig.pair_of_states) list)
  Ckappa_sig.AgentSite_map_and_set.Map.t

type remove_action =
  (int list * Ckappa_sig.c_rule_id list) Ckappa_sig.AgentSite_map_and_set.Map.t

type potential_side_effect =
  ((Ckappa_sig.c_agent_id
   * Ckappa_sig.c_agent_name
   * Ckappa_sig.c_site_name
   * Ckappa_sig.c_state)
  * (Ckappa_sig.c_site_name * Ckappa_sig.c_state))
  list
  Ckappa_sig.AgentRule_map_and_set.Map.t

type side_effects_views = {
  store_side_effects: half_break_action * remove_action;
  store_potential_side_effects: potential_side_effect;
}

(***************************************************************************)
(*BINDING*)
(***************************************************************************)

type binding_views = {
  store_bonds_rhs:
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_bonds_lhs:
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_action_binding:
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
}

(***************************************************************************)
(*COVERING CLASSES*)
(***************************************************************************)
(*

let new_index_pair_map parameters error l =
  let rec aux acc k map1 map2 error =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error, map1 =
        Ckappa_sig.Site_map_and_set.Map.add parameters error h k map1 in
      let error, map2 =
        Ckappa_sig.Site_map_and_set.Map.add parameters error k h map2 in
      aux
        tl
        (Ckappa_sig.site_name_of_int ((Ckappa_sig.int_of_site_name k)+1))
        map1
        map2
        error
  in
  let error', (map1, map2) =
    aux
      l
      (Ckappa_sig.site_name_of_int 1)
      Ckappa_sig.Site_map_and_set.Map.empty
      Ckappa_sig.Site_map_and_set.Map.empty error
  in
  let error =
    Exception.check_point
      Exception.warn parameters error error' __POS__ Exit
  in
  error,(map1,map2)
*)
(***************************************************************************)
(*COMMON VIEWS*)
(***************************************************************************)

type common_views = {
  store_agent_name:
    Ckappa_sig.c_agent_name Ckappa_sig.RuleAgent_map_and_set.Map.t;
  store_agent_name_from_pattern:
    Ckappa_sig.c_agent_name Ckappa_sig.Agent_id_map_and_set.Map.t;
  store_potential_side_effects_per_rule:
    ((Ckappa_sig.c_agent_id
     * Ckappa_sig.c_agent_name
     * Ckappa_sig.c_site_name
     * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state))
    list
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_side_effects_views: side_effects_views;
  store_binding_views: binding_views;
  store_modification: modification_views;
  store_test: test_views;
  store_test_modification_sites:
    Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentsSite_map_and_set.Map.t;
  store_test_modif_map:
    Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentSite_map_and_set.Map.t;
      (*    store_predicate_covering_classes : predicate_covering_classes;*)
}

(*****************************************************************************)
(*Initial states of bdu common statics*)
(*****************************************************************************)

let empty_rule = Ckappa_sig.Rule_map_and_set.Map.empty
let empty_site = Ckappa_sig.Site_map_and_set.Map.empty
let empty_agentsite = Ckappa_sig.AgentSite_map_and_set.Map.empty
let empty_agentrule = Ckappa_sig.AgentRule_map_and_set.Map.empty

let init_modification_views =
  {
    store_modified_map = empty_rule;
    store_project_modified_map = empty_rule;
    store_modification_sites = Ckappa_sig.AgentsSite_map_and_set.Map.empty;
  }

let init_test_views =
  {
    store_views_rhs = empty_rule;
    store_views_lhs = empty_rule;
    store_test_sites = Ckappa_sig.AgentsSite_map_and_set.Map.empty;
  }

let init_binding_views =
  {
    store_bonds_rhs = empty_rule;
    store_bonds_lhs = empty_rule;
    store_action_binding = empty_rule;
  }

let init_side_effect_views =
  {
    store_side_effects = empty_agentsite, empty_agentsite;
    store_potential_side_effects = empty_agentrule;
  }

let init_common_views _parameters error =
  ( error,
    {
      store_agent_name = Ckappa_sig.RuleAgent_map_and_set.Map.empty;
      store_agent_name_from_pattern = Ckappa_sig.Agent_id_map_and_set.Map.empty;
      store_side_effects_views = init_side_effect_views;
      store_potential_side_effects_per_rule = empty_rule;
      store_binding_views = init_binding_views;
      store_modification = init_modification_views;
      store_test = init_test_views;
      store_test_modification_sites =
        Ckappa_sig.AgentsSite_map_and_set.Map.empty;
      store_test_modif_map = empty_agentsite;
    } )

(****************************************************************************)
(*return agent_name with a pair of key (rule_id and agent_id) in the lhs*)

let collect_agent_name parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Unknown_agent _ ->
          Exception.warn parameter error __POS__ Exit store_result
        | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, store_result =
            Ckappa_sig.RuleAgent_map_and_set.Map.add_or_overwrite parameter
              error (rule_id, agent_id) agent_type store_result
          in
          error, store_result)
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
  in
  error, store_result

(**************************************************************************)
(*return agant_name with a key rule_id in a pattern*)

let collect_agent_name_from_pattern parameters error pattern store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Unknown_agent _ ->
          Exception.warn parameters error __POS__ Exit store_result
        | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, store_result =
            Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite parameters
              error agent_id agent_type store_result
          in
          error, store_result)
      pattern.Cckappa_sig.views store_result
  in
  error, store_result

(**************************************************************************)
(*Side effects*)
(**************************************************************************)

let get_last_entry_in_state_dic parameters error (agent_type, site_type) handler
    =
  let error, state_dic =
    Misc_sa.unsome
      (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
       .get parameters error (agent_type, site_type)
         handler.Cckappa_sig.states_dic)
      (fun error ->
        Exception.warn parameters error __POS__ Exit
          (Ckappa_sig.Dictionary_of_States.init ()))
  in
  let error, last_entry =
    Ckappa_sig.Dictionary_of_States.last_entry parameters error state_dic
  in
  error, last_entry

let get_states_in_handler parameter error add handler state_op =
  let agent_type = add.Cckappa_sig.agent_type in
  let site_type = add.Cckappa_sig.site in
  (*state*)
  let error, (state_min, state_max) =
    match state_op with
    | None ->
      let error, last_entry =
        get_last_entry_in_state_dic parameter error (agent_type, site_type)
          handler
      in
      error, (Some Ckappa_sig.dummy_state_index_1, Some last_entry)
    | Some interval ->
      error, (interval.Cckappa_sig.min, interval.Cckappa_sig.max)
  in
  error, (state_min, state_max)

let half_break_action parameters error handler rule_id half_break store_result =
  (*module (agent_type, site) -> (rule_id, binding_state) list*)
  let error, store_result =
    List.fold_left
      (fun (error, store_result) (site_address, state_op) ->
        (*site_address: {agent_index, site, agent_type}*)
        let agent_type = site_address.Cckappa_sig.agent_type in
        let site_type = site_address.Cckappa_sig.site in
        (*state*)
        let error, (state_min, state_max) =
          get_states_in_handler parameters error site_address handler state_op
        in
        (*-------------------------------------------------------------------*)
        (*return result*)
        let error, store_result =
          Common_map.add_dependency_pair_sites parameters error
            (agent_type, site_type)
            (rule_id, (state_min, state_max))
            store_result
        in
        error, store_result)
      (error, store_result) half_break
  in
  (*--------------------------------------------------------------------*)
  (*map function*)
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map
      (fun (l, x) -> List.rev l, x)
      store_result
  in
  error, store_result

(*state = 0 or there is no state?*)

let remove_action parameters error rule_id remove store_result =
  let error, store_result =
    List.fold_left
      (fun (error, store_result) (_agent_index, agent, list_undoc) ->
        let agent_type = agent.Cckappa_sig.agent_name in
        (*NOTE: if it is a site_free then do not consider this case.*)
        (*result*)
        let error, store_result =
          List.fold_left
            (fun (error, store_result) site_type ->
              Common_map.add_dependency_pair_sites parameters error
                (agent_type, site_type) rule_id store_result)
            (error, store_result) list_undoc
        in
        error, store_result)
      (error, store_result) remove
  in
  (*-------------------------------------------------------------------------*)
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map
      (fun (l, x) -> List.rev l, x)
      store_result
  in
  error, store_result

let collect_side_effects parameter error handler rule_id half_break remove
    store_result =
  let store_half_break_action, store_remove_action = store_result in
  (*if there is a half_break action*)
  let error, store_half_break_action =
    half_break_action parameter error handler rule_id half_break
      store_half_break_action
  in
  (*if there is a remove action*)
  let error, store_remove_action =
    remove_action parameter error rule_id remove store_remove_action
  in
  error, (store_half_break_action, store_remove_action)

(****************************************************************************)
(*return a potential sites of side effects in the case of half break action*)

let collect_potential_free_and_bind parameter error handler rule_id
    (agent_id, agent_type, site_type) k store_result =
  (*potential partner*)
  match Handler.dual parameter error handler agent_type site_type k with
  | error, None -> error, store_result
  | error, Some (agent_type2, site2, state2) ->
    let error, store_potential_free =
      Common_map.add_dependency_pair_sites_rule parameter error
        (agent_type2, rule_id)
        (( (agent_id, agent_type, site_type, k),
           (site2, Ckappa_sig.dummy_state_index) )
        :: [])
        (fst store_result)
    in
    let error, store_potential_bind =
      Common_map.add_dependency_pair_sites_rule parameter error
        (agent_type2, rule_id)
        (((agent_id, agent_type, site_type, k), (site2, state2)) :: [])
        (snd store_result)
    in
    error, (store_potential_free, store_potential_bind)

let get_potential_partner parameter error handler rule_id
    (agent_id, agent_type, site_type) (state_min, state_max) store_result =
  let error, state_min =
    match state_min with
    | None ->
      Exception.warn parameter error __POS__ Exit Ckappa_sig.dummy_state_index
    | Some i -> error, i
  in
  let error, state_max =
    match state_max with
    | None ->
      Exception.warn parameter error __POS__ Exit Ckappa_sig.dummy_state_index
    | Some i -> error, i
  in
  let rec aux k (error, store_result) =
    if Ckappa_sig.compare_state_index k state_max > 0 then
      error, store_result
    else (
      (*potential partner*)
      let error, (store_potential_free, store_potential_bind) =
        collect_potential_free_and_bind parameter error handler rule_id
          (agent_id, agent_type, site_type)
          k store_result
      in
      aux
        (Ckappa_sig.next_state_index k)
        (error, (store_potential_free, store_potential_bind))
    )
  in
  aux state_min (error, store_result)

let store_potential_half_break parameter error handler rule_id half_break
    store_result =
  List.fold_left
    (fun (error, store_result) (add, state_op) ->
      let agent_index = add.Cckappa_sig.agent_index in
      let agent_type = add.Cckappa_sig.agent_type in
      let site_type = add.Cckappa_sig.site in
      (*state*)
      let error, (state_min, state_max) =
        get_states_in_handler parameter error add handler state_op
      in
      (*--------------------------------------------------------------------*)
      let error, store_result =
        get_potential_partner parameter error handler rule_id
          (agent_index, agent_type, site_type)
          (state_min, state_max) store_result
      in
      error, store_result)
    (error, store_result) half_break

let store_potential_remove parameter error handler rule_id remove store_result =
  List.fold_left
    (fun (error, store_result) (agent_index, agent, list_undoc) ->
      let agent_type = agent.Cckappa_sig.agent_name in
      let error, store_result =
        List.fold_left
          (fun (error, store_result) site_type ->
            let error, is_binding =
              Handler.is_binding_site parameter error handler agent_type
                site_type
            in
            if is_binding then (
              let error, last_entry =
                get_last_entry_in_state_dic parameter error
                  (agent_type, site_type) handler
              in
              (*-----------------------------------------------------------*)
              let error, store_result =
                get_potential_partner parameter error handler rule_id
                  (agent_index, agent_type, site_type)
                  (Some Ckappa_sig.dummy_state_index_1, Some last_entry)
                  store_result
              in
              error, store_result
            ) else
              error, store_result)
          (error, store_result) list_undoc
      in
      error, store_result)
    (error, store_result) remove

let combine_half_break_and_remove parameter error fst_or_snd_store_result_hb
    fst_or_snd_store_result_remove store_result_map =
  (*-----------------------------------------------------------------------*)
  Ckappa_sig.AgentRule_map_and_set.Map.fold2 parameter error
    (*exists in 'a t*)
      (fun parameter error (agent_type, rule_id) l1 store_result ->
      let error, store_result =
        Common_map.add_dependency_pair_sites_rule parameter error
          (agent_type, rule_id) l1 store_result
      in
      error, store_result)
    (*exists in 'b t*)
      (fun parameter error (agent_type, rule_id) l2 store_result ->
      let error, store_result =
        Common_map.add_dependency_pair_sites_rule parameter error
          (agent_type, rule_id) l2 store_result
      in
      error, store_result)
    (*exists in both*)
      (fun parameter error (agent_type, rule_id) l1 l2 store_result ->
      let concat = List.concat [ l1; l2 ] in
      let error, store_result =
        Common_map.add_dependency_pair_sites_rule parameter error
          (agent_type, rule_id) concat store_result
      in
      error, store_result)
    fst_or_snd_store_result_hb fst_or_snd_store_result_remove store_result_map

let collect_potential_side_effects_free parameter error handler rule_id
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break parameter error handler rule_id half_break
      ( Ckappa_sig.AgentRule_map_and_set.Map.empty,
        Ckappa_sig.AgentRule_map_and_set.Map.empty )
  in
  let error, store_result_remove =
    store_potential_remove parameter error handler rule_id remove
      ( Ckappa_sig.AgentRule_map_and_set.Map.empty,
        Ckappa_sig.AgentRule_map_and_set.Map.empty )
  in
  (*-----------------------------------------------------------------------*)
  combine_half_break_and_remove parameter error (fst store_result_hb)
    (fst store_result_remove) store_result_map

let collect_potential_side_effects_bind parameter error handler rule_id
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break parameter error handler rule_id half_break
      ( Ckappa_sig.AgentRule_map_and_set.Map.empty,
        Ckappa_sig.AgentRule_map_and_set.Map.empty )
  in
  let error, store_result_remove =
    store_potential_remove parameter error handler rule_id remove
      ( Ckappa_sig.AgentRule_map_and_set.Map.empty,
        Ckappa_sig.AgentRule_map_and_set.Map.empty )
  in
  (*------------------------------------------------------------------------*)
  combine_half_break_and_remove parameter error (snd store_result_hb)
    (snd store_result_remove) store_result_map

let collect_potential_side_effects parameter error handler rule_id half_break
    remove store_result =
  let error, store_result =
    collect_potential_side_effects_bind parameter error handler rule_id
      half_break remove store_result
  in
  error, store_result

let scan_rule_side_effects_views parameter error kappa_handler rule_id rule
    store_result =
  let error, store_side_effects =
    collect_side_effects parameter error kappa_handler rule_id
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
      rule.Cckappa_sig.actions.Cckappa_sig.remove
      store_result.store_side_effects
  in
  (*-----------------------------------------------------------------------*)
  (*potential partner side effects*)
  let error, store_potential_side_effects =
    collect_potential_side_effects parameter error kappa_handler rule_id
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
      rule.Cckappa_sig.actions.Cckappa_sig.remove
      store_result.store_potential_side_effects
  in
  error, { store_side_effects; store_potential_side_effects }

(***************************************************************************)
(*BINDING*)
(***************************************************************************)

let collect_agent_type_binding_state parameter error agent site_type =
  match agent with
  | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _ ->
    error, (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Dead_agent _ ->
    Exception.warn parameter error __POS__ Exit
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Agent agent1 ->
    let agent_type1 = agent1.Cckappa_sig.agent_name in
    let error, state1 =
      match
        Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameter error
          site_type agent1.Cckappa_sig.agent_interface
      with
      | error, None ->
        Exception.warn parameter error __POS__ Exit Ckappa_sig.dummy_state_index
      | error, Some port ->
        let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
        let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
        (* It is a binding state *)
        (match state_min, state_max with
        | Some a, Some b when a = b -> error, a
        | None, _ | _, None | Some _, Some _ ->
          Exception.warn parameter error __POS__ Exit
            Ckappa_sig.dummy_state_index)
    in
    error, (agent_type1, state1)

let collect_fingerprint_of_binding parameter error agent_id site_type views =
  let error, agent_source =
    match
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter
        error agent_id views
    with
    | error, None ->
      Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
    | error, Some agent -> error, agent
  in
  (*get pair agent_type, state*)
  let error, (agent_type, state) =
    collect_agent_type_binding_state parameter error agent_source site_type
  in
  error, (agent_type, state)

let collect_fingerprint_of_bond parameter error site_add agent_id
    site_type_source views =
  let error, pair =
    let agent_index_target = site_add.Cckappa_sig.agent_index in
    let site_type_target = site_add.Cckappa_sig.site in
    let error, (agent_type1, state1) =
      collect_fingerprint_of_binding parameter error agent_id site_type_source
        views
    in
    let error, (agent_type2, state2) =
      collect_fingerprint_of_binding parameter error agent_index_target
        site_type_target views
    in
    let pair =
      ( (agent_type1, site_type_source, state1),
        (agent_type2, site_type_target, state2) )
    in
    error, pair
  in
  error, pair

let collect_bonds_pattern parameters error views bonds store_result =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_id bonds_map store_result ->
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun site_type_source site_add (error, store_result) ->
          let agent_id_target = site_add.Cckappa_sig.agent_index in
          let ( error,
                ( (agent_type1, site_type_source, state1),
                  (agent_type2, site_type_target, state2) ) ) =
            collect_fingerprint_of_bond parameters error site_add agent_id
              site_type_source views
          in
          let error, new_set =
            Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in
              parameters error
              ( (agent_id, agent_type1, site_type_source, state1),
                (agent_id_target, agent_type2, site_type_target, state2) )
              store_result
          in
          let error, store_result =
            Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in
              parameters error
              ( (agent_id_target, agent_type2, site_type_target, state2),
                (agent_id, agent_type1, site_type_source, state1) )
              new_set
          in
          error, store_result)
        bonds_map (error, store_result))
    bonds store_result

let collect_bonds parameters error rule_id views bonds store_result =
  let error, store_set =
    collect_bonds_pattern parameters error views bonds
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
  in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameters error rule_id
      store_set store_result
  in
  error, store_result

let collect_bonds_rhs parameter error rule_id rule store_result =
  collect_bonds parameter error rule_id
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds store_result

let collect_bonds_lhs parameter error rule_id rule store_result =
  collect_bonds parameter error rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds store_result

let collect_action_binding parameter error rule_id rule store_result =
  List.fold_left
    (fun (error, store_result) (site_add1, site_add2) ->
      (*get information of a rule that created a bond*)
      let agent_id1 = site_add1.Cckappa_sig.agent_index in
      let site_type1 = site_add1.Cckappa_sig.site in
      let agent_id2 = site_add2.Cckappa_sig.agent_index in
      let site_type2 = site_add2.Cckappa_sig.site in
      let error, (agent_type1, state1) =
        collect_fingerprint_of_binding parameter error agent_id1 site_type1
          rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      in
      let error, (agent_type2, state2) =
        collect_fingerprint_of_binding parameter error agent_id2 site_type2
          rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      in
      (*add the pair inside the set*)
      let error, old_set =
        Common_map.get_rule_id_map_and_set parameter error rule_id
          Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty store_result
      in
      let error, set =
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in parameter
          error
          ( (agent_id1, agent_type1, site_type1, state1),
            (agent_id2, agent_type2, site_type2, state2) )
          old_set
      in
      let error, set =
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in parameter
          error
          ( (agent_id2, agent_type2, site_type2, state2),
            (agent_id1, agent_type1, site_type1, state1) )
          set
      in
      let error, store_result =
        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id
          set store_result
      in
      error, store_result)
    (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind

let scan_rule_binding_views parameter error rule_id rule store_result =
  let error, store_bonds_rhs =
    collect_bonds_rhs parameter error rule_id rule store_result.store_bonds_rhs
  in
  let error, store_bonds_lhs =
    collect_bonds_lhs parameter error rule_id rule store_result.store_bonds_lhs
  in
  let error, store_action_binding =
    collect_action_binding parameter error rule_id rule
      store_result.store_action_binding
  in
  error, { store_bonds_rhs; store_bonds_lhs; store_action_binding }

(***************************************************************************)
(*VIEWS*)
(***************************************************************************)

let collect_views_pattern_aux ?(init = false) parameter handler error views
    store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent store_result ->
        (* JF: Unknown_agent cannot be dealt as ghost agent *)
        (* -> A ghost agent denotes no agent in a pattern, thus it is always
           satisfy *)
        (* A Dead_agent or an unknown agent, can never be satisfied *)
        (* Whatever you do, a ghost agent shoudl not change the result *)
        (* If the pattern contains an Unknown agent or a dead agent the
           pattern may not be reachable *)
        match agent with
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Unknown_agent _ ->
          Exception.warn parameter error __POS__ Exit store_result
        | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
          let error, site_map =
            Common_map.collect_site_map_for_views ~init parameter handler error
              agent
          in
          let error, store_result =
            Ckappa_sig.Agent_id_map_and_set.Map.add parameter error agent_id
              site_map store_result
          in
          error, store_result)
      views store_result
  in
  error, store_result

let collect_test_sites parameters error rule_id viewslhs store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun _parameters error agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Unknown_agent _ ->
          Exception.warn parameters error __POS__ Exit store_result
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, store_result =
            Common_map.collect_sites_map_in_agent_interface parameters error
              agent rule_id (agent_id, agent_type) store_result
          in
          error, store_result)
      viewslhs store_result
  in
  let store_result =
    Ckappa_sig.AgentsSite_map_and_set.Map.map (fun x -> x) store_result
  in
  error, store_result

let collect_views_aux parameter handler error rule_id views store_result =
  let error, old_map =
    Common_map.get_rule_id_map_and_set parameter error rule_id
      Ckappa_sig.Agent_id_map_and_set.Map.empty store_result
  in
  let error, map =
    collect_views_pattern_aux parameter handler error views old_map
  in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id map
      store_result
  in
  error, store_result

let collect_views_lhs parameter handler error rule_id rule store_result =
  collect_views_aux parameter handler error rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result

let collect_views_rhs parameter handler error rule_id rule store_result =
  collect_views_aux parameter handler error rule_id
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views store_result

let scan_rule_test parameters handler error rule_id rule store_result =
  let error, store_views_rhs =
    collect_views_rhs parameters handler error rule_id rule
      store_result.store_views_rhs
  in
  let error, store_views_lhs =
    collect_views_lhs parameters handler error rule_id rule
      store_result.store_views_lhs
  in
  let error, store_test_sites =
    collect_test_sites parameters error rule_id
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result.store_test_sites
  in
  error, { store_views_rhs; store_views_lhs; store_test_sites }

(***************************************************************************)
(*Modification*)
(***************************************************************************)

let collect_sites_from_agent_interface parameters error kappa_handler agent_id
    agent store_result =
  let agent_type = agent.Cckappa_sig.agent_name in
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun site_type port (error, store_result) ->
      let error, b =
        Handler.is_counter parameters error kappa_handler agent_type site_type
      in
      if b then
        error, store_result
      else (
        let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
        let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
        (*NOTE: state in modification is a singleton state*)
        let error, state =
          match state_min, state_max with
          | Some a, Some b when a = b -> error, a
          | None, _ | _, None | Some _, Some _ ->
            Exception.warn parameters error __POS__ Exit
              Ckappa_sig.dummy_state_index
        in
        let error, store_result =
          Ckappa_sig.AgentsSiteState_map_and_set.Set.add_when_not_in parameters
            error
            (agent_id, agent_type, site_type, state)
            store_result
        in
        error, store_result
      ))
    agent.Cckappa_sig.agent_interface (error, store_result)

let collect_modified_map parameter error kappa_handler rule_id rule store_result
    =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent store_result ->
        (*if there is no modified sites then do nothing*)
        if
          Ckappa_sig.Site_map_and_set.Map.is_empty
            agent.Cckappa_sig.agent_interface
        then
          error, store_result
        else (
          (*old set*)
          let error, old_set =
            Common_map.get_rule_id_map_and_set parameter error rule_id
              Ckappa_sig.AgentsSiteState_map_and_set.Set.empty store_result
          in
          let error, new_set =
            collect_sites_from_agent_interface parameter error kappa_handler
              agent_id agent old_set
          in
          let error, store_result =
            Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
              rule_id new_set store_result
          in
          error, store_result
        ))
      rule.Cckappa_sig.diff_direct store_result
  in
  error, store_result

let collect_modification_sites parameters error rule_id diff_direct store_result
    =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error agent_id agent_modif store_result ->
        if
          Ckappa_sig.Site_map_and_set.Map.is_empty
            agent_modif.Cckappa_sig.agent_interface
        then
          error, store_result
        else (
          let agent_type = agent_modif.Cckappa_sig.agent_name in
          (*return*)
          let error, store_result =
            Common_map.collect_sites_map_in_agent_interface parameters error
              agent_modif rule_id (agent_id, agent_type) store_result
          in
          error, store_result
        ))
      diff_direct store_result
  in
  let store_result =
    Ckappa_sig.AgentsSite_map_and_set.Map.map (fun x -> x) store_result
  in
  error, store_result

module Proj_modif =
  Map_wrapper.Proj
    (Ckappa_sig.AgentsSiteState_map_and_set)
    (Ckappa_sig.AgentSite_map_and_set)

let store_project_modified_map parameter error rule_id store_modified_map
    store_result =
  let error, modified_set =
    Common_map.get_rule_id_map_and_set parameter error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty store_modified_map
  in
  (*project set*)
  let error, project_set =
    Proj_modif.proj_set
      (fun (_, agent_type, site_type, _) -> agent_type, site_type)
      parameter error modified_set
  in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add parameter error rule_id project_set
      store_result
  in
  error, store_result

let scan_rule_modification parameters error kappa_handler rule_id rule
    store_result =
  let error, store_modified_map =
    collect_modified_map parameters error kappa_handler rule_id rule
      store_result.store_modified_map
  in
  let error, store_project_modified_map =
    store_project_modified_map parameters error rule_id store_modified_map
      store_result.store_project_modified_map
  in
  let error, store_modification_sites =
    collect_modification_sites parameters error rule_id
      rule.Cckappa_sig.diff_direct store_result.store_modification_sites
  in
  ( error,
    { store_modified_map; store_project_modified_map; store_modification_sites }
  )

(*********************************************************************)
(*VIEWS and MODIFICATION*)
(*********************************************************************)

let collect_test_modification_sites parameters error store_modification_map
    store_test_map store_result =
  Ckappa_sig.AgentsSite_map_and_set.Map.fold2 parameters error
    (*exists in 'a t*)
      (fun parameters error (agent_id, agent_type, site_type) s1 store_result ->
      let error, store_result =
        Common_map.add_dependency_triple_sites_rule parameters error
          (agent_id, agent_type, site_type)
          s1 store_result
      in
      error, store_result)
    (*exists in 'b t*)
      (fun parameters error (agent_id, agent_type, site_type) s2 store_result ->
      let error, store_result =
        Common_map.add_dependency_triple_sites_rule parameters error
          (agent_id, agent_type, site_type)
          s2 store_result
      in
      error, store_result)
    (*exists in both*)
      (fun parameters error (agent_id, agent_type, site_type) s1 s2 store_result ->
      let error', union =
        Ckappa_sig.Rule_map_and_set.Set.union parameters error s1 s2
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      let error, store_result =
        Common_map.add_dependency_triple_sites_rule parameters error
          (agent_id, agent_type, site_type)
          union store_result
      in
      error, store_result)
    store_modification_map store_test_map store_result

(***************************************************************************)
(*RULE*)
(***************************************************************************)

let scan_rule parameter error kappa_handler rule_id rule store_result =
  (*-----------------------------------------------------------------------*)
  (*get agent_name*)
  let error, store_agent_name =
    collect_agent_name parameter error rule_id rule
      store_result.store_agent_name
  in
  let error, store_agent_name_from_pattern =
    collect_agent_name_from_pattern parameter error rule.Cckappa_sig.rule_lhs
      store_result.store_agent_name_from_pattern
  in
  (*------------------------------------------------------------------------*)
  let error, store_side_effects_views =
    scan_rule_side_effects_views parameter error kappa_handler rule_id rule
      store_result.store_side_effects_views
  in
  (*------------------------------------------------------------------------*)
  let error, store_binding_views =
    scan_rule_binding_views parameter error rule_id rule
      store_result.store_binding_views
  in
  (*------------------------------------------------------------------------*)
  let error, store_modification =
    scan_rule_modification parameter error kappa_handler rule_id rule
      store_result.store_modification
  in
  (*------------------------------------------------------------------------*)
  let error, store_test =
    scan_rule_test parameter kappa_handler error rule_id rule
      store_result.store_test
  in
  (*------------------------------------------------------------------------*)
  let error, store_test_modification_sites =
    collect_test_modification_sites parameter error
      store_modification.store_modification_sites store_test.store_test_sites
      store_result.store_test_modification_sites
  in
  (*--------------------------------------------------------------*)
  (*valuations and update of the views that are tested and modification
    without agent_id*)
  let error, store_test_modif_map =
    Common_map.collect_projection_agent_id_from_triple parameter error
      store_test_modification_sites
  in
  (*--------------------------------------------------------------*)
  ( error,
    {
      store_result with
      store_agent_name;
      store_agent_name_from_pattern;
      store_side_effects_views;
      store_binding_views;
      store_modification;
      store_test;
      store_test_modification_sites;
      store_test_modif_map;
    } )

(******************************************************************************)

module Proj_agent_rule_to_rule =
  Map_wrapper.Proj
    (Ckappa_sig.AgentRule_map_and_set)
    (Ckappa_sig.Rule_map_and_set)

let scan_rule_set parameter error kappa_handler compil =
  let error, init_common_views = init_common_views parameter error in
  let error, store_result =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun parameter error rule_id rule store_result ->
        scan_rule parameter error kappa_handler rule_id
          rule.Cckappa_sig.e_rule_c_rule store_result)
      compil.Cckappa_sig.rules init_common_views
  in
  let error, potential_side_effects_per_rule =
    Proj_agent_rule_to_rule.monadic_proj_map_i
      (fun _parameter error (_, rule_id) -> error, rule_id)
      parameter error []
      (fun _parameters error old (agent_name, _) l ->
        let new_list =
          List.fold_left
            (fun old (source, (x, y)) -> (source, (agent_name, x, y)) :: old)
            old l
        in
        error, new_list)
      store_result.store_side_effects_views.store_potential_side_effects
  in
  ( error,
    {
      store_result with
      store_potential_side_effects_per_rule = potential_side_effects_per_rule;
    } )

(******************************************************************)
(******************************************************************)

type site_to_rules_tmp =
  Ckappa_sig.Rule_map_and_set.Set.t
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t

type site_to_rules =
  Ckappa_sig.c_rule_id list
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t

let add_dependency_site_rule parameter error agent site rule_id site_to_rules =
  let error, oldset =
    match
      Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
      .unsafe_get parameter error (agent, site) site_to_rules
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some old -> error, old
  in
  let error, newset =
    Ckappa_sig.Rule_map_and_set.Set.add_when_not_in parameter error rule_id
      oldset
  in
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.set
    parameter error (agent, site) newset site_to_rules

let empty_site_to_rules parameter error =
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
  .create parameter error (0, 0)

let consolidate_site_rule_dependencies parameter error site_to_rules =
  let error, output = empty_site_to_rules parameter error in
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.fold
    parameter error
    (fun parameter error key set output ->
      let list = Ckappa_sig.Rule_map_and_set.Set.elements set in
      Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
      .set parameter error key list output)
    site_to_rules output

let wake_up parameter error agent site site_to_rules =
  match
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .unsafe_get parameter error (agent, site) site_to_rules
  with
  | error, None -> error, []
  | error, Some l -> error, l

let get_tuple_of_interest parameters error agent site map =
  match
    Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameters
      error (agent, site) map
  with
  | error, None -> error, Ckappa_sig.PairAgentSitesState_map_and_set.Set.empty
  | error, Some s -> error, s
