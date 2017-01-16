(**
  * common_static_type.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 18th of Feburary
  * Last modification: Time-stamp: <Jan 16 2017>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let trace = false

(**************************************************************************)
(*TYPE*)
(**************************************************************************)

type half_break_action =
  (int list * (Ckappa_sig.c_rule_id * Ckappa_sig.pair_of_states) list)
    Ckappa_sig.AgentSite_map_and_set.Map.t

type remove_action =
  (int list * Ckappa_sig.c_rule_id list) Ckappa_sig.AgentSite_map_and_set.Map.t

type free_partner =
  (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list Ckappa_sig.AgentRule_map_and_set.Map.t

type bind_partner = (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list
    Ckappa_sig.AgentRule_map_and_set.Map.t

type potential_partner_free = free_partner
type potential_partner_bind = bind_partner

type bdu_common_static =
  {
    store_agent_name             : Ckappa_sig.c_agent_name
        Ckappa_sig.RuleAgent_map_and_set.Map.t;
    store_agent_name_from_pattern :
      Ckappa_sig.c_agent_name Ckappa_sig.Agent_id_map_and_set.Map.t;
    store_side_effects           : half_break_action * remove_action;
    store_potential_side_effects :
      potential_partner_free *  potential_partner_bind;
    store_potential_side_effects_per_rule:
      (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
        list Ckappa_sig.Rule_map_and_set.Map.t;
    (*bond in the rhs and in the lhs*)
    store_bonds_rhs : Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_bonds_lhs : Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_action_binding : Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_views_rhs :
      Ckappa_sig.AgentsSitePState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_views_lhs :
      Ckappa_sig.AgentsSitePState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_modified_map :
      Ckappa_sig.AgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_project_modified_map : (*use in parallel domain*)
    Ckappa_sig.AgentSite_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;
    store_views_lhs' : (*TODO*)
      (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
       Ckappa_sig.pair_of_states)
        Ckappa_sig.Agent_id_map_and_set.Map.t
        Ckappa_sig.Rule_map_and_set.Map.t
  }

(*****************************************************************************)
(*Initial states of bdu common statics*)
(*****************************************************************************)

let init_bdu_common_static =
  let init_agent_name = Ckappa_sig.RuleAgent_map_and_set.Map.empty in
  let init_half_break     = Ckappa_sig.AgentSite_map_and_set.Map.empty  in
  let init_remove         = Ckappa_sig.AgentSite_map_and_set.Map.empty  in
  let init_potential_free = Ckappa_sig.AgentRule_map_and_set.Map.empty in
  let init_potential_bind = Ckappa_sig.AgentRule_map_and_set.Map.empty in
  let init_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_action_binding = Ckappa_sig.Rule_map_and_set.Map.empty in
  let inite_potential_side_effects_per_rule =
    Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_views_lhs = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_modified_map = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_project_modified_map = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_common_static =
    {
      store_agent_name              = init_agent_name;
      store_agent_name_from_pattern =
        Ckappa_sig.Agent_id_map_and_set.Map.empty;
      store_side_effects            = (init_half_break, init_remove);
      store_potential_side_effects  =
        (init_potential_free, init_potential_bind);
      store_potential_side_effects_per_rule =
        inite_potential_side_effects_per_rule;
      store_bonds_rhs = init_bonds_rhs;
      store_bonds_lhs = init_bonds_lhs;
      store_action_binding = init_action_binding;
      store_views_rhs = init_views_rhs;
      store_views_lhs = init_views_lhs;
      store_modified_map = init_modified_map;
      store_project_modified_map = init_project_modified_map;
      store_views_lhs' = Ckappa_sig.Rule_map_and_set.Map.empty
    }
  in
  init_common_static

(****************************************************************************)
(*return agent_name with a pair of key (rule_id and agent_id) in the lhs*)

let collect_agent_name parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Unknown_agent _ ->
           Exception.warn parameter error __POS__ Exit store_result
         | Cckappa_sig.Dead_agent (agent, _, _, _)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, store_result =
             Ckappa_sig.RuleAgent_map_and_set.Map.add_or_overwrite
               parameter
               error
               (rule_id, agent_id)
               agent_type
               store_result
           in
           error, store_result
      ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
  in
  error, store_result

(**************************************************************************)
(*return agant_name with a key rule_id in a pattern*)

let collect_agent_name_from_pattern parameters error pattern store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameters
      error
      (fun parameters error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Unknown_agent _ ->
           Exception.warn parameters error __POS__ Exit store_result
         | Cckappa_sig.Dead_agent (agent, _, _, _)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, store_result =
             Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
               parameters
               error
               agent_id
               agent_type
               store_result
           in
           error, store_result
      ) pattern.Cckappa_sig.views store_result
  in
  error, store_result

(**************************************************************************)
(*Side effects*)
(**************************************************************************)

(*let add_link_pair_agent_site parameters error (agent_type, site_type) x
    store_result =
  let error, (l, old) =
    match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
            parameters
            error
            (agent_type, site_type)
            store_result
    with
    | error, None -> error, ([], [])
    | error, Some (l, l') -> error, (l, l')
  in
  let error, store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
      parameters
      error
      (agent_type, site_type)
      (l, x :: old)
      store_result
  in
  error, store_result*)

let get_last_entry_in_state_dic parameters error (agent_type, site_type)
    handler  =
  let error, state_dic =
    Misc_sa.unsome
      (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
         parameters
         error
         (agent_type, site_type)
         handler.Cckappa_sig.states_dic)
      (fun error ->
         Exception.warn
           parameters error __POS__ Exit
           (Ckappa_sig.Dictionary_of_States.init()))
  in
  let error, last_entry =
    Ckappa_sig.Dictionary_of_States.last_entry parameters error
      state_dic
  in
  error, last_entry

let half_break_action parameters error handler rule_id half_break store_result =
  (*module (agent_type, site) -> (rule_id, binding_state) list*)
  let error, store_result =
    List.fold_left (fun (error, store_result) (site_address, state_op) ->
        (*site_address: {agent_index, site, agent_type}*)
        let agent_type = site_address.Cckappa_sig.agent_type in
        let site_type = site_address.Cckappa_sig.site in
        (*state*)
        let error, (state_min, state_max) =
          match state_op with
          | None ->
            begin
              let error, last_entry =
                get_last_entry_in_state_dic
                  parameters error
                  (agent_type, site_type)
                  handler
              in
              error, (Ckappa_sig.dummy_state_index_1, last_entry)
            end
          | Some interval ->
            error, (interval.Cckappa_sig.min, interval.Cckappa_sig.max)
        in
        (*-------------------------------------------------------------------*)
        (*return result*)
        let error, store_result =
          Common_map.add_dependency_pair_sites
            parameters
            error
            (agent_type, site_type)
            (rule_id, (state_min, state_max))
            store_result
        in
        error, store_result
      ) (error, store_result) half_break
  in
  (*--------------------------------------------------------------------*)
  (*map function*)
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x)
      store_result
  in
  error, store_result

(***************************************************************************)
(*Side effects remove action*)

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
                Common_map.add_dependency_pair_sites
                  parameters
                  error
                  (agent_type, site_type)
                  rule_id
                  store_result
             ) (error, store_result) list_undoc
         in
         error, store_result
      ) (error, store_result) remove
  in
  (*-------------------------------------------------------------------------*)
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x)
      store_result
  in
  error, store_result

(****************************************************************************)
(*return a potential sites of side effects in the case of half break action*)

(*let add_link_pair_agent_rule parameters error (agent_type, rule_id)
    l store_result =
  let error, old =
    match
      Ckappa_sig.AgentRule_map_and_set.Map.find_option_without_logs
        parameters error
        (agent_type, rule_id)
        store_result
    with
    | error, None -> error, []
    | error, Some l -> error, l
  in
  let new_list = List.concat [l; old] in
  let error, result =
    Ckappa_sig.AgentRule_map_and_set.Map.add_or_overwrite
      parameters error
      (agent_type, rule_id)
      new_list
      store_result
  in
  error, result*)

let collect_potential_free_and_bind parameter error handler rule_id
    (agent_type, site_type) k store_result =
  (*potential partner*)
  match Handler.dual parameter error handler agent_type site_type k with
  | error, None -> error, store_result
  | error, Some (agent_type2, site2, state2) ->
    let error, store_potential_free =
      Common_map.add_dependency_pair_sites_rule parameter error
        (agent_type2, rule_id)
        ((site2,Ckappa_sig.dummy_state_index) :: [])
        (fst store_result)
    in
    let error, store_potential_bind =
      Common_map.add_dependency_pair_sites_rule parameter error
        (agent_type2, rule_id)
        ((site2, state2) :: [])
        (snd store_result)
    in
    error, (store_potential_free, store_potential_bind)

let store_potential_half_break parameter error handler rule_id half_break
    store_result =
  List.fold_left
    (fun (error, store_result) (add, state_op) ->
       let agent_type = add.Cckappa_sig.agent_type in
       let site_type = add.Cckappa_sig.site in
       (*state*)
       let error, (state_min, state_max) =
         match state_op with
         | None ->
           begin
             let error, last_entry =
               get_last_entry_in_state_dic
                 parameter
                 error
                 (agent_type, site_type)
                 handler
             in
             error, (Ckappa_sig.dummy_state_index_1, last_entry)
           end
         | Some interval ->
           error,
           (interval.Cckappa_sig.min, interval.Cckappa_sig.max)
       in
       (*--------------------------------------------------------------------*)
       let rec aux k (error, store_result) =
         if Ckappa_sig.compare_state_index k state_max > 0
         then
           error, store_result
         else
           (*potential partner*)
           let error, (store_potential_free, store_potential_bind) =
             collect_potential_free_and_bind
               parameter error handler rule_id
               (agent_type, site_type)
               k
               store_result
           in
           aux
             (Ckappa_sig.next_state_index k)
             (error, (store_potential_free, store_potential_bind))
       in aux state_min (error, store_result)
    ) (error, store_result) half_break

(***************************************************************************)
(*potential partner of remove action*)

let store_potential_remove parameter error handler rule_id remove store_result =
  List.fold_left (fun (error, store_result) (_agent_index, agent, list_undoc) ->
      let agent_type = agent.Cckappa_sig.agent_name in
      let error, store_result =
        List.fold_left (fun (error, store_result) site_type ->
            let error, is_binding =
              Handler.is_binding_site parameter error handler agent_type
                site_type
            in
            if is_binding
            then
              begin
                let error, last_entry =
                  get_last_entry_in_state_dic parameter error
                    (agent_type, site_type)
                    handler
                in
                (*-----------------------------------------------------------*)
                let rec aux k (error, store_result) =
                  if Ckappa_sig.compare_state_index k last_entry > 0
                  then
                    error, store_result
                  else
                    (*potential partner*)
                    let error, (store_potential_free, store_potential_bind) =
                      collect_potential_free_and_bind
                        parameter error
                        handler
                        rule_id
                        (agent_type, site_type)
                        k
                        store_result
                    in
                    aux
                      (Ckappa_sig.next_state_index k)
                      (error, (store_potential_free, store_potential_bind))
                in
                aux Ckappa_sig.dummy_state_index_1 (error, store_result)
              end
            else
              error, store_result
          ) (error, store_result) list_undoc
      in
      error, store_result
    ) (error, store_result) remove

(***************************************************************************)

let combine_half_break_and_remove parameter error
    fst_or_snd_store_result_hb
    fst_or_snd_store_result_remove
    store_result_map =
    (*-----------------------------------------------------------------------*)
  Ckappa_sig.AgentRule_map_and_set.Map.fold2
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, rule_id) l1 store_result ->
       let error, store_result =
         Common_map.add_dependency_pair_sites_rule parameter error
           (agent_type, rule_id)
           l1
           store_result
       in
       error, store_result
    )
    (*exists in 'b t*)
    (fun paramter error (agent_type, rule_id) l2 store_result ->
       let error, store_result =
         Common_map.add_dependency_pair_sites_rule paramter error
           (agent_type, rule_id)
           l2
           store_result
       in
       error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, rule_id) l1 l2 store_result ->
       let concat = List.concat [l1; l2] in
       let error, store_result =
         Common_map.add_dependency_pair_sites_rule parameter error
           (agent_type, rule_id)
           concat
           store_result
       in
       error, store_result
    )
    fst_or_snd_store_result_hb
    fst_or_snd_store_result_remove
    store_result_map

let collect_potential_side_effects_free parameter error handler rule_id
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break
      parameter
      error
      handler
      rule_id
      half_break
      (Ckappa_sig.AgentRule_map_and_set.Map.empty,
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  let error, store_result_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (Ckappa_sig.AgentRule_map_and_set.Map.empty,
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  (*-----------------------------------------------------------------------*)
  combine_half_break_and_remove parameter error
    (fst store_result_hb)
    (fst store_result_remove)
    store_result_map

(***************************************************************************)

let collect_potential_side_effects_bind parameter error handler rule_id
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break
      parameter
      error
      handler
      rule_id
      half_break
      (Ckappa_sig.AgentRule_map_and_set.Map.empty,
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  let error, store_result_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (Ckappa_sig.AgentRule_map_and_set.Map.empty,
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  (*------------------------------------------------------------------------*)
  combine_half_break_and_remove parameter error
    (snd store_result_hb)
    (snd store_result_remove)
    store_result_map

(***************************************************************************)

let collect_potential_side_effects parameter error handler rule_id half_break
    remove store_result =
  let error, store_result_free =
    collect_potential_side_effects_free
      parameter
      error
      handler
      rule_id
      half_break
      remove
      (fst store_result)
  in
  let error, store_result_bind =
    collect_potential_side_effects_bind
      parameter
      error
      handler
      rule_id
      half_break
      remove
      (snd store_result)
  in
  error, (store_result_free, store_result_bind)

(***************************************************************************)
(*compute side effects: this is an update before discover bond function *)

let collect_side_effects parameter error handler rule_id half_break remove
    store_result =
  let store_half_break_action, store_remove_action = store_result in
  (*if there is a half_break action*)
  let error, store_half_break_action =
    half_break_action
      parameter
      error
      handler
      rule_id
      half_break
      store_half_break_action
  in
  (*if there is a remove action*)
  let error, store_remove_action =
    remove_action
      parameter
      error
      rule_id
      remove
      store_remove_action
  in
  error, (store_half_break_action, store_remove_action)

(***************************************************************************)
(*Collect bonds in the rhs and lhs*)
(***************************************************************************)

let collect_agent_type_binding_state parameter error agent site_type =
  match agent with
  | Cckappa_sig.Ghost
  | Cckappa_sig.Unknown_agent _ ->
    error,
    (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Dead_agent _ ->
    Exception.warn parameter error __POS__ Exit
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Agent agent1 ->
    let agent_type1 = agent1.Cckappa_sig.agent_name in
    let error, state1 =
      match Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
              parameter
              error
              site_type
              agent1.Cckappa_sig.agent_interface
      with
      | error, None ->
        Exception.warn
          parameter error __POS__ Exit Ckappa_sig.dummy_state_index
      | error, Some port ->
        let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
        let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
        (* It is a binding state *)
        if state_min = state_max
        then error, state_min
        else
        Exception.warn
          parameter error __POS__ Exit Ckappa_sig.dummy_state_index
        (*if Ckappa_sig.compare_state_index state Ckappa_sig.dummy_state_index > 0
        then
          error, state
        else
          Exception.warn
            parameter error __POS__ Exit Ckappa_sig.dummy_state_index*)
    in
    error, (agent_type1, state1)

(**************************************************************************)
(* What you collect is the type fingerprint of a bond *)

let collect_fingerprint_of_binding parameter error agent_id site_type views =
  let error, agent_source =
    match
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
        parameter error
        agent_id
        views
    with
    | error, None ->
      Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
    | error, Some agent -> error, agent
  in
  (*get pair agent_type, state*)
  let error, (agent_type, state) =
    collect_agent_type_binding_state
      parameter
      error
      agent_source
      site_type
  in
  error, (agent_type, state)

let collect_fingerprint_of_bond parameter error site_add agent_id site_type_source
    views =
  let error, pair =
    let agent_index_target = site_add.Cckappa_sig.agent_index in
    let site_type_target = site_add.Cckappa_sig.site in
    let error, (agent_type1, state1) =
      collect_fingerprint_of_binding parameter error
        agent_id
        site_type_source
        views
    in
    let error, (agent_type2, state2) =
      collect_fingerprint_of_binding parameter error
        agent_index_target
        site_type_target
        views
    in
    let pair = ((agent_type1, site_type_source, state1),
                (agent_type2, site_type_target, state2))
    in
    error, pair
  in
  error, pair

(**************************************************************************)

let get_rule_id_set parameter error rule_id empty_set store_result =
  let error, set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_result
    with
    | error, None -> error, empty_set
    | error, Some s -> error, s
  in
  error, set

let add_set parameter error (x, y) set =
  let error', set =
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in
      parameter
      error
      (x, y)
      set
  in
  let error =
    Exception.check_point
      Exception.warn parameter error error' __POS__ Exit
  in
  error, set

let add_map_rule parameter error rule_id contain_map store_result =
  Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
    parameter
    error
    rule_id
    contain_map
    store_result

(***************************************************************************)
(*bonds*)
(***************************************************************************)

let collect_bonds_pattern parameters error views bonds store_result =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters error
    (fun parameters error agent_id bonds_map store_result ->
       Ckappa_sig.Site_map_and_set.Map.fold
         (fun site_type_source site_add (error, store_result) ->
         let agent_id_target = site_add.Cckappa_sig.agent_index in
         let error, ((agent_type1, site_type_source, state1),
              (agent_type2, site_type_target, state2)) =
           collect_fingerprint_of_bond parameters
             error
             site_add
             agent_id
             site_type_source
             views
         in
         let error, new_set =
           add_set parameters error
             ((agent_id, agent_type1, site_type_source, state1),
              (agent_id_target, agent_type2, site_type_target, state2))
             store_result
         in
         let error, store_result =
           add_set parameters error
             ((agent_id_target, agent_type2, site_type_target, state2),
              (agent_id, agent_type1, site_type_source, state1))
             new_set
         in
         error, store_result
         ) bonds_map (error, store_result)
    ) bonds store_result

let collect_bonds parameters error rule_id views bonds store_result =
  let error, store_set =
    collect_bonds_pattern parameters error views bonds
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
  in
  let error, store_result =
    add_map_rule parameters error rule_id store_set store_result
  in
  error, store_result

let collect_bonds_rhs parameter error rule_id rule store_result =
  collect_bonds parameter error
    rule_id
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
    store_result

let collect_bonds_lhs parameter error rule_id rule store_result =
  collect_bonds
    parameter error
    rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
    store_result

(***************************************************************************)
(*action binding in the rhs*)
(***************************************************************************)

let collect_action_binding parameter error rule_id rule store_result =
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
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
        collect_fingerprint_of_binding parameter error
          agent_id2
          site_type2
          rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      in
      (*add the pair inside the set*)
      let error, old_set =
        get_rule_id_set parameter error rule_id
          Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
          store_result
      in
      let error, set =
        add_set parameter error ((agent_id1, agent_type1, site_type1, state1),
                                 (agent_id2, agent_type2, site_type2, state2))
          old_set
      in
      let error, set =
        add_set parameter error
          ((agent_id2, agent_type2, site_type2, state2),
           (agent_id1, agent_type1, site_type1, state1))
          set
      in
      let error, store_result =
        add_map_rule parameter error rule_id set store_result
      in
      error, store_result
    ) (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind

(***************************************************************************)
(*views in pattern, this function test pattern on the lhs*)
(***************************************************************************)

let get_agent_info_from_agent_interface parameters error agent_id agent
    store_result =
  let agent_type = agent.Cckappa_sig.agent_name in
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun site_type port (error, store_result) ->
       let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
       let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
       (* JF: No, you should store the interval as a pair of sites,
                 and not only one random bond *)
       let error, store_result =
         Ckappa_sig.AgentsSitePState_map_and_set.Set.add_when_not_in
           parameters error
           (agent_id, agent_type, site_type, (state_min, state_max))
           store_result
       in
       error, store_result
    ) agent.Cckappa_sig.agent_interface (error, store_result)

let collect_views_pattern_aux parameter error views store_result =
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
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let error, store_result =
             get_agent_info_from_agent_interface
               parameter error
               agent_id
               agent
               store_result
           in
           error, store_result
      ) views store_result
  in
  error, store_result

(***************************************************************************)
(*views*)
(***************************************************************************)

let collect_views_aux parameter error rule_id views store_result =
  (*let error, store_set =
    collect_views_pattern_aux parameter error views
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
  in
  let error, store_result =
    add_map_rule parameter error rule_id store_set store_result
  in
  error, store_result*)
   let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Unknown_agent _ ->
           Exception.warn parameter error __POS__ Exit store_result
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, old_set =
             get_rule_id_set parameter error
               rule_id
               Ckappa_sig.AgentsSitePState_map_and_set.Set.empty
               store_result
           in
           let error', set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  (*CHECK max = min*)
                  let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
                    let error, store_set =
                      Ckappa_sig.AgentsSitePState_map_and_set.Set.add_when_not_in
                        parameter error
                        (agent_id, agent_type, site_type,
                         (state_min, state_max))
                        store_set
                    in
                    error, store_set
               ) agent.Cckappa_sig.agent_interface (error, old_set)
           in
           let error =
             Exception.check_point
               Exception.warn parameter error error'
               __POS__ Exit
           in
           let error, store_result =
             add_map_rule parameter error rule_id set store_result
           in
           error, store_result
      ) views store_result
  in
  error, store_result

let collect_views_rhs parameter error rule_id rule store_result =
  collect_views_aux
    parameter error
    rule_id
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    store_result

let collect_views_lhs parameter error rule_id rule store_result =
  collect_views_aux
    parameter error
    rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    store_result

let collect_views_aux' parameter error rule_id views store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Unknown_agent _ ->
           Exception.warn parameter error __POS__ Exit store_result
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, old_map =
             get_rule_id_set parameter error
               rule_id
               Ckappa_sig.Agent_id_map_and_set.Map.empty
               store_result
           in
           let error, map =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_map) ->
                  let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let state_min =
                    port.Cckappa_sig.site_state.Cckappa_sig.min
                  in
                  let error, store_map =
                    Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
                      parameter error
                      agent_id
                      (agent_type, site_type, (state_min, state_max))
                      store_map
                  in
                  error, store_map
               ) agent.Cckappa_sig.agent_interface (error, old_map)
           in
           let error, store_result =
             add_map_rule parameter error rule_id map store_result
           in
           error, store_result
      ) views store_result
  in
  error, store_result

let collect_views_lhs' parameter error rule_id rule store_result =
  collect_views_aux'
    parameter error
    rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    store_result

(***************************************************************************)
(*Modification*)
(***************************************************************************)


let get_agent_info_from_agent_interface' parameters error agent_id agent
    store_result =
  let agent_type = agent.Cckappa_sig.agent_name in
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun site_type port (error, store_result) ->
       let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
       let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
       (*NOTE: state in modification is a singleton state*)
       let error, state =
         if state_min = state_max
         then error, state_min
         else Exception.warn parameters error __POS__ Exit
             Ckappa_sig.dummy_state_index
       in
       let error, store_result =
         Ckappa_sig.AgentsSiteState_map_and_set.Set.add_when_not_in
           parameters error
           (agent_id, agent_type, site_type, state)
           store_result
       in
       error, store_result
    ) agent.Cckappa_sig.agent_interface (error, store_result)

let collect_modified_map parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent store_result->
         (*if there is no modified sites then do nothing*)
         if Ckappa_sig.Site_map_and_set.Map.is_empty
             agent.Cckappa_sig.agent_interface
         then error, store_result
         else
           (*old set*)
           let error, old_set =
             get_rule_id_set parameter error
               rule_id
               Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
               store_result
           in
           let error, new_set =
             get_agent_info_from_agent_interface'
               parameter error
               agent_id
               agent
               old_set
           in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
               parameter
               error
               rule_id
               new_set
               store_result
           in
           error, store_result
      ) rule.Cckappa_sig.diff_direct store_result
  in
  error, store_result

(*********************************************************************)
(*projectioin modification rules from: (id, agent_name, site, state) ->
    (agent_name, site)*)

  module Proj_modif =
    Map_wrapper.Proj
      (Ckappa_sig.AgentsSiteState_map_and_set)
      (Ckappa_sig.AgentSite_map_and_set)

(*AgentsSiteState_map_and_set.Set Rule_map_and_set.Map*)
let store_project_modified_map parameter error rule_id store_modified_map
    store_result =
  let error, modified_set =
    get_rule_id_set parameter error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
      store_modified_map
  in
  (*project set*)
  let error, project_set =
    Proj_modif.proj_set
      (fun (_, agent_type, site_type, _) ->
       (agent_type, site_type)
      )
      parameter
      error
      modified_set
  in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add
      parameter error
      rule_id
      project_set
      store_result
  in
  error, store_result

(***************************************************************************)

let scan_rule parameter error handler_kappa rule_id rule store_result =
  (*-----------------------------------------------------------------------*)
  (*get agent_name*)
  let error, store_agent_name =
    collect_agent_name
      parameter
      error
      rule_id
      rule
      store_result.store_agent_name
  in
  let error, store_agent_name_from_pattern =
    collect_agent_name_from_pattern
      parameter
      error
      rule.Cckappa_sig.rule_lhs
      store_result.store_agent_name_from_pattern
  in
  (*-------------------------------------------------------------------------*)
  (*side effects*)
  let error, store_side_effects =
    collect_side_effects
      parameter
      error
      handler_kappa
      rule_id
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
      rule.Cckappa_sig.actions.Cckappa_sig.remove
      store_result.store_side_effects
  in
  (*-----------------------------------------------------------------------*)
  (*potential partner side effects*)
  let error, store_potential_side_effects =
    collect_potential_side_effects
      parameter
      error
      handler_kappa
      rule_id
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
      rule.Cckappa_sig.actions.Cckappa_sig.remove
      store_result.store_potential_side_effects
  in
  (*------------------------------------------------------------------------*)
  (*bonds in the rhs and lhs*)
  let error, store_bonds_rhs =
    collect_bonds_rhs
      parameter
      error
      rule_id
      rule
      store_result.store_bonds_rhs
  in
  let error, store_bonds_lhs =
    collect_bonds_lhs
      parameter
      error
      rule_id
      rule
      store_result.store_bonds_lhs
  in
  (*------------------------------------------------------------------------*)
  let error, store_action_binding =
    collect_action_binding
      parameter
      error
      rule_id
      rule
      store_result.store_action_binding
  in
  let error, store_views_rhs =
    collect_views_rhs parameter error rule_id rule store_result.store_views_rhs
  in
  let error, store_views_lhs =
    collect_views_lhs parameter error rule_id rule store_result.store_views_lhs
  in
  let error, store_modified_map =
    collect_modified_map parameter error rule_id rule
      store_result.store_modified_map
  in
  let error, store_project_modified_map =
    store_project_modified_map parameter error rule_id store_modified_map
      store_result.store_project_modified_map
  in
  let error, store_views_lhs' =
    collect_views_lhs' parameter error rule_id rule
      store_result.store_views_lhs'
  in
  error,
  {store_result with
   store_agent_name = store_agent_name;
   store_agent_name_from_pattern = store_agent_name_from_pattern;
   store_side_effects = store_side_effects;
   store_potential_side_effects = store_potential_side_effects;
   store_bonds_rhs = store_bonds_rhs;
   store_bonds_lhs = store_bonds_lhs;
   store_action_binding = store_action_binding;
   store_views_rhs = store_views_rhs;
   store_views_lhs = store_views_lhs;
   store_modified_map = store_modified_map;
   store_project_modified_map = store_project_modified_map;
   store_views_lhs' = store_views_lhs'
  }

(******************************************************************************)

module Proj_agent_rule_to_rule =
  Map_wrapper.Proj
    (Ckappa_sig.AgentRule_map_and_set)
    (Ckappa_sig.Rule_map_and_set)

let scan_rule_set parameter error handler_kappa compil =
  let error, store_result =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
         scan_rule
           parameter
           error
           handler_kappa
           rule_id
           rule.Cckappa_sig.e_rule_c_rule
           store_result
      ) compil.Cckappa_sig.rules init_bdu_common_static
  in
  let error, potential_side_effects_per_rule =
    Proj_agent_rule_to_rule.monadic_proj_map_i
      (fun _parameter error (_,rule_id) -> error, rule_id) parameter error []
      (fun _parameters error old (agent_name,_) l ->
         let new_list =
           List.fold_left
             (fun old (x,y) -> (agent_name,x,y)::old)
             old l
         in
         error,new_list)
      (snd store_result.store_potential_side_effects)
  in
  error,
  {store_result
   with
    store_potential_side_effects_per_rule = potential_side_effects_per_rule
  }

type site_to_rules_tmp =
  Ckappa_sig.Rule_map_and_set.Set.t
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t

type site_to_rules =
  Ckappa_sig.c_rule_id list
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t

let add_dependency_site_rule parameter error agent site rule_id site_to_rules =
  let error, oldset =
    match
      Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.unsafe_get
        parameter error
        (agent, site)
        site_to_rules
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some old -> error, old
  in
  let error, newset =
    Ckappa_sig.Rule_map_and_set.Set.add_when_not_in
      parameter error rule_id oldset
  in
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.set
    parameter error
    (agent, site)
    newset
    site_to_rules

let empty_site_to_rules parameter error =
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.create
    parameter error (0,0)

let consolidate_site_rule_dependencies parameter error site_to_rules =
  let error, output = empty_site_to_rules parameter error in
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.fold
    parameter error
    (fun parameter error key set output ->
      let list = Ckappa_sig.Rule_map_and_set.Set.elements set in
      Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.set
        parameter error key list output
    )
    site_to_rules
    output

let wake_up parameter error agent site site_to_rules =
  match
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.unsafe_get
      parameter error
      (agent,site)
      site_to_rules
  with
  | error, None -> error, []
  | error, Some l -> error, l

let get_tuple_of_interest parameters error agent site map =
  match
    Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
      parameters error
      (agent, site)
      map
  with
  | error, None -> error, Ckappa_sig.PairAgentSitesState_map_and_set.Set.empty
  | error, Some s -> error, s
