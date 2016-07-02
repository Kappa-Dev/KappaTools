(**
   * site_accross_bonds_domain_static.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 29th of June
   * Last modification: Time-stamp: <Jul 02 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Site accross domain static information") message exn
    (fun () -> default)

let local_trace = false

(***************************************************************)
(*type*)

type basic_static_information =
  {
    store_views_rhs : Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_bonds_rhs: Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_modified_map :
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_tuple_pair : Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t;
    store_created_bond :         Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_modified_internal_state_and_bond :
      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_question_marks_rhs :
      Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_implicit_static :
      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_explicit_static :
      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
  }

(***************************************************************)
(*collect views on the right hand side*)

let collect_views_rhs parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Unknown_agent _
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           (*--------------------------------------------*)
           let error, set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, state)
                      store_set
                  in
                  error, store_set
               ) agent.Cckappa_sig.agent_interface (error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty)
           in
           (*--------------------------------------------*)
           (*old set*)
           let error, old_set =
             match
               Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                 parameter error rule_id store_result
             with
             | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
             | error, Some s -> error, s
           in
           (*--------------------------------------------*)
           (*new set*)
           let error', new_set =
             Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error old_set set
           in
           let error = Exception.check warn parameter error error'
               (Some "line 210") Exit
           in
           (*--------------------------------------------*)
           (*store*)
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
               parameter error rule_id
               new_set
               store_result
           in
           error, store_result
      ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.views store_result
  in
  error, store_result

(***************************************************************)
(*collect rule that can be bound on the right hand side*)

let collect_agent_type_state parameter error agent site_type =
  match agent with
  | Cckappa_sig.Ghost
  | Cckappa_sig.Unknown_agent _ -> error, (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Dead_agent _ ->
    warn parameter error (Some "line 127") Exit (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
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
        warn parameter error (Some "line 228") Exit Ckappa_sig.dummy_state_index
      | error, Some port ->
        let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
        if (Ckappa_sig.int_of_state_index state) > 0
        then
          error, state
        else
          warn parameter error (Some "line 196") Exit Ckappa_sig.dummy_state_index
    in
    error, (agent_type1, state1)

let collect_bonds_rhs parameter error rule_id rule store_result =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter error
    (fun parameter error agent_id bonds_map store_result ->
       Ckappa_sig.Site_map_and_set.Map.fold
         (fun site_type_source site_add (error, store_result) ->
            let agent_id_target = site_add.Cckappa_sig.agent_index in
            let site_type_target = site_add.Cckappa_sig.site in
            let error, agent_source =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
              with
              | error, None -> warn parameter error (Some "line 332") Exit Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            (*get the first pair (agent_type, state)*)
            let error, (agent_type1, state1) =
              collect_agent_type_state
                parameter
                error
                agent_source
                site_type_source
            in
            (*----------------------------------------------------*)
            (*the second pair*)
            let error, agent_target =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id_target rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
              with
              | error, None -> warn parameter error (Some "line 350") Exit Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            let error, (agent_type2, state2) =
              collect_agent_type_state
                parameter
                error
                agent_target
                site_type_target
            in
            (*-----------------------------------------------------*)
            (*get old set*)
            let error, old_set =
              match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result
              with
              | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
              | error, Some p -> error, p
            in
            let error', set =
              Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                parameter error
                ((agent_id, agent_type1, site_type_source, state1),
                 (agent_id_target, agent_type2, site_type_target, state2))
                old_set
            in
            let error = Exception.check warn parameter error error' (Some "line 375") Exit in
            let error, store_result =
              Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                parameter
                error
                rule_id
                set
                store_result
            in
            error, store_result
         ) bonds_map (error, store_result)
    ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds store_result

(***************************************************************)
(*collect rule that can be modified*)

let collect_site_modified parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun parameter error agent_id agent store_result->
         (*if there is no modified sites then do nothing*)
         if Ckappa_sig.Site_map_and_set.Map.is_empty agent.Cckappa_sig.agent_interface
         then error, store_result
         else
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, pair_set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in parameter error
                      (agent_id, agent_type, site_type, state)
                      store_set
                  in
                  error, store_set
               )
               agent.Cckappa_sig.agent_interface (error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty)
           in
           (*old set*)
           let error, old_set =
             match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
             | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
             | error, Some s -> error, s
           in
           let error, new_set =
             Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error pair_set old_set
           in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
           in
           error, store_result
      ) rule.Cckappa_sig.diff_direct store_result
  in
  error, store_result

(***************************************************************)
(*collect a set of tuple pair (A.x.y, B.z.t)*)

let collect_pair_sites_aux parameter error rule_id store_views_rhs =
  let error, views_rhs_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_views_rhs
    with
    | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*fold over on the rhs*)
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
        (*fold again views rhs*)
        let error, pair_set =
          Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id', _, site_type', state') (error, current_set) ->
              if agent_id = agent_id' && site_type <> site_type'
              then
                let error, pair_set =
                  Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                    parameter error
                    (agent_id, agent_type, site_type, site_type', state, state')
                    current_set
                in
                error, pair_set
              else error, current_set
            ) views_rhs_set (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
        in
        let error, old_set =
          match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                  parameter error rule_id store_result with
          | error, None -> error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, new_set =
          Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error old_set pair_set
        in
        let error, store_result =
          Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
            parameter error rule_id new_set store_result
        in
        error, store_result
      ) views_rhs_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
  in
  error, store_result

let collect_tuple_pair parameter error rule_id store_pair_rhs store_result =
  let error, store_pair_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_pair_rhs with
    | error, None -> error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, store_result =
    (*fold over this set*)
    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
      (fun x (error, current_set) ->
         let (agent_id, agent_type, site_type, site_type2, _, _) = x in (*A*)
         (*fold again*)
         let error, pair_set =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
             (fun z (error, current_set) ->
                let (agent_id', agent_type', site_type', site_type2', _, _) = z in
                if agent_id <> agent_id'
                then
                  let error, pair_set =
                    Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error
                      ((agent_id, agent_type, site_type, site_type2),
                       (agent_id', agent_type', site_type', site_type2'))
                      current_set
                  in
                  error, pair_set
                else error, current_set
             ) store_pair_set (error, current_set)
         in
         error, pair_set
      ) store_pair_set (error, store_result)
  in
  error, store_result

(***************************************************************)
(*collect rule that created bond*)

let collect_created_bond parameter error rule_id rule store_tuple_pair store_result =
  let error, store_result =
    List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
        let agent_id1 = site_add1.Cckappa_sig.agent_index in
        (*      let agent_type1 = site_add1.Cckappa_sig.agent_type in*)
        let site_type1 = site_add1.Cckappa_sig.site in
        let agent_id2 = site_add2.Cckappa_sig.agent_index in
        (*let agent_type2 = site_add2.Cckappa_sig.agent_type in*)
        let site_type2 = site_add2.Cckappa_sig.site in
        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
          (fun (x, y) (error, store_result) ->
             let (agent_id, _, site_type, _site_type') = x in
             let (agent_id', _, site_type', _site_type2') = y in
             (*if the first site in the pair is the site that created a bound*)
             if agent_id1 = agent_id && agent_id2 = agent_id' && site_type = site_type1 && site_type2 = site_type' ||
                agent_id1 = agent_id' && agent_id2 = agent_id &&
                site_type' = site_type1 && site_type2 = site_type
             then
               let error, set =
                 Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error (x,y)
                   Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
               in
               let error, old_set =
                 match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                 | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                 | error, Some s -> error, s
               in
               let error', new_set =
                 Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
               in
               let error = Exception.check warn parameter error error' (Some "line 540") Exit in
               let error, store_result =
                 Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
               in
               error, store_result
             else error, store_result
          ) store_tuple_pair (error, store_result)
      )(error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind
  in
  error, store_result

(***************************************************************)
(*collect internal state in case of modificiation and know the bond*)

let collect_modified_internal_and_bond parameter error rule_id store_tuple_pair store_bonds_rhs store_modified_map store_result =
  let empty_pair_set = Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
  in
  let empty_set = Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
  in
  let empty_sitestate_set = Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
  in
  let error, store_bond_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_bonds_rhs with
    | error, None -> error, empty_pair_set
    | error, Some s -> error, s
  in
  let error, store_modified_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_modified_map with
    | error, None -> error, empty_sitestate_set
    | error, Some s -> error, s
  in
  Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       let (agent_id, _, site_type, _site_type') = x in
       let (agent_id', _, site_type', site_type2') = y in
       (*if the second site belong to modified and the first site belong to the bond set*)
       let error, store_result =
         Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun m (error, store_result) ->
             Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
               (fun (t, r) (error, store_result) ->
                  let (agent_id_m, _, site_type_m, _) = m in
                  let (agent_id_b, _, site_type_b, _) = t in
                  let (agent_id_b', _, site_type_b', _) = r in
                  if (agent_id = agent_id_m && site_type' = site_type_m || agent_id' = agent_id_m && site_type2' = site_type_m)
                     (*if the first site belong to the bond*) &&
                     (agent_id = agent_id_b && site_type = site_type_b &&
                      agent_id' = agent_id_b' && site_type' = site_type_b')
                  then
                    let error, set =
                      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error (x,y) empty_set
                    in
                    let error, old_set =
                      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                      | error, None -> error, empty_set
                      | error, Some s -> error, s
                    in
                    let error', new_set =
                      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
                    in
                    let error = Exception.check warn parameter error error' (Some "line 540") Exit in
                    let error, store_result =
                      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
                    in
                    error, store_result
                  else
                    error, store_result
               ) store_bond_set (error, store_result)
           ) store_modified_set (error, store_result)
       in
       error, store_result
    ) store_tuple_pair (error, store_result)

(***************************************************************)
(*collect rule that has question marks on the right hand side*)

let collect_question_marks_rhs parameter error rule_id handler_kappa rule store_modified_map store_result =
  let error, _, question_marks_r =
    Preprocess.translate_mixture parameter error handler_kappa
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.c_mixture
  in
  let error, store_modified_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_modified_map with
    | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, new_set =
    List.fold_left (fun (error, store_set) (agent_id, site_type) ->
        Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun m (error, store_set) ->
            let (_, agent_type_m, site_type_m, _) = m in
            let error, agent =
              Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
            in
            match agent with
            | Some Cckappa_sig.Dead_agent _
            | Some Cckappa_sig.Ghost
            | None | Some Cckappa_sig.Unknown_agent _ -> error, store_set
            | Some Cckappa_sig.Agent agent ->
              let agent_type = agent.Cckappa_sig.agent_name in
              let site_type' = Ckappa_sig.site_name_of_int (Ckappa_sig.int_of_site_name site_type - 1) in
              if agent_type_m = agent_type then
                let error, set =
                  Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.add_when_not_in parameter error
                    (agent_id, agent_type, site_type', site_type_m)
                    store_set
                in
                error, set
              else error, store_set
          ) store_modified_set (error, store_set)
      ) (error, Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.empty) question_marks_r
  in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
      rule_id new_set store_result
  in
  error, store_result

(***************************************************************)
(*Implicit static information*)
(***************************************************************)

let collect_implicit_static parameter error store_tuple_pair store_question_marks_rhs =
  let store_result =
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
         (*fold over this set*)
         let _, new_set =
           Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.fold
             (fun x (error, store_set) ->
                let (_, agent_type, site_type, site_type') = x in
                (*fold over tuple pair set*)
                let error, set =
                  Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
                    (fun (y, t) (error, store_set) ->
                       let (_, agent_type1, site_type1, site_type2) = y in
                       let (_, agent_type1', site_type1', site_type2') = t in
                       let error, first_pair =
                         if agent_type = agent_type1 &&
                            site_type = site_type1
                            && site_type' = site_type2 ||
                            agent_type = agent_type1' &&
                            site_type = site_type1'
                            && site_type' = site_type2'
                         then
                           let error, set =
                             Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in
                               parameter error (y,t)
                               store_set
                           in
                           error, set
                         else error, store_set
                       in
                       error, first_pair
                    ) store_tuple_pair (error, store_set)
                in
                error, set
             ) set (error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty)
         in
         new_set
      ) store_question_marks_rhs
  in
  store_result

(***************************************************************)
(*Explicit static information*)
(***************************************************************)

let collect_explicit_static parameter error store_created_bond store_modified_internal_state_and_bond store_result =
  let add_link parameter error rule_id set store_result =
    let error, old_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
      | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error', new_set =
      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
    in
    let error = Exception.check warn parameter error error' (Some "line 706") Exit in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
    in
    error, store_result
  in
  Ckappa_sig.Rule_map_and_set.Map.fold2 parameter error
    (fun parameter error rule_id set store_result ->
       let error, store_result =
         add_link parameter error rule_id set store_result
       in
       error, store_result)
    (fun parameter error rule_id set store_result ->
       let error, store_result =
         add_link parameter error rule_id set store_result
       in
       error, store_result
    )
    (fun parameter error rule_id set1 set2 store_result ->
       let error, new_set =
         Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set1 set2
       in
       let error, store_result =
         add_link parameter error rule_id new_set store_result
       in
       error, store_result
    )
    store_created_bond store_modified_internal_state_and_bond store_result

(****************************************************************)
(*Init*)
(****************************************************************)

let init_basic_static_information =
  {
    store_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_modified_map = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_created_bond = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_tuple_pair = Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty;
    store_modified_internal_state_and_bond = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_explicit_static = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_question_marks_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_implicit_static = Ckappa_sig.Rule_map_and_set.Map.empty;
  }

(****************************************************************)
(*PRINT*)
(****************************************************************)

let print_views_rhs parameter error handler_kappa log store_result =
  Loggers.fprintf log
    "\n* Views on the right hand side rule:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
         (fun (agent_id, agent_type, site_type, state) error ->
            let error, (agent_string, site_string, state_string) =
              Print_site_accross_bonds_domain.print_agents_site_state parameter error handler_kappa
                (agent_id, agent_type, site_type, state)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s:%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                state_string
                (Ckappa_sig.string_of_state_index state)
            in error
         ) set error
    ) store_result error

(****************************************************************)
(*bonds on the right hand side*)

let print_bonds_rhs parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Rule that has site can be bound:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
         (fun (x, y) error ->
            let (_, _, site_type, state) = x in
            let (_, _, site_type', state') = y in
            let error, ((agent_string, site_string, state_string),
                        (agent_string', site_string', state_string')) =
              Print_site_accross_bonds_domain.print_pair_agents_site_state parameter error handler_kappa (x, y)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s:%s:%s), %s(%s:%s:%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                state_string
                (Ckappa_sig.string_of_state_index state)
                agent_string'
                (Ckappa_sig.string_of_site_name site_type')
                site_string'
                state_string'
                (Ckappa_sig.string_of_state_index state')
            in
            error
         ) set error
    ) store_result error

(****************************************************************)

let print_modified_map parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Rule that has site can be modified:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
         (fun x error ->
            let (_, _, site_type, state) = x in
            let error, (agent_string, site_string, state_string) =
              Print_site_accross_bonds_domain.print_agents_site_state parameter error handler_kappa x
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s:%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                state_string
                (Ckappa_sig.string_of_state_index state)
            in
            error
         ) set error
    ) store_result error

(****************************************************************)

let print_created_bond parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Rule that created a site can be bound:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
         (fun (x, y) error ->
            let (_, _, site_type, site_type2) = x in
            let (_, _, site_type', site_type2') = y in
            let _,
                ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
              Print_site_accross_bonds_domain.print_pair_agents_sites parameter error handler_kappa log
                (x, y)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s,%s:%s), %s(%s:%s,%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                (Ckappa_sig.string_of_site_name site_type2)
                site_string2
                agent_string'
                (Ckappa_sig.string_of_site_name site_type')
                site_string'
                (Ckappa_sig.string_of_site_name site_type2')
                site_string2'
            in
            error
         ) set error
    ) store_result error

(****************************************************************)

let print_modified_internal_state_bond parameter error
    handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Rule that has the first site can be bound, and the second site has internal state can be modified:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
         (fun (x, y) error ->
            let (_, _, site_type, site_type2) = x in
            let (_, _, site_type', site_type2') = y in
            let _,
                ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
              Print_site_accross_bonds_domain.print_pair_agents_sites parameter error handler_kappa log
                (x, y)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s,%s:%s), %s(%s:%s,%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                (Ckappa_sig.string_of_site_name site_type2)
                site_string2
                agent_string'
                (Ckappa_sig.string_of_site_name site_type')
                site_string'
                (Ckappa_sig.string_of_site_name site_type2')
                site_string2'
            in
            error
         ) set error
    ) store_result error

(****************************************************************)

let print_question_marks_rhs parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Question marks on the rhs:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.fold (fun (agent_id, agent_type, site_type, site_type') error ->
           let error, (agent_string, site_string) =
             Print_site_accross_bonds_domain.print_agents_site parameter error handler_kappa (agent_id, agent_type, site_type)
           in
           let error, site_string' =
             try
               Handler.string_of_site parameter error handler_kappa
                 agent_type site_type'
             with
               _ -> warn parameter error (Some "line 30") Exit
                      (Ckappa_sig.string_of_site_name site_type')
           in
           let () =
             Loggers.fprintf log
               "rule_id:%s: %s(%s:%s,%s:%s)\n"
               (Ckappa_sig.string_of_rule_id rule_id)
               agent_string
               (Ckappa_sig.string_of_site_name site_type)
               site_string
               (Ckappa_sig.string_of_site_name site_type')
               site_string'
           in
           error
         ) set error
    ) store_result error

(****************************************************************)

let print_explicit_static parameter error
    handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Explicit static information:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
         (fun (x, y) error ->
            let (_, _, site_type, site_type2) = x in
            let (_, _, site_type', site_type2') = y in
            let _,
                ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
              Print_site_accross_bonds_domain.print_pair_agents_sites parameter error handler_kappa log
                (x, y)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s,%s:%s), %s(%s:%s,%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                (Ckappa_sig.string_of_site_name site_type2)
                site_string2
                agent_string'
                (Ckappa_sig.string_of_site_name site_type')
                site_string'
                (Ckappa_sig.string_of_site_name site_type2')
                site_string2'
            in
            error
         ) set error
    ) store_result error

(****************************************************************)

let print_implicit_static parameter error
    handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Implicit static information:\n";
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set error ->
       Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
         (fun (x, y) error ->
            let (_, _, site_type, site_type2) = x in
            let (_, _, site_type', site_type2') = y in
            let _,
                ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
              Print_site_accross_bonds_domain.print_pair_agents_sites parameter error handler_kappa log
                (x, y)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%s: %s(%s:%s,%s:%s), %s(%s:%s,%s:%s)\n"
                (Ckappa_sig.string_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.string_of_site_name site_type)
                site_string
                (Ckappa_sig.string_of_site_name site_type2)
                site_string2
                agent_string'
                (Ckappa_sig.string_of_site_name site_type')
                site_string'
                (Ckappa_sig.string_of_site_name site_type2')
                site_string2'
            in
            error
         ) set error
    ) store_result error

(****************************************************************)

let print_basic_static_information parameter error handler_kappa log store_views_rhs store_bonds_rhs store_modified_map store_created_bond store_modified_internal_state_and_bond store_question_marks_rhs store_explicit_static store_implicit_static =
  (*--------------------------------------------------------*)
  (*views on the right hand side*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_views_rhs parameter error handler_kappa log store_views_rhs
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*bonds on the right hand side*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_bonds_rhs parameter error handler_kappa log store_bonds_rhs
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*modification*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_modified_map parameter error handler_kappa log store_modified_map
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*created bonds*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_created_bond parameter error handler_kappa log store_created_bond
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*internal state*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_modified_internal_state_bond parameter error handler_kappa log store_modified_internal_state_and_bond
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*question marks on the right hand side*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_question_marks_rhs parameter error handler_kappa log store_question_marks_rhs
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*explicit static*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_explicit_static parameter error handler_kappa log store_explicit_static
      in error
    else error
  in
  (*--------------------------------------------------------*)
  (*implicit*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_implicit_static parameter error handler_kappa log store_implicit_static
      in error
    else error
  in
  error
