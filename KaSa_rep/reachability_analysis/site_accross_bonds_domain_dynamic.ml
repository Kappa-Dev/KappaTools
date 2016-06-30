(**
   * site_accross_bonds_domain_dynamic.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 29th of June
   * Last modification:
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Site accross domain dynamic information") message exn
    (fun () -> default)

let local_trace = false

(***************************************************************)
(*type*)

type basic_dynamic_information =
  {
    store_pair_tuple_init: Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t;
    (*return the internal state in the case of explicit static information*)
    store_explicit_dynamic: Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t;
    (*return the internal state in the case of implicit static information, todo*)
    store_implicit_dynamic:
      (Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t * Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t);
    (*prepare database mvbdu to print in natural language, firstly print the explicit dynamic *)
    store_relation_mvbdu :
    Ckappa_sig.Views_bdu.mvbdu
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
    store_range_mvbdu1 :Ckappa_sig.Views_bdu.mvbdu
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t ;
    store_range_mvbdu2:
    Ckappa_sig.Views_bdu.mvbdu
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
  }

(***************************************************************)
(*Initial state*)
(***************************************************************)

(*views on the initial states*)

let collect_views_init parameter error init_state =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter error
    (fun parameter error agent_id agent store_result ->
       match agent with
       | Cckappa_sig.Ghost
       | Cckappa_sig.Unknown_agent _ -> error, store_result
       | Cckappa_sig.Dead_agent (agent, _, _, _)
       | Cckappa_sig.Agent agent ->
         let agent_type = agent.Cckappa_sig.agent_name in
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
         let error', new_set =
           Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error store_result set
         in
         let error = Exception.check warn parameter error error'
             (Some "line 62") Exit
         in
         error, new_set
    ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty

(***************************************************************)
(*return an agent in the initial state that has two sites different*)

let collect_sites_init parameter error store_views_init =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
        (*fold again views in the initial states*)
        let error, pair_set =
          Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id', _, site_type', state') (error, current_set) ->
              if agent_id = agent_id' && site_type <> site_type'
              then
                let error, set =
                  Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                    parameter error
                    (agent_id, agent_type, site_type, site_type', state, state')
                    current_set
                in
                error, set
              else error, current_set
            ) store_views_init (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
        in
        let error, new_set =
          Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error store_result pair_set
        in
        error, new_set
      ) store_views_init (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
  in
  error, store_result

(***************************************************************)
(*(A.x.y, B.z.t)*)

let collect_pair_sites_init parameter error store_sites_init =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
      (fun x (error, current_set) ->
         let (agent_id, agent_type, site_type, site_type2, state, state2) = x in (*A*)
         (*fold again*)
         let error, pair_set =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
             (fun z (error, current_set) ->
                let (agent_id', agent_type', site_type', site_type2', state', state2') = z in
                if agent_id <> agent_id'
                then
                  let error, pair_set =
                    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                      ((agent_id, agent_type, site_type, site_type2, state, state2),
                       (agent_id', agent_type', site_type', site_type2', state', state2'))
                      current_set
                  in
                  error, pair_set
                else error, current_set
             ) store_sites_init (error, current_set)
         in
         error, pair_set
      ) store_sites_init (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
  in
  error, store_result

  (***************************************************************)
  (*collect bonds in the initial states*)

  let collect_bonds parameter error site_add agent_id site_type_source views =
    let error, pair =
      let agent_index_target = site_add.Cckappa_sig.agent_index in
      let site_type_target = site_add.Cckappa_sig.site in
      let error, agent_source =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_id views
        with
        | error, None -> warn parameter error (Some "line 632") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, agent_target =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_index_target views
        with
        | error, None -> warn parameter error (Some "line 640") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, (agent_type1, state1) =
        Site_accross_bonds_domain_static.collect_agent_type_state
          parameter
          error
          agent_source
          site_type_source
      in
      let error, (agent_type2, state2) =
        Site_accross_bonds_domain_static.collect_agent_type_state
          parameter
          error
          agent_target
          site_type_target
      in
      let pair = ((agent_type1, site_type_source, state1),
                  (agent_type2, site_type_target, state2))
      in
      error, pair
    in
    error, pair

  let collect_bonds_init parameter error init_state =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id bonds_map store_result ->
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type_source site_add (error, store_result) ->
                let error,
                    ((agent_type_source, site_type_source, state_source),
                     (agent_type_target, site_type_target, state_target)) =
                  collect_bonds
                    parameter error site_add agent_id site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let pair =
                  ((agent_id, agent_type_source, site_type_source, state_source),
                   (site_add.Cckappa_sig.agent_index, agent_type_target, site_type_target, state_target))
                in
                let error, store_result =
                  Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                    parameter
                    error
                    pair
                    store_result
                in
                error, store_result
             ) bonds_map (error, store_result)
         in
         error, store_result
      ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty

  (***************************************************************)

  let collect_pair_tuple_init parameter error init_state store_result =
    (*views in the initial state that has two agents and their sites are different*)
    let error, store_views_init =
      collect_views_init parameter error init_state in
    let error, store_sites_init =
      collect_sites_init parameter error store_views_init in
    let error, store_pair_sites_init =
      collect_pair_sites_init parameter error store_sites_init in
    (*a set of site that can be bounds*)
    let error, store_bonds_init =
      collect_bonds_init parameter error init_state in
    (*fold over a set of pair and check the first site whether or not it belongs to a set of sites that can be bound*)
    let error, store_result =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
        (fun (x, y) (error, store_result) ->
           let (agent_id, agent_type, site_type, _, state, state2) = x in
           let (agent_id', agent_type', site_type', _, state', state2') = y in
           if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
               ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state'))
               store_bonds_init && state2 = state2' (*todo*)
           then
             let error, pair_set =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                 (x, y)
                 Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
             in
             let error', new_set =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error pair_set store_result
             in
             let error = Exception.check warn parameter error error'
                 (Some "line 235") Exit
             in
             error, new_set
           else
             error, store_result
        ) store_pair_sites_init (error, store_result)
    in
    error, store_result

(***************************************************************)
(*dynamic information*)
(***************************************************************)

(*check in the case of explicit static information*)

let collect_internal_state_explicit_aux parameter error rule_id store_views_rhs store_explicit_static =
let error, views_set =
  match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_views_rhs with
  | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
  | error, Some s -> error, s
in
let error, tuple_pair_explicit_set =
  match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_explicit_static with
  | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
  | error, Some s -> error, s
in
(*------------------------------------------------------------*)
let empty_set = Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty
in
let error, store_result =
  (*fold over a views rhs set, to test each rule*)
  Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
    (fun x (error, store_result) ->
       (*------------------------------------------------------*)
       let (_, agent_type, site_type, state) = x in
       (*fold over an explicit tuple pair*)
       let error, store_result = Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold (fun (t, z) (error, store_result) ->
           let (agent_id1, agent_type1, site_type1, site_type2) = t in
           let (agent_id1', agent_type1', site_type1', site_type2') = z in
           (*--------------------------------------------------*)
           (*check if the site of the views on the rhs belongs to the second site of the pair, if yes then return its state*)
           let error, first_agent =
             if agent_type = agent_type1 && site_type = site_type2
             then
               let internal_state =
                 (agent_id1, agent_type1, site_type1, site_type2, state)
               in
               let error, set =
                 Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.add_when_not_in parameter error
                   internal_state
                   empty_set
               in
               error, set
             else
               error, empty_set
           in
           (*--------------------------------------------------*)
           (*second agent*)
           let error, second_agent =
             if agent_type = agent_type1' && site_type = site_type2'
             then
               let internal_state =
                 (agent_id1', agent_type1', site_type1', site_type2', state)
               in
               let error, set =
                 Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.add_when_not_in parameter error
                   internal_state
                   empty_set
               in
               error, set
             else
               error, empty_set
           in
           (*---------------------------------------------------*)
           let error, (old_set1, old_set2) =
             match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
             | error, None -> error, (empty_set, empty_set)
             | error, Some (s1, s2) -> error, (s1, s2)
           in
           let error, new_set1 = Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.union parameter error first_agent old_set1 in
           let error, new_set2 = Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.union parameter error second_agent old_set2 in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id (new_set1, new_set2) store_result
           in
           error, store_result
         ) tuple_pair_explicit_set (error, store_result)
       in
       error, store_result
    ) views_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
in
error, store_result

(***************************************************************)

let collect_internal_state_explicit parameter error rule_id store_internal_state_collect_internal_state_explicit_aux =
  (*let error, store_internal_state_collect_internal_state_explicit_aux = collect_internal_state_explicit_aux
      parameter error rule_id store_views_rhs store_explicit_static
  in*)
  let store_result =
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun (set1, set2) ->
         let _, set =
           Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.fold (fun (agent_id, agent_type, site_type, site_type2, state2) (error, store_set) ->
               Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.fold
                 (fun (agent_id', agent_type', site_type', site_type2', state2') (error, store_set) ->
                    if agent_type <> agent_type'
                    then
                      let pair =
                        (agent_id, agent_type, site_type, site_type2, state2),
                        (agent_id', agent_type', site_type', site_type2', state2')
                      in
                      let error, new_set =
                        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in
                          parameter error pair store_set
                      in
                      error, new_set
                    else
                      error, store_set
                 ) set2 (error, store_set)
             ) set1 (error, Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty)
         in
         set
      ) store_internal_state_collect_internal_state_explicit_aux
  in
  store_result

(****************************************************************)
(*return both state information of binding state and internal state of tuple pair*)

let collect_tuple_pair_binding_internal_state_explicit_aux parameter error rule_id store_views_rhs store_internal_state_explicit =
let error, views_set =
  match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_views_rhs with
  | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
  | error, Some s -> error, s
in
(*------------------------------------------------*)
let error, tuple_pair_internal_state_set =
  match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_internal_state_explicit with
  | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty
  | error, Some s -> error, s
in
(*------------------------------------------------*)
let empty_set =
  Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
in
let error, store_result =
  (*fold over the views on the rhs*)
  Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
    (fun x (error, store_result) ->
       let (_, agent_type, site_type, state) = x in
       (*fold over a tuple pair which the information of the state of the second site*)
       let error, store_result =
         Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
           (fun (t, z) (error, store_result) ->
              let (agent_id1, agent_type1, site_type1, site_type2, state2) = t in
              let (agent_id1', agent_type1', site_type1', site_type2', state2') = z in
              (*------------------------------------------------*)
              (*check the site on the rhs belongs to the first site of the tuple pair*)
              let error, first_agent =
                if agent_type = agent_type1 && site_type = site_type1 (*todo*)
                then
                  let binding_state =
                    (agent_id1, agent_type1, site_type1, site_type2, state, state2)
                  in
                  let error, set =
                    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter
                      error
                      binding_state
                      empty_set
                  in
                  error, set
                else
                  error, empty_set
              in
              (*------------------------------------------------*)
              let error, second_agent =
                if agent_type = agent_type1' && site_type = site_type1'
                then
                  let binding_state =
                    (agent_id1', agent_type1', site_type1', site_type2', state, state2')
                  in
                  let error, set =
                    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter
                      error
                      binding_state
                      empty_set
                  in
                  error, set
                else
                  error, empty_set
              in
              (*------------------------------------------------*)
              let error, (old_set1, old_set2) =
                match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                | error, None -> error, (empty_set, empty_set)
                | error, Some (s1, s2) -> error, (s1, s2)
              in
              let error, new_set1 = Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union
                  parameter
                  error
                  first_agent
                  old_set1
              in
              let error, new_set2 = Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union
                  parameter
                  error
                  second_agent
                  old_set2
              in
              let error, store_result =
                Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
                  rule_id
                  (new_set1, new_set2)
                  store_result
              in
              error, store_result
           ) tuple_pair_internal_state_set (error, store_result)
       in
       error, store_result
    ) views_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
in
error, store_result

(****************************************************************)
(**)

let collect_tuple_pair_binding_internal_state_explicit parameter error store_tuple_pair_binding_internal_state_explicit_aux =
  let store_result =
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun (set1, set2) ->
         let _, set =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold (fun (agent_id, agent_type, site_type, site_type2, state, state2) (error, store_set) ->
               Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
                 (fun (agent_id', agent_type', site_type', site_type2', state', state2') (error, store_set) ->
                    if agent_type <> agent_type' && state2 = state2' (*todo*)
                    then
                      let pair =
                        ((agent_id, agent_type, site_type, site_type2, state, state2),
                         (agent_id', agent_type', site_type', site_type2', state', state2'))
                      in
                      let error, set =
                        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error pair store_set
                      in
                      error, set
                    else error, store_set
                 ) set2 (error, store_set)
             ) set1 (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
         in
         set
      ) store_tuple_pair_binding_internal_state_explicit_aux
  in
  store_result

(****************************************************************)

let collect_explicit_dynamic parameter error
    store_tuple_pair_binding_internal_state_explicit
    store_pair_tuple_init store_result =
  (*add information about initial state*)
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.fold
      (fun _ pair_set (error, store_result) ->
         let error, store_result =
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error pair_set store_result
         in
         error, store_result
      ) store_tuple_pair_binding_internal_state_explicit (error, store_result)
  in
  (*FIXME: is it union?*)
  let error, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union
      parameter
      error
      store_pair_tuple_init
      store_result
  in
  error, store_result

(***************************************************************)
(*check in the case of implicit static information*)
(***************************************************************)

let collect_implicit_dynamic_aux parameter error rule_id store_views_rhs store_implicit_static =
  let error, views_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_views_rhs with
    | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, implicit_static_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_implicit_static with
    | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let empty_site_set = Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.empty
  in
  let empty_site_state_set =   Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.empty
  in
  let error, store_result =
    (*fold over a views rhs, to test each rule and get the information of internal state*)
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun x (error, store_result) ->
        let (_, agent_type, site_type, state) = x in
        (*fold over implicit static tuple pair*)
        let error, store_result =
          Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold (fun (t, z) (error, store_result) ->
              let (agent_id1, agent_type1, site_type1, site_type2) = t in
              let (agent_id1', agent_type1', site_type1', site_type2') = z in
              (*check if the second site of the tuple is the site on the rhs: return the internal state of B(t~)*)
              let error, first_agent =
                if agent_type = agent_type1 && site_type = site_type2
                then
                  let internal_state =
                    (agent_id1, agent_type1, site_type1, site_type2, state), z
                  in
                  let error, set =
                    Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.add_when_not_in parameter error
                      internal_state
                      empty_site_set
                  in
                  error, set
                else error, empty_site_set
              in
              (*--------------------------------------------------*)
              let error, second_agent =
                if agent_type = agent_type1' && site_type = site_type2'
                then
                  let internal_state =
                    t, (agent_id1', agent_type1', site_type1', site_type2', state)
                  in

                  let error, set =
                    Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.add_when_not_in parameter error
                      internal_state
                    empty_site_state_set
                  in
                  error, set
                else error, empty_site_state_set
              in
              (*------------------------------------------------*)
              let error, (old_set1, old_set2) =
                match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                | error, None -> error, (empty_site_set, empty_site_state_set)
                | error, Some (s1, s2) -> error, (s1, s2)
              in
              let error, new_set1 = Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.union parameter error first_agent old_set1 in
              let error, new_set2 = Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.union parameter error second_agent old_set2 in
              let error, store_result =
                Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id (new_set1, new_set2) store_result
              in
              error, store_result
            ) implicit_static_set (error, store_result)
        in
        error, store_result
      ) views_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
  in
  error, store_result

(***************************************************************)

let collect_implicit_dynamic parameter error rule_id store_tuple_pair_init store_implicit_dynamic_aux store_result =
  let error, (pair_set1, _pair_set2) =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_implicit_dynamic_aux with
    | error, None -> error, (Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.empty, Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.empty)
    | error, Some (s1, s2) -> error, (s1, s2)
  in
  (*fold over a set of tuple pair in implicit static information*)
  let error, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.fold
      (fun (x, y) (error, store_result) ->
         (*B question mark*)
         let (agent_id, agent_type, site_type, site_type2, state2) =
           x in
         let (agent_id', agent_type', site_type', site_type2') = y in
         (*fold over the initial*)
         let error, store_result =
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
             (fun (z, t) (error, store_result) ->
                let (store_result1, store_result2) = store_result in
                let (agent_id_init, agent_type_init, site_type_init, site_type_init2, _, state_init2) = z in
                let (agent_id_init', agent_type_init', site_type_init', site_type_init2', _, state_init2') = t in
                (*check the information in y with z first*)
                let error, first_agent =
                  if (agent_type' = agent_type_init &&
                      site_type' = site_type_init &&
                      site_type2' = site_type_init2)
                  then
                    let pair =
                      x,
                      (agent_id_init, agent_type_init, site_type_init, site_type_init2, state_init2)
                    in
                    let error, set =
                      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in parameter error pair
                        store_result1
                    in
                    error, set
                  else error, store_result1
                in
                (**)
                let error, second_agent =
                  if (agent_type' = agent_type_init' &&
                      site_type' = site_type_init' &&
                      site_type2' = site_type_init2')
                  then
                    let pair =
                      (agent_id_init', agent_type_init', site_type_init', site_type_init2', state_init2'), x
                    in
                    let error, set =
                      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in parameter error pair
                        store_result2
                    in
                    error, set
                  else error, store_result2
                in
                (**)
                error, (first_agent, second_agent)
             ) store_tuple_pair_init (error, store_result)
         in
         error, store_result
      ) pair_set1 (error, store_result)
  in
  error, store_result

(***************************************************************)
(*relation mvbdu*)

let collect_relation_mvbdu parameter error handler store_explicit_dynamic store_result =
(*get the set of tuple when both agents are connected via x.z*)
  let error, handler, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
      (fun (x, y) (error, handler, store_result)->
        (*build the mvbdu of two sites y(site_type2) and z(site_type2')*)
         let (_, _, site_type, site_type2, state, state2) = x in
         (*build (key * value) list *)
         (*let pair_list =
           [(site_type2, state2);(site_type2', state2')]
           in*)
         (*binding and internal state of agent type A*)
         let pair_list = [(site_type, state); (site_type2, state2)] in
         let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
         let error, store_result =
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
             parameter
             error
             (x,y)
             mvbdu
             store_result
         in
         error, handler, store_result
      ) store_explicit_dynamic (error, handler, store_result)
  in
  error, handler, store_result

  (***************************************************************)
  (*relation mvbdu*)

  let collect_range_mvbdu1 parameter error handler store_explicit_dynamic store_result =
  (*get the set of tuple when both agents are connected via x.z*)
    let error, handler, store_result =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
        (fun (x, y) (error, handler, store_result)->
          (*build the mvbdu of two sites y(site_type2) and z(site_type2')*)
           let (_, _, _, site_type2, _, state2) = x in
           (*build (key * value) list *)
           (*let pair_list =
             [(site_type2, state2);(site_type2', state2')]
             in*)
           (*binding and internal state of agent type A*)
           let pair_list = [(site_type2, state2)] in
           let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
           let error, store_result =
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
               parameter
               error
               (x,y)
               mvbdu
               store_result
           in
           error, handler, store_result
        ) store_explicit_dynamic (error, handler, store_result)
    in
    error, handler, store_result

    (***************************************************************)
    (*relation mvbdu*)

    let collect_range_mvbdu2 parameter error handler store_explicit_dynamic store_result =
    (*get the set of tuple when both agents are connected via x.z*)
      let error, handler, store_result =
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
          (fun (x, y) (error, handler, store_result)->
            (*build the mvbdu of two sites y(site_type2) and z(site_type2')*)
             let (_, _, _, site_type2, _, state2) = y in
             (*build (key * value) list *)
             (*let pair_list =
               [(site_type2, state2);(site_type2', state2')]
               in*)
             (*binding and internal state of agent type A*)
             let pair_list = [(site_type2, state2)] in
             let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
             let error, store_result =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                 parameter
                 error
                 (x,y)
                 mvbdu
                 store_result
             in
             error, handler, store_result
          ) store_explicit_dynamic (error, handler, store_result)
      in
      error, handler, store_result


(***************************************************************)
(*initial*)

let init_basic_dynamic_information =
  {
    store_pair_tuple_init =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty;
    store_explicit_dynamic =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty;
    store_implicit_dynamic =
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty,
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty;
    store_relation_mvbdu = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
    store_range_mvbdu1 = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
    store_range_mvbdu2 = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
  }

(***************************************************************)
(*PRINT*)
(***************************************************************)

(*print initial state*)

let print_init parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";  Loggers.fprintf log "* Inititial states:\n";
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
    (fun (x, y) error ->
       let () =
         Print_site_accross_bonds_domain.print_tuple parameter error handler_kappa log (x, y)
       in
       error
    ) store_result error

(***************************************************************)

let print_explicit_dynamic parameter error handler_kappa log store_result =
  Loggers.fprintf log
    "------------------------------------------------------------\n";
  Loggers.fprintf log "* Tuple set when modification of the second site accross a bound that is preserved \nor creation of a bond with/without of the first site (including initial state):\n";
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold (fun (x, y) error ->
      let () =
        Print_site_accross_bonds_domain.print_tuple parameter error handler_kappa log (x, y)
      in error
    ) store_result error

(***************************************************************)

let print_implicit_dynamic parameter error handler_kappa log store_result =
  let store_result1, store_result2 = store_result in
  Loggers.fprintf log "------------------------------------------------------------\n";  Loggers.fprintf log "* Tuple set:\n";
  let error =
    Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold (fun (x, y) error ->
      let (agent_id, agent_type, site_type, site_type2, state2) =
        x in
      let (agent_id', agent_type', site_type', site_type2', state2') =
        y in
        let error, (agent_string, site_string2, state_string2) =
          Print_site_accross_bonds_domain.print_agents_site_state parameter error handler_kappa
            (agent_id, agent_type, site_type2, state2)
        in
        let error, site_string =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type site_type
          with
            _ -> warn parameter error (Some "line 30") Exit
                   (Ckappa_sig.string_of_site_name site_type)
        in
        (**)
        let error, (agent_string', site_string2', state_string2') =
          Print_site_accross_bonds_domain.print_agents_site_state parameter error handler_kappa
            (agent_id', agent_type', site_type2', state2')
        in
        let error, site_string' =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type' site_type'
          with
            _ -> warn parameter error (Some "line 30") Exit
                   (Ckappa_sig.string_of_site_name site_type')
        in
        let () =
          Loggers.fprintf log
            "%s(%i:%s, %i:%s), %s(%i:%s, %i:%s) -> %i:%s, %i:%s\n"
            agent_string
            (Ckappa_sig.int_of_site_name site_type)
            site_string
            (Ckappa_sig.int_of_site_name site_type2)
            site_string2
            (**)
            agent_string'
            (Ckappa_sig.int_of_site_name site_type')
            site_string'
            (Ckappa_sig.int_of_site_name site_type2')
            site_string2'
            (**)
            (Ckappa_sig.int_of_state_index state2)
            state_string2
            (**)
            (Ckappa_sig.int_of_state_index state2')
            state_string2'
        in
        error
      ) store_result1 error
  in
  Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold (fun (x, y) error ->
      let (agent_id, agent_type, site_type, site_type2, state2) =
        x in
      let (agent_id', agent_type', site_type', site_type2', state2') =
        y in
        let error, (agent_string, site_string2, state_string2) =
          Print_site_accross_bonds_domain.print_agents_site_state parameter error handler_kappa
            (agent_id, agent_type, site_type2, state2)
        in
        let error, site_string =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type site_type
          with
            _ -> warn parameter error (Some "line 30") Exit
                   (Ckappa_sig.string_of_site_name site_type)
        in
        (**)
        let error, (agent_string', site_string2', state_string2') =
          Print_site_accross_bonds_domain.print_agents_site_state parameter error handler_kappa
            (agent_id', agent_type', site_type2', state2')
        in
        let error, site_string' =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type' site_type'
          with
            _ -> warn parameter error (Some "line 30") Exit
                   (Ckappa_sig.string_of_site_name site_type')
        in
        let () =
          Loggers.fprintf log
            "%s(%i:%s, %i:%s), %s(%i:%s, %i:%s) -> %i:%s, %i:%s\n"
            agent_string
            (Ckappa_sig.int_of_site_name site_type)
            site_string
            (Ckappa_sig.int_of_site_name site_type2)
            site_string2
            (**)
            agent_string'
            (Ckappa_sig.int_of_site_name site_type')
            site_string'
            (Ckappa_sig.int_of_site_name site_type2')
            site_string2'
            (**)
            (Ckappa_sig.int_of_state_index state2)
            state_string2
            (**)
            (Ckappa_sig.int_of_state_index state2')
            state_string2'
        in
        error
    ) store_result2 error

(***************************************************************)

let print_basic_dynamic_information parameter error handler_kappa log store_pair_tuple_init store_explicit_dynamic store_implicit_dynamic =
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_init parameter error handler_kappa log store_pair_tuple_init
      in error
    else error
  in
  (*--------------------------------------------------------*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_explicit_dynamic parameter error handler_kappa log store_explicit_dynamic
      in error
    else error
  in
  (*--------------------------------------------------------*)
  let error =
    if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
    then
      let error =
        print_implicit_dynamic parameter error handler_kappa log store_implicit_dynamic
      in error
    else error
  in
  error
