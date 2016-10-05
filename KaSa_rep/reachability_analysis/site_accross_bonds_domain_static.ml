(**
   * site_accross_bonds_domain_static.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 29th of June
   * Last modification: Time-stamp: <Sep 26 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

(***************************************************************)
(*type*)
(* agent_ids makes sense only in the context of a given rule *)

type basic_static_information =
  {
    (*------------------------------------------------------------------*)
    (*this is the potential tuple in the rhs*)
    store_potential_tuple_pair_rhs :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t;
    (*tuple of interest in the rhs: A(z!1, t~u/~p), B(z!1, t~u~/p)*)
    store_rule_potential_tuple_pair_rhs :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*the potential tuple in the lhs*)
    store_rule_potential_tuple_pair_lhs :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*tuple of interest in the lhs: A(z!1, t~u/~p), B(z!1, t~u~/p)*)
    (*store_rule_proj_potential_tuple_pair_lhs :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;*)
    store_tuples_of_interest :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t;
    (*------------------------------------------------------------------*)
    (*projection or combination*)
    store_partition_bonds_rhs_map :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.t;
    store_partition_created_bonds_map :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.t;
    store_partition_modified_map_1 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    store_partition_modified_map_2 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    (*a map from tuples -> sites (agent_type, site_name)*)
    (*store_tuple_to_sites :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.PairAgentSite_map_and_set.Map.t;*)

  }

(****************************************************************)
(*Init*)
(****************************************************************)

let init_basic_static_information =
  {
    (*-------------------------------------------------------*)
    store_potential_tuple_pair_rhs =
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty;
    store_rule_potential_tuple_pair_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_potential_tuple_pair_lhs =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    (*store_rule_proj_potential_tuple_pair_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;*)
    store_tuples_of_interest =
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.empty;
    (*-------------------------------------------------------*)
    (*projection or combination*)
    store_partition_bonds_rhs_map =
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.empty;
    store_partition_created_bonds_map =
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.empty;
    store_partition_modified_map_1 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_partition_modified_map_2 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    (*-------------------------------------------------------*)
    (*a map from tuples -> sites*)
    (*store_tuple_to_sites =
      Site_accross_bonds_domain_type.PairAgentSite_map_and_set.Map.empty;*)
  }

(***************************************************************)
(*collect a set of tuple pair (A.x.y, B.z.t) on the rhs*)

let collect_potential_tuple_pair_rhs parameter error
    rule_id store_bonds_rhs store_views_rhs store_result =
  let error, bonds_set =
    Common_static.get_set parameter error rule_id
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      store_bonds_rhs
  in
  let error, views_set =
    Common_static.get_set parameter error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
      store_views_rhs
  in
  Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       let (agent_id, agent_type, site_type, state) = x in
       let (agent_id1, agent_type1, site_type1, state1) = y in
       let error, fst_list =
         Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
           (fun v (error, current_list) ->
              let (agent_id_v, agent_type_v, site_type_v, _state_v) = v in
              if agent_id = agent_id_v &&
                 agent_type = agent_type_v &&
                 site_type <> site_type_v
              then
                let fst_list =
                  (agent_type, site_type, site_type_v, state) ::
                  current_list
                in
                error, fst_list
              else error, current_list
           ) views_set (error, [])
       in
       let error, snd_list =
         Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
           (fun v (error, current_list) ->
              let (agent_id_v, agent_type_v, site_type_v, _state_v) = v in
              if agent_id1 = agent_id_v &&
                 agent_type1 = agent_type_v &&
                 site_type1 <> site_type_v
              then
                let snd_list =
                  (agent_type1, site_type1, site_type_v, state1) ::
                  current_list
                in
                error, snd_list
              else error, current_list
           ) views_set (error, [])
       in
       let error, store_result =
         List.fold_left (fun (error, store_result) x ->
             List.fold_left (fun (error, store_result) y ->
                 let error, store_result =
                   Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.add_when_not_in
                     parameter error
                     (x, y)
                     store_result
                 in
                 error, store_result
               ) (error, store_result) snd_list
           ) (error, store_result) fst_list
       in
       error, store_result
    ) bonds_set (error, store_result)

(*-------------------------------------------------------*)
(*potential tuple pair on the rhs*)

let collect_rule_potential_tuple_pair_aux
    parameter error rule_id store_bonds
    store_views store_result =
  let error, bonds_set =
    Common_static.get_set parameter error rule_id
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      store_bonds
  in
  let error, views_set =
    Common_static.get_set parameter error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
      store_views
  in
  let error, pair_set =
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       (*binding information*)
       let (agent_id, agent_type, site_type, state) = x in
       let (agent_id1, agent_type1, site_type1, state1) = y in
       let error, fst_list =
         Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
           (fun v (error, current_list) ->
              (*views information*)
              let (agent_id_v, agent_type_v, site_type_v, state_v) = v in
              if agent_id = agent_id_v &&
                 agent_type = agent_type_v &&
                 site_type <> site_type_v
              then
                let fst_list =
                  (agent_type, site_type, site_type_v, state, state_v) ::
                  current_list
                in
                error, fst_list
              else error, current_list
           ) views_set (error, [])
       in
       let error, snd_list =
         Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
           (fun v (error, current_list) ->
              let (agent_id_v, agent_type_v, site_type_v, state_v) = v in
              if agent_id1 = agent_id_v &&
                 agent_type1 = agent_type_v &&
                 site_type1 <> site_type_v
              then
                let snd_list =
                  (agent_type1, site_type1, site_type_v, state1, state_v) ::
                  current_list
                in
                error, snd_list
              else error, current_list
           ) views_set (error, [])
       in
       let error, store_result =
         List.fold_left (fun (error, store_result) x ->
             List.fold_left (fun (error, store_result) y ->
                 let error, store_result =
                   Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                     (x, y)
                     store_result
                 in
                 error, store_result
               ) (error, store_result) snd_list
           ) (error, store_result) fst_list
       in
       error, store_result
    ) bonds_set
    (error,
     Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.empty)
  in
  (*add the set of tuple to rule_id map*)
    Ckappa_sig.Rule_map_and_set.Map.add
      parameter error rule_id pair_set store_result

let collect_rule_potential_tuple_pair_lhs
    parameter error rule_id store_bonds_lhs
    store_views_lhs store_result =
  collect_rule_potential_tuple_pair_aux
    parameter error rule_id store_bonds_lhs
    store_views_lhs store_result

let collect_rule_potential_tuple_pair_rhs
    parameter error rule_id store_bonds_rhs
    store_views_rhs store_result =
  collect_rule_potential_tuple_pair_aux
    parameter error rule_id store_bonds_rhs
    store_views_rhs store_result

(*****************************************************************************)

let collect_rule_proj_potential_tuple_pair_lhs parameter error
    store_rule_potential_tuple_pair_lhs store_result =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id set (error, store_result) ->
       let error, new_set =
       Site_accross_bonds_domain_type.Proj_potential_tuple_pair_set_lhs.proj_set
         (fun (x,y) ->
            let proj (a, b, c, d, e) = (a, b, c, d) in
            proj x, proj y
         )
         parameter error
         set
       in
       let error, store_result =
         Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
           parameter error
           rule_id
           new_set
           store_result
       in
       error, store_result
    ) store_rule_potential_tuple_pair_lhs (error, store_result)

(*-------------------------------------------------------*)
(*PairAgentSites_map_and_set.Set.t PairAgentSite_map_and_set.Map.t
  this Map maps the pair
  ((id, ag,site,state),(id', ag',site', state')) to the set of tuples of
  interest of the form:
  ((id, ag,site,state,_,_),(id', ag',site',state',_,_))*)

let collect_partition_bonds_rhs_map parameter error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, c, _, e) = (b, c, e) in
  (*set_a map_b*)
  Site_accross_bonds_domain_type.Partition_bonds_rhs_map.monadic_partition_set
    (fun _parameter error (x, y) ->
       error, (proj x, proj y)
    )
    parameter
    error
    store_potential_tuple_pair_set (*set_a*)

(***************************************************************)
(*collect a map of rule that store a set of sites can created bonds*)

let collect_partition_created_bonds_map parameter error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, c, _, e) = (b, c, e) in
  (*set_a map_b*)
  Site_accross_bonds_domain_type.Partition_created_bonds_map.monadic_partition_set
    (fun _parameter error (x, y) ->
       error, (proj x, proj y)
    )
    parameter
    error
    store_potential_tuple_pair_set (*set_a*)

(*****************************************************************)

let collect_partition_modified_map_1 parameter error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, _, d, _) = b, d in
  Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
    (fun _ error (x, _) ->
       (*get the first site*)
       error, proj x
    )
    parameter
    error
    store_potential_tuple_pair_set

let collect_partition_modified_map_2 parameter error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, _, d, _) = b, d in
  Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
    (fun _ error (_, y) ->
       (*get the second site *)
       error, proj y
    )
    parameter
    error
    store_potential_tuple_pair_set

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
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type port (error, store_set) ->
                let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                  parameter error
                  (agent_id, agent_type, site_type, state)
                  store_set
             ) agent.Cckappa_sig.agent_interface
             (error, store_result)
         in
         error, store_result
    ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty

(***************************************************************)
(*return an agent in the initial state that has two sites different*)

let collect_sites_init parameter error store_views_init =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
      (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
         let error, store_result =
           Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
             (fun (agent_id', _, site_type', state') (error, store_result) ->
                if agent_id = agent_id' && site_type <> site_type'
                then
                  let error, store_result =
                    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, site_type', state, state')
                      store_result
                  in
                  error, store_result
                else error, store_result
             ) store_views_init (error, store_result)
         in
         error, store_result
      ) store_views_init
      (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
  in
  error, store_result

(***************************************************************)
(*(A.x.y, B.z.t)*)

let collect_pair_sites_init parameter error store_sites_init =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
      (fun x (error, store_result) ->
         let (agent_id, agent_type, site_type, site_type2, state, state2) = x in (*A*)
         let error, store_result =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
             (fun z (error, store_result) ->
                let (agent_id', agent_type', site_type', site_type2', state', state2') = z in
                if agent_id <> agent_id'
                then
                  let error, store_result =
                    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                      ((agent_id, agent_type, site_type, site_type2, state, state2),
                       (agent_id', agent_type', site_type', site_type2', state', state2'))
                      store_result
                  in
                  error, store_result
                else error, store_result
             ) store_sites_init (error, store_result)
         in
         error, store_result
      ) store_sites_init
      (error,
       Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
  in
  error, store_result

(***************************************************************)
(*collect bonds in the initial states*)

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
                Common_static.collect_pair_of_bonds
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
              let error, store_result =
                Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                  parameter
                  error
                  ((fun (x,y) -> (y,x)) pair)
                  store_result
              in
              error, store_result
           ) bonds_map (error, store_result)
       in
       error, store_result
    ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
    Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty

(***************************************************************)

let collect_pair_tuple_init parameter error bdu_false handler kappa_handler
   store_bonds_init store_pair_sites_init tuples_of_interest store_result =
  (*fold over a set of pair and check the first site whether or not it
    belongs to a set of sites that can be bound*)
  let error, handler, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
      (fun (x, y) (error, handler, store_result) ->
         let (agent_id, agent_type, site_type, _, state, state2) = x in
         let (agent_id', agent_type', site_type', _, state', state2') = y in
         (*check the first site belong to the bonds*)
         if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
             ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state'))
             store_bonds_init
         then
           (*-----------------------------------------------------------*)
           (*use the number 1 to indicate for the first agent, and number 2
             for the second agent*)
           let pair_list =
             [(Ckappa_sig.fst_site, state2);
              (Ckappa_sig.snd_site, state2')]
           in
           (*test*)
           let proj (_, b,c, d, e, _) = (b, c, d, e) in
           let pair = proj x, proj y in
           if
             Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.mem pair tuples_of_interest
           then
             (*-----------------------------------------------------------*)
             let error, handler, mvbdu =
               Ckappa_sig.Views_bdu.mvbdu_of_association_list
                 parameter handler error pair_list
             in
             let error, handler, store_result =
               Site_accross_bonds_domain_type.add_link
                 parameter error bdu_false handler kappa_handler pair mvbdu store_result
             in
             error, handler, store_result
           else error, handler, store_result
         else
           error, handler, store_result
      ) store_pair_sites_init (error, handler, store_result)
  in
error, handler, store_result

(***************************************************************)
(*a map from tuples -> sites*)

let collect_tuple_to_sites parameter error tuples_of_interest =
  let proj (a, b, _, _) = (a, b) in
  let proj2 (a, _, c, _) = (a, c) in
  Site_accross_bonds_domain_type.Partition_tuples_to_sites_map.monadic_partition_set
    (fun _ error (x, y) ->
       error, (proj x, proj2 x, proj y, proj2 y)
    )
    parameter
    error
    tuples_of_interest
