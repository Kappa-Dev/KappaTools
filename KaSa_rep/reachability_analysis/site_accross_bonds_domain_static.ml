(**
   * site_accross_bonds_domain_static.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 29th of June
   * Last modification: Time-stamp: <Nov 28 2016>
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
    store_potential_tuple_pair :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t;
    (*the potential tuple in the lhs*)
    store_potential_tuple_pair_lhs :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (**)
    store_potential_tuple_pair_rule_rhs :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    (*projection or combination*)
    store_partition_created_bonds_map : (*if the rule already apply we don't
                                          need to apply it again*)
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.t;
    (*-------------------------------------------------------*)
    store_partition_created_bonds_map_1 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    store_partition_created_bonds_map_2 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    (**)
    store_rule_partition_created_bonds_map_1 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_rule_partition_created_bonds_map_2 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*-------------------------------------------------------*)
    store_partition_modified_map_1 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    store_partition_modified_map_2 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    (**)
    store_rule_partition_modified_map_1 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_rule_partition_modified_map_2 :
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
  }

(****************************************************************)
(*Init*)
(****************************************************************)

let init_basic_static_information =
  {
    (*-------------------------------------------------------*)
    store_potential_tuple_pair =
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty;
    store_potential_tuple_pair_lhs =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_potential_tuple_pair_rule_rhs =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    (*-------------------------------------------------------*)
    (*projection or combination*)
    store_partition_created_bonds_map =
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.empty;
    (*-------------------------------------------------------*)
    store_partition_created_bonds_map_1 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_partition_created_bonds_map_2 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_rule_partition_created_bonds_map_1 =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_partition_created_bonds_map_2 =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    (*-------------------------------------------------------*)
    store_partition_modified_map_1 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_partition_modified_map_2 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_rule_partition_modified_map_1 =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_partition_modified_map_2 =
      Ckappa_sig.Rule_map_and_set.Map.empty;
  }

(***************************************************************)
(*collect a set of tuple pair (A.x.y, B.z.t) on the rhs*)

let collect_tuple error (agent_id, agent_type, site_type, state) views_set =
  let error, list =
    Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
      (fun v (error, current_list) ->
         let (agent_id_v, agent_type_v, site_type_v, _state_v) = v in
         if agent_id = agent_id_v &&
            agent_type = agent_type_v &&
            site_type <> site_type_v
         then
           let list =
             (agent_type, site_type, site_type_v, state) ::
             current_list
           in
           error, list
         else error, current_list
      ) views_set (error, [])
  in
  error, list

  let collect_tuples error (agent_id, agent_type, site_type, state) views_set =
    let error, list =
      Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
        (fun v (error, current_list) ->
           let (agent_id_v, agent_type_v, site_type_v, state_v) = v in
           if agent_id = agent_id_v &&
              agent_type = agent_type_v &&
              site_type <> site_type_v
           then
             let list =
               (agent_type, site_type, site_type_v, state, state_v) ::
               current_list
             in
             error, list
           else error, current_list
        ) views_set (error, [])
    in
    error, list

let store_set parameters error fst_list snd_list store_result =
  List.fold_left (fun (error, store_result) x ->
      List.fold_left (fun (error, store_result) y ->
          let error, store_result =
            Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.add_when_not_in
              parameters error
              (x, y)
              store_result
          in
          error, store_result
        ) (error, store_result) snd_list
    ) (error, store_result) fst_list

let collect_potential_tuple_pair parameters error
    rule_id store_bonds_rhs store_views_rhs store_result =
  let error, bonds_set =
    Common_static.get_set parameters error rule_id
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      store_bonds_rhs
  in
  let error, views_set =
    Common_static.get_set parameters error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
      store_views_rhs
  in
  Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       let error, fst_list =
         collect_tuple error
           x
           views_set
       in
       let error, snd_list =
         collect_tuple error
           y
           views_set
       in
       let error, store_result =
         List.fold_left (fun (error, store_result) x ->
             List.fold_left (fun (error, store_result) y ->
                 let error, store_result =
                   Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.add_when_not_in
                     parameters error
                     (x, y)
                     store_result
                 in
                 error, store_result
               ) (error, store_result) snd_list
           ) (error, store_result) fst_list
       in
       error, store_result
    ) bonds_set (error, store_result)

let collect_potential_tuple_pair_rule_rhs parameters error rule_id
    store_potential_tuple_pair_rhs store_result =
    Ckappa_sig.Rule_map_and_set.Map.add
      parameters error
      rule_id
      store_potential_tuple_pair_rhs
      store_result

(*-------------------------------------------------------*)
(*potential tuple pair on the rhs*)

let collect_potential_tuple_pair_lhs parameters error rule_id store_bonds_lhs
    store_views_lhs store_result =
  let error, bonds_lhs_set =
    Common_static.get_set parameters error rule_id
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      store_bonds_lhs
  in
  let error, views_lhs_set =
    Common_static.get_set parameters error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
      store_views_lhs
  in
  let error, pair_set =
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       (*binding information*)
       let error, fst_list =
         collect_tuples error x views_lhs_set
       in
       let error, snd_list =
         collect_tuples error y views_lhs_set
       in
       let error, store_result =
         store_set parameters error fst_list snd_list store_result
       in
       error, store_result
    ) bonds_lhs_set
    (error,
     Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.empty)
  in
  (*add the set of tuple to rule_id map*)
    Ckappa_sig.Rule_map_and_set.Map.add
      parameters error rule_id pair_set store_result

(***************************************************************)
(*collect a map of rule that store a set of sites can created bonds*)

let collect_partition_created_bonds_map parameters error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, c, _, e) = (b, c, e) in
  (*set_a map_b*)
  Site_accross_bonds_domain_type.Partition_created_bonds_map.monadic_partition_set
    (fun _parameters error (x, y) ->
       error, (proj x, proj y)
    )
    parameters
    error
    store_potential_tuple_pair_set (*set_a*)

let collect_partition_created_bonds_map_aux parameters error x tuple_set
    store_result =
  let error, old_set =
    match
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.find_option_without_logs
        parameters error
        x
        store_result
    with
    | error, None ->
      error,
      Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error', new_set =
    Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.union
      parameters
      error
      old_set
      tuple_set
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error, store_result =
    Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.add_or_overwrite
      parameters error
      x
      new_set
      store_result
  in
  error, store_result

let collect_partition_created_bonds_map_1 parameters error
    store_partition_created_bonds_map store_result =
  Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.fold
    (fun (x, _) tuple_set (error, store_result) ->
       let proj (b, c, _) = (b, c) in
       collect_partition_created_bonds_map_aux parameters error
         (proj x)
         tuple_set
         store_result
    )
    store_partition_created_bonds_map (error, store_result)

let collect_partition_created_bonds_map_2 parameters error
    store_partition_created_bonds_map store_result =
  Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.fold
    (fun (_, y) tuple_set (error, store_result) ->
       let proj (b, c, _) = (b, c) in
       collect_partition_created_bonds_map_aux parameters error
         (proj y)
         tuple_set
         store_result
    )
    store_partition_created_bonds_map (error, store_result)

let collect_rule_partition_aux parameters error rule_id
    map store_result =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.fold
      (fun pair_site tuple_set (error, store_result) ->
         let error, old_set =
           match
             Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
               parameters error
               rule_id
               store_result
           with
           | error, None ->
             error,
             Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty
           | error, Some s -> error, s
         in
         let error', new_set =
           Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.union
             parameters error
             old_set
             tuple_set
         in
         let error = Exception.check_point Exception.warn parameters error
             error' __POS__ Exit in
         let error, store_result =
           Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
             parameters error
             rule_id
             new_set
             store_result
         in
         error, store_result
      ) map (error, store_result)
  in
  error, store_result

(*let collect_partition_created_bonds_map_1 parameters error tuple_set =
  let proj (b, c, _, _) = (b, c) in
  (*-------------------------------------------------------*)
  let error, map1 =
  (*set_a map_b*)
    Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
      (fun _ error (x, _) ->
         error, (proj x)
      )
      parameters
      error
      tuple_set
  in
  error, map1

let collect_partition_created_bonds_map_2 parameters error tuple_set =
  let proj (b, c, _, _) = (b, c) in
  (*-------------------------------------------------------*)
  let error, map2 =
  (*set_a map_b*)
    Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
      (fun _ error (_, y) ->
         error, (proj y)
      )
      parameters
      error
      tuple_set
  in
  error, map2*)

let collect_rule_partition_created_bonds_map_1 parameters error
    store_rule_potential_tuple_pair_set_rhs store_result =
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun rule_id tuple_set (error, store_result) ->
           let proj (b, c, _, _) = (b, c) in
           (*-------------------------------------------------------*)
           let error, map1 =
             (*set_a map_b*)
             Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
               (fun _ error (x, _) ->
                  error, (proj x)
               )
               parameters
               error
               tuple_set
           in
           let error, store_result =
           collect_rule_partition_aux
             parameters error
             rule_id
             map1
             store_result
           in
           error, store_result
        ) store_rule_potential_tuple_pair_set_rhs (error, store_result)
    in
    error, store_result

let collect_rule_partition_created_bonds_map_2 parameters error
    store_rule_potential_tuple_pair_set_rhs store_result =
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.fold
      (fun rule_id tuple_set (error, store_result) ->
         let proj (b, c, _, _) = (b, c) in
         (*-------------------------------------------------------*)
         let error, map2 =
           (*set_a map_b*)
           Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
             (fun _ error (_, y) ->
                error, (proj y)
             )
             parameters
             error
             tuple_set
         in
         let error, store_result =
           collect_rule_partition_aux
             parameters error
             rule_id
             map2
             store_result
         in
         error, store_result
      ) store_rule_potential_tuple_pair_set_rhs (error, store_result)
  in
  error, store_result

(*****************************************************************)

let collect_partition_modified_map_1 parameters error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, _, d, _) = b, d in
  Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
    (fun _ error (x, _) ->
       (*get the first site*)
       error, proj x
    )
    parameters
    error
    store_potential_tuple_pair_set

let collect_partition_modified_map_2 parameters error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  let proj (b, _, d, _) = b, d in
  Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
    (fun _ error (_, y) ->
       (*get the second site *)
       error, proj y
    )
    parameters
    error
    store_potential_tuple_pair_set

(***************************************************************)
(*add rule_id *)

let collect_rule_partition_modified_map_1 parameters error
    store_potential_tuple_pair_rule_rhs store_result =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id tuple_set (error, store_result) ->
       let error, map =
         let proj (b, _, d, _) = b, d in
         Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
           (fun _ error (x, _) ->
              error, proj x
           )
           parameters
           error
           tuple_set
       in
       let error, store_result =
         collect_rule_partition_aux
           parameters error
           rule_id
           map
           store_result
       in
       error, store_result
    ) store_potential_tuple_pair_rule_rhs (error, store_result)

let collect_rule_partition_modified_map_2 parameters error
    store_potential_tuple_pair_rule_rhs store_result =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id tuple_set (error, store_result) ->
       let error, map =
         let proj (b, _, d, _) = b, d in
         Site_accross_bonds_domain_type.Partition_modified_map.monadic_partition_set
           (fun _ error (_, y) ->
              (*get the second site *)
              error, proj y
           )
           parameters
           error
           tuple_set
       in
       let error, store_result =
         collect_rule_partition_aux
           parameters error
           rule_id
           map
           store_result
       in
       error, store_result
    ) store_potential_tuple_pair_rule_rhs (error, store_result)

(*views on the initial states*)

let collect_views_init parameters error init_state =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters error
    (fun parameters error agent_id agent store_result ->
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
                Ckappa_sig.AgentsSiteState_map_and_set.Set.add_when_not_in
                  parameters error
                  (agent_id, agent_type, site_type, state)
                  store_set
             ) agent.Cckappa_sig.agent_interface
             (error, store_result)
         in
         error, store_result
    ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
    Ckappa_sig.AgentsSiteState_map_and_set.Set.empty

(***************************************************************)
(*return an agent in the initial state that has two sites different*)

(*let collect_sites_init parameters error store_views_init =
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
                      parameters error
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
  error, store_result*)

(***************************************************************)
(*(A.x.y, B.z.t)*)

(* You should not compute this *)
(* This set is way too big and it is useless *)
(* When enumerating, always start by the most discriminating criterion *)
(* And then enumerate over further criterion within the elts that satisfy the previous ones *)
(* Stop doing two separate enumerations before crossing the results *)
(* It does not scale *)

(*let collect_pair_sites_init parameters error store_sites_init =
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
                      parameters error
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
  error, store_result*)

(***************************************************************)
(*collect bonds in the initial states*)

let collect_bonds_init parameters error init_state =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters error
    (fun parameters error agent_id bonds_map store_result ->
       let error, store_result =
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site_type_source site_add (error, store_result) ->
              let error,
                  ((agent_type_source, site_type_source, state_source),
                   (agent_type_target, site_type_target, state_target)) =
                Common_static.collect_pair_of_bonds
                  parameters error site_add agent_id site_type_source
                  init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
              in
              let pair =
                ((agent_id, agent_type_source, site_type_source, state_source),
                 (site_add.Cckappa_sig.agent_index, agent_type_target, site_type_target, state_target))
              in
              let error, store_result =
                Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                  parameters
                  error
                  pair
                  store_result
              in
              let error, store_result =
                Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                  parameters
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

(* JF: this function is too slow *)
(* You should first enumerate over bonds *)
(* Then one you have a bond, you have two agent ids *)
(* Then you have to fold over the sites of this two agents (only) *)

(*let collect_pair_tuple_init' parameters error bdu_false handler kappa_handler
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
                 parameters handler error pair_list
             in
             let error, handler, store_result =
               Site_accross_bonds_domain_type.add_link
                 parameters error bdu_false handler kappa_handler pair mvbdu store_result
             in
             error, handler, store_result
           else error, handler, store_result
         else
           error, handler, store_result
      ) store_pair_sites_init (error, handler, store_result)
  in
  error, handler, store_result*)

(***************************************************************)
(*TODO*)

let collect_tuple_pair_init parameters error store_bonds_init
    store_views_init =
  Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       let error, fst_list =
         collect_tuples error
           x
           store_views_init
       in
       let error, snd_list =
         collect_tuples error
           y
           store_views_init
       in
       let error, store_result =
         store_set
           parameters
           error
           fst_list
           snd_list
           store_result
       in
       error, store_result
    ) store_bonds_init
    (error,
     Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.empty)

let collect_potential_tuple_pair_init
    parameters error bdu_false handler kappa_handler
    tuple_init (*tuple_of_interest*)
    store_result =
  Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.fold
    (fun (x, y) (error, handler, store_result) ->
       let (agent_type, site_type1, site_type2, state1, state2) = x in
       let (agent_type', site_type1', site_type2', state1', state2') = y in
       (*if
         Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.mem
           ((agent_id, agent_type, site_type1, site_type2, state1),
            (agent_id', agent_type', site_type1', site_type2', state1'))
           tuple_of_interest
       then*)
         let pair_list =
           [(Ckappa_sig.fst_site, state2);
            (Ckappa_sig.snd_site, state2')]
         in
         let error, handler, mvbdu =
           Ckappa_sig.Views_bdu.mvbdu_of_association_list
             parameters
             handler error pair_list
         in
         let error, handler, store_result =
           Site_accross_bonds_domain_type.add_link
             parameters error bdu_false handler
             kappa_handler
             ((agent_type, site_type1, site_type2, state1),
              (agent_type', site_type1', site_type2', state1'))
             mvbdu
             store_result
         in
         error, handler, store_result
         (*else error, handler, store_result*)
    ) tuple_init (error, handler, store_result)
