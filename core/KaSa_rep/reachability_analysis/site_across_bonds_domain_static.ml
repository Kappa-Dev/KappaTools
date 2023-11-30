(**
   * site_across_bonds_domain_static.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 29th of June
   * Last modification: Time-stamp: <Feb 26 2018>
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

type potential_tuple_pair_views = {
  store_potential_tuple_pair:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t;
  store_potential_tuple_pair_lhs:
    Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_potential_tuple_pair_rule_rhs:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
}

type potential_tuple_pair_creation_bonds = {
  store_partition_created_bonds_map:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Site_across_bonds_domain_type.PairAgentSiteState_map_and_set.Map.t;
  store_partition_created_bonds_map_1:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Site_across_bonds_domain_type.AgentSite_map_and_set.Map.t;
  store_partition_created_bonds_map_2:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Site_across_bonds_domain_type.AgentSite_map_and_set.Map.t;
  store_rule_partition_created_bonds_map_1:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_rule_partition_created_bonds_map_2:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
}

type potential_tuple_pair_modification = {
  store_partition_modified_map_1:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Site_across_bonds_domain_type.AgentSite_map_and_set.Map.t;
  store_partition_modified_map_2:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Site_across_bonds_domain_type.AgentSite_map_and_set.Map.t;
  (**)
  store_rule_partition_modified_map_1:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_rule_partition_modified_map_2:
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.t
    Ckappa_sig.Rule_map_and_set.Map.t;
}

type basic_static_information = {
  store_potential_tuple_pair_views: potential_tuple_pair_views;
  store_potential_tuple_pair_creation_bonds:
    potential_tuple_pair_creation_bonds;
  store_potential_tuple_pair_modification: potential_tuple_pair_modification;
}

(****************************************************************)
(*Init*)
(****************************************************************)

let init_potential_tuple_pair_views =
  {
    store_potential_tuple_pair =
      Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty;
    store_potential_tuple_pair_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_potential_tuple_pair_rule_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
  }

let init_potential_tuple_pair_creation_bonds =
  {
    store_partition_created_bonds_map =
      Site_across_bonds_domain_type.PairAgentSiteState_map_and_set.Map.empty;
    store_partition_created_bonds_map_1 =
      Site_across_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_partition_created_bonds_map_2 =
      Site_across_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_rule_partition_created_bonds_map_1 =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_partition_created_bonds_map_2 =
      Ckappa_sig.Rule_map_and_set.Map.empty;
  }

let init_potential_tuple_pair_modification =
  {
    store_partition_modified_map_1 =
      Site_across_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_partition_modified_map_2 =
      Site_across_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_rule_partition_modified_map_1 = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_partition_modified_map_2 = Ckappa_sig.Rule_map_and_set.Map.empty;
  }

let init_basic_static_information =
  {
    store_potential_tuple_pair_views = init_potential_tuple_pair_views;
    store_potential_tuple_pair_creation_bonds =
      init_potential_tuple_pair_creation_bonds;
    store_potential_tuple_pair_modification =
      init_potential_tuple_pair_modification;
  }

(***************************************************************)
(*TUPLE/S*)
(*collect a set of tuple pair (A.x.y, B.z.t) on the rhs*)
(***************************************************************)

let collect_tuple parameters error kappa_handler
    (agent_id, agent_type, site_type, state) views_set current_list =
  let error, b =
    Handler.is_counter parameters error kappa_handler agent_type site_type
  in
  if b then
    error, current_list
  else (
    let error, site_map =
      Common_map.get_agent_id parameters error agent_id
        Ckappa_sig.Site_map_and_set.Map.empty views_set
    in
    Ckappa_sig.Site_map_and_set.Map.fold
      (fun site_type_v _pair_of_state_v (error, current_list) ->
        if site_type <> site_type_v then (
          let error, b =
            Handler.is_counter parameters error kappa_handler agent_type
              site_type_v
          in
          if b then
            error, current_list
          else (
            let list =
              (agent_type, site_type, site_type_v, state) :: current_list
            in
            error, list
          )
        ) else
          error, current_list)
      site_map (error, current_list)
  )

let collect_tuples parameters error kappa_handler
    (agent_id, agent_type, site_type, state) views_set current_list =
  let error, b =
    Handler.is_counter parameters error kappa_handler agent_type site_type
  in
  if b then
    error, current_list
  else (
    let error, site_map =
      Common_map.get_agent_id parameters error agent_id
        Ckappa_sig.Site_map_and_set.Map.empty views_set
    in
    Ckappa_sig.Site_map_and_set.Map.fold
      (fun site_type_v pair_of_state_v (error, current_list) ->
        if site_type <> site_type_v then (
          let error, b =
            Handler.is_counter parameters error kappa_handler agent_type
              site_type_v
          in
          if b then
            error, current_list
          else (
            let list =
              (agent_type, site_type, site_type_v, state, pair_of_state_v)
              :: current_list
            in
            error, list
          )
        ) else
          error, current_list)
      site_map (error, current_list)
  )

let store_set parameters error fst_list snd_list store_result =
  List.fold_left
    (fun (error, store_result) x ->
      List.fold_left
        (fun (error, store_result) y ->
          let error, store_result =
            Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set
            .add_when_not_in parameters error (x, y) store_result
          in
          error, store_result)
        (error, store_result) snd_list)
    (error, store_result) fst_list

(***************************************************************)
(*POTENTIAL TUPLE PAIR*)
(***************************************************************)

let collect_potential_tuple_pair parameters error kappa_handler rule_id
    store_bonds_rhs store_views_rhs store_result =
  let error, bonds_set =
    Common_map.get_rule_id_map_and_set parameters error rule_id
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty store_bonds_rhs
  in
  let error, views_set =
    Common_map.get_rule_id_map_and_set parameters error rule_id
      Ckappa_sig.Agent_id_map_and_set.Map.empty store_views_rhs
  in
  Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
      let error, fst_list =
        collect_tuple parameters error kappa_handler x views_set []
      in
      let error, snd_list =
        collect_tuple parameters error kappa_handler y views_set []
      in
      let error, store_result =
        List.fold_left
          (fun (error, store_result) x ->
            List.fold_left
              (fun (error, store_result) y ->
                let error, store_result =
                  Site_across_bonds_domain_type.PairAgentSitesState_map_and_set
                  .Set
                  .add_when_not_in parameters error (x, y) store_result
                in
                error, store_result)
              (error, store_result) snd_list)
          (error, store_result) fst_list
      in
      error, store_result)
    bonds_set (error, store_result)

let collect_potential_tuple_pair_rule_rhs parameters error rule_id
    store_potential_tuple_pair_rhs store_result =
  Ckappa_sig.Rule_map_and_set.Map.add parameters error rule_id
    store_potential_tuple_pair_rhs store_result

let build_potential_tuple_pair_set parameters error kappa_handler bonds_set
    views_map =
  let error, tuple_set =
    Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
      (fun (x, y) (error, store_result) ->
        let error, fst_list =
          collect_tuples parameters error kappa_handler x views_map []
        in
        let error, snd_list =
          collect_tuples parameters error kappa_handler y views_map []
        in
        let error, store_result =
          store_set parameters error fst_list snd_list store_result
        in
        error, store_result)
      bonds_set
      ( error,
        Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set
        .empty )
  in
  error, tuple_set

let collect_potential_tuple_pair_lhs parameters error kappa_handler rule_id
    store_bonds_lhs store_views_lhs store_result =
  let error, bonds_lhs_set =
    Common_map.get_rule_id_map_and_set parameters error rule_id
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty store_bonds_lhs
  in
  let error, views_lhs_map =
    Common_map.get_rule_id_map_and_set parameters error rule_id
      Ckappa_sig.Agent_id_map_and_set.Map.empty store_views_lhs
  in
  let error, pair_set =
    build_potential_tuple_pair_set parameters error kappa_handler bonds_lhs_set
      views_lhs_map
  in
  (*add the set of tuple to rule_id map*)
  Ckappa_sig.Rule_map_and_set.Map.add parameters error rule_id pair_set
    store_result

(***************************************************************)
(*CREATION*)
(***************************************************************)

(*collect a map of rule that store a set of sites can created bonds*)

let collect_partition_created_bonds_map parameters error
    store_potential_tuple_pair_set =
  Site_across_bonds_domain_type.Partition_created_bonds_map
  .monadic_partition_set
    (fun _parameters error (x, y) ->
      error, (Common_map.project_second_site x, Common_map.project_second_site y))
    parameters error store_potential_tuple_pair_set

let collect_partition_created_bonds_map_aux parameters error x tuple_set
    store_result =
  let error, old_set =
    match
      Site_across_bonds_domain_type.AgentSite_map_and_set.Map
      .find_option_without_logs parameters error x store_result
    with
    | error, None ->
      ( error,
        Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty
      )
    | error, Some s -> error, s
  in
  let error', new_set =
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.union
      parameters error old_set tuple_set
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error, store_result =
    Site_across_bonds_domain_type.AgentSite_map_and_set.Map.add_or_overwrite
      parameters error x new_set store_result
  in
  error, store_result

let collect_partition_created_bonds_map_1 parameters error
    store_partition_created_bonds_map store_result =
  Site_across_bonds_domain_type.PairAgentSiteState_map_and_set.Map.fold
    (fun (x, _) tuple_set (error, store_result) ->
      collect_partition_created_bonds_map_aux parameters error
        (Common_map.project_state x)
        tuple_set store_result)
    store_partition_created_bonds_map (error, store_result)

let collect_partition_created_bonds_map_2 parameters error
    store_partition_created_bonds_map store_result =
  Site_across_bonds_domain_type.PairAgentSiteState_map_and_set.Map.fold
    (fun (_, y) tuple_set (error, store_result) ->
      collect_partition_created_bonds_map_aux parameters error
        (Common_map.project_state y)
        tuple_set store_result)
    store_partition_created_bonds_map (error, store_result)

let collect_rule_partition_aux parameters error rule_id map store_result =
  let error, store_result =
    Site_across_bonds_domain_type.AgentSite_map_and_set.Map.fold
      (fun _pair_site tuple_set (error, store_result) ->
        let error', old_set =
          Common_map.get_rule_id_map_and_set parameters error rule_id
            Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set
            .empty store_result
        in
        let error =
          Exception.check_point Exception.warn parameters error error __POS__
            Exit
        in
        let error'', new_set =
          Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set
          .union parameters error' old_set tuple_set
        in
        let error =
          Exception.check_point Exception.warn parameters error error'' __POS__
            Exit
        in
        let error, store_result =
          Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameters error
            rule_id new_set store_result
        in
        error, store_result)
      map (error, store_result)
  in
  error, store_result

let collect_rule_partition_created_bonds_map_1 parameters error
    store_rule_potential_tuple_pair_set_rhs store_result =
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.fold
      (fun rule_id tuple_set (error, store_result) ->
        let error, map1 =
          Site_across_bonds_domain_type.Partition_modified_map
          .monadic_partition_set
            (fun _ error (x, _) ->
              error, Common_map.project_second_site_state x)
            parameters error tuple_set
        in
        let error, store_result =
          collect_rule_partition_aux parameters error rule_id map1 store_result
        in
        error, store_result)
      store_rule_potential_tuple_pair_set_rhs (error, store_result)
  in
  error, store_result

let collect_rule_partition_created_bonds_map_2 parameters error
    store_rule_potential_tuple_pair_set_rhs store_result =
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.fold
      (fun rule_id tuple_set (error, store_result) ->
        let error, map2 =
          Site_across_bonds_domain_type.Partition_modified_map
          .monadic_partition_set
            (fun _ error (_, y) ->
              error, Common_map.project_second_site_state y)
            parameters error tuple_set
        in
        let error, store_result =
          collect_rule_partition_aux parameters error rule_id map2 store_result
        in
        error, store_result)
      store_rule_potential_tuple_pair_set_rhs (error, store_result)
  in
  error, store_result

(***************************************************************)
(*MODIFICATION*)
(***************************************************************)

let collect_partition_modified_map_1 parameters error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  Site_across_bonds_domain_type.Partition_modified_map.monadic_partition_set
    (fun _ error (x, _) -> error, Common_map.project_first_site_state x)
    parameters error store_potential_tuple_pair_set

let collect_partition_modified_map_2 parameters error
    store_potential_tuple_pair_set =
  (*agent_type, site_type, site_type', state*)
  Site_across_bonds_domain_type.Partition_modified_map.monadic_partition_set
    (fun _ error (_, y) -> error, Common_map.project_first_site_state y)
    parameters error store_potential_tuple_pair_set

let collect_rule_partition_modified_map_1 parameters error
    store_potential_tuple_pair_rule_rhs store_result =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id tuple_set (error, store_result) ->
      let error, map =
        Site_across_bonds_domain_type.Partition_modified_map
        .monadic_partition_set
          (fun _ error (x, _) -> error, Common_map.project_first_site_state x)
          parameters error tuple_set
      in
      let error, store_result =
        collect_rule_partition_aux parameters error rule_id map store_result
      in
      error, store_result)
    store_potential_tuple_pair_rule_rhs (error, store_result)

let collect_rule_partition_modified_map_2 parameters error
    store_potential_tuple_pair_rule_rhs store_result =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun rule_id tuple_set (error, store_result) ->
      let error, map =
        Site_across_bonds_domain_type.Partition_modified_map
        .monadic_partition_set
          (fun _ error (_, y) -> error, Common_map.project_first_site_state y)
          parameters error tuple_set
      in
      let error, store_result =
        collect_rule_partition_aux parameters error rule_id map store_result
      in
      error, store_result)
    store_potential_tuple_pair_rule_rhs (error, store_result)

(***************************************************************)
(*INITIAL STATE*)
(***************************************************************)

let collect_potential_tuple_pair_init parameters error bdu_false handler
    kappa_handler tuple_init store_result =
  Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set.fold
    (fun (x, y) (error, handler, store_result) ->
      let agent_type, site_type1, site_type2, state1, pair_of_state2 = x in
      let agent_type', site_type1', site_type2', state1', pair_of_state2' = y in
      let pair_list =
        [
          Ckappa_sig.fst_site, pair_of_state2;
          Ckappa_sig.snd_site, pair_of_state2';
        ]
      in
      let error, handler, mvbdu =
        Ckappa_sig.Views_bdu.mvbdu_of_range_list parameters handler error
          pair_list
      in
      let error, handler, store_result =
        Site_across_bonds_domain_type.add_link parameters error bdu_false
          handler kappa_handler
          ( (agent_type, site_type1, site_type2, state1),
            (agent_type', site_type1', site_type2', state1') )
          mvbdu store_result
      in
      error, handler, store_result)
    tuple_init
    (error, handler, store_result)
