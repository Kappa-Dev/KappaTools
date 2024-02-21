(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Aug 10 2018>
   *
   * Abstract domain to detect whether when two sites of an agent are bound,
   * they must be bound to the same agent.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

type local_static_information = {
  (*rule has two bonds (parallel or not) on the lhs*)
  store_rule_double_bonds_lhs:
    bool Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  (*rule has two bonds (parallel or not) on the rhs*)
  store_rule_double_bonds_rhs:
    (*use this*)
    bool Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t
    Ckappa_sig.Rule_map_and_set.Map.t;
      (*is a union set of double binding in the lhs and the rhs*)
  store_tuples_of_interest:
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.t;
  (* information of partial formation of parallel or non-parallel bonds in
     rules *)
  store_closure:
    (Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state
    * Ckappa_sig.c_state
    * ((Ckappa_sig.c_agent_name
       * Ckappa_sig.c_site_name
       * Ckappa_sig.c_site_name
       * Ckappa_sig.c_state
       * Ckappa_sig.c_state)
      * (Ckappa_sig.c_agent_name
        * Ckappa_sig.c_site_name
        * Ckappa_sig.c_site_name
        * Ckappa_sig.c_state
        * Ckappa_sig.c_state))
    * ((Ckappa_sig.c_agent_name
       * Ckappa_sig.c_site_name
       * Ckappa_sig.c_site_name
       * Ckappa_sig.c_state
       * Ckappa_sig.c_state)
      * (Ckappa_sig.c_agent_name
        * Ckappa_sig.c_site_name
        * Ckappa_sig.c_site_name
        * Ckappa_sig.c_state
        * Ckappa_sig.c_state)))
    list
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
  store_fst_site_create_parallel_bonds_rhs:
    ((Ckappa_sig.c_agent_id
     * Ckappa_sig.c_agent_name
     * Ckappa_sig.c_site_name
     * Ckappa_sig.c_site_name
     * Ckappa_sig.c_state
     * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state
      * Ckappa_sig.c_state))
    list
    Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  store_snd_site_create_parallel_bonds_rhs:
    ((Ckappa_sig.c_agent_id
     * Ckappa_sig.c_agent_name
     * Ckappa_sig.c_site_name
     * Ckappa_sig.c_site_name
     * Ckappa_sig.c_state
     * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state
      * Ckappa_sig.c_state))
    list
    Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t
    Ckappa_sig.Rule_map_and_set.Map.t;
  (*A map from tuples -> sites (agent_type, site_name)*)
  store_tuple_to_sites:
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.t
    Parallel_bonds_type.PairAgentSite_map_and_set.Map.t;
  (*a map from sites -> tuples *)
  store_sites_to_tuple:
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.t
    Parallel_bonds_type.AgentSite_map_and_set.Map.t;
}

(*******************************************************************)

let init_local_static =
  {
    store_tuples_of_interest =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.empty;
    store_rule_double_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_fst_site_create_parallel_bonds_rhs =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_snd_site_create_parallel_bonds_rhs =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_double_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_tuple_to_sites =
      Parallel_bonds_type.PairAgentSite_map_and_set.Map.empty;
    store_closure =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
    store_sites_to_tuple = Parallel_bonds_type.AgentSite_map_and_set.Map.empty;
  }

(*******************************************************************)

let collect_double_bonds_in_pattern parameters error ?tuple_of_interest pattern
    =
  let good_tuple =
    match tuple_of_interest with
    | None -> fun _ -> true
    | Some t_set ->
      fun tuple ->
        Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.mem
          (Parallel_bonds_type.project2 tuple)
          t_set
  in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_id_source bonds_map store_result ->
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun site_type_source site_add (error, store_result) ->
          let ( error,
                ( (agent_type_source, site_type_source, state_source),
                  (agent_type_target, site_type_target, state_target) ) ) =
            (*translate_bond*)
            Common_static.collect_fingerprint_of_bond parameters error site_add
              agent_id_source site_type_source pattern.Cckappa_sig.views
          in
          Ckappa_sig.Site_map_and_set.Map.fold
            (fun site_type_source' site_add' (error, store_result) ->
              (* the two bonds necessarily start from the same agent (id) *)
              (* we check that they start from two different sites *)
              (* and that they go into two agents with the same type *)
              if
                site_type_source <> site_type_source'
                && site_add.Cckappa_sig.agent_type
                   = site_add'.Cckappa_sig.agent_type
              then (
                let agent_id_target = site_add.Cckappa_sig.agent_index in
                let agent_id_target' = site_add'.Cckappa_sig.agent_index in
                let bool =
                  (* if the ids of the targets is the same, we have a parallel bond, other with it is a non parallel bond *)
                  agent_id_target = agent_id_target'
                in
                let ( error,
                      ( (_, _, state_source'),
                        (_, site_type_target', state_target') ) ) =
                  (*translate_bond*)
                  Common_static.collect_fingerprint_of_bond parameters error
                    site_add' agent_id_source site_type_source'
                    pattern.Cckappa_sig.views
                in
                (* the two target sites  should also have different types *)
                if site_type_target <> site_type_target' then (
                  let tuple =
                    ( agent_id_source,
                      ( ( agent_type_source,
                          site_type_source,
                          site_type_source',
                          state_source,
                          state_source' ),
                        ( agent_type_target,
                          site_type_target,
                          site_type_target',
                          state_target,
                          state_target' ) ) )
                  in
                  if
                    (* only tuples of interest are interesting :-) *)
                    good_tuple tuple
                  then
                    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map
                    .add parameters error tuple bool store_result
                  else
                    error, store_result
                ) else
                  error, store_result
              ) else
                error, store_result)
            bonds_map (error, store_result))
        bonds_map (error, store_result))
    pattern.Cckappa_sig.bonds
    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty

let project_away_ag_id_gen f parameters error big_store acc =
  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.fold
    (fun tuple value (error, acc) ->
      f parameters error (Parallel_bonds_type.project2 tuple) value acc)
    big_store (error, acc)

let project_away_ag_id parameters _kappa_handler error big_store acc =
  let f parameters error tuple value acc =
    Parallel_bonds_type.add_value parameters error tuple
      (Usual_domains.Val value) acc
  in
  project_away_ag_id_gen f parameters error big_store acc

let project_away_ag_id_and_convert_into_set parameters error big_store acc =
  let f parameters error tuple _ acc =
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.add_when_not_in
      parameters error tuple acc
  in
  project_away_ag_id_gen f parameters error big_store acc

(****************************************************************)
(** Detect pair of bonds *)
(****************************************************************)

let collect_rule_double_bonds_lhs parameters error rule_id rule store_result =
  let error, map =
    collect_double_bonds_in_pattern parameters error rule.Cckappa_sig.rule_lhs
  in
  Ckappa_sig.Rule_map_and_set.Map.add parameters error rule_id map store_result

let collect_rule_double_bonds_rhs parameters error rule_id rule store_result =
  let error, map =
    collect_double_bonds_in_pattern parameters error rule.Cckappa_sig.rule_rhs
  in
  Ckappa_sig.Rule_map_and_set.Map.add parameters error rule_id map store_result

(**************************************************************************)
(*a map (A,x,y, B,z,t) -> (Ag_id, Ag_id) RuleIDMap to explain
  which rules can create a bond of type A.x.z.B (and at which position)*)

type pos = Fst | Snd

let collect_site_create_parallel_bonds_gen pos parameters error
    store_action_binding store_parallel_bonds =
  let pick pos a b =
    match pos with
    | Fst -> a
    | Snd -> b
  in
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun k set (error, map) ->
      let error, new_set =
        (*Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold*)
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
          (*A.x -> B.z; B.z -> A.x*)
            (fun ( (agent_id, agent_type, site_type, state),
                   (agent_id', agent_type', site_type', state') )
                 (error, store_result) ->
            let error, old_list =
              match
                Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map
                .find_option_without_logs parameters error
                  ( (agent_id, agent_type, site_type, state),
                    (agent_id', agent_type', site_type', state') )
                  store_result
              with
              | error, None -> error, []
              | error, Some l -> error, l
            in
            let error', new_list =
              Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.fold_inv
                (fun ( (agent_type1, site_type1, site_type2, state1, state2),
                       (agent_type1', site_type1', site_type2', state1', state2')
                     ) (error, current_list) ->
                  if
                    agent_type = agent_type1
                    && site_type = pick pos site_type1 site_type2
                    && agent_type' = agent_type1'
                    && site_type' = pick pos site_type1' site_type2'
                  then (
                    (*A.x.B.z, B.z.A.x*)
                    let new_list =
                      ( ( agent_id,
                          agent_type1,
                          site_type1,
                          site_type2,
                          state1,
                          state2 ),
                        ( agent_id',
                          agent_type1',
                          site_type1',
                          site_type2',
                          state1',
                          state2' ) )
                      :: current_list
                    in
                    error, new_list
                  ) else
                    error, current_list)
                store_parallel_bonds (error, old_list)
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error, store_result =
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map
              .add_or_overwrite parameters error
                ( (agent_id, agent_type, site_type, state),
                  (agent_id', agent_type', site_type', state') )
                new_list store_result
            in
            error, store_result)
          set
          (error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty)
      in
      Ckappa_sig.Rule_map_and_set.Map.add parameters error k new_set map)
    store_action_binding
    (error, Ckappa_sig.Rule_map_and_set.Map.empty)

let collect_fst_site_create_parallel_bonds =
  collect_site_create_parallel_bonds_gen Fst

let collect_snd_site_create_parallel_bonds =
  collect_site_create_parallel_bonds_gen Snd

(**************************************************************************)
(*in the rhs*)
(*the fst map (A,x,y, B,z,t) -> A.x.z.B*)
(**************************************************************************)

let collect_fst_site_create_parallel_bonds_rhs parameters error
    store_action_binding store_parallel_bonds =
  collect_fst_site_create_parallel_bonds parameters error store_action_binding
    store_parallel_bonds

(**************************************************************************)
(*the second map (A,x,y, B,z,t) -> A.y.t.B*)
(**************************************************************************)

let collect_snd_site_create_parallel_bonds_rhs parameters error
    store_action_binding store_parallel_bonds =
  let error, store_result =
    collect_snd_site_create_parallel_bonds parameters error store_action_binding
      store_parallel_bonds
  in
  error, store_result

(*******************************************************************)
(*A map from tuples -> sites
  ex: A(x!1, y!2), B(x!1, y!2) -> {(A,x); (A,y); (B,x); (B,y)}
*)
(*******************************************************************)

let proj_first_site (a, b, _, _, _) = a, b
let proj_second_site (a, _, c, _, _) = a, c

let collect_tuple_to_sites parameters error tuples_of_interest =
  Parallel_bonds_type.Partition_tuples_to_sites_map.monadic_partition_set
    (fun _ error (u, v) ->
      ( error,
        ( proj_first_site u,
          proj_second_site u,
          proj_first_site v,
          proj_second_site v ) ))
    parameters error tuples_of_interest

(*******************************************************************)
(*A map from sites -> tuples
  ex: {(A,x); (A,y); (B,x); (B,y)} -> A(x!1, y!2), B(x!1, y!2)
*)
(*******************************************************************)

let compare_first_pair parameters error x tuple_set store_result =
  let agent_type_x, site_type_x = x in
  (*A,x*)
  Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.fold
    (fun (u, v) (error, store_result) ->
      let agent_type, site_type, _site_type', _state, _state' = u in
      let agent_type1, site_type1, _site_type1', _state1, _state1' = v in
      if
        (agent_type_x = agent_type && site_type_x = site_type)
        || (agent_type_x = agent_type1 && site_type_x = site_type1)
      then (
        let error', new_set =
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set
          .add_when_not_in parameters error (u, v)
            Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.empty
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error, store_result =
          Parallel_bonds_type.AgentSite_map_and_set.Map.add_or_overwrite
            parameters error x new_set store_result
        in
        error, store_result
      ) else
        error, store_result)
    tuple_set (error, store_result)

let compare_snd_pair parameters error y tuple_pair store_result =
  let agent_type_y, site_type_y = y in
  (*A,x*)
  Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.fold
    (fun (u, v) (error, store_result) ->
      let agent_type, _site_type, site_type', _state, _state' = u in
      let agent_type1, _site_type1, site_type1', _state1, _state1' = v in
      if
        (agent_type_y = agent_type && site_type_y = site_type')
        || (agent_type_y = agent_type1 && site_type_y = site_type1')
      then (
        let error', new_set =
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set
          .add_when_not_in parameters error (u, v)
            Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.empty
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error, store_result =
          Parallel_bonds_type.AgentSite_map_and_set.Map.add_or_overwrite
            parameters error y new_set store_result
        in
        error, store_result
      ) else
        error, store_result)
    tuple_pair (error, store_result)

(*map from sites to tuple *)

let collect_sites_to_tuple parameters error map_of_sites store_result =
  Parallel_bonds_type.PairAgentSite_map_and_set.Map.fold
    (fun (x, y, _z, _t) tuple_set (error, store_result) ->
      (*---------------------------------------------------------------*)
      let error', store_result1 =
        compare_first_pair parameters error x tuple_set
          Parallel_bonds_type.AgentSite_map_and_set.Map.empty
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      (*---------------------------------------------------------------*)
      let error, store_result2 =
        compare_snd_pair parameters error y tuple_set
          Parallel_bonds_type.AgentSite_map_and_set.Map.empty
      in
      (*---------------------------------------------------------------*)
      let add_link parameters error x tuple_set store_result =
        let error, old_set =
          match
            Parallel_bonds_type.AgentSite_map_and_set.Map
            .find_option_without_logs parameters error x store_result
          with
          | error, None ->
            ( error,
              Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.empty )
          | error, Some s -> error, s
        in
        let error', new_set =
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.union
            parameters error old_set tuple_set
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error, store_result =
          Parallel_bonds_type.AgentSite_map_and_set.Map.add_or_overwrite
            parameters error x new_set store_result
        in
        error, store_result
      in
      (*---------------------------------------------------------------*)
      let error, store_result =
        Parallel_bonds_type.AgentSite_map_and_set.Map.fold2 parameters error
          (fun parameters error elt tuple_set_x store_result ->
            let error, store_result =
              add_link parameters error elt tuple_set_x store_result
            in
            error, store_result)
          (fun parameters error elt tuple_set_y store_result ->
            let error, store_result =
              add_link parameters error elt tuple_set_y store_result
            in
            error, store_result)
          (fun parameters error elt tuple_set_x tuple_set_y store_result ->
            let error', new_set =
              Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.union
                parameters error tuple_set_x tuple_set_y
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error, store_result =
              add_link parameters error elt new_set store_result
            in
            error, store_result)
          store_result1 store_result2 store_result
      in
      error, store_result)
    map_of_sites (error, store_result)
