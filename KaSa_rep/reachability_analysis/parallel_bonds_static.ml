(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Sep 19 2016>
   *
   * Abstract domain to detect whether when two sites of an agent are bound,
   * they must be bound to the same agent.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

type local_static_information =
  {
    (*rule has two bonds (parallel or not) on the lhs*)
      store_rule_double_bonds_lhs :
      (bool Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t)
        Ckappa_sig.Rule_map_and_set.Map.t ;
    (*rule has two bonds (parallel or not) on the rhs*)      store_rule_double_bonds_rhs :
      (bool Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t)
        Ckappa_sig.Rule_map_and_set.Map.t ;
      (*is a union set of double binding in the lhs and the rhs*)
    store_tuples_of_interest: Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.t;
    (* information of partial formation of parallel or non-parallel bonds in rules *)
    (*store_views_rhs :
      Parallel_bonds_type.AgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;*)
    (*store_action_binding :
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;*)
    store_fst_site_create_parallel_bonds_rhs:
      ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
        Ckappa_sig.c_state * Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
        Ckappa_sig.c_state * Ckappa_sig.c_state)
      ) list
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_snd_site_create_parallel_bonds_rhs:
      ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
        Ckappa_sig.c_state * Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
        Ckappa_sig.c_state * Ckappa_sig.c_state)
      ) list
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*A map from tuples -> sites (agent_type, site_name)*)
    store_tuple_to_sites :
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.t
        Parallel_bonds_type.PairAgentSite_map_and_set.Map.t;
    store_sites_to_tuple :
      ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
       Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
      (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
       Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state))
        Parallel_bonds_type.AgentSite_map_and_set.Map.t *
        ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
         Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
        (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
         Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state))
        Parallel_bonds_type.AgentSite_map_and_set.Map.t
  }

(*******************************************************************)

let init_local_static =
  {
    (*store_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;*)
    (*store_action_binding = Ckappa_sig.Rule_map_and_set.Map.empty;*)
    store_tuples_of_interest = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.empty;
    store_rule_double_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_fst_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_snd_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_double_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_tuple_to_sites = Parallel_bonds_type.PairAgentSite_map_and_set.Map.empty;
    store_sites_to_tuple =
      Parallel_bonds_type.AgentSite_map_and_set.Map.empty,
      Parallel_bonds_type.AgentSite_map_and_set.Map.empty
  }

(*******************************************************************)

let collect_agent_type_state parameter error agent site_type =
  let dummy_agent = Ckappa_sig.dummy_agent_name in
  let dummy_state = Ckappa_sig.dummy_state_index in
  match agent with
  | Cckappa_sig.Ghost
  | Cckappa_sig.Unknown_agent _ -> error,(dummy_agent, dummy_state)
  | Cckappa_sig.Dead_agent _ ->
    Exception.warn
      parameter error __POS__ Exit (dummy_agent, dummy_state)
  | Cckappa_sig.Agent agent1 ->
    let agent_type1 = agent1.Cckappa_sig.agent_name in
    let error, state1 =
      match
        Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
          parameter
          error
          site_type
          agent1.Cckappa_sig.agent_interface
      with
      | error, None ->
        Exception.warn parameter error __POS__ Exit dummy_state
      | error, Some port ->
        let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
        if Ckappa_sig.compare_state_index state dummy_state > 0
        then
          error, state
        else
          Exception.warn parameter error __POS__ Exit dummy_state
    in
    error, (agent_type1, state1)

let translate_bond parameter error site_add agent_id site_type_source views =
  let error, pair =
    let agent_index_target = site_add.Cckappa_sig.agent_index in
    let site_type_target = site_add.Cckappa_sig.site in
    let error, agent_source =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_id views
      with
      | error, None ->
        Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, agent_target =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_index_target views
      with
      | error, None ->
        Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, (agent_type1, state1) =
      collect_agent_type_state
        parameter
        error
        agent_source
        site_type_source
    in
    let error, (agent_type2, state2) =
      collect_agent_type_state
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

let collect_double_bonds_in_pattern
    parameter error ?tuple_of_interest pattern =
  let good_tuple =
    match tuple_of_interest with
    | None -> (fun _ -> true)
    | Some t_set ->
      (fun tuple ->
         Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.mem
           (Parallel_bonds_type.project2 tuple) t_set
      )
  in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter error
    (fun parameter error agent_id_source bonds_map store_result
      ->
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site_type_source site_add
            (error, store_result) ->
            let error,
                ((agent_type_source, site_type_source, state_source),
                 (agent_type_target, site_type_target, state_target)) =
              translate_bond
                parameter error
                site_add
                agent_id_source
                site_type_source
                pattern.Cckappa_sig.views
            in
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source' site_add'
                (error, store_result) ->
                (* the two bonds necessarily start from the same agent (id) *)
                (* we check that they start from two different sites *)
                (* and that they go into two agents with the same type *)
                if site_type_source <> site_type_source'
                && site_add.Cckappa_sig.agent_type = site_add'.Cckappa_sig.agent_type
                then
                  let agent_id_target = site_add.Cckappa_sig.agent_index in
                  let agent_id_target' = site_add'.Cckappa_sig.agent_index in
                  let bool =
                    (* if the ids of the targets is the same, we have a parallel bond, other with it is a non parallel bond *)
                    agent_id_target = agent_id_target'
                  in
                  let error,
                      ((_,_,state_source'),
                       (_, site_type_target', state_target')) =
                    translate_bond
                      parameter error
                      site_add'
                      agent_id_source
                      site_type_source'
                      pattern.Cckappa_sig.views
                  in
                  (* the two target sites  should also have different types *)
                  if site_type_target <> site_type_target'
                  then
                    let tuple =
                      agent_id_source,((agent_type_source, site_type_source, site_type_source', state_source, state_source'),
                                       (agent_type_target,
                                        site_type_target, site_type_target', state_target, state_target'))
                    in
                    if (* only tuples of interest are interesting :-) *)
                      good_tuple tuple
                    then
                      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add
                        parameter error
                        tuple
                        bool
                        store_result
                    else
                      error, store_result
                  else
                    error, store_result
                else error, store_result
              ) bonds_map (error, store_result)
          ) bonds_map (error, store_result)
    )
    pattern.Cckappa_sig.bonds
    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty

let project_away_ag_id_gen f parameter error big_store acc =
  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.fold
    (fun tuple value (error, acc) ->
       f
         parameter error
         (Parallel_bonds_type.project2 tuple)
         value
         acc)
    big_store (error, acc)

let project_away_ag_id parameter kappa_handler error big_store acc =
  let f parameter error tuple value acc =
    Parallel_bonds_type.add_value
      parameter error kappa_handler
      tuple
      (Usual_domains.Val value)
      acc
  in
  project_away_ag_id_gen f parameter error big_store acc

let project_away_ag_id_and_convert_into_set
    parameter error big_store acc =
  let f parameter error tuple _ acc =
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.add_when_not_in
      parameter error tuple acc
  in
  project_away_ag_id_gen f parameter error big_store acc

(************************************************************************)
(*action binding in the rhs*)
(************************************************************************)
(*
let collect_action_binding parameter error rule_id rule store_result =
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
      (*get information of a rule that created a bond*)
      let agent_id1 = site_add1.Cckappa_sig.agent_index in
      let site_type1 = site_add1.Cckappa_sig.site in
      let agent_id2 = site_add2.Cckappa_sig.agent_index in
      let site_type2 = site_add2.Cckappa_sig.site in
      let error, agent_source =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id1 rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None ->
          Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      (*get pair agent_type, state*)
      let error, (agent_type1, state1) =
        collect_agent_type_state
          parameter
          error
          agent_source
          site_type1
      in
      (*----------------------------------------------------------*)
      (*second pair*)
      let error, agent_target =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id2 rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None -> Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, (agent_type2, state2) =
        collect_agent_type_state
          parameter
          error
          agent_target
          site_type2
      in
      (*add the pair inside the set*)
      let error, old_set =
        match
          Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
            parameter
            error
            rule_id
            store_result
        with
        | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
        | error, Some s -> error, s
      in
      let error', set =
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
          parameter
          error
          ((agent_id1, agent_type1, site_type1, state1), (agent_id2, agent_type2, site_type2, state2))
          old_set
      in
      let error', set =
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
          parameter
          error'
          ((agent_id2, agent_type2, site_type2, state2),
           (agent_id1, agent_type1, site_type1, state1))
          set
      in
      let error =
        Exception.check_point
          Exception.warn parameter error error' __POS__ Exit
      in
      let error, store_result =
        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
          parameter
          error
          rule_id
          set
          store_result
      in
      error, store_result
    ) (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind
  *)
(****************************************************************)
(** Detect pair of bonds *)
(****************************************************************)

let collect_rule_double_bonds_lhs
    parameter error rule_id rule store_result  =
  let error, map =
    collect_double_bonds_in_pattern
      parameter error rule.Cckappa_sig.rule_lhs
  in
  Ckappa_sig.Rule_map_and_set.Map.add
    parameter error rule_id map store_result

let collect_rule_double_bonds_rhs
    parameter error rule_id rule store_result  =
  let error, map =
    collect_double_bonds_in_pattern
      parameter error rule.Cckappa_sig.rule_rhs
  in
  Ckappa_sig.Rule_map_and_set.Map.add
    parameter error rule_id map store_result

(**************************************************************************)
(*views on the rhs*)
(**************************************************************************)

(*let collect_views_rhs parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Unknown_agent _
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, old_set =
             match
               Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                 parameter error rule_id store_result
             with
             | error, None -> error, Parallel_bonds_type.AgentsSiteState_map_and_set.Set.empty
             | error, Some s -> error, s
           in
           let error', set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Parallel_bonds_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, state)
                      store_set
                  in
                  error, store_set
               ) agent.Cckappa_sig.agent_interface
               (error, old_set)
           in
           let error =
             Exception.check_point
               Exception.warn parameter error error'
               __POS__ Exit
           in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
               parameter error rule_id
               set
               store_result
           in
           error, store_result
      ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.views store_result
  in
  error, store_result*)

(**************************************************************************)
(*a map (A,x,y, B,z,t) -> (Ag_id, Ag_id) RuleIDMap to explain
  which rules can create a bond of type A.x.z.B (and at which position)*)
type pos = Fst | Snd

let collect_site_create_parallel_bonds_gen pos parameter error store_action_binding store_parallel_bonds =
  let pick pos a b =
    match pos
    with
    | Fst -> a
    | Snd -> b
  in
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun k set (error,map) ->
       let error, new_set =
         (*Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold*)
         Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
           (*A.x -> B.z; B.z -> A.x*)
           (fun ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state')) (error,store_result) ->
             let error, old_list =
               match
                 Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.find_option_without_logs
                   parameter
                   error
                   ((agent_id, agent_type, site_type, state),
                    (agent_id', agent_type', site_type', state'))
                   store_result
               with
               | error, None -> error, []
               | error, Some l -> error, l
             in
             let error, new_list =
               Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.fold_inv
                 (fun ((agent_type1, site_type1, site_type2, state1, state2),
                       (agent_type1', site_type1', site_type2', state1', state2'))
                   (error, current_list) ->
                   if
                     agent_type = agent_type1 &&
                     site_type = pick pos site_type1 site_type2 &&
                     agent_type' = agent_type1' &&
                     site_type' = pick pos site_type1' site_type2'
                   then
                     (*A.x.B.z, B.z.A.x*)
                     let new_list =
                       ((agent_id, agent_type1, site_type1, site_type2, state1, state2),
                        (agent_id', agent_type1', site_type1', site_type2', state1', state2'))
                       :: current_list
                     in
                     error, new_list
                   else
                     error, current_list
                 ) store_parallel_bonds (error, old_list)
             in
             let error, store_result =
               Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.add_or_overwrite
                 parameter
                 error
                 ((agent_id, agent_type, site_type, state),
                  (agent_id', agent_type', site_type', state'))
                 new_list
                 store_result
             in
             error, store_result
           )
           set
           (error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty)
       in
       Ckappa_sig.Rule_map_and_set.Map.add parameter error k new_set map)
    store_action_binding
    (error,Ckappa_sig.Rule_map_and_set.Map.empty)

let collect_fst_site_create_parallel_bonds  =
  collect_site_create_parallel_bonds_gen Fst
let collect_snd_site_create_parallel_bonds =
  collect_site_create_parallel_bonds_gen Snd

(**************************************************************************)
(*in the rhs*)
(*the fst map (A,x,y, B,z,t) -> A.x.z.B*)
(**************************************************************************)

let collect_fst_site_create_parallel_bonds_rhs parameter error store_action_binding store_parallel_bonds  =
  collect_fst_site_create_parallel_bonds
    parameter error
    store_action_binding
    store_parallel_bonds

(**************************************************************************)
(*the second map (A,x,y, B,z,t) -> A.y.t.B*)
(**************************************************************************)

let collect_snd_site_create_parallel_bonds_rhs parameter error store_action_binding
    store_parallel_bonds =
  let error, store_result =
    collect_snd_site_create_parallel_bonds
      parameter
      error
      store_action_binding
      store_parallel_bonds
  in
  error, store_result

(*******************************************************************)
(*A map from tuples -> sites
  ex: A(x!1, y!2), B(x!1, y!2) -> {(A,x); (A,y); (B,x); (B,y)}
*)
(*******************************************************************)

let collect_tuple_to_sites parameter error tuples_of_interest =
  let proj (a, b, _, _, _) = (a, b) in
  let proj2 (a, _, c, _, _) = (a, c) in
  Parallel_bonds_type.Partition_tuples_to_sites_map.monadic_partition_set
    (fun _ error (x, y) ->
       error, (proj x, proj2 x, proj y, proj2 y)
    )
    parameter
    error
    tuples_of_interest

(*******************************************************************)
(*A map from sites -> tuples
  ex: {(A,x); (A,y); (B,x); (B,y)} -> A(x!1, y!2), B(x!1, y!2)
*)
(*******************************************************************)

let compare_first_pair parameter error x tuple_set store_result =
  let (agent_type_x, site_type_x) = x in (*A,x*)
  Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.fold
    (fun (u, v) (error, store_result) ->
       let (agent_type, site_type, site_type', state, state') = u in
       let (agent_type1, site_type1, site_type1', state1, state1') = v in
       if agent_type_x = agent_type && site_type_x = site_type
       || agent_type_x = agent_type1  && site_type_x = site_type1
       then
         let error, store_result =
           Parallel_bonds_type.AgentSite_map_and_set.Map.add_or_overwrite
             parameter error
             x
             (u,v)
             store_result
         in
         error, store_result
       else error, store_result
    ) tuple_set (error, store_result)

let compare_snd_pair parameter error y tuple_pair store_result =
  let (agent_type_y, site_type_y) = y in (*A,x*)
  Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.fold
    (fun (u, v) (error, store_result) ->
       let (agent_type, site_type, site_type', state, state') = u in
       let (agent_type1, site_type1, site_type1', state1, state1') = v in
       if agent_type_y = agent_type && site_type_y = site_type'
       || agent_type_y = agent_type1 && site_type_y = site_type1'
       then
         let error, store_result =
           Parallel_bonds_type.AgentSite_map_and_set.Map.add_or_overwrite
             parameter error
             y
             (u,v)
             store_result
         in
         error, store_result
       else error, store_result
    ) tuple_pair (error, store_result)


(*map from sites to tuple *)

let collect_sites_to_tuple parameter error map_of_sites store_result =
  Parallel_bonds_type.PairAgentSite_map_and_set.Map.fold
    (fun (x, y, z, t) tuple_set (error, store_result) ->
       let store_result1, store_result2 = store_result in
       let error, store_result1 =
         compare_first_pair parameter error x tuple_set
           store_result1
       in
       let error, store_result2 =
         compare_snd_pair parameter error y tuple_set
           store_result2
       in
       error, (store_result1, store_result2)
    ) map_of_sites (error, store_result)
