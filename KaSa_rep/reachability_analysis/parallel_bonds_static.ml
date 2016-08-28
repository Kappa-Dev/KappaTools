(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Aug 28 2016>
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
    store_views_rhs :
      Parallel_bonds_type.AgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_action_binding :
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_bonds_rhs_full :
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_rule_has_parallel_bonds_rhs:
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    (*a map check a bond between A.x and B.z*)
    store_fst_site_create_parallel_bonds_rhs:
      ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
        Ckappa_sig.c_state * Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
        Ckappa_sig.c_state * Ckappa_sig.c_state)
      ) list
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t;
    (*rule has non parallel bonds in the rhs*)
    store_rule_has_non_parallel_bonds_rhs:
      ((Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.key *
        Ckappa_sig.c_agent_name * Ckappa_sig.Site_map_and_set.Map.elt *
        Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.Site_map_and_set.Map.elt * Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
        Ckappa_sig.c_site_name * Ckappa_sig.c_state))
        list
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*a map check a bond between A.y and B.t*)
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
    (*lhs*)
    store_bonds_lhs_full :
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*rule has parallel bonds on the lhs*)
    store_rule_has_parallel_or_not_bonds_lhs :
      (bool Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t)
        Ckappa_sig.Rule_map_and_set.Map.t ;
    (*rule has non parallel bonds on the lhs*)
    (*  store_rule_has_non_parallel_bonds_lhs:
      (
        ((Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.key *
          Ckappa_sig.c_agent_name * Ckappa_sig.Site_map_and_set.Map.elt *
          Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.Site_map_and_set.Map.elt * Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.c_site_name * Ckappa_sig.c_state))
          list)
        Ckappa_sig.Rule_map_and_set.Map.t;*)
    store_parallel_bonds_rhs:
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t;
    store_non_parallel_bonds_rhs:
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t;
    (*  store_parallel_or_not_bonds_lhs:
        bool Usual_domains.flat_lattice  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t;*)
    (*   store_non_parallel_bonds_lhs:
          Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t;*)
    store_tuples_of_interest: Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.t;

  }

(*******************************************************************)

let init_local_static =
  {
    store_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_action_binding = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_bonds_rhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_parallel_bonds_rhs = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty;
    (*    store_parallel_or_not_bonds_lhs = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty;*)
    store_non_parallel_bonds_rhs = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty;
    (*    store_non_parallel_bonds_lhs = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty;*)
    store_tuples_of_interest = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Set.empty;
    store_rule_has_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_has_non_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_fst_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_snd_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_bonds_lhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_rule_has_parallel_or_not_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    (*store_rule_has_non_parallel_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;*)
  }


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


let collect_parallel_or_not_bonds_in_pattern
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
                      ((agent_id_source,agent_type_source, site_type_source, site_type_source', state_source, state_source'),
                       (agent_id_target,agent_type_target,
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


(**************************
 *****************************************)
(*Right hand side bonds:
  (agent_id, site_type, state, -> agent_id, site_type, state)*)
(*******************************************************************)



(************************************************************************)
(*action binding in the rhs*)
(************************************************************************)

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


(************************************************************************)
(* Binding information *)
(************************************************************************)

let collect_bonds_full parameter error rule_id views bonds store_result =
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
                  parameter error agent_id views
              with
              | error, None ->
                Exception.warn
                  parameter error __POS__ Exit Cckappa_sig.Ghost
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
                  parameter error agent_id_target views
              with
              | error, None ->
                Exception.warn
                  parameter error __POS__ Exit Cckappa_sig.Ghost
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
              match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
                      rule_id store_result
              with
              | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
              | error, Some p -> error, p
            in
            let error', set =
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                parameter error
                ((agent_id, agent_type1, site_type_source, state1),
                 (agent_id_target, agent_type2, site_type_target, state2))
                old_set
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
         ) bonds_map (error, store_result)
    ) bonds store_result

(**************************************************************)

let collect_bonds_rhs_full parameter error rule_id rule store_result =
  collect_bonds_full
    parameter
    error
    rule_id
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
    store_result

let collect_bonds_lhs_full parameter error rule_id rule store_result =
  collect_bonds_full
    parameter
    error
    rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
    store_result

(****************************************************************)
(** Potential parallel bindings *)
(****************************************************************)

let collect_rule_has_parallel_bonds parameter error rule_id
    views bonds store_bonds_full store_result =
  (*--------------------------------------------*)
  let error, bonds_full_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_bonds_full
    with
    | error, None ->
      error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*--------------------------------------------*)
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id_source bonds_map store_result ->
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site_type_source site_add (error, store_result) ->
              let agent_id_target = site_add.Cckappa_sig.agent_index in
              let site_type_target = site_add.Cckappa_sig.site in
              (*--------------------------------------------------*)
              (*the first pair*)
              let error, agent_source =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None ->
                  Exception.warn
                    parameter error __POS__ Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*--------------------------------------------------*)
              (*the second pair*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None ->
                  Exception.warn
                    parameter error __POS__ Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_target, state_target) =
                collect_agent_type_state
                  parameter
                  error
                  agent_target
                  site_type_target
              in
              (*------------------------------------------------------------------------------*)
              (*parallel bonds*)
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
                (fun ((agent_id, _, site_type, state),
                      (agent_id', _, site_type', state')) (error, store_result) ->
                  if agent_id = agent_id_source &&
                     agent_id' = agent_id_target &&
                     site_type <> site_type_source &&
                     site_type' <> site_type_target
                  then
                    let error, old_parallel_set =
                      match
                        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                          parameter
                          error
                          rule_id
                          store_result
                      with
                      | error, None ->
                        error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
                      | error, Some s -> error, s
                    in
                    let error', set =
                      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                        parameter
                        error
                        ((agent_id_source, agent_type_source,
                          site_type_source, site_type, state_source, state),
                         (agent_id_target, agent_type_target,
                          site_type_target, site_type', state_target,state'))
                        old_parallel_set
                    in
                    let error =
                      Exception.check_point
                        Exception.warn parameter error error'
                        __POS__ Exit
                    in
                    let error, store_result =
                      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
                        rule_id
                        set
                        store_result
                    in
                    error, store_result
                  else
                    error, store_result
                ) bonds_full_set (error, store_result)
           ) bonds_map (error, store_result)
      ) bonds store_result
  in
  error, store_result

(******************************************************************)
(*A set of rule has a potential double bindings on the rhs*)

let collect_rule_has_parallel_bonds_rhs parameter store_bonds_rhs_full
    error rule_id rule store_result =
  let error, store_result =
    collect_rule_has_parallel_bonds
      parameter
      error
      rule_id
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
      store_bonds_rhs_full
      store_result
  in
  error, store_result

let collect_rule_has_parallel_or_not_bonds_lhs
    parameter
    error rule_id rule store_result  =
  let error, map =
    collect_parallel_or_not_bonds_in_pattern
    parameter error rule.Cckappa_sig.rule_lhs
  in
  Ckappa_sig.Rule_map_and_set.Map.add
    parameter error
    rule_id
    map
    store_result


(**************************************************************************)
(*non parallel bonds*)
(**************************************************************************)

let collect_rule_has_non_parallel_bonds parameter error rule_id views bonds
    store_bonds_full store_result =
  (*--------------------------------------------*)
  let error, bonds_full_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_bonds_full
    with
    | error, None ->
      error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*--------------------------------------------*)
  (*fold over bonds in the rhs*)
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id_source bonds_map store_result ->
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site_type_source site_add (error, store_result) ->
              let agent_id_target = site_add.Cckappa_sig.agent_index in
              let site_type_target = site_add.Cckappa_sig.site in
              (*----------------------------------------------------------------------*)
              (*the first pair: A.x*)
              let error, agent_source =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None ->
                  Exception.warn
                    parameter error __POS__ Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*----------------------------------------------------------------------*)
              (*the second pair:B.z*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None ->
                  Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_target, state_target) =
                collect_agent_type_state
                  parameter
                  error
                  agent_target
                  site_type_target
              in
              (*------------------------------------------------------------------------------*)
              (*non parallel bonds*)
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
                (fun ((agent_id, agent_type, site_type, state) (*A.y*),
                      (agent_id', agent_type', site_type', state') (*B.t*)) (error, store_result) ->
                  (*if B = B and theirs id are different*)
                  if site_type <> site_type_source &&
                     site_type' <> site_type_target &&
                     agent_id = agent_id_source &&
                     agent_id' <> agent_id_target &&
                     agent_type' = agent_type_target
                  then
                    (*two elements in the list of A: A.x.y*)
                    (*non parallel bonds*)
                    let error, old_list =
                      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                              parameter error rule_id store_result
                      with
                      | error, None -> error, []
                      | error, Some l -> error, l
                    in
                    (*A.x.y, B.z.t*)
                    let new_list =
                      ((agent_id_source, agent_type_source, site_type_source, state_source) (*A.x*),
                       (agent_id, agent_type, site_type, state) (*A.y*),
                       (agent_id_target, agent_type_target, site_type_target, state_target) (*B.z*),
                       (agent_id', agent_type', site_type', state')) :: old_list
                    in
                    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                      parameter error
                      rule_id
                      new_list
                      store_result
                  else
                    error, store_result
                ) bonds_full_set (error, store_result)
           ) bonds_map (error, store_result)
      ) bonds store_result
  in
  error, store_result

(**************************************************************************)

let collect_rule_has_non_parallel_bonds_rhs parameter error rule_id rule
    store_bonds_rhs_full store_result =
  let error, store_result =
    collect_rule_has_non_parallel_bonds
      parameter
      error
      rule_id
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
      store_bonds_rhs_full
      store_result
  in
  error, store_result

(*let collect_rule_has_non_parallel_bonds_lhs parameter error rule_id rule
    store_bonds_lhs_full store_result =
  let error, store_result1 =
    collect_rule_has_non_parallel_bonds
      parameter
      error
      rule_id
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
      store_bonds_lhs_full
      Ckappa_sig.Rule_map_and_set.Map.empty
  in
  let store_result =
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
         (false, set)
      ) store_result1
  in
  error, store_result*)

(**************************************************************************)
(*A set of potential parallel bonds on the rhs*)


let collect_parallel_bonds_rhs parameter store_rule_has_parallel_bonds error rule_id store_result =
  let error, parallel_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_rule_has_parallel_bonds
    with
    | error, None ->
      error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, store_result =
    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold
      (fun (x, y) (error, store_result) ->
         let error, set =
           Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
             parameter
             error
             (x, y)
             store_result
         in
         error, set
      ) parallel_set (error, store_result)
  in
  error, store_result

let collect_non_parallel_bonds_rhs parameter store_rule_has_non_parallel_bonds error rule_id store_result =
  let error, parallel_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_rule_has_non_parallel_bonds
    with
    | error, None ->
      error,
      []
    | error, Some s -> error, s
  in
  let error, store_result =
    List.fold_left
      (fun (error, set)
        ((id,agent,site,state),
         (_id',_agent',site',state'),
         (id'',agent'',site'',state''),
         (_id''',_agent''',site''',state'''))           ->
        let error, set =
          Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
            parameter
            error
            ((id,agent,site,site',state,state'),
             (id'',agent'',site'',site''',state'',state'''))
            set
        in
        let error, set =
          Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
            parameter
            error
            (
              (id'',agent'',site'',site''',state'',state'''),
              (id,agent,site,site',state,state'))
            set
        in
        error, set
      )  (error, store_result) parallel_set
  in
  error, store_result
(**************************************************************************)
(*views on the rhs*)
(**************************************************************************)

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
  error, store_result

(**************************************************************************)
(*a map (A,x,y, B,z,t) -> (Ag_id, Ag_id) RuleIDMap to explain
  which rules can create a bond of type A.x.z.B (and at which position)*)

(*FIXME*)

let collect_fst_site_create_parallel_bonds parameter error store_action_binding store_parallel_bonds =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun k set (error,map) ->
       let error, new_set =
         Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
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
                     site_type = site_type1 &&
                     agent_type' = agent_type1' &&
                     site_type' = site_type1'
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

(**************************************************************************)
(*in the rhs*)

let collect_fst_site_create_parallel_bonds_rhs parameter error store_action_binding store_parallel_bonds  =
  collect_fst_site_create_parallel_bonds
    parameter error
    store_action_binding
    store_parallel_bonds

(**************************************************************************)
(*the second map (A,x,y, B,z,t) -> A.y.t.B*)

(*FIXME*)

let collect_snd_site_create_parallel_bonds parameter error store_action_binding store_parallel_bonds =
  Ckappa_sig.Rule_map_and_set.Map.fold
    (fun k set (error,store_result) ->
       let error, new_set =
         Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
           (*A.y -> B.t; B.t -> A.y*)
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
                   (*check site_type2, and site_type2': A.y -> B.t*)
                   if
                     agent_type = agent_type1 &&
                     site_type = site_type2 &&
                     agent_type' = agent_type1' &&
                     site_type' = site_type2'
                   then
                     let new_list =
                       (*A.x.y.B.z.t, B.z.t.A.x.y*)
                       ((agent_id, agent_type1, site_type1, site_type2, state1, state2),
                        (agent_id', agent_type1', site_type1', site_type2', state1', state2')) :: current_list
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
           (error,Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty)
       in
       Ckappa_sig.Rule_map_and_set.Map.add parameter error k new_set store_result
    ) store_action_binding (error, Ckappa_sig.Rule_map_and_set.Map.empty)

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
