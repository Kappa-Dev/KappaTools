(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to detect whether when two sites of an agent are bound,
   * they must be bound to the same agent.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Parallel bonds static") message exn
    (fun () -> default)

let local_trace = false

(**************************************************************************)
(*Right hand side bonds:
  (agent_id, site_type, state, -> agent_id, site_type, state)*)
(**************************************************************************)

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

(************************************************************************************)
(*action binding in the rhs*)

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
        | error, None -> warn parameter error (Some "line 267") Exit Cckappa_sig.Ghost
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
      (*------------------------------------------------------------------------------*)
      (*second pair*)
      let error, agent_target =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id2 rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None -> warn parameter error (Some "line 275") Exit Cckappa_sig.Ghost
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
      let error = Exception.check warn parameter error error' (Some "line 358") Exit in
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

(************************************************************************************)

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
              | error, None -> warn parameter error (Some "line 269") Exit Cckappa_sig.Ghost
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
            (*------------------------------------------------------------------------------*)
            (*the second pair*)
            let error, agent_target =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id_target views
              with
              | error, None -> warn parameter error (Some "line 287") Exit Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            let error, (agent_type2, state2) =
              collect_agent_type_state
                parameter
                error
                agent_target
                site_type_target
            in
            (*------------------------------------------------------------------------------*)
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
            let error = Exception.check warn parameter error error' (Some "line 312") Exit in
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

(**************************************************************************)

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

(**************************************************************************)
(**Parallel bonds*)
(**************************************************************************)

let collect_rule_has_parallel_bonds parameter error rule_id views bonds store_bonds_full store_result =
  (*let parameter = get_parameter static in*)
  (*--------------------------------------------*)
  let error, bonds_full_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_bonds_full
    with
    | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
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
              (*------------------------------------------------------------------------------*)
              (*the first pair*)
              let error, agent_source =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None -> warn parameter error (Some "line 335") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*------------------------------------------------------------------------------*)
              (*the second pair*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None -> warn parameter error (Some "line 352") Exit Cckappa_sig.Ghost
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
                (fun ((agent_id, agent_type, site_type, state),
                      (agent_id', agent_type', site_type', state')) (error, store_result) ->
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
                      | error, None -> error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
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
                    let error = Exception.check warn parameter error error' (Some "line 393") Exit in
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

(**************************************************************************)
(*collect a set of parallel bonds in the rhs correspond with its rule*)

let collect_rule_has_parallel_bonds_rhs parameter store_bonds_rhs_full error rule_id rule store_result =
  (*let store_bonds_rhs_full = get_bonds_rhs_full static in
  let store_result = get_rule_has_parallel_bonds_rhs static in*)
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
(*let static = set_rule_has_parallel_bonds_rhs store_result static in
error, static*)

(**************************************************************************)
(*non parallel bonds*)
(**************************************************************************)

let collect_rule_has_non_parallel_bonds parameter error rule_id views bonds
    store_bonds_full store_result =
  (*let parameter = get_parameter static in*)
  (*--------------------------------------------*)
  let error, bonds_full_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_bonds_full
    with
    | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
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
              (*------------------------------------------------------------------------------*)
              (*the first pair: A.x*)
              let error, agent_source =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None -> warn parameter error (Some "line 335") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*------------------------------------------------------------------------------*)
              (*the second pair:B.z*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None -> warn parameter error (Some "line 677") Exit Cckappa_sig.Ghost
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
                  if agent_id' <> agent_id_target && agent_type' = agent_type_target
                  then
                    (*two elements in the list of A: A.x.y*)
                    if site_type <> site_type_source
                    then
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
                  else
                    error, store_result
                ) bonds_full_set (error, store_result)
           ) bonds_map (error, store_result)
      ) bonds store_result
  in
  error, store_result

(**************************************************************************)

let collect_rule_has_non_parallel_bonds_rhs parameter error rule_id rule store_bonds_rhs_full store_result =
  (*let store_bonds_rhs_full = get_bonds_rhs_full static in
  let store_result = get_rule_has_non_parallel_bonds_rhs static in*)
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
(*let static = set_rule_has_non_parallel_bonds_rhs store_result static in
error, static*)


(**************************************************************************)
(*collect a set of parallel bonds in the rhs*)

let collect_parallel_bonds_rhs parameter store_rule_has_parallel_bonds error rule_id store_result =
  (*let parameter = get_parameter static in
  let store_rule_has_parallel_bonds = get_rule_has_parallel_bonds_rhs static in*)
  let error, parallel_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_rule_has_parallel_bonds
    with
    | error, None -> error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*let store_result = get_parallel_bonds_rhs static in*)
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
  (*let static = set_parallel_bonds_rhs store_result static in
  error, static*)

(**************************************************************************)
(*a map (A,x,y, B,z,t) -> (Ag_id, Ag_id) RuleIDMap to explain
  which rules can create a bond of type A.x.z.B (and at which position)*)

let collect_fst_site_create_parallel_bonds parameter error rule_id store_action_binding
    store_parallel_bonds store_result =
  (*let parameter = get_parameter static in*)
  Ckappa_sig.Rule_map_and_set.Map.map
    (fun set ->
       Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
         (*A.x -> B.z; B.z -> A.x*)
         (fun ((agent_id, agent_type, site_type, state),
               (agent_id', agent_type', site_type', state')) store_result ->
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
             Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
               (fun ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                     (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                 (error, current_list) ->
                 if
                   agent_id = agent_id1 &&
                   site_type = site_type1 &&
                   agent_id' = agent_id1' &&
                   site_type' = site_type1'
                 then
                   (*A.x.B.z, B.z.A.x*)
                   let new_list =
                     ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                      (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) :: current_list
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
           store_result
         ) set Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
    ) store_action_binding

(**************************************************************************)
(*in the rhs*)

let collect_fst_site_create_parallel_bonds_rhs parameter error rule_id store_action_binding store_parallel_bonds store_result =
  (*let store_action_binding = get_action_binding static in
  let store_parallel_bonds = get_parallel_bonds_rhs static in
  let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in*)
  let store_result =
    collect_fst_site_create_parallel_bonds
      parameter error rule_id
      store_action_binding
      store_parallel_bonds
      store_result
  in
  error, store_result
  (*let static = set_fst_site_create_parallel_bonds_rhs store_result static in
  error, static*)

(**************************************************************************)
(*the second map (A,x,y, B,z,t) -> A.y.t.B*)

let collect_snd_site_create_parallel_bonds parameter error rule_id store_action_binding
    store_parallel_bonds store_result =
  (*let parameter = get_parameter static in*)
  Ckappa_sig.Rule_map_and_set.Map.map
    (fun set ->
       Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
         (*A.y -> B.t; B.t -> A.y*)
         (fun ((agent_id, agent_type, site_type, state),
               (agent_id', agent_type', site_type', state')) store_result ->
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
             Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
               (fun ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                     (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                 (error, current_list) ->
                 (*check site_type2, and site_type2': A.y -> B.t*)
                 if
                   agent_id = agent_id1 &&
                   site_type = site_type2 &&
                   agent_id' = agent_id1' &&
                   site_type' = site_type2'
                 then
                   let new_list =
                     (*A.x.y.B.z.t, B.z.t.A.x.y*)
                     ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                      (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) :: current_list
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
           store_result
         ) set Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
    ) store_action_binding

(**************************************************************************)

let collect_snd_site_create_parallel_bonds_rhs parameter error rule_id store_action_binding  store_parallel_bonds store_result =
  (*let store_parallel_bonds = get_parallel_bonds_rhs static in
  let store_action_binding = get_action_binding static in
  let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in*)
  let store_result =
    collect_snd_site_create_parallel_bonds
      parameter
      error
      rule_id
      store_action_binding
      store_parallel_bonds
      store_result
  in
  error, store_result
  (*let static = set_snd_site_create_parallel_bonds_rhs store_result static in
  error, static*)
