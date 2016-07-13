(**
     * parallel_bonds.ml
     * openkappa
     * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
     *
     * Creation: 2016, the 31th of March
     * Last modification: Time-stamp: <Jul 02 2016>
     *
     * Abstract domain to detect whether when two sites of an agent are bound,
     * they must be bound to the same agent.
     *
     * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
     * en Informatique et en Automatique.
     * All rights reserved.  This file is distributed
     * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false

(******************************************************************)
(*parallel bonds in the initial states*)
(******************************************************************)

let collect_pair_of_bonds parameter error site_add agent_id site_type_source views =
  let error, pair =
    let agent_index_target = site_add.Cckappa_sig.agent_index in
    let site_type_target = site_add.Cckappa_sig.site in
    let error, agent_source =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_id views
      with
      | error, None -> warn parameter error (Some "line 36") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, agent_target =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_index_target views
      with
      | error, None -> warn parameter error (Some "line 43") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, (agent_type1, state1) =
      Parallel_bonds_static.collect_agent_type_state
        parameter
        error
        agent_source
        site_type_source
    in
    let error, (agent_type2, state2) =
      Parallel_bonds_static.collect_agent_type_state
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

(******************************************************************)
(*collect a set of binding sites in the initial states*)
  
let collect_bonds_initial parameter error init_state =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id bonds_map store_result ->
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type_source site_add (error, store_result) ->
                let error, ((agent_type_source, site_type_source, state_source),
                            (agent_type_target, site_type_target, state_target)) =
                  collect_pair_of_bonds
                    parameter error
                    site_add
                    agent_id
                    site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let pair = 
                  ((agent_id, agent_type_source, site_type_source, state_source),
                   (site_add.Cckappa_sig.agent_index, agent_type_target, site_type_target,
                    state_target))
                in
                let error, store_result =
                  Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
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
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
  in
  error, store_result

(**************************************************************************)
(*a set of potential parallel bonds in the initial states*)
(**************************************************************************)

let collect_parallel_bonds_init parameter kappa_handler store_bonds_init error init_state =
  let views = init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views in
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id_source bonds_map store_result ->
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type_source site_add (error, store_result) ->
                let agent_id_target = site_add.Cckappa_sig.agent_index in
                let error, pair =
                  collect_pair_of_bonds
                    parameter error 
                    site_add 
                    agent_id_source 
                    site_type_source
                    views
                in
                let ((agent_type_source, site_type_source, state_source),
                     (agent_type_target, site_type_target, state_target)) = pair in
                (*fold over a set of binding sites in the inititial states*)
                let error, store_result =
                  Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
                    (fun ((agent_id, agent_type, site_type, state),
                          (agent_id', agent_type', site_type', state')) (error, store_result) ->
                      if agent_id = agent_id_source &&
                         agent_id' = agent_id_target &&
                         site_type_source <> site_type &&
                         site_type_target <> site_type'
                      then
                        let x = (agent_id, agent_type_source, site_type_source,
                                 site_type, state_source, state) in
                        let y = (agent_id_target, agent_type_target,
                                 site_type_target, site_type', state_target, state') in
                        let pair = Parallel_bonds_type.project2 (x, y) in
                        let error, store_result =
                          Parallel_bonds_type.add_value
                            parameter error 
                            kappa_handler
                            pair
                            (Usual_domains.Val true)
                            store_result
                        in
                        error, store_result
                      else
                        error, store_result
                    ) store_bonds_init (error, store_result)
                in
                error, store_result
             ) bonds_map (error, store_result)
         in
         error, store_result
      ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty
  in
  error, store_result

(**************************************************************************)
(*non parallel bonds in the initial state*)
(**************************************************************************)

let collect_site_pair_list parameter store_bonds_init error init_state =
  let error, store_result =
    Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
      (fun ((agent_id, _, site_type, state),
            (agent_id', agent_type', site_type', state')) (error, store_result) ->
        (*get old*)
        let error, old_list =
          match 
            Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
              parameter error 
              agent_type'
              store_result
          with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*get a map of agent_type with a site pair list: (A.x, B.x)*)
        let site_pair_list =
          ((agent_id, site_type, state), (agent_id', site_type', state')) :: old_list
        in
        let error, store_result =
          Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
            parameter error 
            agent_type' (*B*)
            site_pair_list 
            store_result
        in
        error, store_result
      ) store_bonds_init (error, Ckappa_sig.Agent_map_and_set.Map.empty)
  in
  error, store_result

(**************************************************************************)
(*non parallel bonds in initial states*)

let collect_non_parallel_init parameter kappa_handler store_bonds_init
    store_site_pair_list error init_state =
  let error, store_result =
    Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
      (*A.x, B.z*)
      (fun (x, y) (error, store_result) ->
         let (agent_id, agent_type, site_type, state) (*A*) = x in
         let (agent_id', agent_type', site_type', state') = y in
         let error, store_result =
           Ckappa_sig.Agent_map_and_set.Map.fold
             (fun agent_type1' pair_list (error, store_result) ->
                let error, store_result =
                  List.fold_left
                    (fun (error, store_result) (z, t) ->
                       let (agent_id1, site_type1, state1) (*A*) = z in
                       let (agent_id1', site_type1', state1') = t in
                       (*B = B, and its id are different*)
                       if agent_id' <> agent_id1' &&  
                          agent_type' = agent_type1'
                       then
                         (*two elements in the list*)
                         if site_type <> site_type1 (*A.x <> A.y*)
                         then
                           let x = (agent_id, agent_type, site_type, site_type1, state, state1) in
                           let y = (agent_id', agent_type', site_type', site_type1', state', state1') in
                           let pair = Parallel_bonds_type.project2 (x, y) in
                           let error, store_result =
                             Parallel_bonds_type.add_value
                               parameter error
                               kappa_handler
                               pair
                               (Usual_domains.Val false)
                               store_result
                           in
                           error, store_result
                         else
                           error, store_result
                       else
                       error, store_result
                   ) (error, store_result) pair_list
               in
               error, store_result
            ) store_site_pair_list (error, store_result)
        in
        error, store_result
      ) store_bonds_init (error, Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty)
  in
  error, store_result
