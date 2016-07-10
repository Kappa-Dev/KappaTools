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
(*parallel bonds in the initial state*)
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
      | error, None -> warn parameter error (Some "line 632") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, agent_target =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_index_target views
      with
      | error, None -> warn parameter error (Some "line 640") Exit Cckappa_sig.Ghost
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

(*collect a set of binding sites in the initial states*)

let collect_bonds_initial parameter error init_state =
  (*let parameter = get_parameter static in*)
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
                    parameter error site_add agent_id site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let pair = ((agent_id, agent_type_source, site_type_source, state_source),
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
(*a set of parallel bonds in the initial states*)

let collect_parallel_bonds_init parameter store_bonds_init error init_state =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id_source bonds_map store_result ->
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type_source site_add (error, store_result) ->
                let error, pair =
                  collect_pair_of_bonds
                    parameter error site_add agent_id_source site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let ((agent_type_source, site_type_source, state_source),
                     (agent_type_target, site_type_target, state_target)) = pair in
                (*fold over a set of binding sites in the inititial states*)
                let error, store_result =
                  Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
                    (fun ((agent_id, agent_type, site_type, state),
                          (agent_id', agent_type', site_type', state')) (error, store_result) ->
                      if agent_id = agent_id_source &&
                         agent_id' = site_add.Cckappa_sig.agent_index &&
                         site_type_source <> site_type &&
                         site_type_target <> site_type'
                      then
                        let error', store_result =
                          Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                            parameter error
                            ((agent_id, agent_type_source, site_type_source, site_type, state_source, state),
                             (site_add.Cckappa_sig.agent_index, agent_type_target,
                              site_type_target, site_type', state_target, state'))
                            store_result
                        in
                        let error = Exception.check warn parameter error error' (Some "line 872") Exit in
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
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
  in
  error, store_result

(**************************************************************************)
(*non parallel bonds in the initial state*)
(**************************************************************************)

let collect_non_parallel_init_aux parameter store_bonds_init error init_state =
  (*let parameter = get_parameter static in
    let error, store_bonds_init = collect_bonds_initial static error init_state in*)
  let error, store_result =
    Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
      (fun ((agent_id, agent_type, site_type, state),
            (agent_id', agent_type', site_type', state')) (error, store_result) ->
        (*get old*)
        let error, old_list =
          match Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
                  parameter error agent_type' store_result
          with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*get a map of agent_type with a site pair list: (A.x, B.x)*)
        let site_pair_list =
          (*id:0:x:1, id:1:y:1*)
          ((agent_id, site_type, state), (agent_id', site_type', state')) :: old_list
        in
        let error, store_result =
          Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
            parameter error agent_type' site_pair_list store_result
        in
        error, store_result
      ) store_bonds_init (error, Ckappa_sig.Agent_map_and_set.Map.empty)
  in
  error, store_result

(**************************************************************************)
(*non parallel bonds in initial state*)

let collect_non_parallel_init parameter store_bonds_init store_site_pair_list error init_state =
  (*let parameter = get_parameter static in
    let error, store_bonds_init = collect_bonds_initial static error init_state in
    let error, store_site_pair_list = collect_non_parallel_init_aux static dynamic error init_state in*)
  let error, store_result =
    Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
      (*A.x, B.z*)
      (fun ((agent_id, agent_type, site_type, state),
            (agent_id', agent_type', site_type', state')) (error, store_result) ->
        let error, store_result =
          Ckappa_sig.Agent_map_and_set.Map.fold
            (fun agent_type1' pair_list (error, store_result) ->
               let error, store_result =
                 List.fold_left (fun (error, store_result)
                                  ((agent_id1, site_type1, state1), (agent_id1', site_type1', state1')) ->
                                  (*B = B, and their id are different*)
                                  if agent_id' <> agent_id1' &&  agent_type' = agent_type1'
                                  then
                                    (*two elements in the list*)
                                    if site_type <> site_type1 (*A.x <> A.y*)
                                    then
                                      (*non parallel set*)
                                      let error, old_list =
                                        match Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
                                                parameter error agent_type' store_result
                                        with
                                        | error, None -> error, []                                            | error, Some l -> error, l                                          in
                                      (*A.x.y, B.z.t*)
                                      let new_list =
                                        ((agent_id, agent_type, site_type, state), (*A.x*)                                             (agent_id1, agent_type, site_type1, state1), (*A.y*)                                              (agent_id', agent_type', site_type', state'), (*B.z*)                                             (agent_id1', agent_type', site_type1', state1')) :: old_list                                          in                                          Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite                                            parameter error                                            agent_type'
                                        new_list
                                        store_result
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
      ) store_bonds_init (error, Ckappa_sig.Agent_map_and_set.Map.empty)
  in
  error, store_result

(**************************************************************************)
(*collect result of parallel bonds in the intitial state*)

let collect_value_parallel_bonds parameter store_parallel_bonds_init error kappa_handler init_state store_result =
  (*let parameter = get_parameter static in
    let error, store_parallel_bonds_init = collect_parallel_bonds_init static dynamic error init_state in*)
  (*--------------------------------------------------------------------*)
  (*parallel bonds in the initial state:
    (agent_type, site_type, site_type, state, state -> agent_type, site_type, site_type, state, state)
  *)
  let error, parallel_list =
    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold
      (fun ((agent_id, agent_type, site_type, site_type1, state, state1),
            (agent_id', agent_type', site_type', site_type1', state', state1')) (error, current_list) ->
        error, ((agent_type, site_type, site_type1, state, state1),
                (agent_type', site_type', site_type1', state', state1')) :: current_list
      ) store_parallel_bonds_init (error, [])
  in
  (*--------------------------------------------------------------------*)
  (*let store_result = get_value_parallel_bonds_init dynamic in*)
  let error, value_parallel_bonds =
    List.fold_left (fun (error, store_result) x ->
        let error, store_result =
          Parallel_bonds_type.add_value
            parameter error kappa_handler
            x
            (Usual_domains.Val true)
            store_result
        in
        error, store_result
      ) (error, store_result) parallel_list
  in
  error, value_parallel_bonds
(*let dynamic = set_value_parallel_bonds_init value_parallel_bonds dynamic in
  error, dynamic*)

(**************************************************************************)
(*collect result of non parallel bonds in the initital state*)

let collect_value_non_parallel_bonds parameter store_non_parallel_init error kappa_handler init_state store_result =
  (*let parameter = get_parameter static in
    (*non parallel bonds*)
    let error, store_non_parallel_init = collect_non_parallel_init static dynamic error init_state in*)
  (*--------------------------------------------------------------------*)
  (*a map contents a list of non parallel bonds:
    (agent_type, site_type, site_type, state, state) -> (agent_type, site_type, site_type, state, state)
  *)
  let store_non_parallel_init_list =
    Ckappa_sig.Agent_map_and_set.Map.map
      (fun list ->
         let new_pair_list =
           List.fold_left (fun current_list
                            ((agent_id, agent_type, site_type, state),
                             (agent_id1, agent_type1, site_type1, state1),
                             (agent_id', agent_type'', site_type', state'),
                             (agent_id1', agent_type1', site_type1', state1')) ->
                            ((agent_type, site_type, site_type1, state, state1),
                             (agent_type'', site_type', site_type1', state', state1')) :: current_list
                          ) [] list
         in
         new_pair_list
      ) store_non_parallel_init
  in
  (*--------------------------------------------------------------------*)
  (*let store_result = get_value_non_parallel_bonds_init dynamic in*)
  (*value of a set of non parallel bonds is Val false*)
  let error, value_non_parallel_bonds =
    Ckappa_sig.Agent_map_and_set.Map.fold
      (fun agent_type list (error, store_result) ->
         let error, store_result =
           List.fold_left (fun (error, store_result) x ->
               Parallel_bonds_type.add_value
                 parameter error kappa_handler
                 x
                 (Usual_domains.Val false)
                 store_result
             ) (error, store_result) list
         in
         error, store_result
      ) store_non_parallel_init_list (error, store_result)
  in
  error, value_non_parallel_bonds
(*let dynamic = set_value_non_parallel_bonds_init value_non_parallel_bonds dynamic in
  error, dynamic*)

(**************************************************************************)
(*return a value of initial state, if it has parallel bonds -> yes,
  if non parallel bonds -> no, if it is both -> any, if there is non:
  undefined*)
    (*
let collect_value_of_init parameter value_parallel_bonds value_non_parallel_bonds error init_state store_result =
  (*let parameter = get_parameter static in
    let value_parallel_bonds = get_value_parallel_bonds_init dynamic in
    let value_non_parallel_bonds = get_value_non_parallel_bonds_init dynamic in*)
  (*------------------------------------------------------------------------------*)
  (*do the lub in the initial state*)
  (*let store_result = get_value_of_init dynamic in*)
  (*------------------------------------------------------------------------------*)
  let error, store_result =
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold2 parameter error
      (fun parameter error x p_value store_result ->
         Parallel_bonds_type.add_value parameter error x p_value store_result)
      (fun parameter error x nonp_value store_result ->
         Parallel_bonds_type.add_value parameter error x nonp_value store_result)
      (fun parameter error x p_value nonp_value store_result ->
         let new_value = Usual_domains.lub p_value nonp_value in
         Parallel_bonds_type.add_value parameter error x new_value store_result
      ) value_parallel_bonds value_non_parallel_bonds store_result
  in
  error, store_result
(*let dynamic = set_value_of_init store_result dynamic in
  error, dynamic*)
*)
