(**
     * parallel_bonds.ml
     * openkappa
     * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
     *
     * Creation: 2016, the 31th of March
     * Last modification: Time-stamp: <Aug 24 2016>
     *
     * Abstract domain to detect whether when two sites of an agent are bound,
     * they must be bound to the same agent.
     *
     * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
     * en Informatique et en Automatique.
     * All rights reserved.  This file is distributed
     * under the terms of the GNU Library General Public License *)

let local_trace = false

(******************************************************************)
(*parallel bonds in the initial states*)
(******************************************************************)

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
                        (Usual_domains.Val bool)
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

let collect_parallel_or_not_bonds_init
    parameter kappa_handler error tuple_of_interest init_state store_result =
  let tuple_of_interest = Some tuple_of_interest in
  let error, big_store =
    collect_parallel_or_not_bonds_in_pattern
      parameter error ?tuple_of_interest init_state.Cckappa_sig.e_init_c_mixture
  in
  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.fold
    (fun tuple value (error, store_result) ->
       Parallel_bonds_type.add_value
         parameter error kappa_handler
         (Parallel_bonds_type.project2 tuple)
         value
         store_result)
    big_store (error, store_result)
