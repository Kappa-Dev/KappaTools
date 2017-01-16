(**
   * bdu_static_modification_action
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 18th of Feburary
   * Last modification: Time-stamp: <Jan 16 2017>
   *
   *
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

(*type modification_static =
  {
    (*common_static*)
    store_modified_map :
      Ckappa_sig.AgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_project_modified_map : (*use in parallel domain*)
    Ckappa_sig.AgentSite_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;

  }

let init_modification_static =
  {
    store_modified_map = init_modified_map;
    store_project_modified_map = init_project_modified_map;

  }*)

(*********************************************************************)

let collect_sites_from_agent_interface parameters error agent_id agent
    store_result =
  let agent_type = agent.Cckappa_sig.agent_name in
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun site_type port (error, store_result) ->
       let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
       let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
       (*NOTE: state in modification is a singleton state*)
       let error, state =
         if state_min = state_max
         then error, state_min
         else Exception.warn parameters error __POS__ Exit
             Ckappa_sig.dummy_state_index
       in
       let error, store_result =
         Ckappa_sig.AgentsSiteState_map_and_set.Set.add_when_not_in
           parameters error
           (agent_id, agent_type, site_type, state)
           store_result
       in
       error, store_result
    ) agent.Cckappa_sig.agent_interface (error, store_result)

let collect_modified_map parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent store_result ->
           (*if there is no modified sites then do nothing*)
         if Ckappa_sig.Site_map_and_set.Map.is_empty
             agent.Cckappa_sig.agent_interface
         then error, store_result
         else
           (*old set*)
           let error, old_set =
             Common_map.get_rule_id_set
               parameter error
               rule_id
               Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
               store_result
           in
           let error, new_set =
             collect_sites_from_agent_interface
               parameter error
               agent_id
               agent
               old_set
           in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
               parameter
               error
               rule_id
               new_set
               store_result
           in
           error, store_result
      ) rule.Cckappa_sig.diff_direct store_result
  in
  error, store_result

(*********************************************************************)

module Proj_modif =
  Map_wrapper.Proj
    (Ckappa_sig.AgentsSiteState_map_and_set)
    (Ckappa_sig.AgentSite_map_and_set)

let store_project_modified_map parameter error rule_id store_modified_map
    store_result =
  let error, modified_set =
    Common_map.get_rule_id_set
      parameter error rule_id
      Ckappa_sig.AgentsSiteState_map_and_set.Set.empty
      store_modified_map
  in
  (*project set*)
  let error, project_set =
    Proj_modif.proj_set
      (fun (_, agent_type, site_type, _) ->
         (agent_type, site_type)
      )
      parameter
      error
      modified_set
  in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add
      parameter error
      rule_id
      project_set
      store_result
  in
  error, store_result
