(**
  * common_mape.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 18th of Feburary
  * Last modification: Time-stamp: <Jan 17 2017>
  *
  *
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let trace = false

(***************************************************************************)

let add_dependency_pair_sites parameters error
    (agent_type, site_type)
    x
    store_result =
  let error, (l, old) =
    match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
            parameters
            error
            (agent_type, site_type)
            store_result
    with
    | error, None -> error, ([], [])
    | error, Some (l, l') -> error, (l, l')
  in
  let error, store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
      parameters
      error
      (agent_type, site_type)
      (l, x :: old)
      store_result
  in
  error, store_result

(****************************************************************************)

let add_dependency_pair_sites_rule parameters error
    (agent_type, rule_id)
    l store_result =
  let error, old =
    match
      Ckappa_sig.AgentRule_map_and_set.Map.find_option_without_logs
        parameters error
        (agent_type, rule_id)
        store_result
    with
    | error, None -> error, []
    | error, Some l -> error, l
  in
  let new_list = List.concat [l; old] in
  let error, store_result =
    Ckappa_sig.AgentRule_map_and_set.Map.add_or_overwrite
      parameters error
      (agent_type, rule_id)
      new_list
      store_result
  in
  error, store_result

(****************************************************************************)

let add_dependency_triple_sites_rule parameters error
    (agent_id, agent_type, site_type)
    rule_id_set store_result =
  let error, old =
    match Ckappa_sig.AgentsSite_map_and_set.Map.find_option_without_logs
            parameters error
            (agent_id, agent_type, site_type)
            store_result
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error', union =
    Ckappa_sig.Rule_map_and_set.Set.union parameters error old rule_id_set in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error, store_result =
    Ckappa_sig.AgentsSite_map_and_set.Map.add_or_overwrite
      parameters error
      (agent_id, agent_type, site_type)
      union
      store_result
  in
  error, store_result

(****************************************************************************)

let add_triple_agents_site_rule parameters error
    (agent_id, agent_type, site_type) rule_id
    store_result =
  let error', current_set =
    Ckappa_sig.Rule_map_and_set.Set.add
      parameters
      error
      rule_id
      Ckappa_sig.Rule_map_and_set.Set.empty
  in
  let error =
    Exception.check_point
      Exception.warn parameters error error'
      __POS__ Exit
  in
  let error, store_result =
    Ckappa_sig.AgentsSite_map_and_set.Map.add_or_overwrite
      parameters error
      (agent_id, agent_type, site_type)
      current_set
      store_result
  in
  error, store_result

(****************************************************************************)
(*PROJECTION*)
(****************************************************************************)

let collect_projection_agent_id_from_triple parameters error store_result =
  Covering_classes_type.Project2_modif.monadic_proj_map
    (fun parameters error (_agent_id, agent_type, site_type) ->
       error, (agent_type, site_type))
    parameters
    error
    (Ckappa_sig.Rule_map_and_set.Set.empty)
    (fun parameters error s1 s2 ->
       let error', new_set =
         Ckappa_sig.Rule_map_and_set.Set.union parameters error s1 s2
       in
       let error =
         Exception.check_point
           Exception.warn parameters error error' __POS__ Exit
       in
       error, new_set
    ) store_result

let project_second_site (b, c, _, e) = (b, c, e)
let project_second_site_state (b, c, _, _) = (b, c)
let project_first_site_state (b, _, d, _) = (b, d)
let project_state (b, c, _) = (b, c)


(****************************************************************************)

let get_rule_id_set parameter error rule_id empty store_result =
  let error, result =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_result
    with
    | error, None -> error, empty
    | error, Some s -> error, s
  in
  error, result

let get_agent_id parameter error agent_id empty store_result =
  let error, result =
    match
      Ckappa_sig.Agent_id_map_and_set.Map.find_option_without_logs
        parameter
        error
        agent_id
        store_result
    with
    | error, None -> error, empty
    | error, Some s -> error, s
  in
  error, result

(****************************************************************************)

let collect_sites_map_in_agent_interface parameters error agent
    rule_id
    (agent_id, agent_type)
    store_result =
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun site_type _ (error, store_result) ->
       let error, store_result =
         add_triple_agents_site_rule
           parameters
           error
           (agent_id, agent_type, site_type)
           rule_id
           store_result
       in
       error, store_result
    ) agent.Cckappa_sig.agent_interface (error, store_result)

let collect_site_map_for_views parameters error agent =
  let error, site_map =
    Ckappa_sig.Site_map_and_set.Map.fold
      (fun site_type port (error, store_map) ->
         let state_max = port.Cckappa_sig.site_state.Cckappa_sig.max in
         let state_min = port.Cckappa_sig.site_state.Cckappa_sig.min in
         let error, store_map =
           Ckappa_sig.Site_map_and_set.Map.add
             parameters error
             site_type
             (state_min, state_max)
             store_map
         in
         error, store_map
      )
      agent.Cckappa_sig.agent_interface
      (error, Ckappa_sig.Site_map_and_set.Map.empty)
  in
  error, site_map
