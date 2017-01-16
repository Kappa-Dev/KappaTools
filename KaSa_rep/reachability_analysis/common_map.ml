(**
  * common_mape.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
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

let add_triple_agents_site_rule parameters error (*CHECK this function*)
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

(****************************************************************************)

let get_rule_id_set parameter error rule_id empty_set store_result =
  let error, set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter
        error
        rule_id
        store_result
    with
    | error, None -> error, empty_set
    | error, Some s -> error, s
  in
  error, set

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
