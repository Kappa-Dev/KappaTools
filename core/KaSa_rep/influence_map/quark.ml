(**
 * quark.ml
 * openkappa
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: 2011, the 7th of March
 * Last modification: Time-stamp: <Feb 27 2018>
 *
 * Compute the influence relations between rules and sites.
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let local_trace = false

let empty_quarks parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, agent_modif_plus =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, agent_modif_minus =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, agent_test =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, site_modif_plus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_modif_minus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_test =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_modif_bound_plus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_modif_bound_minus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_bound_test =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, agent_var_plus =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, site_var_plus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_bound_var_minus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, site_bound_var_plus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, agent_var_minus =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, site_var_minus =
    Quark_type.SiteMap.create_biggest_key parameter error
      (n_agents, (Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_state_index))
  in
  let error, dead_sites_plus =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, dead_states_plus =
    Quark_type.DeadSiteMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  let error, dead_sites_minus =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, dead_states_minus =
    Quark_type.DeadSiteMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  let error, dead_sites =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameter error n_agents
  in
  let error, dead_states =
    Quark_type.DeadSiteMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  let error, counter_test_geq =
    Quark_type.CounterMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  let error, counter_test_leq =
    Quark_type.CounterMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  let error, counter_delta_plus =
    Quark_type.CounterMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  let error, counter_delta_minus =
    Quark_type.CounterMap.create_biggest_key parameter error
      (n_agents, Ckappa_sig.dummy_site_name)
  in
  ( error,
    {
      Quark_type.dead_agent_plus = Quark_type.StringMap.empty;
      Quark_type.dead_agent_minus = Quark_type.StringMap.empty;
      Quark_type.dead_sites_minus;
      Quark_type.dead_states_minus;
      Quark_type.dead_agent = Quark_type.StringMap.empty;
      Quark_type.dead_sites;
      Quark_type.dead_states;
      Quark_type.dead_sites_plus;
      Quark_type.dead_states_plus;
      Quark_type.agent_modif_plus;
      Quark_type.agent_modif_minus;
      Quark_type.agent_test;
      Quark_type.agent_var_minus;
      Quark_type.site_modif_plus;
      Quark_type.site_modif_minus;
      Quark_type.site_test;
      Quark_type.site_modif_bound_plus;
      Quark_type.site_modif_bound_minus;
      Quark_type.site_test_bound = site_bound_test;
      Quark_type.site_var_minus;
      Quark_type.site_bound_var_minus;
      Quark_type.site_bound_var_plus;
      Quark_type.agent_var_plus;
      Quark_type.site_var_plus;
      Quark_type.counter_test_geq;
      Quark_type.counter_test_leq;
      Quark_type.counter_delta_plus;
      Quark_type.counter_delta_minus;
    } )

let member_generic get parameter error rule_id agent_id key map =
  let error, agent =
    match get parameter error key map with
    | error, None ->
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameter
        error 0
    | error, Some x -> error, x
  in
  match
    Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get parameter
      error rule_id agent
  with
  | error, None -> error, false
  | error, Some x -> error, Quark_type.Labels.member agent_id x

let member_site parameters error rule_id agent_id agent_type site_type state =
  member_generic Quark_type.SiteMap.unsafe_get parameters error rule_id agent_id
    (agent_type, (site_type, state))

let add_generic get set parameter error rule_id agent_id key map =
  let error, old_agent =
    match get parameter error key map with
    | error, None ->
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameter
        error 0
    | error, Some x -> error, x
  in
  let error, old_label_set =
    match
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
        parameter error rule_id old_agent
    with
    | error, None -> error, Quark_type.Labels.empty
    | error, Some x -> error, x
  in
  let new_label_set = Quark_type.Labels.add_set agent_id old_label_set in
  if new_label_set == old_label_set then
    error, map
  else (
    let error, new_agent =
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.set parameter error
        rule_id new_label_set old_agent
    in
    set parameter error key new_agent map
  )

let remove_generic get set parameter error rule_id agent_id key map =
  let error, old_agent =
    match get parameter error key map with
    | error, None ->
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameter
        error 0
    | error, Some x -> error, x
  in
  let error, old_label_set =
    match
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
        parameter error rule_id old_agent
    with
    | error, None -> error, Quark_type.Labels.empty
    | error, Some x -> error, x
  in
  let new_label_set = Quark_type.Labels.remove_set agent_id old_label_set in
  if new_label_set == old_label_set then
    error, map
  else (
    let error, new_agent =
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.set parameter error
        rule_id new_label_set old_agent
    in
    set parameter error key new_agent map
  )

let add_agent parameters error rule_id agent_id agent_type =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "rule_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ "\n")
  in
  add_generic
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set parameters
    error rule_id agent_id agent_type

let add_var parameters error var_id agent_id agent_type =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "var_id:"
        ^ Ckappa_sig.string_of_rule_id var_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ "\n")
  in
  add_generic
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set parameters
    error var_id agent_id agent_type

let add_site parameters error rule_id agent_id agent_type site_type state =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "rule_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name site_type
        ^ ",state:"
        ^ Ckappa_sig.string_of_state_index state
        ^ "\n")
  in
  add_generic Quark_type.SiteMap.unsafe_get Quark_type.SiteMap.set parameters
    error rule_id agent_id
    (agent_type, (site_type, state))

let add_bound_site_test parameters error rule_id agent_id agent_type site_type =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "rule_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name site_type
        ^ ",state: Bound \n")
  in
  add_generic Quark_type.SiteMap.unsafe_get Quark_type.SiteMap.set parameters
    error rule_id agent_id
    (agent_type, (site_type, Ckappa_sig.state_index_of_int 1))

let add_bound_site parameters error rule_id agent_id agent_type site_type set =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "rule_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name site_type
        ^ ",state: bound\n")
  in
  ( error,
    Quark_type.BoundSite.Set.add (rule_id, agent_id, agent_type, site_type) set
  )

let add_site_var parameters error var_id agent_id agent_type site_type state =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "var_id:"
        ^ Ckappa_sig.string_of_rule_id var_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name site_type
        ^ ",state:"
        ^ Ckappa_sig.string_of_state_index state
        ^ "\n")
  in
  add_generic Quark_type.SiteMap.unsafe_get Quark_type.SiteMap.set parameters
    error var_id agent_id
    (agent_type, (site_type, state))

let add_counter string parameters error rule_id agent_id agent_type counter map
    =
  let _ =
    Misc_sa.trace parameters (fun () ->
        "rule_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name counter
        ^ string ^ "\n")
  in
  add_generic Quark_type.CounterMap.unsafe_get Quark_type.CounterMap.set
    parameters error rule_id agent_id (agent_type, counter) map

let add_half_bond_breaking parameter error handler rule_id agent_id agent_type
    site k (site_modif_plus, site_modif_minus, site_bound_modif_minus) =
  match Handler.dual parameter error handler agent_type site k with
  | error, None ->
    error, (site_modif_plus, site_modif_minus, site_bound_modif_minus)
  | error, Some (agent_type2, site2, k2) ->
    let error, site_modif_plus =
      add_site parameter error rule_id agent_id agent_type2 site2
        Ckappa_sig.dummy_state_index site_modif_plus
    in
    let error, site_modif_minus =
      add_site parameter error rule_id agent_id agent_type2 site2 k2
        site_modif_minus
    in
    let error, site_bound_modif_minus =
      add_bound_site parameter error rule_id agent_id agent_type2 site2
        site_bound_modif_minus
    in
    error, (site_modif_plus, site_modif_minus, site_bound_modif_minus)

let add_dead_state s parameters error var_id agent_id agent_type site_type map
    map' =
  let _ =
    Misc_sa.trace parameters (fun () ->
        s ^ "_id:"
        ^ Ckappa_sig.string_of_rule_id var_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name site_type
        ^ "\n")
  in
  let error, map =
    add_generic Quark_type.DeadSiteMap.unsafe_get Quark_type.DeadSiteMap.set
      parameters error var_id agent_id (agent_type, site_type) map
  in
  let error, map' =
    add_generic Quark_type.DeadSiteMap.unsafe_get Quark_type.DeadSiteMap.set
      parameters error var_id agent_id (agent_type, site_type) map'
  in
  error, map, map'

let remove_dead_state s parameters error var_id agent_id agent_type site_type
    map =
  let _ =
    Misc_sa.trace parameters (fun () ->
        s ^ "_id:"
        ^ Ckappa_sig.string_of_rule_id var_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ ",site_type:"
        ^ Ckappa_sig.string_of_site_name site_type
        ^ " (REMOVE)\n")
  in
  remove_generic Quark_type.DeadSiteMap.unsafe_get Quark_type.DeadSiteMap.set
    parameters error var_id agent_id (agent_type, site_type) map

let add_dead_agent s parameters error rule_id agent_id agent_type map map' =
  let _ =
    Misc_sa.trace parameters (fun () ->
        s ^ "_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:" ^ agent_type ^ "(Dead agent)\n")
  in
  let f error map =
    let error, old_agent =
      match Quark_type.StringMap.find_option agent_type map with
      | None ->
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      | Some x -> error, x
    in
    let error, old_label_set =
      match
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error rule_id old_agent
      with
      | error, None -> error, Quark_type.Labels.empty
      | error, Some x -> error, x
    in
    let new_label_set = Quark_type.Labels.add_set agent_id old_label_set in
    if new_label_set == old_label_set then
      error, map
    else (
      let error, new_agent =
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.set parameters
          error rule_id new_label_set old_agent
      in
      error, Quark_type.StringMap.add agent_type new_agent map
    )
  in
  let error, map = f error map in
  let error, map' = f error map' in
  error, map, map'

let remove_dead_agent s parameters error rule_id agent_id agent_type map =
  let _ =
    Misc_sa.trace parameters (fun () ->
        s ^ "_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:" ^ agent_type ^ "(REMOVE Dead agent)\n")
  in
  let f error map =
    let error, old_agent =
      match Quark_type.StringMap.find_option agent_type map with
      | None ->
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      | Some x -> error, x
    in
    let error, old_label_set =
      match
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error rule_id old_agent
      with
      | error, None -> error, Quark_type.Labels.empty
      | error, Some x -> error, x
    in
    let new_label_set = Quark_type.Labels.remove_set agent_id old_label_set in
    if new_label_set == old_label_set then
      error, map
    else (
      let error, new_agent =
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.set parameters
          error rule_id new_label_set old_agent
      in
      error, Quark_type.StringMap.add agent_type new_agent map
    )
  in
  f error map

let add_dead_sites s parameters error rule_id agent_id agent_type site map map'
    =
  let _ =
    Misc_sa.trace parameters (fun () ->
        s ^ "_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ "site: todo (Dead site)\n")
  in
  let f error map =
    let error, old_agent =
      match
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error agent_type map
      with
      | error, None -> error, Cckappa_sig.KaSim_Site_map_and_set.Map.empty
      | error, Some x -> error, x
    in
    let error, old_site =
      match
        Cckappa_sig.KaSim_Site_map_and_set.Map.find_option_without_logs
          parameters error site old_agent
      with
      (* this is a partial map, not associated key are implicitely associated
         to an empty map *)
      | error, None ->
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      | error, Some x -> error, x
    in
    let error, old_label_set =
      match
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error rule_id old_site
      with
      | error, None -> error, Quark_type.Labels.empty
      | error, Some x -> error, x
    in
    let new_label_set = Quark_type.Labels.add_set agent_id old_label_set in
    if new_label_set == old_label_set then
      error, map
    else (
      let error, new_site =
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.set parameters
          error rule_id new_label_set old_site
      in
      let error, new_agent =
        Cckappa_sig.KaSim_Site_map_and_set.Map.add_or_overwrite parameters error
          site new_site old_agent
      in
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
        parameters error agent_type new_agent map
    )
  in
  let error, map = f error map in
  let error, map' = f error map' in
  error, map, map'

let remove_dead_sites s parameters error rule_id agent_id agent_type site map =
  let _ =
    Misc_sa.trace parameters (fun () ->
        s ^ "_id:"
        ^ Ckappa_sig.string_of_rule_id rule_id
        ^ ",agent_type:"
        ^ Ckappa_sig.string_of_agent_name agent_type
        ^ "site: todo (REMOVE Dead site)\n")
  in
  let f error map =
    let error, old_agent =
      match
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error agent_type map
      with
      | error, None -> error, Cckappa_sig.KaSim_Site_map_and_set.Map.empty
      | error, Some x -> error, x
    in
    let error, old_site =
      match
        Cckappa_sig.KaSim_Site_map_and_set.Map.find_option_without_logs
          parameters error site old_agent
      with
      (* this is a partial map, not associated key are implicitely associated
         to an empty map *)
      | error, None ->
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      | error, Some x -> error, x
    in
    let error, old_label_set =
      match
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error rule_id old_site
      with
      | error, None -> error, Quark_type.Labels.empty
      | error, Some x -> error, x
    in
    let new_label_set = Quark_type.Labels.remove_set agent_id old_label_set in
    if new_label_set == old_label_set then
      error, map
    else (
      let error, new_site =
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.set parameters
          error rule_id new_label_set old_site
      in
      let error, new_agent =
        Cckappa_sig.KaSim_Site_map_and_set.Map.add_or_overwrite parameters error
          site new_site old_agent
      in
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
        parameters error agent_type new_agent map
    )
  in
  f error map

let clean_bound_modif parameter error common_prefix (map1, set1) (map2, set2) =
  let agent_id_of_label l =
    let n =
      if l >= 0 then
        l
      else
        1 - l
    in
    Ckappa_sig.agent_id_of_int n
  in
  let filter =
    Quark_type.BoundSite.Set.filter (fun (_, agent_id, _, _) ->
        Ckappa_sig.compare_agent_id (agent_id_of_label agent_id) common_prefix
        < 0)
  in
  let set1_ = filter set1 in
  let set2_ = filter set2 in
  let set1' = Quark_type.BoundSite.Set.minus set1 set2_ in
  let set2' = Quark_type.BoundSite.Set.minus set2 set1_ in
  let error, map1 =
    Quark_type.BoundSite.Set.fold
      (fun (rule_id, agent_id, agent_type, site_type) (error, map) ->
        let _ =
          Misc_sa.trace parameter (fun () ->
              "MODIF PLUS rule_id:"
              ^ Ckappa_sig.string_of_rule_id rule_id
              ^ ",agent_type:"
              ^ Ckappa_sig.string_of_agent_name agent_type
              ^ ",site_type:"
              ^ Ckappa_sig.string_of_site_name site_type
              ^ ",state: BOUND\n")
        in

        add_site parameter error rule_id agent_id agent_type site_type
          (Ckappa_sig.state_index_of_int 1)
          map)
      set1' (error, map1)
  in
  let error, map2 =
    Quark_type.BoundSite.Set.fold
      (fun (rule_id, agent_id, agent_type, site_type) (error, map) ->
        let _ =
          Misc_sa.trace parameter (fun () ->
              "MODIF MOINS: rule_id:"
              ^ Ckappa_sig.string_of_rule_id rule_id
              ^ ",agent_type:"
              ^ Ckappa_sig.string_of_agent_name agent_type
              ^ ",site_type:"
              ^ Ckappa_sig.string_of_site_name site_type
              ^ ",state: BOUND\n")
        in
        add_site parameter error rule_id agent_id agent_type site_type
          (Ckappa_sig.state_index_of_int 1)
          map)
      set2' (error, map2)
  in
  error, map1, map2

let scan_mixture_in_var bool parameter error handler var_id mixture quarks =
  let views = mixture.Cckappa_sig.views in
  let agent_var, site_var, site_bound_var, dead_agents, dead_sites, dead_states
      =
    if bool then
      ( quarks.Quark_type.agent_var_plus,
        quarks.Quark_type.site_var_plus,
        quarks.Quark_type.site_bound_var_plus,
        quarks.Quark_type.dead_agent_plus,
        quarks.Quark_type.dead_sites_plus,
        quarks.Quark_type.dead_states_plus )
    else
      ( quarks.Quark_type.agent_var_minus,
        quarks.Quark_type.site_var_minus,
        quarks.Quark_type.site_bound_var_minus,
        quarks.Quark_type.dead_agent_minus,
        quarks.Quark_type.dead_sites_minus,
        quarks.Quark_type.dead_states_minus )
  in
  let ( error,
        ( agent_var,
          site_var,
          site_bound_var,
          dead_agents,
          dead_sites,
          dead_states ) ) =
    (*what is tested in the mixture*)
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error _agent_id agent
           ( agent_var,
             site_var,
             site_bound_var,
             dead_agents,
             dead_sites,
             dead_states ) ->
        let dealwith agent error (agent_var, site_var, site_bound_var) =
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
          in
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, agent_var =
            add_var parameter error var_id kasim_id agent_type agent_var
          in
          let error, site_var, site_bound_var =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site port (error, site_var, site_bound_var) ->
                let interval = port.Cckappa_sig.site_state in
                let max = interval.Cckappa_sig.max in
                let min = interval.Cckappa_sig.min in
                let error, binding_state =
                  Handler.is_binding_site parameter error handler agent_type
                    site
                in
                if
                  (binding_state && port.Cckappa_sig.site_free = None)
                  (* The state is a wildcard, it should be ignored *)
                  || ((not binding_state) && min <> max)
                then
                  error, site_var, site_bound_var
                else (
                  match min, max with
                  | Some min, Some max when min = max ->
                    let error, site_var =
                      add_site_var parameter error var_id kasim_id agent_type
                        site min site_var
                    in
                    error, site_var, site_bound_var
                  | Some _, Some _ ->
                    let error, site_bound_var =
                      add_site_var parameter error var_id kasim_id agent_type
                        site
                        (Ckappa_sig.state_index_of_int 1)
                        site_bound_var
                    in
                    error, site_var, site_bound_var
                  | None, _ | _, None ->
                    let error, (site_var, site_bound_var) =
                      Exception.warn parameter error __POS__ Exit
                        (site_var, site_bound_var)
                    in
                    error, site_var, site_bound_var
                ))
              agent.Cckappa_sig.agent_interface
              (error, site_var, site_bound_var)
          in
          error, (agent_var, site_var, site_bound_var)
        in
        match agent with
        | Cckappa_sig.Unknown_agent (string, id_int) ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id id_int)
          in
          let error, dead_agents, _ =
            add_dead_agent "var" parameter error var_id kasim_id string
              dead_agents dead_agents
          in
          ( error,
            ( agent_var,
              site_var,
              site_bound_var,
              dead_agents,
              dead_sites,
              dead_states ) )
        | Cckappa_sig.Ghost ->
          ( error,
            ( agent_var,
              site_var,
              site_bound_var,
              dead_agents,
              dead_sites,
              dead_states ) )
        | Cckappa_sig.Dead_agent (agent, deadsite, deadstate, deadstate') ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
          in
          let error, (agent_var, site_var, site_bound_var) =
            dealwith agent error (agent_var, site_var, site_bound_var)
          in
          let error, dead_sites, _ =
            Cckappa_sig.KaSim_Site_map_and_set.Set.fold
              (fun s (error, deadsite, dead_site_) ->
                add_dead_sites "var" parameter error var_id kasim_id
                  agent.Cckappa_sig.agent_name s deadsite dead_site_)
              deadsite
              (error, dead_sites, dead_sites)
          in
          let error, dead_states, _ =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun s _ (error, dead_states, dead_states_) ->
                add_dead_state "var" parameter error var_id kasim_id
                  agent.Cckappa_sig.agent_name s dead_states dead_states_)
              deadstate
              (error, dead_states, dead_states)
          in
          let error, dead_states, _ =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun s _ (error, dead_states, dead_states_) ->
                add_dead_state "var" parameter error var_id kasim_id
                  agent.Cckappa_sig.agent_name s dead_states dead_states_)
              deadstate'
              (error, dead_states, dead_states)
          in
          ( error,
            ( agent_var,
              site_var,
              site_bound_var,
              dead_agents,
              dead_sites,
              dead_states ) )
        | Cckappa_sig.Agent agent ->
          let error, (agent_var, site_var, site_bound_var) =
            dealwith agent error (agent_var, site_var, site_bound_var)
          in
          ( error,
            ( agent_var,
              site_var,
              site_bound_var,
              dead_agents,
              dead_sites,
              dead_states ) ))
      views
      (agent_var, site_var, site_bound_var, dead_agents, dead_sites, dead_states)
  in
  if bool then
    ( error,
      {
        quarks with
        Quark_type.agent_var_plus = agent_var;
        Quark_type.site_var_plus = site_var;
        Quark_type.site_bound_var_plus = site_bound_var;
        Quark_type.dead_agent_plus = dead_agents;
        Quark_type.dead_sites_plus = dead_sites;
        Quark_type.dead_states_plus = dead_states;
      } )
  else
    ( error,
      {
        quarks with
        Quark_type.agent_var_minus = agent_var;
        Quark_type.site_var_minus = site_var;
        Quark_type.site_bound_var_minus = site_bound_var;
        Quark_type.dead_agent_minus = dead_agents;
        Quark_type.dead_sites_minus = dead_sites;
        Quark_type.dead_states_minus = dead_states;
      } )

let scan_pos_mixture = scan_mixture_in_var true
let scan_neg_mixture = scan_mixture_in_var false

let scan_var parameter error handler var_id var quarks =
  let aux error var list_pos list_neg =
    match var with
    | Alg_expr.KAPPA_INSTANCE mixture -> error, mixture :: list_pos, list_neg
    | Alg_expr.BIN_ALG_OP _ | Alg_expr.UN_ALG_OP _ | Alg_expr.CONST _
    | Alg_expr.TOKEN_ID _ | Alg_expr.STATE_ALG_OP _ | Alg_expr.ALG_VAR _
    | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _ | Alg_expr.IF _ ->
      (* to do *)
      error, list_pos, list_neg
  in
  let error, list_pos, list_neg = aux error var [] [] in
  let error, quarks =
    List.fold_left
      (fun (error, quarks) mixture ->
        scan_pos_mixture parameter error handler var_id mixture quarks)
      (error, quarks) list_pos
  in
  let error, quarks =
    List.fold_left
      (fun (error, quarks) mixture ->
        scan_neg_mixture parameter error handler var_id mixture quarks)
      (error, quarks) list_neg
  in
  error, quarks

let is_tested_and_not_question_mark parameter error agent_id site views =
  let error, agent_opt =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter
      error agent_id views
  in
  match agent_opt with
  | None
  | Some Cckappa_sig.Ghost
  | Some (Cckappa_sig.Dead_agent _)
  | Some (Cckappa_sig.Unknown_agent _) ->
    error, false
  | Some (Cckappa_sig.Agent agent) ->
    let error, port_opt =
      Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameter error
        site agent.Cckappa_sig.agent_interface
    in
    (match port_opt with
    | None -> error, false
    | Some port ->
      let interval = port.Cckappa_sig.site_state in
      (match interval.Cckappa_sig.min, interval.Cckappa_sig.max with
      | Some min, Some max ->
        let min = Ckappa_sig.int_of_state_index min in
        let max = Ckappa_sig.int_of_state_index max in
        if max = 0 || min > 0 then
          error, true
        else
          error, false
      | None, Some _ | Some _, None ->
        Exception.warn parameter error __POS__ Exit true
      | None, None -> Exception.warn parameter error __POS__ Exit false))

let scan_rule parameter error handler rule_id rule quarks =
  let common_prefix = Ckappa_sig.agent_id_of_int rule.Cckappa_sig.prefix in
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let viewsrhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let agent_test = quarks.Quark_type.agent_test in
  let site_test = quarks.Quark_type.site_test in
  let site_bound_test = quarks.Quark_type.site_test_bound in
  let dead_agents_test = quarks.Quark_type.dead_agent in
  let dead_sites_test = quarks.Quark_type.dead_sites in
  let dead_states_test = quarks.Quark_type.dead_states in
  let dead_agents_minus = quarks.Quark_type.dead_agent_minus in
  let dead_sites_minus = quarks.Quark_type.dead_sites_minus in
  let dead_states_minus = quarks.Quark_type.dead_states_minus in
  let agent_modif_plus = quarks.Quark_type.agent_modif_plus in
  let agent_modif_minus = quarks.Quark_type.agent_modif_minus in
  let site_modif_plus = quarks.Quark_type.site_modif_plus in
  let site_modif_minus = quarks.Quark_type.site_modif_minus in
  let site_bound_modif_plus = Quark_type.BoundSite.Set.empty in
  let site_bound_modif_minus = Quark_type.BoundSite.Set.empty in
  let counter_test_geq = quarks.Quark_type.counter_test_geq in
  let counter_test_leq = quarks.Quark_type.counter_test_leq in
  let counter_delta_plus = quarks.Quark_type.counter_delta_plus in
  let counter_delta_minus = quarks.Quark_type.counter_delta_minus in

  let _ = Misc_sa.trace parameter (fun () -> "TEST\n") in
  let ( error,
        ( agent_test,
          site_test,
          _site_bound_test,
          _dead_agents,
          _dead_sites,
          _dead_states,
          dead_agents_minus,
          dead_sites_minus,
          dead_states_minus ) ) =
    (*what is tested in the lhs*)
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error _agent_id agent
           ( agent_test,
             site_test,
             site_bound_test,
             dead_agents,
             dead_sites,
             dead_states,
             dead_agents_minus,
             dead_sites_minus,
             dead_states_minus ) ->
        let dealwith agent error (agent_test, site_test, site_bound_test) =
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
          in
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, agent_test =
            add_agent parameter error rule_id kasim_id agent_type agent_test
          in
          let error, site_test, site_bound_test =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site port (error, site_test, site_bound_test) ->
                let error, b =
                  Handler.is_counter parameter error handler agent_type site
                in
                if b then
                  error, site_test, site_bound_test
                else (
                  let interval = port.Cckappa_sig.site_state in
                  let max = interval.Cckappa_sig.max in
                  let min = interval.Cckappa_sig.min in
                  let error, binding_state =
                    Handler.is_binding_site parameter error handler agent_type
                      site
                  in
                  match min, max with
                  | Some min, Some max ->
                    if
                      (binding_state && port.Cckappa_sig.site_free = None)
                      (* The state is a wildcard, it should be ignored *)
                      || ((not binding_state) && min <> max)
                    then
                      error, site_test, site_bound_test
                    else if min = max then (
                      let error, site_test =
                        add_site parameter error rule_id kasim_id agent_type
                          site min site_test
                      in
                      error, site_test, site_bound_test
                    ) else (
                      let error, site_bound_test =
                        add_bound_site_test parameter error rule_id kasim_id
                          agent_type site site_bound_test
                      in
                      error, site_test, site_bound_test
                    )
                  | None, _ | _, None ->
                    let error, (site_test, site_bound_test) =
                      Exception.warn parameter error __POS__ Exit
                        (site_test, site_bound_test)
                    in
                    error, site_test, site_bound_test
                ))
              agent.Cckappa_sig.agent_interface
              (error, site_test, site_bound_test)
          in
          error, (agent_test, site_test, site_bound_test)
        in
        match agent with
        | Cckappa_sig.Unknown_agent (string, id_int) ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id id_int)
          in
          let error, dead_agents, dead_agents_minus =
            add_dead_agent "rule" parameter error rule_id kasim_id string
              dead_agents dead_agents_minus
          in

          ( error,
            ( agent_test,
              site_test,
              site_bound_test,
              dead_agents,
              dead_sites,
              dead_states,
              dead_agents_minus,
              dead_sites_minus,
              dead_states_minus ) )
        | Cckappa_sig.Ghost ->
          ( error,
            ( agent_test,
              site_test,
              site_bound_test,
              dead_agents,
              dead_sites,
              dead_states,
              dead_agents_minus,
              dead_sites_minus,
              dead_states_minus ) )
        | Cckappa_sig.Dead_agent (agent, deadsite, deadstate, deadstate') ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
          in
          let error, (agent_test, site_test, site_bound_test) =
            dealwith agent error (agent_test, site_test, site_bound_test)
          in
          let error, dead_sites, dead_sites_minus =
            Cckappa_sig.KaSim_Site_map_and_set.Set.fold
              (fun s (error, deadsite, deadsite_minus) ->
                add_dead_sites "rule" parameter error rule_id kasim_id
                  agent.Cckappa_sig.agent_name s deadsite deadsite_minus)
              deadsite
              (error, dead_sites, dead_sites_minus)
          in
          let error, dead_states, dead_states_minus =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun s _ (error, dead_states, dead_states_minus) ->
                add_dead_state "rule" parameter error rule_id kasim_id
                  agent.Cckappa_sig.agent_name s dead_states dead_states_minus)
              deadstate
              (error, dead_states, dead_states_minus)
          in
          let error, dead_states, dead_states_minus =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun s _ (error, dead_states, dead_states_minus) ->
                add_dead_state "rule" parameter error rule_id kasim_id
                  agent.Cckappa_sig.agent_name s dead_states dead_states_minus)
              deadstate'
              (error, dead_states, dead_states_minus)
          in
          ( error,
            ( agent_test,
              site_test,
              site_bound_test,
              dead_agents,
              dead_sites,
              dead_states,
              dead_agents_minus,
              dead_sites_minus,
              dead_states_minus ) )
        | Cckappa_sig.Agent agent ->
          let error, (agent_test, site_test, site_bound_test) =
            dealwith agent error (agent_test, site_test, site_bound_test)
          in
          ( error,
            ( agent_test,
              site_test,
              site_bound_test,
              dead_agents,
              dead_sites,
              dead_states,
              dead_agents_minus,
              dead_sites_minus,
              dead_states_minus ) ))
      viewslhs
      ( agent_test,
        site_test,
        site_bound_test,
        dead_agents_test,
        dead_sites_test,
        dead_states_test,
        dead_agents_minus,
        dead_sites_minus,
        dead_states_minus )
  in
  let _ =
    Misc_sa.trace parameter (fun () -> "CLEANING QUARKS FOR DEAD AGENTS\n")
  in

  let error, (dead_agents_minus, dead_sites_minus, dead_states_minus) =
    (*what is dead in the rhs*)
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error _agent_id agent
           (dead_agents_minus, dead_sites_minus, dead_states_minus) ->
        match agent with
        | Cckappa_sig.Unknown_agent (string, id_int) ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id id_int)
          in
          let error, dead_agents_minus =
            remove_dead_agent "rule" parameter error rule_id kasim_id string
              dead_agents_minus
          in
          error, (dead_agents_minus, dead_sites_minus, dead_states_minus)
        | Cckappa_sig.Ghost ->
          error, (dead_agents_minus, dead_sites_minus, dead_states_minus)
        | Cckappa_sig.Dead_agent (agent, deadsite, deadstate, deadstate') ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
          in
          let error, dead_sites_minus =
            Cckappa_sig.KaSim_Site_map_and_set.Set.fold
              (fun s (error, deadsite_minus) ->
                remove_dead_sites "rule" parameter error rule_id kasim_id
                  agent.Cckappa_sig.agent_name s deadsite_minus)
              deadsite (error, dead_sites_minus)
          in
          let error, dead_states_minus =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun s _ (error, dead_states_minus) ->
                remove_dead_state "rule" parameter error rule_id kasim_id
                  agent.Cckappa_sig.agent_name s dead_states_minus)
              deadstate (error, dead_states_minus)
          in
          let error, dead_states_minus =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun s _ (error, dead_states_minus) ->
                remove_dead_state "rule" parameter error rule_id kasim_id
                  agent.Cckappa_sig.agent_name s dead_states_minus)
              deadstate' (error, dead_states_minus)
          in
          error, (dead_agents_minus, dead_sites_minus, dead_states_minus)
        | Cckappa_sig.Agent _ ->
          error, (dead_agents_minus, dead_sites_minus, dead_states_minus))
      viewsrhs
      (dead_agents_minus, dead_sites_minus, dead_states_minus)
  in
  let _ = Misc_sa.trace parameter (fun () -> "CREATION\n") in
  let error, agent_modif_plus =
    (*the agents that are created*)
    List.fold_left
      (fun (error, agent_modif_plus) (agent_id, agent_type) ->
        let error, agent =
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id viewsrhs
        in
        match agent with
        | None -> Exception.warn parameter error __POS__ Exit agent_modif_plus
        | Some (Cckappa_sig.Unknown_agent _) | Some Cckappa_sig.Ghost ->
          error, agent_modif_plus
        | Some (Cckappa_sig.Dead_agent (agent, _, _, _))
        | Some (Cckappa_sig.Agent agent) ->
          let error, kasim_id =
            Quark_type.Labels.label_of_int parameter error
              (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
          in
          add_agent parameter error rule_id kasim_id agent_type agent_modif_plus)
      (error, agent_modif_plus) rule.Cckappa_sig.actions.Cckappa_sig.creation
  in
  let _ = Misc_sa.trace parameter (fun () -> "REMOVAL\n") in
  let ( error,
        ( agent_modif_minus,
          site_modif_plus,
          site_modif_minus,
          site_bound_modif_minus ) ) =
    (*the agents that are removed *)
    List.fold_left
      (fun ( error,
             ( agent_modif_minus,
               site_modif_plus,
               site_modif_minus,
               site_bound_modif_minus ) ) (agent_id, agent, list) ->
        let agent_type = agent.Cckappa_sig.agent_name in
        let error, kasim_id =
          Quark_type.Labels.label_of_int parameter error
            (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
        in
        let error, mkasim_id =
          Quark_type.Labels.label_of_int parameter error
            (-1 - Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
        in
        let error, agent_modif_minus =
          add_agent parameter error rule_id kasim_id agent_type
            agent_modif_minus
        in
        let error, (site_modif_plus, site_modif_minus, site_bound_modif_minus) =
          List.fold_left
            (fun ( error,
                   (site_modif_plus, site_modif_minus, site_bound_modif_minus)
                 ) site ->
              let error, is_binding =
                Handler.is_binding_site parameter error handler agent_type site
              in
              if is_binding then (
                let error, is_tested_and_not_question_mark =
                  is_tested_and_not_question_mark parameter error agent_id site
                    viewslhs
                in
                if not is_tested_and_not_question_mark then (
                  let error, state_dic =
                    Misc_sa.unsome
                      (Ckappa_sig
                       .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                       .get parameter error (agent_type, site)
                         handler.Cckappa_sig.states_dic)
                      (fun error ->
                        Exception.warn parameter error __POS__ Exit
                          (Ckappa_sig.Dictionary_of_States.init ()))
                  in
                  let error, last_entry =
                    Ckappa_sig.Dictionary_of_States.last_entry parameter error
                      state_dic
                  in
                  let rec aux k
                      ( error,
                        ( site_modif_plus,
                          site_modif_minus,
                          site_bound_modif_minus ) ) =
                    if k > last_entry then
                      ( error,
                        ( site_modif_plus,
                          site_modif_minus,
                          site_bound_modif_minus ) )
                    else (
                      let ( error,
                            ( site_modif_plus,
                              site_modif_minus,
                              site_bound_modif_minus ) ) =
                        add_half_bond_breaking parameter error handler rule_id
                          mkasim_id agent_type site k
                          ( site_modif_plus,
                            site_modif_minus,
                            site_bound_modif_minus )
                      in
                      aux
                        (Ckappa_sig.next_state_index k)
                        ( error,
                          ( site_modif_plus,
                            site_modif_minus,
                            site_bound_modif_minus ) )
                    )
                  in
                  aux Ckappa_sig.dummy_state_index_1
                    ( error,
                      (site_modif_plus, site_modif_minus, site_bound_modif_minus)
                    )
                ) else
                  ( error,
                    (site_modif_plus, site_modif_minus, site_bound_modif_minus)
                  )
              ) else
                ( error,
                  (site_modif_plus, site_modif_minus, site_bound_modif_minus) ))
            (error, (site_modif_plus, site_modif_minus, site_bound_modif_minus))
            list
        in
        ( error,
          ( agent_modif_minus,
            site_modif_plus,
            site_modif_minus,
            site_bound_modif_minus ) ))
      ( error,
        ( agent_modif_minus,
          site_modif_plus,
          site_modif_minus,
          site_bound_modif_minus ) )
      rule.Cckappa_sig.actions.Cckappa_sig.remove
  in
  let _ = Misc_sa.trace parameter (fun () -> "MODIFICATION+\n") in
  let error, (site_modif_plus, site_bound_modif_plus) =
    (*the sites that are directly modified (excluding side-effects)*)
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error _agent_id agent
           (site_modif_plus, site_bound_modif_plus) ->
        let error, kasim_id =
          Quark_type.Labels.label_of_int parameter error
            (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
        in
        let agent_type = agent.Cckappa_sig.agent_name in
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site port (error, (site_modif_plus, site_bound_modif_plus)) ->
            let error, b =
              Handler.is_counter parameter error handler agent_type site
            in
            if b then
              error, (site_modif_plus, site_bound_modif_plus)
            else (
              let interval = port.Cckappa_sig.site_state in
              let max = interval.Cckappa_sig.max in
              let min = interval.Cckappa_sig.min in
              let error, binding_state =
                Handler.is_binding_site parameter error handler agent_type site
              in
              let error, site_bound_modif_plus =
                match min, max with
                | Some min, Some _max ->
                  if
                    binding_state
                    && Ckappa_sig.compare_state_index min
                         Ckappa_sig.dummy_state_index
                       > 0
                  then
                    add_bound_site parameter error rule_id kasim_id agent_type
                      site site_bound_modif_plus
                  else
                    error, site_bound_modif_plus
                | None, _ | _, None ->
                  Exception.warn parameter error __POS__ Exit
                    site_bound_modif_plus
              in
              match min, max with
              | Some min, Some max ->
                let rec aux k (error, site_modif_plus) =
                  if k > max then
                    error, site_modif_plus
                  else
                    aux
                      (Ckappa_sig.next_state_index k)
                      (add_site parameter error rule_id kasim_id agent_type site
                         k site_modif_plus)
                in
                let error, site_modif_plus = aux min (error, site_modif_plus) in
                error, (site_modif_plus, site_bound_modif_plus)
              | None, _ | _, None ->
                Exception.warn parameter error __POS__ Exit
                  (site_modif_plus, site_bound_modif_plus)
            ))
          agent.Cckappa_sig.agent_interface
          (error, (site_modif_plus, site_bound_modif_plus)))
      rule.Cckappa_sig.diff_direct
      (site_modif_plus, site_bound_modif_plus)
  in
  let _ = Misc_sa.trace parameter (fun () -> "MODIFICATION-\n") in
  let error, (site_modif_minus, site_bound_modif_minus) =
    (*the sites that are directly modified (excluding side-effects)*)
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter
      error
      (fun parameter error agent_id agent
           (site_modif_minus, site_bound_modif_minus) ->
        let error, kasim_id =
          Quark_type.Labels.label_of_int parameter error
            (Ckappa_sig.int_of_agent_id agent.Cckappa_sig.agent_kasim_id)
        in
        let is_in_common_prefix =
          Ckappa_sig.compare_agent_id agent_id
            (Ckappa_sig.agent_id_of_int rule.Cckappa_sig.prefix)
          < 0
        in
        let agent_type = agent.Cckappa_sig.agent_name in
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site port (error, (site_modif_minus, site_bound_modif_minus)) ->
            let error, b =
              Handler.is_counter parameter error handler agent_type site
            in
            if b then
              error, (site_modif_minus, site_bound_modif_minus)
            else (
              let interval = port.Cckappa_sig.site_state in
              let max = interval.Cckappa_sig.max in
              let min = interval.Cckappa_sig.min in
              match min, max with
              | Some min, Some max ->
                let error, binding_state =
                  Handler.is_binding_site parameter error handler agent_type
                    site
                in
                let error, site_bound_modif_minus =
                  if
                    binding_state
                    && Ckappa_sig.compare_state_index min
                         Ckappa_sig.dummy_state_index
                       > 0
                  then
                    add_bound_site parameter error rule_id kasim_id agent_type
                      site site_bound_modif_minus
                  else
                    error, site_bound_modif_minus
                in
                let rec aux k (error, site_modif_minus) =
                  if k > max then
                    error, site_modif_minus
                  else (
                    let site_modif_minus =
                      if is_in_common_prefix then (
                        match
                          member_site parameter error rule_id kasim_id
                            agent_type site k site_modif_plus
                        with
                        | error, true -> error, site_modif_minus
                        | error, false ->
                          add_site parameter error rule_id kasim_id agent_type
                            site k site_modif_minus
                      ) else
                        add_site parameter error rule_id kasim_id agent_type
                          site k site_modif_minus
                    in
                    aux (Ckappa_sig.next_state_index k) site_modif_minus
                  )
                in
                let error, modif = aux min (error, site_modif_minus) in
                error, (modif, site_bound_modif_minus)
              | None, _ | _, None ->
                Exception.warn parameter error __POS__ Exit
                  (site_modif_minus, site_bound_modif_minus)
            ))
          agent.Cckappa_sig.agent_interface
          (error, (site_modif_minus, site_bound_modif_minus)))
      rule.Cckappa_sig.diff_reverse
      (site_modif_minus, site_bound_modif_minus)
  in
  let error, (site_modif_plus, site_modif_minus, site_bound_modif_minus) =
    (*the sites that are released via half bond breaking*)
    let _ = Misc_sa.trace parameter (fun () -> "HALF BOND BREAKING\n") in
    List.fold_left
      (fun (error, (site_modif_plus, site_modif_minus, site_bound_modif_minus))
           (add, state) ->
        let agent_index = add.Cckappa_sig.agent_index in
        let error, mkasim_id =
          Quark_type.Labels.label_of_int parameter error
            (-1 - Ckappa_sig.int_of_agent_id agent_index)
        in
        let agent_type = add.Cckappa_sig.agent_type in
        let site = add.Cckappa_sig.site in
        let error, (min, max) =
          match state with
          | None ->
            let error, state_dic =
              Misc_sa.unsome
                (Ckappa_sig
                 .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                 .get parameter error (agent_type, site)
                   handler.Cckappa_sig.states_dic)
                (fun error ->
                  Exception.warn parameter error __POS__ Exit
                    (Ckappa_sig.Dictionary_of_States.init ()))
            in
            let error, last_entry =
              Ckappa_sig.Dictionary_of_States.last_entry parameter error
                state_dic
            in
            error, (Ckappa_sig.dummy_state_index_1, last_entry)
          | Some interval ->
            (match interval.Cckappa_sig.min, interval.Cckappa_sig.max with
            | Some min, Some max -> error, (min, max)
            | None, _ | _, None ->
              Exception.warn parameter error __POS__ Exit
                (Ckappa_sig.dummy_state_index, Ckappa_sig.dummy_state_index))
        in
        let rec aux k
            (error, (site_modif_plus, site_modif_minus, site_bound_modif_minus))
            =
          if Ckappa_sig.compare_state_index k max > 0 then
            error, (site_modif_plus, site_modif_minus, site_bound_modif_minus)
          else (
            let ( error,
                  (site_modif_plus, site_modif_minus, site_bound_modif_minus) )
                =
              add_half_bond_breaking parameter error handler rule_id mkasim_id
                agent_type site k
                (site_modif_plus, site_modif_minus, site_bound_modif_minus)
            in
            aux
              (Ckappa_sig.next_state_index k)
              ( error,
                (site_modif_plus, site_modif_minus, site_bound_modif_minus) )
          )
        in
        aux min
          (error, (site_modif_plus, site_modif_minus, site_bound_modif_minus)))
      (error, (site_modif_plus, site_modif_minus, site_bound_modif_minus))
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
  in
  let error, site_bound_modif_plus, site_bound_modif_minus =
    (* if a quark appers in both, remove them *)
    clean_bound_modif parameter error common_prefix
      (quarks.Quark_type.site_modif_bound_plus, site_bound_modif_plus)
      (quarks.Quark_type.site_modif_bound_minus, site_bound_modif_minus)
  in
  let ( error,
        counter_test_geq,
        counter_test_leq,
        counter_delta_plus,
        counter_delta_minus ) =
    List.fold_left
      (fun ( error,
             counter_test_geq,
             counter_test_leq,
             counter_delta_plus,
             counter_delta_minus ) (site, action) ->
        let pre = action.Cckappa_sig.precondition in
        let delta = action.Cckappa_sig.increment in
        let error, counter_test_geq =
          match pre.Cckappa_sig.max with
          | None -> error, counter_test_geq
          | Some _ ->
            add_counter ">=" parameter error rule_id
              (Ckappa_sig.int_of_agent_id site.Cckappa_sig.agent_index)
              site.Cckappa_sig.agent_type site.Cckappa_sig.site counter_test_geq
        in
        let error, counter_test_leq =
          match pre.Cckappa_sig.min with
          | None -> error, counter_test_leq
          | Some _ ->
            add_counter "<=" parameter error rule_id
              (Ckappa_sig.int_of_agent_id site.Cckappa_sig.agent_index)
              site.Cckappa_sig.agent_type site.Cckappa_sig.site counter_test_leq
        in
        let error, counter_delta_minus =
          if delta < 0 then
            add_counter "-=" parameter error rule_id
              (Ckappa_sig.int_of_agent_id site.Cckappa_sig.agent_index)
              site.Cckappa_sig.agent_type site.Cckappa_sig.site
              counter_delta_minus
          else
            error, counter_delta_minus
        in
        let error, counter_delta_plus =
          if delta > 0 then
            add_counter "+=" parameter error rule_id
              (Ckappa_sig.int_of_agent_id site.Cckappa_sig.agent_index)
              site.Cckappa_sig.agent_type site.Cckappa_sig.site
              counter_delta_plus
          else
            error, counter_delta_plus
        in
        ( error,
          counter_test_geq,
          counter_test_leq,
          counter_delta_plus,
          counter_delta_minus ))
      ( error,
        counter_test_geq,
        counter_test_leq,
        counter_delta_plus,
        counter_delta_minus )
      rule.Cckappa_sig.actions.Cckappa_sig.translate_counters
  in

  ( error,
    {
      quarks with
      Quark_type.agent_test;
      Quark_type.site_test;
      Quark_type.dead_agent = dead_agents_test;
      Quark_type.dead_sites = dead_sites_test;
      Quark_type.dead_states = dead_states_test;
      Quark_type.dead_agent_minus = dead_agents_minus;
      Quark_type.dead_sites_minus;
      Quark_type.dead_states_minus;
      Quark_type.agent_modif_plus;
      Quark_type.site_modif_plus;
      Quark_type.agent_modif_minus;
      Quark_type.site_modif_minus;
      Quark_type.site_modif_bound_plus = site_bound_modif_plus;
      Quark_type.site_modif_bound_minus = site_bound_modif_minus;
      Quark_type.counter_test_geq;
      Quark_type.counter_test_leq;
      Quark_type.counter_delta_plus;
      Quark_type.counter_delta_minus;
    } )

let scan_rule_set parameter error handler rules =
  let error, init = empty_quarks parameter error handler in
  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error rule_id rule quark_maps ->
      let _ =
        Misc_sa.trace parameter (fun () ->
            "Rule " ^ Ckappa_sig.string_of_rule_id rule_id ^ "\n")
      in
      scan_rule parameter error handler rule_id rule.Cckappa_sig.e_rule_c_rule
        quark_maps)
    rules init

let scan_var_set parameter error handler vars quarks =
  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error var_id var quark_maps ->
      let _, (var, _) = var.Cckappa_sig.e_variable in
      let _ =
        Misc_sa.trace parameter (fun () ->
            "Var " ^ Ckappa_sig.string_of_rule_id var_id ^ "\n")
      in
      scan_var parameter error handler var_id var quark_maps)
    vars quarks

let quarkify parameters error handler cc_compil =
  let _ = Misc_sa.trace parameters (fun () -> "Quarkify\n") in
  let error, quarks =
    scan_rule_set parameters error handler cc_compil.Cckappa_sig.rules
  in
  scan_var_set parameters error handler cc_compil.Cckappa_sig.variables quarks
