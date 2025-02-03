(* Time-stamp: <Jul 02 2016> *)

type agent_site =
  Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state

type agent_id_site =
  Ckappa_sig.c_agent_id
  * Ckappa_sig.c_agent_name
  * Ckappa_sig.c_site_name
  * Ckappa_sig.c_state

type agent_two_sites =
  Ckappa_sig.c_agent_name
  * Ckappa_sig.c_site_name
  * Ckappa_sig.c_site_name
  * Ckappa_sig.c_state
  * Ckappa_sig.c_state

type agent_id_two_sites =
  Ckappa_sig.c_agent_id
  * Ckappa_sig.c_agent_name
  * Ckappa_sig.c_site_name
  * Ckappa_sig.c_site_name
  * Ckappa_sig.c_state
  * Ckappa_sig.c_state

module PairAgentsSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = agent_id_site * agent_id_site

  let compare = compare
  let print _ _ = ()
end))

module PairAgentSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = agent_site * agent_site

  let compare = compare
  let print _ _ = ()
end))

(*parallel*)
module PairAgentsSitesStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_agent_id * (agent_two_sites * agent_two_sites)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentSitesStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = agent_two_sites * agent_two_sites

  let compare = compare
  let print _ _ = ()
end))

(*******************************************************************)
(*a map from tuples to sites*)
(*******************************************************************)

module PairAgentSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)

  let compare = compare
  let print _ _ = ()
end))

module Partition_tuples_to_sites_map =
  Map_wrapper.Proj
    (PairAgentSitesStates_map_and_set)
    (*set*)
    (PairAgentSite_map_and_set)
(*map*)

module AgentSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name

  let compare = compare
  let print _ _ = ()
end))

(*******************************************************************)

module AgentsSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = agent_id_site

  let compare = compare
  let print _ _ = ()
end))

module AgentsSitesStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    Ckappa_sig.c_rule_id
    * Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state
    * Ckappa_sig.c_state

  let compare = compare
  let print _ _ = ()
end))

(*******************************************************************)

let convert_pair parameters error kappa_handler pair =
  let agent, site = pair in
  let error, site =
    Handler.string_of_site_contact_map parameters error kappa_handler agent site
  in
  let error, agent =
    Handler.translate_agent ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  error, (agent, site)

let convert_single parameters error kappa_handler single =
  let agent, site, site', _, _ = single in
  let error, site =
    Handler.string_of_site_contact_map parameters error kappa_handler agent site
  in
  let error, site' =
    Handler.string_of_site_contact_map parameters error kappa_handler agent
      site'
  in
  let error, agent =
    Handler.translate_agent ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  error, (agent, site, site')

let convert_tuple parameters error kappa_handler tuple =
  let (agent, site, site', _, _), (agent'', site'', site''', _, _) = tuple in
  let error, site =
    Handler.string_of_site_contact_map parameters error kappa_handler agent site
  in
  let error, site' =
    Handler.string_of_site_contact_map parameters error kappa_handler agent
      site'
  in
  let error, agent =
    Handler.translate_agent ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  let error, site'' =
    Handler.string_of_site_contact_map parameters error kappa_handler agent''
      site''
  in
  let error, site''' =
    Handler.string_of_site_contact_map parameters error kappa_handler agent''
      site'''
  in
  let error, agent'' =
    Handler.translate_agent ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent''
  in
  error, (agent, site, site', agent'', site'', site''')

let convert_refined_tuple parameters error kappa_handler tuple =
  let error, (agent, site, site', agent'', site'', site''') =
    convert_tuple parameters error kappa_handler (snd tuple)
  in
  ( error,
    ( Ckappa_sig.string_of_agent_id (fst tuple),
      agent,
      site,
      site',
      agent'',
      site'',
      site''' ) )

let cons_opt a l =
  match a with
  | None -> l
  | Some a -> a :: l

let add_parallel_bond_variable_to_mvbdu parameters bdu_handler error bool
    variable_name mvbdu =
  let pair_list =
    [ variable_name, Ckappa_sig.state_index_of_int (Bool.to_int bool) ]
  in
  let error, bdu_handler, is_parallel_bond_mvbdu =
    Ckappa_sig.Views_bdu.mvbdu_of_association_list parameters bdu_handler error
      pair_list
  in
  Common_static.mvbdu_and_for_guards parameters bdu_handler error mvbdu
    is_parallel_bond_mvbdu

let print_parallel_constraint ?(verbose = true) ?(sparse = false)
    ?final_resul:(final_result = false) ?(dump_any = false) parameters error
    kappa_handler tuple value bdu_handler =
  let last_variable =
    Ckappa_sig.guard_p_then_site_of_guard
      (Handler.get_nr_guard_parameters kappa_handler)
  in
  let modalite =
    if final_result then
      "are necessarily"
    else
      "may be"
  in
  let prefix = Remanent_parameters.get_prefix parameters in
  let ( error,
        ( string_agent,
          string_site,
          string_site',
          string_agent'',
          string_site'',
          string_site''' ) ) =
    convert_tuple parameters error kappa_handler tuple
  in
  let (agent, site, site', _, _), (agent'', site'', site''', _, _) = tuple in
  let t_precondition = Site_graphs.KaSa_site_graph.empty in
  let error, agent_id, t_precondition =
    Site_graphs.KaSa_site_graph.add_agent parameters error kappa_handler agent
      t_precondition
  in
  let error, t_precondition =
    Site_graphs.KaSa_site_graph.add_bond_type parameters error kappa_handler
      agent_id site agent'' site'' t_precondition
  in
  let error, t_precondition =
    Site_graphs.KaSa_site_graph.add_bond_type parameters error kappa_handler
      agent_id site' agent'' site''' t_precondition
  in
  let error, t_same_self =
    if agent = agent'' && site <> site'' && site' <> site''' then (
      let error, t_same_self =
        Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler
          agent_id site agent_id site'' t_precondition
      in
      error, Some t_same_self
    ) else
      error, None
  in
  let error, agent_id'', t_same =
    Site_graphs.KaSa_site_graph.add_agent parameters error kappa_handler agent''
      t_precondition
  in
  let error, t_distinct_self1 =
    if agent = agent'' && site <> site'' && site <> site' && site' <> site''
    then (
      let error, t_distinct_self1 =
        Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler
          agent_id site' agent_id'' site''' t_same
      in
      let error, t_distinct_self1 =
        Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler
          agent_id site agent_id site'' t_distinct_self1
      in
      error, Some t_distinct_self1
    ) else
      error, None
  in
  let error, t_same =
    Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler agent_id
      site agent_id'' site'' t_same
  in
  let error, t_distinct_self2 =
    if agent = agent'' && site' <> site''' && site' <> site && site <> site'''
    then (
      let error, t_distinct_self2 =
        Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler
          agent_id site' agent_id site''' t_same
      in
      error, Some t_distinct_self2
    ) else
      error, None
  in
  let error, agent_id''', t_distinct =
    Site_graphs.KaSa_site_graph.add_agent parameters error kappa_handler agent''
      t_same
  in
  let error, t_distinct =
    Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler agent_id
      site' agent_id''' site''' t_distinct
  in
  let error, t_same =
    Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler agent_id
      site' agent_id'' site''' t_same
  in
  let list_same = t_same :: cons_opt t_same_self [] in
  let list_distinct =
    t_distinct :: cons_opt t_distinct_self1 (cons_opt t_distinct_self2 [])
  in
  if sparse && compare site site' > 0 then
    error, bdu_handler
  else (
    let error, bdu_handler =
      (*for which values of the guards are all double bonds parallel?*)
      let error, bdu_handler, parallel_bond_mvbdu =
        add_parallel_bond_variable_to_mvbdu parameters bdu_handler error true
          last_variable value
      in
      let error, bdu_handler, is_false =
        Common_static.mvbdu_is_false_for_guards parameters bdu_handler error
          parallel_bond_mvbdu
      in
      if is_false then
        error, bdu_handler
      else (
        match Remanent_parameters.get_backend_mode parameters with
        | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
          let error, bdu_handler =
            if verbose then (
              (*print hyp*)
              let error =
                Site_graphs.KaSa_site_graph.print
                  (Remanent_parameters.get_logger parameters)
                  parameters error t_precondition
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  " => "
              in
              error, bdu_handler
            ) else (
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s" prefix
              in
              error, bdu_handler
            )
          in
          (*print the list of refinement*)
          let error =
            Site_graphs.KaSa_site_graph.print_list
              (Remanent_parameters.get_logger parameters)
              parameters error kappa_handler list_same
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error, bdu_handler
        | Remanent_parameters_sig.Natural_language ->
          if verbose then (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%sWhen the agent %s has its site %s bound to the site %s of a \
                 %s, and its site %s bound to the site %s of a %s, then both \
                 instances of %s %s the same."
                prefix string_agent string_site string_site'' string_agent''
                string_site' string_site''' string_agent'' string_agent''
                modalite
            in
            error, bdu_handler
          ) else (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s" prefix
            in
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t_same
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error, bdu_handler
          )
      )
    in
    let error, bdu_handler =
      (*for which values of the guards are all double bonds non-parallel?*)
      match Remanent_parameters.get_backend_mode parameters with
      | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
        let error, bdu_handler =
          if verbose then (
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t_precondition
            in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameters) " => "
            in
            error, bdu_handler
          ) else (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s"
                (Remanent_parameters.get_prefix parameters)
            in
            error, bdu_handler
          )
        in
        let error =
          Site_graphs.KaSa_site_graph.print_list
            (Remanent_parameters.get_logger parameters)
            parameters error kappa_handler list_distinct
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error, bdu_handler
      | Remanent_parameters_sig.Natural_language ->
        let error, bdu_handler =
          if verbose then (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%sWhen the agent %s has its site %s bound to the site %s of a \
                 %s, and its site %s bound to the site %s of a %s, then both \
                 instances of %s %s  different."
                prefix string_agent string_site string_site'' string_agent''
                string_site' string_site''' string_agent'' string_agent''
                modalite
            in
            error, bdu_handler
          ) else (
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t_distinct
            in
            error, bdu_handler
          )
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error, bdu_handler
      (* for which values of the guards can the double bonds be both parallel and non-parallel? *)
    in
    let error, bdu_handler =
      let error =
        if dump_any then
          if verbose then (
            let () =
              match Remanent_parameters.get_backend_mode parameters with
              | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
                ()
              | Remanent_parameters_sig.Natural_language ->
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%sWhen the agent %s has its site %s bound to the site %s of \
                   a %s, and its site %s bound to the site %s of a %s, then \
                   both instances of %s may be  different or not."
                  prefix string_agent string_site string_site'' string_agent''
                  string_site' string_site''' string_agent'' string_agent''
            in
            error
          ) else (
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t_same
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t_distinct
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error
          )
        else
          error
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      error, bdu_handler
    in
    error, bdu_handler
  )

let add_value_lattice parameters error x value store_result =
  let error, old_value =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs parameters
        error x store_result
    with
    | error, None -> error, Usual_domains.Undefined
    | error, Some v -> error, v
  in
  let new_value = Usual_domains.lub old_value value in
  if compare new_value old_value = 0 then
    error, store_result
  else (
    (*check whether or not if this is a fresh value*)
    let error, store_result =
      PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameters error x
        new_value store_result
    in
    error, store_result
  )

let add_parallel_bond_lattice_variable_to_mvbdu parameters bdu_handler error
    value variable_name mvbdu =
  match value with
  | Usual_domains.Val bool ->
    add_parallel_bond_variable_to_mvbdu parameters bdu_handler error bool
      variable_name mvbdu
  | Usual_domains.Any -> error, bdu_handler, mvbdu
  | Usual_domains.Undefined ->
    Ckappa_sig.Views_bdu.mvbdu_false parameters bdu_handler error

(*TODO merge with above*)
let add_value_mvbdu parameters error x bdu_handler store_result mvbdu
    restriction_mvbdu =
  let error, bdu_handler, mvbdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false parameters bdu_handler error
  in
  let error, old_mvbdu =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs parameters
        error x store_result
    with
    | error, None -> error, mvbdu_false
    | error, Some old_mvbdu -> error, old_mvbdu
  in
  (* let error, bdu_handler, mvbdu_refined =
     add_parallel_bond_variable_to_mvbdu parameters bdu_handler error bool
       last_variable mvbdu
     in *)
  let error, bdu_handler, new_mvbdu =
    Common_static.mvbdu_or_for_guards parameters bdu_handler error old_mvbdu
      mvbdu restriction_mvbdu
  in
  let error, store_result =
    PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameters error x
      new_mvbdu store_result
  in
  error, (bdu_handler, store_result)
(*use at apply_gen*)

(* add value used in parallel_bonds_static.ml, project_away_ag_id *)
let add_value_bool parameters error x bdu_handler bool store_result mvbdu
    restriction_mvbdu last_variable =
  let error, bdu_handler, mvbdu_refined =
    add_parallel_bond_variable_to_mvbdu parameters bdu_handler error bool
      last_variable mvbdu
  in
  add_value_mvbdu parameters error x bdu_handler store_result mvbdu_refined
    restriction_mvbdu

let add_value_and_event parameters error kappa_handler x value store_set
    store_result guard_mvbdu bdu_handler restriction_mvbdu last_variable =
  let error, bdu_handler, value_mvbdu =
    add_parallel_bond_lattice_variable_to_mvbdu parameters bdu_handler error
      value last_variable guard_mvbdu
  in
  let error, bdu_handler, mvbdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false parameters bdu_handler error
  in
  let error, old_value =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs parameters
        error x store_result
    with
    | error, None -> error, mvbdu_false
    | error, Some v -> error, v
  in
  let proj (a, b, _, _, _) = a, b in
  let proj' (a, _, c, _, _) = a, c in
  let pair (x, y) = proj x, proj' x, proj y, proj' y in
  let error, bdu_handler, new_value =
    Common_static.mvbdu_or_for_guards parameters bdu_handler error old_value
      value_mvbdu restriction_mvbdu
  in
  if Ckappa_sig.Views_bdu.equal new_value old_value then
    error, (bdu_handler, store_set, store_result)
  else (
    (*check whether or not if this is a fresh value*)
    let error, bdu_handler =
      if Remanent_parameters.get_dump_reachability_analysis_diff parameters then
        print_parallel_constraint ~verbose:false ~dump_any:true parameters error
          kappa_handler x value_mvbdu bdu_handler
      else
        error, bdu_handler
    in
    (*compute new value only when it is needed*)
    let error, store_result =
      PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameters error x
        new_value store_result
    in
    let error', new_set =
      PairAgentSite_map_and_set.Set.add_when_not_in parameters error (pair x)
        store_set
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    error, (bdu_handler, new_set, store_result)
  )

let project (_, b, c, d, e, f) = b, c, d, e, f
let get_id ((a, _, _, _, _, _), _) = a
let get_tuple (a, b) = project a, project b
let project2 = snd

let add_value_from_refined_tuple parameters error x =
  add_value_lattice parameters error (project2 x)

let swap_sites_in_tuple (a, b, s, s', st, st') = a, b, s', s, st', st

let add_symmetric_tuple_pair f parameters error (x, y) remanent =
  let x' = swap_sites_in_tuple x in
  let y' = swap_sites_in_tuple y in
  List.fold_left
    (fun (error, remanent) t ->
      f parameters error (get_id t, get_tuple t) remanent)
    (error, remanent)
    [ x, y; (*y,x;*) x', y' (*y',x'*) ]
