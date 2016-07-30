(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Aug 06 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

module PairStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_state * Ckappa_sig.c_state
            * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSites_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name) *
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSites_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name)
         let compare = compare
         let print _ _ = ()
       end))

module AgentSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name
            * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name
            * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

(*PAIR*)
module PairAgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSite_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSites_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name )
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSitesState_Sites_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSites_SitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

let convert_single_without_state parameters error kappa_handler single =
  let (agent, site) = single in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site)

let convert_pair_single_without_state parameters error kappa_handler pair =
  let (agent, site), (agent', site') = pair in
  let error, (agent, site) = convert_single_without_state parameters error kappa_handler (agent, site) in
  let error, (agent', site') = convert_single_without_state parameters error kappa_handler (agent', site') in
  error, (agent, site, agent', site')

let convert_without_state parameters error kappa_handler single =
  let (agent, site, site1) = single in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site1 = Handler.string_of_site_contact_map parameters error kappa_handler agent site1 in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site, site1)

let convert_pair_without_state parameters error kappa_handler pair =
  let (agent, site, site1), (agent', site', site1') = pair in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site1 = Handler.string_of_site_contact_map parameters error kappa_handler agent site1 in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent' site' in
  let error, site1' = Handler.string_of_site_contact_map parameters error kappa_handler agent' site1' in
  let error, agent' = Handler.translate_agent parameters error kappa_handler agent' in
  error, (agent, site, site1, agent', site', site1')

let convert_single parameters error kappa_handler single =
  let (agent, site, state) = single in
  let error, state = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site state in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site, state)

let convert_pair_single parameters error kappa_handler pair =
  let (agent, site, site', state), (agent', site1', site'', state') = pair in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent' site'' in
  let error, (agent, site, state) = convert_single parameters error kappa_handler (agent, site, state) in
  let error, (agent', site1', state') = convert_single parameters error kappa_handler (agent', site1', state') in
  error, (agent, site, site', state, agent', site1', site'', state')

let convert_double parameters error kappa_handler double =
  let (agent, site, site', state, state') = double in
  let error, state = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site state in
  let error, state' = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site' state' in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site, site', state, state')

let convert_tuple parameters error kappa_handler tuple =
  let (agent,site,site',state,state'),(agent'',site'',site''',state'',state''') =
    tuple
  in
  let error, state = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site state in
  let error, state' = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site' state' in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  (**)
  let error, state'' =
    Handler.string_of_state_fully_deciphered parameters error kappa_handler agent'' site'' state'' in
  let error, state''' =
    Handler.string_of_state_fully_deciphered parameters error kappa_handler agent'' site''' state''' in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site'' in
  let error, site''' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site''' in
  let error, agent'' = Handler.translate_agent parameters error kappa_handler agent'' in
  error, (agent,site,site',state, state', agent'',site'',site''', state'', state''')

let project (_,b,c,d,e,f) = (b,c,d,e,f)
let project2 (x,y) = (project x,project y)

(*todo*)
let print_site_accross_domain
    ?verbose:(verbose = true)
    ?sparse: (sparse = false)
    ?final_resul:(_final_result = false)
    ?dump_any:(_dump_any = false) parameters error kappa_handler handler tuple mvbdu =
  let prefix = Remanent_parameters.get_prefix parameters in
  let (agent_type, _site_type, _, _, _),
      (agent_type', _site_type', _, _, _) = tuple in
  (*state1 and state1' are a binding states*)
  let error, (agent1, site1, site2, _state1, state2,
              agent1', site1', site2', _state1', state2') =
    convert_tuple parameters error kappa_handler tuple
  in
  if sparse && compare site1 site2 > 0
  then error, handler
  else
      if verbose
      then
        let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameters)
            "%s %s.%s and %s.%s is equal to %s,%s \
             when %s.%s is connected to %s.%s\n"
            prefix
            (*internal sites are site2*)
            agent1 site2  agent1' site2'
            (*its internal states are*)
            state2 state2'
            (*when its binding sites*)
            agent1 site1
            agent1' site1'
        in
        let error, handler, pair_list =
          Ckappa_sig.Views_bdu.extensional_of_mvbdu
            parameters handler error mvbdu
        in
        let l = List.flatten pair_list in
        let error, list1 =
          List.fold_left (fun (error, current_list) (x, y) ->
              let error, (agentx, sitex, statex) =
                convert_single
                  parameters error kappa_handler
                  (agent_type, x, y)
              in
              let list1 =
                (agentx, sitex, statex) :: current_list
              in
              error, list1
            ) (error, []) l
        in
        let error, list2 =
          List.fold_left (fun (error, current_list) (x, y) ->
              let error, (agentx, sitex, statex) =
                convert_single
                  parameters error kappa_handler
                  (agent_type', x, y)
              in
              let list2 =
                (agentx, sitex, statex) :: current_list
              in
              error, list2
            ) (error, []) l
        in
        let error =
          List.fold_left (fun error (agentx, sitex, statex) ->
              List.fold_left (fun error (agenty, sitey, statey) ->
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters)
                      "Whenever the site %s of %s and the site %s of %s are bound together, then the site %s of %s and %s of %s can have the following respective states: %s, %s\n"
                      site1 agent1 site1' agent1'
                      site2 agent1 site2' agent1'
                      statex statey
                  in
                  error

                ) error list2
            ) error list1
        in

        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error, handler
      else
        let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameters)
            "test\n" in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error, handler


let add_link parameter error bdu_false handler kappa_handler pair mvbdu
    store_result =
  let error, bdu_old =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs
        parameter error
        pair
        store_result
    with
    | error, None -> error, bdu_false
    | error, Some bdu -> error, bdu
  in
  (*-----------------------------------------------------------*)
  (*new bdu, union*)
  let error, handler, new_bdu =
    Ckappa_sig.Views_bdu.mvbdu_or
      parameter handler error bdu_old mvbdu
  in
  (*TODO: print each step*)
  (*let error, handler =
    if Remanent_parameters.get_dump_reachability_analysis_diff parameter
    then
      let parameter = Remanent_parameters.update_prefix parameter "         "
      in
      print_site_accross_domain
        ~verbose:true
        ~dump_any:true parameter error kappa_handler handler pair mvbdu
    else error, handler
  in*)
  let error, store_result =
    PairAgentSitesStates_map_and_set.Map.add_or_overwrite
      parameter error
      pair
      new_bdu
      store_result
  in
  error, handler, store_result


let swap_sites_in_tuple (a, b, s, s') = (a, b, s', s)

let add_symmetric_tuple_pair f parameter error (x, y) remanent =
  let x' = swap_sites_in_tuple x in
  let y' = swap_sites_in_tuple y in
  List.fold_left
    (fun (error, remanent) t ->
       f parameter error t remanent
    ) (error, remanent) [x, y; x', y']
