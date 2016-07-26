(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Jul 02 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "views_domain") message exn
    (fun () -> default)

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

let project_single (_, b, c, d) = (b, c, d)

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

(*convert a site int to string without using information from the agent in
  handler*)

(*todo*)
(*let print_site_accross_domain
    ?verbose:(verbose = true)
    ?sparse: (sparse = false)
    ?final_resul:(final_result = false)
    ?dump_any:(dump_any = false) parameters error kappa_handler handler tuple mvbdu1 mvbdu2 =
  let prefix = Remanent_parameters.get_prefix parameters in
  let (agent_type, site_type, site_type', state_b, state'),
      (agent_type1, site_type1, site_type1', state_b1, state1') = tuple in
  let error, (agent, site, site', state, state',
              agent1, site1, site1', state1, state1') =
    convert_tuple parameters error kappa_handler tuple
  in
  (*state and state1 are a binding states*)
  if sparse && compare site site' > 0
  then error
  else
    let () =
      if verbose
      then
        let _ =
          Loggers.print_newline
            (Remanent_parameters.get_logger parameters)
        in
        let error, handler, list1 =
          Ckappa_sig.Views_bdu.extensional_of_mvbdu
            parameters handler error mvbdu1
        in
        let error, handler, list2 =
          Ckappa_sig.Views_bdu.extensional_of_mvbdu
            parameters handler error mvbdu2
        in
        let error =
          List.fold_left (fun error l ->
              List.fold_left (fun  error (x, y) (*A*)->
              let error, (agentx, sitex, statex) =
                convert_single
                  parameters error kappa_handler
                  (agent_type, x, y)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s (%s: %s, %s:%s) -> %s (%s:%s, %s:%s) : %s:%s:%s\n"
                  agent site state site' state'
                  agent1 site1 state1 site1' state1'
                  agentx sitex statex
              in
              error
                ) error l
            ) error list1
        in
        let () =
          if list1 = []
          then ()
          else Loggers.print_newline
              (Remanent_parameters.get_logger parameters)
        in
        ()
      else
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "test\n";
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error*)

let add_link parameter error bdu_false handler kappa_handler pair
    (mvbdu1, mvbdu2) store_result =
  let error, (bdu_old1, bdu_old2) =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs
        parameter error
        pair
        store_result
    with
    | error, None -> error, (bdu_false, bdu_false)
     | error, Some (bdu1, bdu2) -> error, (bdu1, bdu2)
   in
   (*-----------------------------------------------------------*)
   (*new bdu, union*)
   let error, handler, new_bdu1 =
      Ckappa_sig.Views_bdu.mvbdu_or
        parameter handler error bdu_old1 mvbdu1
   in
   let error, handler, new_bdu2 =
   Ckappa_sig.Views_bdu.mvbdu_or
     parameter handler error bdu_old2 mvbdu2
    in
   (*print each step*)
    (*let error =
     if Remanent_parameters.get_dump_reachability_analysis_diff parameter
     then
       let parameter =
         Remanent_parameters.update_prefix parameter "         "
       in
       print_site_accross_domain
         ~verbose:true
         ~dump_any:true parameter error kappa_handler handler pair
         new_bdu1
         new_bdu2
     else error
    in*)
   let error, store_result =
     PairAgentSitesStates_map_and_set.Map.add_or_overwrite
       parameter error
       pair
       (new_bdu1, new_bdu2)
       store_result
   in
   error, handler, store_result


let swap_sites_in_tuple (a, b, s, s', st, st') = (a, b, s', s, st, st')

let add_symmetric_tuple_pair f parameter error (x, y) remanent =
  let x' = swap_sites_in_tuple x in
  let y' = swap_sites_in_tuple y in
  List.fold_left
    (fun (error, remanent) t ->
       f parameter error t remanent
    ) (error, remanent) [x, y; x', y']
