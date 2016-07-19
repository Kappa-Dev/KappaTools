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

let convert_single parameters error kappa_handler single =
  let (agent, site, state) = single in
  let error, state = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site state in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site, state)

let convert_pair_single parameters error kappa_handler pair =
  let (agent, site, state), (agent', site', state') = pair in
  let error, (agent, site, state) = convert_single parameters error kappa_handler (agent, site, state) in
  let error, (agent', site', state') = convert_single parameters error kappa_handler (agent', site', state') in
  error, (agent, site, state, agent', site', state')
  
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
    ?final_resul:(final_result = false) 
    ?dump_any:(dump_any = false) parameters error kappa_handler tuple =
  let prefix = Remanent_parameters.get_prefix parameters in
   let error, (agent1, site1, site2, state1, state2, agent1', site1', site2', state1', state2') =
    convert_tuple parameters error kappa_handler tuple
  in
  if sparse && compare site1 site2 > 0
  then error
  else
    let () =
      if verbose
      then
        let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameters)
            "%s %s.%s and %s.%s is equal to %s,%s \
            when %s.%s is connected to %s.%s\n"
            prefix 
            agent1 site1
            agent1' site1'
            state1 state1'
            agent1 site2
            agent1' site2'            
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      else 
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "test\n";
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error
