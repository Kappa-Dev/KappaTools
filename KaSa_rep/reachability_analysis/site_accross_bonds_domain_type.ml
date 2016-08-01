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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Site_accross_bonds_domain_type") message exn
    (fun () -> default)

module AgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name *
            Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state)
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

(************************************************************)
(*PAIR*)

module PairAgentSites_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name) *
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name *
            Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name *
            Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSitesStates_map_and_set = (*REMOVE*)
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

(*-----------------------------------------------------*)

module PairAgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name
            * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSitesState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentsSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

(***************************************************************)

let convert_single_without_state parameters error kappa_handler single =
  let (agent, site) = single in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site)

let convert_single parameters error kappa_handler single =
  let (agent, site, state) = single in
  let error, state = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site state in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site, state)

let convert_double parameters error kappa_handler double =
  let (agent, site, site', state, state') = double in
  let error, state = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site state in
  let error, state' = Handler.string_of_state_fully_deciphered parameters error kappa_handler agent site' state' in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  error, (agent, site, site', state, state')

let convert_tuple parameters error kappa_handler tuple =
  let (agent,site,site'),(agent'',site'',site''') = tuple in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site'' in
  let error, site''' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site''' in
  let error, agent'' = Handler.translate_agent parameters error kappa_handler agent'' in
  error, (agent,site,site', agent'',site'',site''')

(*
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
  error, (agent,site,site',state, state', agent'',site'',site''', state'', state''')*)

(*
let project (_,b,c,d,e,f) = (b,c,d,e,f)
let project2 (x,y) = (project x,project y)*)

(*remove states*)
let project (_,b,c,d,_,_) = (b,c,d)
let project2 (x,y) = (project x,project y)

(*todo*)
let print_site_accross_domain
    ?verbose:(verbose = true)
    ?sparse: (sparse = false)
    ?final_resul:(_final_result = false)
    ?dump_any:(_dump_any = false) parameters error kappa_handler handler tuple mvbdu =
  let prefix = Remanent_parameters.get_prefix parameters in
  let (agent_type, _, site_type), (agent_type1, _, site_type1) = tuple in
  (*----------------------------------------------------*)
  (*state1 and state1' are a binding states*)
  let error, (agent, site, site', agent1, site1, site1') =
    convert_tuple parameters error kappa_handler tuple
  in
  (*----------------------------------------------------*)
  let error, handler, pair_list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu
      parameters handler error mvbdu
  in
  (*----------------------------------------------------*)
  if sparse && pair_list = [] && compare site site' > 0
  then error, handler
  else
    List.fold_left (fun (error, handler) l ->
        match l with
        | [siteone, statex; sitetwo, statey] when
            siteone == Ckappa_sig.fst_site
            && sitetwo == Ckappa_sig.snd_site ->
          let error, (_, _, statex) =
            convert_single parameters error kappa_handler
              (agent_type, site_type, statex)
          in
          let error, (_, _, statey) =
            convert_single parameters error kappa_handler
              (agent_type1, site_type1, statey)
          in
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameters)
              "%sWhenever the site %s of %s and the site %s of %s are bound \
               together, then the site %s of %s and %s of %s can have the \
               following respective states: %s, %s\n"
              prefix site agent site1 agent1
              site' agent site1' agent1
              statex statey
          in
          error, handler
        | [] | _::_ ->
          let error, () =
            warn parameters error (Some "233") Exit ()
          in
          error, handler
      ) (error, handler) pair_list

let add_link parameter error bdu_false handler kappa_handler pair mvbdu
    store_result =
  let error, bdu_old =
    match
      PairAgentSites_map_and_set.Map.find_option_without_logs
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
  (*print each step*)
  let error, handler =
    if Remanent_parameters.get_dump_reachability_analysis_diff parameter
    then
      let parameter = Remanent_parameters.update_prefix parameter "         "
      in
      print_site_accross_domain
        ~verbose:true
        ~dump_any:true parameter error kappa_handler handler pair mvbdu
    else error, handler
  in
  let error, store_result =
    PairAgentSites_map_and_set.Map.add_or_overwrite
      parameter error
      pair
      new_bdu
      store_result
  in
  error, handler, store_result
