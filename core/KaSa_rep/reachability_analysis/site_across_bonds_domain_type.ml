(**
   * site_across_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Dec 22 2018>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(*static views rhs/lhs*)
module AgentsSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state

  let compare = compare
  let print _ _ = ()
end))

(*static question mark*)
module AgentsSitesState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state

  let compare = compare
  let print _ _ = ()
end))

(*views in initial state*)
module AgentsSitesStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state
    * Ckappa_sig.c_state

  let compare = compare
  let print _ _ = ()
end))

(************************************************************)
(*PAIR*)

(*partition bonds/created bond rhs map*)
module PairAgentSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentSitesState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state)

  let compare = compare
  let print _ _ = ()
end))

(*collect tuple in the lhs*)
module PairAgentSitesStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state
    * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state
      * Ckappa_sig.c_state)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentSitesPStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state
    * Ckappa_sig.pair_of_states)
    * (Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state
      * Ckappa_sig.pair_of_states)

  let compare = compare
  let print _ _ = ()
end))

(*-----------------------------------------------------*)

module PairAgentsSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentsSitesState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentsSitesStates_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_state
    * Ckappa_sig.c_state)
    * (Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_state
      * Ckappa_sig.c_state)

  let compare = compare
  let print _ _ = ()
end))

(***************************************************************)
(*Projection*)

module PairSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_site_name * Ckappa_sig.c_site_name

  let compare = compare
  let print _ _ = ()
end))

(*project second site*)
module Proj_potential_tuple_pair_set =
  Map_wrapper.Proj
    (PairAgentSitesState_map_and_set)
    (*potential tuple pair set*)
    (PairSite_map_and_set)
(*use to search the set in bonds rhs*)

module Partition_bonds_rhs_map =
  Map_wrapper.Proj
    (PairAgentSitesState_map_and_set)
    (PairAgentSiteState_map_and_set)

module Partition_created_bonds_map =
  Map_wrapper.Proj
    (PairAgentSitesState_map_and_set)
    (PairAgentSiteState_map_and_set)

(*partition modified map*)
module AgentSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name

  let compare = compare
  let print _ _ = ()
end))

module Partition_modified_map =
  Map_wrapper.Proj (PairAgentSitesState_map_and_set) (AgentSite_map_and_set)

module PairAgentSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)

  let compare = compare
  let print _ _ = ()
end))

(***************************************************************************)
(*PRINT*)
(***************************************************************************)

let convert_single_without_state parameters error kappa_handler single =
  let agent, site = single in
  let error, site =
    Handler.string_of_site_contact_map parameters error kappa_handler agent site
  in
  let error, agent =
    Handler.translate_agent ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  error, (agent, site)

let convert_single parameters error kappa_handler single =
  let agent, site, state = single in
  let error, state =
    Handler.string_of_state_fully_deciphered parameters error kappa_handler
      agent site state
  in
  let error, site =
    Handler.string_of_site_contact_map parameters error kappa_handler agent site
  in
  let error, agent =
    Handler.translate_agent ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  error, (agent, site, state)

let convert_tuple parameters error kappa_handler tuple =
  let (agent, site, site', state), (agent'', site'', site''', state'') =
    tuple
  in
  let error, state =
    Handler.string_of_state_fully_deciphered parameters error kappa_handler
      agent site state
  in
  let error, state'' =
    Handler.string_of_state_fully_deciphered parameters error kappa_handler
      agent'' site'' state''
  in
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
  error, (agent, site, site', state, agent'', site'', site''', state'')

(***************************************************************************)
(*PROJECTION*)
(***************************************************************************)

let project (_, b, c, d, _, _) = b, c, d
let project2 (x, y) = project x, project y

(***************************************************************************)
(*PRINT*)
(***************************************************************************)

let print_site_across_domain ?verbose:(_verbose = true) ?(sparse = false)
    ?(final_result = false) ?dump_any:(_dump_any = false) parameters error
    kappa_handler handler tuple mvbdu =
  let prefix = Remanent_parameters.get_prefix parameters in
  let ( (agent_type1, site_type1, site_type1', _),
        (agent_type2, site_type2, site_type2', _) ) =
    tuple
  in
  (*----------------------------------------------------*)
  (*state1 and state1' are a binding states*)
  let error, (agent1, site1, site1', _, agent2, site2, site2', _) =
    convert_tuple parameters error kappa_handler tuple
  in
  if sparse && compare (agent1, site1, site1') (agent2, site2, site2') > 0 then
    error, handler
  else (
    (*only print the final_result in the case of final_result is set true*)
    let error, handler, non_relational =
      if final_result then
        (*at the final result needs to check the non_relational condition*)
        Translation_in_natural_language.non_relational parameters handler error
          mvbdu
      else
        (*other cases will by pass this test*)
        error, handler, false
    in
    if non_relational then
      error, handler
    else (
      (*----------------------------------------------------*)
      let error, handler, pair_list =
        Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error mvbdu
      in
      (*----------------------------------------------------*)
      match Remanent_parameters.get_backend_mode parameters with
      | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
        let pattern = Site_graphs.KaSa_site_graph.empty in
        let error, agent_id1, pattern =
          Site_graphs.KaSa_site_graph.add_agent parameters error kappa_handler
            agent_type1 pattern
        in
        let error, agent_id2, pattern =
          Site_graphs.KaSa_site_graph.add_agent parameters error kappa_handler
            agent_type2 pattern
        in
        let error, pattern =
          Site_graphs.KaSa_site_graph.add_bond parameters error kappa_handler
            agent_id1 site_type1 agent_id2 site_type2 pattern
        in
        let error =
          (*do not print the precondition if it is not the final result*)
          if final_result then (
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error pattern
            in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameters) " => "
            in
            error
          ) else
            error
        in
        (match pair_list with
        | [] ->
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameters) ""
          in
          error, handler
        | _ :: _ ->
          let () =
            if final_result then (
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              Loggers.fprintf (Remanent_parameters.get_logger parameters) "\t["
            ) else
              ()
          in
          let error, _ =
            List.fold_left
              (fun (error, bool) l ->
                match l with
                | [ (siteone, state1); (sitetwo, state2) ]
                  when siteone == Ckappa_sig.fst_site
                       && sitetwo == Ckappa_sig.snd_site ->
                  let () =
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters)
                  in
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      (if bool then
                         "\t\tv "
                       else if final_result then
                         "\t\t  "
                       else
                         "\t\t")
                  in
                  let error, pattern =
                    Site_graphs.KaSa_site_graph.add_state parameters error
                      kappa_handler agent_id1 site_type1' state1 pattern
                  in
                  let error, pattern =
                    Site_graphs.KaSa_site_graph.add_state parameters error
                      kappa_handler agent_id2 site_type2' state2 pattern
                  in
                  let error =
                    Site_graphs.KaSa_site_graph.print
                      (Remanent_parameters.get_logger parameters)
                      parameters error pattern
                  in
                  error, true
                | _ -> Exception.warn parameters error __POS__ Exit bool)
              (error, false) pair_list
          in
          let () =
            if final_result then (
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "\t]"
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              ()
            )
          in
          error, handler)
      | Remanent_parameters_sig.Natural_language ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%sWhenever the site %s of %s and the site %s of %s are bound \
             together, then the site %s of %s and %s of %s can have the \
             following respective states:"
            prefix site1 agent1 site2 agent2 site1' agent1 site2' agent2
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let prefix = prefix ^ "\t" in
        List.fold_left
          (fun (error, handler) l ->
            match l with
            | [ (siteone, statex); (sitetwo, statey) ]
              when siteone == Ckappa_sig.fst_site
                   && sitetwo == Ckappa_sig.snd_site ->
              let error, (_, _, statex) =
                convert_single parameters error kappa_handler
                  (agent_type1, site_type1, statex)
              in
              let error, (_, _, statey) =
                convert_single parameters error kappa_handler
                  (agent_type2, site_type2, statey)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s%s, %s\n" prefix statex statey
              in
              error, handler
            | [] | _ :: _ ->
              let error, () = Exception.warn parameters error __POS__ Exit () in
              error, handler)
          (error, handler) pair_list
    )
  )

(***************************************************************************)
(***************************************************************************)

let get_mvbdu_from_tuple_pair parameters error tuple bdu_false store_value =
  let error, mvbdu_value =
    match
      PairAgentSitesState_map_and_set.Map.find_option_without_logs parameters
        error tuple store_value
    with
    | error, None -> error, bdu_false
    | error, Some mvbdu -> error, mvbdu
  in
  error, mvbdu_value

let add_link parameter error bdu_false handler kappa_handler pair mvbdu
    store_result =
  let error, bdu_old =
    get_mvbdu_from_tuple_pair parameter error pair bdu_false store_result
  in
  (*-----------------------------------------------------------*)
  (*new bdu, union*)
  let error, handler, new_bdu =
    Ckappa_sig.Views_bdu.mvbdu_or parameter handler error bdu_old mvbdu
  in
  (*print each step*)
  let error, handler =
    if Remanent_parameters.get_dump_reachability_analysis_diff parameter then (
      let parameter =
        Remanent_parameters.update_prefix parameter "                "
      in
      print_site_across_domain ~verbose:false ~dump_any:true parameter error
        kappa_handler handler pair mvbdu
    ) else
      error, handler
  in
  let error, store_result =
    PairAgentSitesState_map_and_set.Map.add_or_overwrite parameter error pair
      new_bdu store_result
  in
  error, handler, store_result

let add_sites_from_tuples parameters error tuple modified_sites =
  let (agent, site1, site2, _), (agent', site1', site2', _) = tuple in
  List.fold_left
    (fun (error, modified_sites) (agent, site) ->
      Communication.add_site parameters error agent site modified_sites)
    (error, modified_sites)
    [ agent, site1; agent, site2; agent', site1'; agent', site2' ]

let check parameters error bdu_false handler pair mvbdu store_result =
  let error, bdu_old =
    get_mvbdu_from_tuple_pair parameters error pair bdu_false store_result
  in
  let error, handler, new_bdu =
    Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu_old mvbdu
  in
  if Ckappa_sig.Views_bdu.equal new_bdu bdu_false then
    error, handler, false
  else
    error, handler, true

let add_link_and_check parameter error bdu_false handler kappa_handler bool
    dump_title x mvbdu modified_sites store_result =
  let error, bdu_old =
    get_mvbdu_from_tuple_pair parameter error x bdu_false store_result
  in
  (*-----------------------------------------------------------*)
  (*new bdu, union*)
  let error, handler, new_bdu =
    Ckappa_sig.Views_bdu.mvbdu_or parameter handler error bdu_old mvbdu
  in
  (*-----------------------------------------------------------*)
  (*check the freshness of the pair*)
  (*compare mvbdu and old mvbdu*)
  if Ckappa_sig.Views_bdu.equal new_bdu bdu_old then
    error, bool, handler, modified_sites, store_result
  else (
    (*-----------------------------------------------------------*)
    (*print each step*)
    let error, handler =
      if Remanent_parameters.get_dump_reachability_analysis_diff parameter then (
        let parameter = Remanent_parameters.update_prefix parameter "\t\t" in
        let () =
          if bool then
            ()
          else
            dump_title ()
        in
        print_site_across_domain ~verbose:false ~dump_any:true parameter error
          kappa_handler handler x mvbdu
      ) else
        error, handler
    in
    let error, store_result =
      PairAgentSitesState_map_and_set.Map.add_or_overwrite parameter error x
        new_bdu store_result
    in
    let error', modified_sites =
      add_sites_from_tuples parameter error x modified_sites
    in
    let error =
      Exception.check_point Exception.warn parameter error error' __POS__ Exit
    in
    error, true, handler, modified_sites, store_result
  )
