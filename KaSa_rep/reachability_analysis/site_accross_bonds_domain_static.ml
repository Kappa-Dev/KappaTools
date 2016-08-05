(**
   * site_accross_bonds_domain_static.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 29th of June
   * Last modification: Time-stamp: <Aug 06 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Site accross domain static information") message exn
    (fun () -> default)

let local_trace = false

(***************************************************************)
(*type*)

type basic_static_information =
  {
    store_views_rhs : Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    store_bonds_rhs: Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    store_bonds_lhs :
    Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*a set of site that are bonds on the rhs without rule_id§*)
    (*------------------------------------------------------------------*)
    store_bonds_rhs_set :
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Set.t;
    (*------------------------------------------------------------------*)
    store_modified_map :
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    (*a map of rule that store a set of sites created a bond*)
    store_created_bonds :
    Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    store_potential_tuple_pair : (* JF: it does not make sense *)
      (* agent_ids makes sense only in the contact of a given rule *)
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    (*store_potential_tuple_pair set*)
    store_potential_tuple_pair_set :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t;
    (*------------------------------------------------------------------*)
    (*potential tuple pair has the first site belongs to rule has a site
      can created a bond *)
    store_potential_tuple_pair_created_bonds :
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;
    (*------------------------------------------------------------------*)
    (*projection the agent_id and second site*)
    store_potential_tuple_pair_created_bonds_proj1 :
      Site_accross_bonds_domain_type.PairAgentIDSite_map_and_set.Set.t;
    (*------------------------------------------------------------------*)
    store_potential_tuple_pair_created_bonds_proj2 :
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Set.t;
    (*------------------------------------------------------------------*)
    (*project map*)
    store_proj_map1 :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
        Site_accross_bonds_domain_type.PairAgentSite_map_and_set.Map.t;
    store_proj_reverse_map1 :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
        Site_accross_bonds_domain_type.PairAgentSite_map_and_set.Map.t;
    store_proj_map2 :
    Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;
    store_proj_reverse_map2 :
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.t
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.t;

    (*store_created_bond_with_potential_pair :
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    (**)
    store_modified_internal_state_and_bond : (*REMOVE*)
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_question_marks_rhs :
      Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
    store_implicit_static :
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t; (*TODO: do I need state?*)*)
  }


(* JF: I do not see the information that you need to perform computations in the abstract domain *)
(* You need (at least) four maps :
   PairAgentSites_map_and_set.Set.t PairAgentSite_map_and_set.Map.t
   this Map maps the pair ((ag,site),(ag',site')) to the set of tuples of interest of the form:
   ((ag,site,_,_,_),(ag',site',_,_,_))

   PairAgentSites_map_and_set.Set.t PairAgentSite_map_and_set.Map.t
   maps the pair ((ag,site),(ag',site')) to the set of tuples of interest of the form:
   ((ag',site',_,_,_),(ag,site,_,_,_))

   PairAgentSites_map_and_set.Set.t AgentSite_map_and_set.Map.t
   maps the pair (ag,site) to the set of tuples of interest of the form:
   ((ag',_,site',_,_),_)

   PairAgentSites_map_and_set.Set.t AgentSite_map_and_set.Map.t
   maps the pair (ag,site) to the set of tuples of interest of the form:
   (_,(ag,_,site,_,_))

   I have not found the module declaration for PairAgentSite_map_and_set (type t = (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) )

   nor for AgentSite_map_and_set (type t = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name )

   To compute this four maps, you have to use the function partition_set.
*)
(****************************************************************)
(*Init*)
(****************************************************************)

let init_basic_static_information =
  {
    store_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_bonds_rhs_set =
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Set.empty;
    store_modified_map = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_created_bonds = Ckappa_sig.Rule_map_and_set.Map.empty;
    (*-------------------------------------------------------*)
    store_potential_tuple_pair = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_potential_tuple_pair_set =
      Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.empty;
    (*-------------------------------------------------------*)
    store_potential_tuple_pair_created_bonds = Ckappa_sig.Rule_map_and_set.Map.empty;
    (*-------------------------------------------------------*)
    store_potential_tuple_pair_created_bonds_proj1 =
      Site_accross_bonds_domain_type.PairAgentIDSite_map_and_set.Set.empty;
    store_potential_tuple_pair_created_bonds_proj2 =
      Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Set.empty;
    (*-------------------------------------------------------*)
    (*project map*)
    store_proj_map1 =
      Site_accross_bonds_domain_type.PairAgentSite_map_and_set.Map.empty;
    store_proj_reverse_map1 =
      Site_accross_bonds_domain_type.PairAgentSite_map_and_set.Map.empty;
    store_proj_map2 =
      Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    store_proj_reverse_map2 =
        Site_accross_bonds_domain_type.AgentSite_map_and_set.Map.empty;
    (*store_created_bond_with_potential_pair =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_modified_internal_state_and_bond =
      Ckappa_sig.Rule_map_and_set.Map.empty;
    store_question_marks_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
    store_implicit_static = Ckappa_sig.Rule_map_and_set.Map.empty;*)
  }

(***************************************************************)
(*collect views on the right hand side*)

let get_set parameter error rule_id empty_set store_result =
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

let collect_views_rhs parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Unknown_agent _
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           (*--------------------------------------------*)
           (*old set*)
           let error, old_set =
             get_set parameter error rule_id
               Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
               store_result
           in
           (*--------------------------------------------*)
           let error', new_set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, state)
                      store_set
                  in
                  error, store_set
               ) agent.Cckappa_sig.agent_interface
               (error, old_set)
           in
           let error = Exception.check warn parameter error error'
               (Some "line 109") Exit
           in
           (*--------------------------------------------*)
           (*store*)
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
               parameter error rule_id
               new_set
               store_result
           in
           error, store_result
      ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.views store_result
  in
  error, store_result

(***************************************************************)
(*collect rule that can be bound on the right hand side*)

let collect_agent_type_state parameter error agent site_type =
  let dump_agent = Ckappa_sig.dummy_agent_name in
  let dump_state = Ckappa_sig.dummy_state_index in
  match agent with
  | Cckappa_sig.Ghost
  | Cckappa_sig.Unknown_agent _ -> error, (dump_agent, dump_state)
  | Cckappa_sig.Dead_agent _ ->
    warn parameter error (Some "line 127") Exit
      (dump_agent, dump_state)
  | Cckappa_sig.Agent agent1 ->
    let agent_type1 = agent1.Cckappa_sig.agent_name in
    let error, state1 =
      match Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
              parameter
              error
              site_type
              agent1.Cckappa_sig.agent_interface
      with
      | error, None ->
        warn parameter error (Some "line 228") Exit dump_state
      | error, Some port ->
        let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
        if (Ckappa_sig.int_of_state_index state) > 0
        then
          error, state
        else
          warn parameter error (Some "line 196") Exit dump_state
    in
    error, (agent_type1, state1)

(***************************************************************)

let collect_bonds parameter error rule_id views bonds store_result =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter error
    (fun parameter error agent_id bonds_map store_result ->
       Ckappa_sig.Site_map_and_set.Map.fold
         (fun site_type_source site_add (error, store_result) ->
            let agent_id_target = site_add.Cckappa_sig.agent_index in
            let site_type_target = site_add.Cckappa_sig.site in
            let error, agent_source =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id views
              with
              | error, None -> warn parameter error (Some "line 332") Exit
                                 Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            (*----------------------------------------------------*)
            (*the first pair*)
            let error, (agent_type1, state1) =
              collect_agent_type_state
                parameter
                error
                agent_source
                site_type_source
            in
            (*----------------------------------------------------*)
            (*the second pair*)
            let error, agent_target =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id_target views
              with
              | error, None -> warn parameter error (Some "line 350") Exit
                                 Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            let error, (agent_type2, state2) =
              collect_agent_type_state
                parameter
                error
                agent_target
                site_type_target
            in
            (*-----------------------------------------------------*)
            (*get old set*)
            let error, old_set =
              get_set parameter error rule_id
                Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
                store_result
            in
            let error', new_set =
              Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                parameter error
                ((agent_id, agent_type1, site_type_source, state1),
                 (agent_id_target, agent_type2, site_type_target, state2))
                old_set
            in
            let error =
              Exception.check warn parameter error error' (Some "line 197") Exit in
            let error, store_result =
              Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                parameter
                error
                rule_id
                new_set
                store_result
            in
            error, store_result
         ) bonds_map (error, store_result)
    ) bonds store_result

let collect_bonds_rhs parameter error rule_id rule store_result =
  collect_bonds parameter error rule_id
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
    store_result

let collect_bonds_lhs parameter error rule_id rule store_result =
  collect_bonds parameter error rule_id
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
    store_result

(***************************************************************)
(*use the projection of set*)

let collect_bonds_rhs_set parameter error rule_id store_bonds_rhs =
  let error, bonds_rhs_set =
    get_set parameter error rule_id
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
      store_bonds_rhs
  in
  (*using project_set*)
  let proj (_, b, c, d) = (b, c, d) in
  let error, new_set =
    Site_accross_bonds_domain_type.Proj_agent_id_away1.monadic_proj_set
      (fun _parameter error (x, y) ->
         error, (proj x, proj y)
      )
      parameter
      error
      bonds_rhs_set (*set_a*)
  in
  error, new_set

(***************************************************************)
(*collect rule that can be modified*)

let collect_site_modified parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun parameter error agent_id agent store_result->
         (*if there is no modified sites then do nothing*)
         if Ckappa_sig.Site_map_and_set.Map.is_empty agent.Cckappa_sig.agent_interface
         then error, store_result
         else
           let agent_type = agent.Cckappa_sig.agent_name in
           (*old set*)
           let error, old_set =
             get_set parameter error rule_id
               Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
               store_result
           in
           let error', new_set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, state)
                      store_set
                  in
                  error, store_set
               )
               agent.Cckappa_sig.agent_interface
               (error, old_set)
           in
           let error = Exception.check warn parameter error error' (Some "line 244") Exit in
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

(***************************************************************)
(*collect a set of tuple pair (A.x.y, B.z.t)*)

let collect_pair_sites_aux parameter error rule_id store_views_rhs =
  let error, views_rhs_set =
    get_set parameter error rule_id
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      store_views_rhs
  in
  (*fold over on the rhs*)
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv
      (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
         let error, old_set =
           get_set parameter error rule_id
             Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
             store_result
         in
         (*fold again views rhs*)
         let error, new_set =
          Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv
            (fun (agent_id', _, site_type', state') (error, current_set) ->
               if agent_id = agent_id' && site_type <> site_type'
               then
                let error', new_set =
                  Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                    parameter error
                    (agent_id, agent_type, site_type, site_type', state, state')
                    current_set
                in
                let error = Exception.check warn parameter error error' (Some "line 244") Exit in
                error, new_set
              else error, current_set
            ) views_rhs_set (error, old_set)
        in
        let error, store_result =
          Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
            parameter error rule_id new_set store_result
        in
        error, store_result
      ) views_rhs_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
  in
  error, store_result

let collect_potential_tuple_pair parameter error _kappa_handler rule_id store_pair_rhs store_result =
  let error, store_pair_set =
    get_set parameter error rule_id
      Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
      store_pair_rhs
  in
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
      (fun x (error, store_result) ->
         let (agent_id, _, _, _, _, _) = x in (*A*)
         let error, store_result =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
             (fun y (error, store_result) ->
                let (agent_id1, _, _, _, _, _) = y in
                if agent_id <> agent_id1
                then
                  let error, old_set =
                    get_set parameter error rule_id
                      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
                      store_result
                  in
                  let error', new_set =
                    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                      (x, y)
                      old_set
                  in
                  let error = Exception.check warn parameter error error' (Some "line 433") Exit in
                  let error, store_result =
                    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                      parameter error
                      rule_id
                      new_set
                      store_result
                  in
                  error, store_result
                else error, store_result
             ) store_pair_set (error, store_result)
         in
         error, store_result
      ) store_pair_set (error, store_result)
  in
  error, store_result

let collect_potential_tuple_pair_set parameter error rule_id store_potential_tuple_pair =
  let error,  potential_tuple_pair_set =
    get_set parameter error
      rule_id
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
      store_potential_tuple_pair
  in
  let proj (_,b, c, d, e, f) = (b,c, d,e,f) in
  let error, new_set =
    Site_accross_bonds_domain_type.Proj_agent_id_away2.monadic_proj_set
      (fun _parameter error (x, y) ->
         error,
         (proj x, proj y)
      )
      parameter
      error
      potential_tuple_pair_set
  in
  error, new_set

(***************************************************************)
(*collect a map of rule that store a set of sites can created bonds*)

let collect_created_bonds parameter error rule rule_id store_result =
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
      let agent_id = site_add1.Cckappa_sig.agent_index in
      let site_type = site_add1.Cckappa_sig.site in
      let error, agent =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error
            agent_id
            rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None -> warn parameter error (Some "line 470") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, (agent_type, state) =
        collect_agent_type_state
          parameter
          error
          agent
          site_type
      in
      (*------------------------------------------------------------*)
      (*second agent*)
      let agent_id1 = site_add2.Cckappa_sig.agent_index in
      let site_type1 = site_add2.Cckappa_sig.site in
      let error, agent1 =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error
            agent_id1
            rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None -> warn parameter error (Some "line 470") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, (agent_type1, state1) =
        collect_agent_type_state
          parameter
          error
          agent1
          site_type1
      in
      (*------------------------------------------------------------*)
      let error, old_set =
        get_set parameter error rule_id
          Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
          store_result
      in
      let pair =
        (agent_id, agent_type, site_type, state),
        (agent_id1, agent_type1, site_type1, state1)
      in
      let error', new_set =
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
          parameter error
          pair
          old_set
      in
      let error = Exception.check warn parameter error error' (Some "line 488") Exit in
      (*------------------------------------------------------------*)
      let error, store_result =
        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
          parameter error
          rule_id
          new_set
          store_result
      in
      error, store_result
  )(error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind

(***************************************************************)
(*collect rule that created bond*)

let collect_potential_tuple_pair_created_bonds parameter error rule_id
    store_created_bonds store_potential_tuple_pair store_result =
  let error, created_bonds_set =
    get_set parameter error rule_id
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
      store_created_bonds
  in
  (*------------------------------------------------------------*)
  let error, potential_tuple_pair_set =
    get_set parameter error rule_id
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
      store_potential_tuple_pair
  in
  (*------------------------------------------------------------*)
  let proj1 (a, b, c, _, e, _) = (a, b, c, e) in
  let proj2 (a, b, _, d, _, f) = (a, b, d, f) in
  (*------------------------------------------------------------*)
  (*fold over a potential_tuple_pair_set*)
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       let pair_bond = proj1 x, proj2 y in
       let reverse_pair_bond = proj2 y, proj1 x in
       (*------------------------------------------------------------*)
       (*check if the first one belong to created_bonds_set*)
       if
         Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
           pair_bond
           created_bonds_set
         ||
         Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
           reverse_pair_bond
           created_bonds_set
       (*------------------------------------------------------------*)
       then
         let error, old_set =
           get_set parameter error rule_id
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
             store_result
         in
         let error', new_set =
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
             parameter error
             (x, y)
             old_set
         in
         let error = Exception.check warn parameter error error' (Some "line 603") Exit in
         (*------------------------------------------------------------*)
         let error, store_result =
           Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
             parameter error
             rule_id
             new_set
             store_result
         in
         error, store_result
       else
         error, store_result
    ) potential_tuple_pair_set (error, store_result)

(*-------------------------------------------------------*)
(*project the agent_id and second site in the pair,
  this will be used to get a list of state in the precondition*)

let collect_potential_tuple_pair_created_bonds_proj1 parameter error rule_id
    store_potential_tuple_pair_created_bonds =
  let error, potential_tuple_pair_created_bonds_set =
    get_set parameter error
      rule_id
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
      store_potential_tuple_pair_created_bonds
  in
  let proj (a, _, _, d, _, _) = (a, d) in
  let error, new_set =
    Site_accross_bonds_domain_type.Proj_get_agent_id_snd_site.monadic_proj_set
      (fun _parameter error (x, y) ->
         error,
         (proj x, proj y)
      )
      parameter
      error
      potential_tuple_pair_created_bonds_set
  in
  error, new_set

(*-------------------------------------------------------*)
(*project the tuple pair return in the result*)

let collect_potential_tuple_pair_created_bonds_proj2 parameter error rule_id
    store_potential_tuple_pair_created_bonds =
  let error, potential_tuple_pair_created_bonds_set =
    get_set parameter error
      rule_id
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
      store_potential_tuple_pair_created_bonds
  in
  let proj (_, b, c, _, e, _) = (b, c, e) in
  let error, new_set =
    Site_accross_bonds_domain_type.Proj_get_agents_sites.monadic_proj_set
      (fun _parameter error (x, y) ->
         error, (proj x, proj y)
      )
      parameter
      error
      potential_tuple_pair_created_bonds_set
  in
  error, new_set

(****************************************************************)
(*project map*)

let collect_proj_map1 parameter error store_potential_tuple_pair_set =
  let proj (b, c, _, _, _) = (b,c) in
  (*set_a map_b*)
  Site_accross_bonds_domain_type.Proj_map1.monadic_partition_set
    (fun parameter error (x, y) ->
       (*map_b*)
       error, (proj x, proj y)
    )
    parameter
    error
    store_potential_tuple_pair_set (*set_a*)

(*-------------------------------------------------------*)

let collect_proj_reverse_map1 parameter error store_potential_tuple_pair_set =
let proj (b, c, _, _, _) = (b,c) in
(*set_a map_b*)
Site_accross_bonds_domain_type.Proj_map1.monadic_partition_set
  (fun parameter error (x, y) ->
     (*map_b*)
     error, (proj y, proj x)
  )
  parameter
  error
  store_potential_tuple_pair_set (*set_a*)

(*-------------------------------------------------------*)

let collect_proj_map2 parameter error store_potential_tuple_pair_set =
  let proj (b, _, d, _, _) = (b,d) in
  (*set_a map_b*)
  Site_accross_bonds_domain_type.Proj_map2.monadic_partition_set
    (fun parameter error (x, _) ->
       (*map_b*)
       error, (proj x)
    )
    parameter
    error
    store_potential_tuple_pair_set (*set_a*)

(*-------------------------------------------------------*)

let collect_proj_reverse_map2 parameter error store_potential_tuple_pair_set =
  let proj (b, _, d, _, _) = (b,d) in
  (*set_a map_b*)
  Site_accross_bonds_domain_type.Proj_map2.monadic_partition_set
    (fun parameter error (_, y) ->
       (*map_b*)
       error, (proj y)
    )
    parameter
    error
    store_potential_tuple_pair_set (*set_a*)

(*
let collect_created_bond_with_potential_pair parameter error rule_id
    rule store_tuple_pair store_result =
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
        (fun (x, y) (error, store_result) ->
           let (agent_id, _agent_type, site_type, _, _state) = x in
           let agent_id1 = site_add1.Cckappa_sig.agent_index in
           let site_type1 = site_add1.Cckappa_sig.site in
           (*second agent*)
           let (agent_id', _agent_type', site_type', _, _state') = y in
           let agent_id2 = site_add2.Cckappa_sig.agent_index in
           let site_type2 = site_add2.Cckappa_sig.site in
           (*if the first site in the pair is the site that created a bound*)
           if (agent_id = agent_id1 && site_type = site_type1 &&
               agent_id2 = agent_id' && site_type' = site_type2) ||
              (agent_id1 = agent_id' && site_type' = site_type1 &&
               agent_id2 = agent_id &&  site_type = site_type2)
           then
             let error, old_set =
               match
                 Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                   parameter error rule_id store_result
               with
               | error, None ->
                 error,
                 Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty
               | error, Some s -> error, s
             in
             let error', new_set =
               Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in
                 parameter error
                 (x, y)
                 old_set
             in
             let error = Exception.check warn parameter error error' (Some "line 410") Exit in
             let error, store_result =
               Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                 parameter
                 error
                 rule_id
                 new_set
                 store_result
             in
             error, store_result
           else error, store_result
        ) store_tuple_pair (error, store_result)
    )(error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind

(***************************************************************)
(*the first site can be bound and the second site can be modified*)

let collect_modified_internal_and_bond parameter error rule_id
    store_tuple_pair store_bonds_rhs store_modified_map store_result =
  let empty_set =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
  in
  (*------------------------------------------------------------*)
  (*set of bonds on the rhs*)
  let error, store_bond_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_bonds_rhs
    with
    | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*------------------------------------------------------------*)
  (*set of sites can be modified*)
  let error, store_modified_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_modified_map
    with
    | error, None ->
      error,
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*------------------------------------------------------------*)
  Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
    (fun (x, y) (error, store_result) ->
       let (agent_id, agent_type, site_type, site_type2, _) = x in
       let (agent_id', agent_type', site_type', site_type2', _) = y in
       (*the second site belong to modified and the first site belong to the bond set*)
       let error, store_result =
         Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
           (fun m (error, store_result) ->
              (*bonds on the rhs*)
              Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
                (fun (t, r) (error, store_result) ->
                   let (agent_id_m, _, site_type_m, state_m) = m in
                   let (agent_id_b, _, site_type_b, state) = t in
                   let (agent_id_b', _, site_type_b', state') = r in
                   (*the second site belong to modification*)
                   if (agent_id = agent_id_m && site_type2 = site_type_m
                       ||
                       agent_id' = agent_id_m && site_type2' = site_type_m)
                      &&
                      (*if the first site belong to the bond*)
                      (agent_id = agent_id_b && site_type = site_type_b &&
                       agent_id' = agent_id_b' && site_type' = site_type_b')
                   then
                     let error, old_set =
                       match
                         Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                           parameter error rule_id store_result
                       with
                       | error, None -> error, empty_set
                       | error, Some s -> error, s
                     in
                     let pair =
                       (agent_id, agent_type, site_type, site_type2, state, state_m),
                       (agent_id', agent_type', site_type', site_type2', state', state_m) in
                     let error', new_set =
                       Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                         parameter error
                         pair
                         old_set
                     in
                     let error = Exception.check warn parameter error error' (Some "line 540") Exit in
                     let error, store_result =
                       Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                         parameter error rule_id
                         new_set
                         store_result
                     in
                     error, store_result
                   else
                     error, store_result
                ) store_bond_set (error, store_result)
           ) store_modified_set (error, store_result)
       in
       error, store_result
    ) store_tuple_pair (error, store_result)

(***************************************************************)
(*collect rule that has question marks on the right hand side*)

let collect_question_marks_rhs parameter error handler_kappa rule_id rule
    store_modified_map store_result =
  (*-------------------------------------------------------------*)
  (*there is a question marks on the rhs*)
  let error, _, question_marks_r =
    Preprocess.translate_mixture parameter error handler_kappa
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.c_mixture
  in
  (*-------------------------------------------------------------*)
  (*modification*)
  let error, store_modified_set =
    match
      Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_modified_map with
    | error, None ->
      error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*-------------------------------------------------------------*)
  let error, store_result =
    List.fold_left (fun (error, store_result) (agent_id, site_type) ->
        (*site_type is the site that contain the question mark*)
        let error, old_set =
          match
            Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id store_result
          with
          | error, None ->
            error, Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*-------------------------------------------------------------*)
        (**)
        Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
          (fun m (error, store_result) ->
             let (agent_id_m, _agent_type_m, site_type_m, state_m) = m in
             let error, agent =
               Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                 parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
             in
             match agent with
             | Some Cckappa_sig.Dead_agent _
             | Some Cckappa_sig.Ghost
             | None | Some Cckappa_sig.Unknown_agent _ -> error, store_result
             | Some Cckappa_sig.Agent agent ->
               let agent_type = agent.Cckappa_sig.agent_name in
               let site_type' =
                 Ckappa_sig.site_name_of_int
                   (Ckappa_sig.int_of_site_name site_type - 1)
               in
               (*check if agent of question mark is also the agent that can
                 be modified*)
               if agent_id_m = agent_id
               then
                 let error, new_set =
                   Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.add_when_not_in
                     parameter error
                     (agent_id, agent_type, site_type', site_type_m, state_m)
                    old_set
                 in
                 let error, store_result =
                   Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                     parameter error rule_id new_set store_result
                 in
                 error, store_result
               else error, store_result
          ) store_modified_set (error, store_result)
      ) (error, store_result) question_marks_r
  in
  error, store_result

(***************************************************************)
(*Implicit static information: the first site belong to the question mark,
  the second site is modified *)
(***************************************************************)

let collect_implicit_static parameter error store_tuple_pair store_question_marks_rhs =
  let store_result =
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
         (*fold over this set*)
         let _, new_set =
           Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.fold
             (fun x (error, store_set) ->
                (*an agent with question mark*)
                (*B.z.t*)
                let (_agent_id, _agent_type, site_type, site_type', state') = x in
                (*fold over tuple pair set*)
                let error, set =
                  Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
                    (fun (y, t) (error, store_set) ->
                       let (agent_id1, agent_type1, site_type1, site_type2, _) = y in
                       let (agent_id1', agent_type1', site_type1', site_type2', _) = t in
                        let error, first_pair =
                         (*the first site belongs to the question mark*)
                         if site_type = site_type1 && site_type' = site_type2 ||
                            site_type = site_type1' && site_type' = site_type2'
                         then
                           let pair =
                             (agent_id1, agent_type1, site_type1, site_type2, state' ), (*B?*)
                             (agent_id1', agent_type1', site_type1', site_type2', state')
                           in
                           let error, set =
                             Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in
                               parameter error
                               pair (*(y, t)*)
                               store_set
                           in
                           error, set
                         else error, store_set
                       in
                       error, first_pair
                    ) store_tuple_pair (error, store_set)
                in
                error, set
             ) set (error, Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty)
         in
         new_set
      ) store_question_marks_rhs
  in
  store_result
  *)

(***************************************************************)
(*Explicit static information*)
(***************************************************************)

(*let collect_explicit_static parameter error store_created_bond
    store_modified_internal_state_and_bond store_result =
  let add_link parameter error rule_id set store_result =
    let error, old_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id store_result with
      | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error', new_set =
      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
    in
    let error = Exception.check warn parameter error error' (Some "line 706") Exit in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
    in
    error, store_result
  in
  Ckappa_sig.Rule_map_and_set.Map.fold2 parameter error
    (fun parameter error rule_id set store_result ->
       let error, store_result =
         add_link parameter error rule_id set store_result
       in
       error, store_result)
    (fun parameter error rule_id set store_result ->
       let error, store_result =
         add_link parameter error rule_id set store_result
       in
       error, store_result
    )
    (fun parameter error rule_id set1 set2 store_result ->
       let error, new_set =
         Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union
           parameter error set1 set2
       in
       let error, store_result =
         add_link parameter error rule_id new_set store_result
       in
       error, store_result
    )
    store_created_bond store_modified_internal_state_and_bond store_result*)

(***************************************************************)
(*Initial state*)
(***************************************************************)

(*views on the initial states*)

let collect_views_init parameter error init_state =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter error
    (fun parameter error agent_id agent store_result ->
       match agent with
       | Cckappa_sig.Ghost
       | Cckappa_sig.Unknown_agent _ -> error, store_result
       | Cckappa_sig.Dead_agent (agent, _, _, _)
       | Cckappa_sig.Agent agent ->
         let agent_type = agent.Cckappa_sig.agent_name in
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type port (error, store_set) ->
                let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                  parameter error
                  (agent_id, agent_type, site_type, state)
                  store_set
             ) agent.Cckappa_sig.agent_interface
             (error, store_result)
         in
         error, store_result
    ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty

(***************************************************************)
(*return an agent in the initial state that has two sites different*)

let collect_sites_init parameter error store_views_init =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
      (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
        let error, store_result =
          Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
            (fun (agent_id', _, site_type', state') (error, store_result) ->
              if agent_id = agent_id' && site_type <> site_type'
              then
                let error, store_result =
                  Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                    parameter error
                    (agent_id, agent_type, site_type, site_type', state, state')
                    store_result
                in
                error, store_result
              else error, store_result
            ) store_views_init (error, store_result)
        in
        error, store_result
      ) store_views_init
      (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
  in
  error, store_result

(***************************************************************)
(*(A.x.y, B.z.t)*)

let collect_pair_sites_init parameter error store_sites_init =
  let error, store_result =
    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
      (fun x (error, store_result) ->
         let (agent_id, agent_type, site_type, site_type2, state, state2) = x in (*A*)
         let error, store_result =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
             (fun z (error, store_result) ->
                let (agent_id', agent_type', site_type', site_type2', state', state2') = z in
                if agent_id <> agent_id'
                then
                  let error, store_result =
                    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                      ((agent_id, agent_type, site_type, site_type2, state, state2),
                       (agent_id', agent_type', site_type', site_type2', state', state2'))
                      store_result
                  in
                  error, store_result
                else error, store_result
             ) store_sites_init (error, store_result)
         in
         error, store_result
      ) store_sites_init
      (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
  in
  error, store_result

(***************************************************************)
(*collect bonds in the initial states*)

let collect_bonds parameter error site_add agent_id site_type_source views =
  let error, pair =
    let agent_index_target = site_add.Cckappa_sig.agent_index in
    let site_type_target = site_add.Cckappa_sig.site in
    let error, agent_source =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_id views
      with
      | error, None -> warn parameter error (Some "line 632") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, agent_target =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_index_target views
      with
      | error, None -> warn parameter error (Some "line 640") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, (agent_type1, state1) =
      collect_agent_type_state
        parameter
        error
        agent_source
        site_type_source
    in
    let error, (agent_type2, state2) =
      collect_agent_type_state
        parameter
        error
        agent_target
        site_type_target
    in
    let pair = ((agent_type1, site_type_source, state1),
                (agent_type2, site_type_target, state2))
    in
    error, pair
  in
  error, pair

let collect_bonds_init parameter error init_state =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter error
    (fun parameter error agent_id bonds_map store_result ->
       let error, store_result =
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site_type_source site_add (error, store_result) ->
              let error,
                  ((agent_type_source, site_type_source, state_source),
                   (agent_type_target, site_type_target, state_target)) =
                collect_bonds
                  parameter error site_add agent_id site_type_source
                  init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
              in
              let pair =
                ((agent_id, agent_type_source, site_type_source, state_source),
                 (site_add.Cckappa_sig.agent_index, agent_type_target, site_type_target, state_target))
              in
              let error, store_result =
                Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                  parameter
                  error
                  pair
                  store_result
              in
              error, store_result
           ) bonds_map (error, store_result)
       in
       error, store_result
    ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
    Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty

(***************************************************************)

let collect_pair_tuple_init parameter error bdu_false handler kappa_handler
    _init_state store_bonds_init store_pair_sites_init store_result =
  (*fold over a set of pair and check the first site whether or not it
    belongs to a set of sites that can be bound*)
  let error, handler, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
      (fun (x, y) (error, handler, store_result) ->
         let (agent_id, agent_type, site_type, _site_type2, state, state2) = x in
         let (agent_id', agent_type', site_type', _site_type2', state', state2') = y in
         (*check the first site belong to the bonds*)
         if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
             ((agent_id, agent_type, site_type, state),
               (agent_id', agent_type', site_type', state'))
              store_bonds_init
         then
           (*-----------------------------------------------------------*)
           (*use the number 1 to indicate for the first agent, and number 2
             for the second agent*)
           let pair_list =
             [(Ckappa_sig.site_name_of_int 1, state2);
              (Ckappa_sig.site_name_of_int 2, state2')]
           in
           let _new_pair_list =
             List.fold_left (fun current_list (x, y) ->
                 (x, y) :: current_list
               ) [] pair_list
           in
           let _prefix = Remanent_parameters.get_prefix parameter in
           (*test*)
           let pair = Site_accross_bonds_domain_type.project2 (x, y) in
           (*-----------------------------------------------------------*)
           let error, handler, mvbdu =
             Ckappa_sig.Views_bdu.mvbdu_of_association_list
               parameter handler error pair_list
           in
           let error, handler, store_result =
             Site_accross_bonds_domain_type.add_link
               parameter error bdu_false handler kappa_handler pair mvbdu store_result
           in
           error, handler, store_result
         else
           error, handler, store_result
      ) store_pair_sites_init (error, handler, store_result)
  in
  error, handler, store_result
