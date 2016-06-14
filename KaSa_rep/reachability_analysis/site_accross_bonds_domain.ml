(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  (* the type of the struct that contains all static information as in the previous version of the analysis *)

  (* domain specific info: *)
  (* collect the set of tuples (A,x,y,B,z,t) such that there exists a rule with a bond connecting the site A.x and B.z and that the agent of type A document a site y <> x, and the agent of type B document a site t <> z *)

  (* for each tuple, collect three maps -> (A,x,y,B,z,t) -> Ag_id list
     RuleIdMap to explain in which rule and which agent_id the site y can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and which agent_id the site t can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and which agent_id (A) a site x of A may become bound to a site z of B *)

  type local_static_information =
    {
      store_views_rhs : Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_site_accross_bonds_rhs: Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      dummy:unit;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  (*--------------------------------------------------------------*)
  (* a triple of maps *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with two variables that describes the relation between the state of y and the state of t, when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables that decribes the range of y when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables that decribes the range of t when both agents are connected via x and z *)

  type local_dynamic_information = unit (* to be replaced *)

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value of type static_information. Kappa handler is static and thus it should never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*-------------------------------------------------------------*)

  let get_local_static_information static = static.local_static_information

  let set_local_static_information local static =
    {
      static with
        local_static_information = local
    }

  let get_views_rhs static =
    (get_local_static_information static).store_views_rhs

  let set_views_rhs views static =
    set_local_static_information
      {
        (get_local_static_information static) with           store_views_rhs = views
      } static

  let get_site_accross_bonds_rhs static =
    (get_local_static_information static).store_site_accross_bonds_rhs

  let set_site_accross_bonds_rhs site_accross static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_site_accross_bonds_rhs = site_accross
      } static

  (*------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic = {dynamic with global = gdynamic}

  (*-------------------------------------------------------------*)

  type 'a zeroary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> Exception.method_handler * dynamic_information * 'c

(****************************************************************)
(*Implementation of local static information*)
(*looking on the rhs and return the agent that has two sites that are different, and one of the first site is bound.*)

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
         let error, set =
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
             ) agent.Cckappa_sig.agent_interface (error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty)
         in
         (*--------------------------------------------*)
         (*old set*)
         let error, old_set =
           match
             Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
               parameter error rule_id store_result
           with
           | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
           | error, Some s -> error, s
         in
         (*--------------------------------------------*)
         (*new set*)
         let error', new_set =
           Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error old_set set
         in
         let error = Exception.check warn parameter error error'
             (Some "line 210") Exit
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

(****************************************************************)
(*return a set of agent with two different sites on the rhs*)

let collect_pair_sites_aux parameter error rule_id store_views_rhs =
    let error, views_rhs_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_views_rhs
      with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*fold over on the rhs*)
    let error, store_result =
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
          (*fold again views rhs*)
          let error, pair_set =
            Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id', _, site_type', state') (error, current_set) ->
                if agent_id = agent_id' && site_type <> site_type'
                then
                  let error, pair_set =
                    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                    (agent_id, agent_type, site_type, site_type', state, state')
                    current_set
                  in
                  error, pair_set
                else error, current_set
              ) views_rhs_set (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
          in
          let error, old_set =
            match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                    parameter error rule_id store_result with
            | error, None -> error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
            | error, Some s -> error, s
          in
          let error, new_set =
              Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error old_set pair_set
          in
          let error, store_result =
            Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
              parameter error rule_id new_set store_result
          in
          error, store_result
        ) views_rhs_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
      in
      error, store_result

(****************************************************************)
(*return a set of two different agents, each agent has two different sites *)

let collect_pair_sites parameter error rule_id store_pair_rhs store_result =
    let error, store_pair_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_pair_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, store_result =
      (*fold over this set*)
      let error, lists = Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
        (fun x (error, current_set) ->
          let (agent_id, agent_type, site_type, site_type2, state, state2) = x in (*A*)
          (*fold again*)
          let error, pair_set =
           Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
             (fun z (error, current_set) ->
                let (agent_id', agent_type', site_type', site_type2', state', state2') = z in
                if agent_id <> agent_id'
                then
                  let error, pair_set =
                    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                    ((agent_id, agent_type, site_type, site_type2, state, state2),
                     (agent_id', agent_type', site_type', site_type2', state', state2'))
                    current_set
                  in
                  error, pair_set
                else error, current_set
             ) store_pair_set (error, current_set)
          in
          error, pair_set
        ) store_pair_set (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
      in
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
        rule_id lists store_result
    in
    error, store_result

(***************************************************************)
(*return a set of agent that are bound on the rhs*)

let collect_agent_type_state parameter error agent site_type =
  match agent with
  | Cckappa_sig.Ghost
  | Cckappa_sig.Unknown_agent _ -> error, (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Dead_agent _ ->
    warn parameter error (Some "line 127") Exit (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
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
        warn parameter error (Some "line 228") Exit Ckappa_sig.dummy_state_index
      | error, Some port ->
        let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
        if (Ckappa_sig.int_of_state_index state) > 0
        then
          error, state
        else
          warn parameter error (Some "line 196") Exit Ckappa_sig.dummy_state_index
    in
    error, (agent_type1, state1)

let collect_bonds_rhs parameter error rule_id rule store_result =
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
                parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
            with
            | error, None -> warn parameter error (Some "line 332") Exit Cckappa_sig.Ghost
            | error, Some agent -> error, agent
          in
          (*get the first pair (agent_type, state)*)
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
                parameter error agent_id_target rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
            with
            | error, None -> warn parameter error (Some "line 350") Exit Cckappa_sig.Ghost
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
            match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result
            with
            | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
            | error, Some p -> error, p
          in
          let error', set =
            Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
              parameter error
              ((agent_id, agent_type1, site_type_source, state1),
               (agent_id_target, agent_type2, site_type_target, state2))
              old_set
          in
          let error = Exception.check warn parameter error error' (Some "line 375") Exit in
          let error, store_result =
            Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
              parameter
              error
              rule_id
              set
              store_result
          in
          error, store_result
       ) bonds_map (error, store_result)
  ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds store_result

(****************************************************************)
(*return a set of pair sites that has the first site is bound and the result of the second site is not bound.*)

let collect_result_aux parameter error rule_id store_bonds_rhs store_pair_sites store_result =
  let error, store_pair_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_pair_sites with
    | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  let error, store_bonds_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_bonds_rhs with
    | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
    in
  (*fold over a set of pair sites*)
  let error, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
      (fun (x, y) (error, store_result) ->
         let (agent_id, agent_type, site_type, site_type2, state, state2) = x in
         let (agent_id', agent_type', site_type', site_type2', state', state2') = y in
         (*check if site_type and site_type' belong to sites that are bound on the rhs *)
         if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
             ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state'))
             store_bonds_set
         then
           (*yes, then return the pair, store it which rule_id*)
           (*let error, pair_set =
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
               (x, y)
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
             in*)
           let error, pair_set =
               Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in parameter error
               ((agent_id, agent_type, site_type2, state2),
                (agent_id', agent_type', site_type2', state2'))
               Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
             in
           let error, old_set =
             match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
             | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
             | error, Some s -> error, s
           in
           let error, new_set =
             Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.union parameter error pair_set old_set
             in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
           in
           error, store_result
         else
           error, store_result
      ) store_pair_set (error, store_result)
  in
  error, store_result

(****************************************************************)

let collect_result parameter error rule_id store_result_aux store_result =
  let error, store_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result_aux with
    | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
    in
    Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold (fun (x, y) (error, store_result) ->
       let (agent_id, agent_type, site_type2, state2) = x in
       let (agent_id', agent_type', site_type2',state2') = y in
       (*state2 = state2'*)
       if state2 = state2'
       then
         let error, pair_set =
         Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in parameter error
         ((agent_id, agent_type, site_type2, state2),
          (agent_id', agent_type', site_type2', state2'))
         Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
         in
         let error, old_set =
           match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
           | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
           | error, Some s -> error, s
           in
       let error, new_set =
         Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.union parameter error pair_set old_set
       in
       let error, store_result =
         Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
           rule_id new_set store_result
       in
       error, store_result
       else error, store_result
      ) store_set (error, store_result)

(****************************************************************)

  let collect_rule parameter error rule_id rule static =
    (*bonds on the rhs*)
    let store_views_rhs = get_views_rhs static in
    let error, store_views_rhs =
      collect_views_rhs parameter error rule_id rule store_views_rhs
    in
    let static = set_views_rhs store_views_rhs static in
    let store_views_rhs = get_views_rhs static in
    let error, store_test =
      collect_pair_sites_aux parameter error rule_id store_views_rhs
    in
    let error, store_pair_sites =
      collect_pair_sites parameter error rule_id store_test
        Ckappa_sig.Rule_map_and_set.Map.empty
    in
    let error, store_bonds_rhs =
      collect_bonds_rhs parameter error rule_id rule Ckappa_sig.Rule_map_and_set.Map.empty
    in
    let error, store_result_aux =
      collect_result_aux parameter error rule_id store_bonds_rhs store_pair_sites Ckappa_sig.Rule_map_and_set.Map.empty
    in
    let store_result = get_site_accross_bonds_rhs static in
    let error, store_result =
      collect_result parameter error rule_id store_result_aux store_result
    in
    let static = set_site_accross_bonds_rhs store_result static in
    error, static

let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun parameter error rule_id rule static ->
           let error, static =
             collect_rule parameter error rule_id
               rule.Cckappa_sig.e_rule_c_rule static
           in
           error, static
        ) compil.Cckappa_sig.rules static
    in
    error, static, dynamic

  (****************************************************************)
  (** [get_scan_rule_set static] *)

  (* todo *)
  let initialize static dynamic error =
    let init_local_static_information =
      {
        store_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_site_accross_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        dummy = ()
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_local_static_information;
      }
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = () ;
      }
    in
    let error, static, dynamic =
      scan_rule_set
        init_global_static_information
        init_global_dynamic_information
        error
    in
    error, static, dynamic


  (* to do *)
  (* take into account bounds that may occur in initial states *)
  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* check for each bond that occur in the lhs, and if half bond, whether
     the constraints in the lhs are consistent *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (* to do *)
  let apply_rule static dynamic error rule_id precondition =
    (* look into the static information which tuples may be affected *)
    (* use the method in precondition, to ask about information captured by the view domains *)
    (* take into account all cases  *)
    (* modification of y and/or t and we do not know whether the agents are bound *)
    (* modification of y and/or t accross a bond that is preserved *)
    (* creation of a bond with/without modification of z,t *)
    let event_list = [] in
    (*  let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
        let compil = get_compil static in*)
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  (* to do *)
  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)

  (* to do *)
  let print static dynamic error loggers =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let log = Remanent_parameters.get_logger parameter in
    let store_result = get_site_accross_bonds_rhs static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_site_accross_bonds_domain parameter error handler_kappa log store_result
        in error
      else error
    in
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
