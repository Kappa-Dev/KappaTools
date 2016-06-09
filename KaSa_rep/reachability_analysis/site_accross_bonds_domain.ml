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
      store_site_accross_bonds_rhs: Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
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

(*first write a function collect bonds on the rhs*)

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
         let error, old_set =
           match
             Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
               parameter error rule_id store_result
           with
           | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
           | error, Some s -> error, s
         in
         let _ =
           Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun x (error, current_list) ->
               Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun y (error, current_list) ->
                   let (agent_id, agent_type, site_type, state) = x in
                   let (agent_id', agent_type', site_type', state') = y
                   in
                   if site_type <> site_type'
                   then
                     (*let _ =
                       Loggers.fprintf (Remanent_parameters.get_logger parameter)
                         "rule_id:%i:agent_id:%i:agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i\n"
                         (Ckappa_sig.int_of_rule_id rule_id)
                         (Ckappa_sig.int_of_agent_id agent_id)
                         (Ckappa_sig.int_of_agent_name agent_type)
                         (Ckappa_sig.int_of_site_name site_type)
                         (Ckappa_sig.int_of_site_name site_type')
                         (Ckappa_sig.int_of_state_index state)
                         (Ckappa_sig.int_of_state_index state')
                     in*)
                     error, (agent_id, agent_type, site_type, site_type', state, state') :: current_list
                   else
                     error, current_list
                 ) set (error, current_list)
             ) old_set (error, [])
         in
         let error', new_set =
           Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error old_set set
         in
         let error = Exception.check warn parameter error error'
             (Some "line 210") Exit
         in
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

  let collect_pair_sites_aux parameter error rule_id rule store_views_rhs =
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
            let error, pair_list =
              Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id', agent_type', site_type', state') (error, current_list)->
                  error,
                  ((agent_id, agent_type, site_type, state),
                  (agent_id', agent_type', site_type', state')
                  ) :: current_list
                ) views_rhs_set (error, [])
            in
            let error, store_result =
              Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
                parameter error rule_id pair_list store_result
            in
            error, store_result
          ) views_rhs_set (error, Ckappa_sig.Rule_map_and_set.Map.empty)
      in
      error, store_result

  (*let collect_pair_sites parameter error rule_id rule store_views_rhs =
    let error, store_sites =
      collect_pair_sites_aux parameter error rule_id rule store_views_rhs
    in
    (*fold over this list*)
    let error, pair_list =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_sites with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, store_result =
      List.fold_left (fun (error, store_result) (x, y) ->
          let (agent_id, agent_type, site_type, state) = x in
          let (agent_id', agent_type', site_type', state') = y in
          let error, new_pair_list =
            List.fold_left (fun (error, current_list) (z, t) ->
                let (ag_id, ag_type, s_type, s_tate) = z in
                let (ag_id', ag_type', s_type', s_tate') = t in
                (*if it is the same agent, and it has two different sites*)
                if agent_id = ag_id (*&& agent_id' = ag_id' && site_type' <> s_type'*)
                then
                  error, ((agent_id, agent_type, site_type, s_type, state, s_tate),
                          (agent_id', agent_type', site_type', s_type', state', s_tate')) :: current_list
                else
                  error, current_list
              ) (error, []) pair_list
          in
          let error, store_result =
            Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
              parameter error rule_id new_pair_list
              store_result
          in
          error, store_result
        ) (error, Ckappa_sig.Rule_map_and_set.Map.empty) pair_list
    in
    error, store_result*)

(****************************************************************)

  let collect_rule parameter error rule_id rule static =
    (*bonds on the rhs*)
    let store_views_rhs = get_views_rhs static in
    let handler_kappa = get_kappa_handler static in
    let error, store_views_rhs =
      collect_views_rhs parameter error rule_id rule store_views_rhs
    in
    let log = Remanent_parameters.get_logger parameter in
    (*todo*)
    let error, store_sites =
      collect_pair_sites_aux parameter error rule_id rule store_views_rhs
    in
    (*let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Loggers.fprintf log "Views on the rhs 2:\n";
          Print_site_accross_bonds_domain.print_pair_sites_aux parameter error handler_kappa log store_sites
        in
        error
      else error
    in*)
    (*let error, store_new_pair =
      collect_pair_sites parameter error rule_id rule store_views_rhs
    in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Loggers.fprintf log "Views on the rhs 3:\n";
          Print_site_accross_bonds_domain.print_pair_sites parameter error handler_kappa log store_new_pair
        in
        error
      else error
    in*)
    let static = set_views_rhs store_views_rhs static in
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
    (*  let kappa_handler = Analyzer_headers.get_kappa_handler static in
        let parameter = Analyzer_headers.get_parameter static in*)
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
    let store_views_rhs = get_views_rhs static in
    let log = Remanent_parameters.get_logger parameter in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Loggers.fprintf log "Views on the rhs:\n";
          Print_site_accross_bonds_domain.print_views_rhs parameter error handler_kappa log store_views_rhs
        in
        error
      else error
    in

    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
