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
      store_bonds_rhs: Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_modified_map :
        Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*first map: (A,x,y,B,z,t) such that A.x bound to B.z and y <> x and z <> t*)
      (*store_tuple_has_first_site_bound_snd_site_different:
        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t;*) (*remove*)
      store_tuple_pair : Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t;
      (*explicit static: creating the bond or modifying one internal state with specifying the bond*)
      store_created_bond :         Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*modifying one internal state with specifying the bond*)
      store_modified_internal_state_and_bond :
        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_explicit_static :
        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*implicit: modified the state of one of the two sites without mentioning the binding sites*)
      store_question_marks_rhs : Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_implicit_static :
        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*from each tuple (A,x,y, B,z,t) -> agent_id rule_id map explain in which rule and which agent_id the site y can be modified*)
      (*store_first_site_y_modified :
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
        (*t can be modified*)
        store_snd_site_t_modified :
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
        (*(A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and which agent_id (A) a site x of A may become bound to a site z of B *)
        store_first_site_is_bound : Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
        store_site_accross_bonds_rhs: Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;*)
      dummy:unit;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  (*--------------------------------------------------------------*)
  (* a triple of maps : mvbdu_of_association_list*)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with two variables that describes the relation between the state of y and the state of t, when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables that decribes the range of y when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables that decribes the range of t when both agents are connected via x and z *)

  type local_dynamic_information =
    {
      store_init : Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t;
      (*store_relation_mvbdu :
        Ckappa_sig.Views_bdu.mvbdu
          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
        (*range of the second site of the first agent, when both agents are connected via the first site*)
        store_range_site_first_agent:
        Ckappa_sig.Views_bdu.mvbdu
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
        store_range_site_second_agent:
        Ckappa_sig.Views_bdu.mvbdu
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;*)
    }

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

  let get_bonds_rhs static =
    (get_local_static_information static).store_bonds_rhs

  let set_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with           store_bonds_rhs = bonds
      } static

  let get_modified_map static =
    (get_local_static_information static).store_modified_map

  let set_modified_map sites static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_modified_map = sites
      } static

  let get_created_bond static =
    (get_local_static_information static).store_created_bond

  let set_created_bond rule static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_created_bond = rule
      } static

  let get_tuple_pair static =
    (get_local_static_information static).store_tuple_pair

  let set_tuple_pair pair static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_tuple_pair = pair
      } static

  let get_modified_internal_state_and_bond static =
    (get_local_static_information static).store_modified_internal_state_and_bond

  let set_modified_internal_state_and_bond rule static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_modified_internal_state_and_bond = rule
      } static

  let get_explicit_rule static =
    (get_local_static_information static).store_explicit_static

  let set_explicit_rule rule static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_explicit_static = rule
      } static

  (*let get_tuple_has_first_site_bound_snd_site_different static =
    (get_local_static_information static).store_tuple_has_first_site_bound_snd_site_different

    let set_tuple_has_first_site_bound_snd_site_different tuple static =
    set_local_static_information
      {
        (get_local_static_information static) with           store_tuple_has_first_site_bound_snd_site_different = tuple
      } static*)

  let get_implicit_rule static =
    (get_local_static_information static).store_implicit_static

  let set_implicit_rule rule static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_implicit_static = rule
      } static

  let get_question_marks_rhs static =
    (get_local_static_information static).store_question_marks_rhs

  let set_question_marks_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with          store_question_marks_rhs = bonds
      } static

  (*
  let get_first_site_y_modified static =
    (get_local_static_information static).store_first_site_y_modified

  let set_first_site_y_modified site static =
    set_local_static_information
      {
        (get_local_static_information static) with           store_first_site_y_modified = site
      } static

  let get_snd_site_t_modified static =
    (get_local_static_information static).store_snd_site_t_modified

  let set_snd_site_t_modified site static =
    set_local_static_information
      {
        (get_local_static_information static) with           store_snd_site_t_modified = site
      } static

  let get_first_site_is_bound static =
    (get_local_static_information static).store_first_site_is_bound

  let set_first_site_is_bound site static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_first_site_is_bound = site
      } static

  let get_site_accross_bonds_rhs static =
    (get_local_static_information static).store_site_accross_bonds_rhs

  let set_site_accross_bonds_rhs site_accross static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_site_accross_bonds_rhs = site_accross
      } static
    *)

  (*------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    {
      dynamic with global = gdynamic
    }

  (*handler*)
  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
      global = Analyzer_headers.set_mvbdu_handler handler (get_global_dynamic_information dynamic)
    }

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  let get_init dynamic =
    (get_local_dynamic_information dynamic).store_init

  let set_init init dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_init = init
      } dynamic

(*
  let get_relation_mvbdu dynamic =
    (get_local_dynamic_information dynamic).store_relation_mvbdu

  let set_relation_mvbdu mvbdu dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_relation_mvbdu = mvbdu
      } dynamic

  let get_range_site_first_agent dynamic =
    (get_local_dynamic_information dynamic).store_range_site_first_agent

  let set_range_site_first_agent mvbdu dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_range_site_first_agent = mvbdu
      } dynamic

  let get_range_site_second_agent dynamic =
    (get_local_dynamic_information dynamic).store_range_site_second_agent

  let set_range_site_second_agent mvbdu dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_range_site_second_agent = mvbdu
      } dynamic
    *)

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
  (*map has first site y can be modified*)

  let collect_site_modified parameter error rule_id rule store_result =
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
        (fun parameter error agent_id agent store_result->
           (*if there is no modified sites then do nothing*)
           if Ckappa_sig.Site_map_and_set.Map.is_empty agent.Cckappa_sig.agent_interface
           then error, store_result
           else
             let agent_type = agent.Cckappa_sig.agent_name in
             let error, pair_set =
               Ckappa_sig.Site_map_and_set.Map.fold
                 (fun site_type port (error, store_set) ->
                    let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                    let error, store_set =
                      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in parameter error
                        (agent_id, agent_type, site_type, state)
                        store_set
                    in
                    error, store_set
                 )
                 agent.Cckappa_sig.agent_interface (error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty)
             in
             (*old set*)
             let error, old_set =
               match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
               | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
               | error, Some s -> error, s
             in
             let error, new_set =
               Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error pair_set old_set
             in
             let error, store_result =
               Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
             in
             error, store_result
        ) rule.Cckappa_sig.diff_direct store_result
    in
    error, store_result

  (****************************************************************)  (**)

  let collect_tuple_pair parameter error rule_id store_pair_rhs store_result =
    let error, store_pair_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_pair_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, store_result =
      (*fold over this set*)
      Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
        (fun x (error, current_set) ->
           let (agent_id, agent_type, site_type, site_type2, _, _) = x in (*A*)
           (*fold again*)
           let error, pair_set =
             Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
               (fun z (error, current_set) ->
                  let (agent_id', agent_type', site_type', site_type2', _, _) = z in
                  if agent_id <> agent_id'
                  then
                    let error, pair_set =
                      Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error
                        ((agent_id, agent_type, site_type, site_type2),
                         (agent_id', agent_type', site_type', site_type2'))
                        current_set
                    in
                    error, pair_set
                  else error, current_set
               ) store_pair_set (error, current_set)
           in
           error, pair_set
        ) store_pair_set (error, store_result)
    in
    error, store_result

  (****************************************************************)
  (*explicit static*)

  (*collect a set of rule that has site created a bond*)
  let collect_created_bond parameter error rule_id rule store_tuple_pair store_result =
    let error, store_result =
      List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
          let agent_id1 = site_add1.Cckappa_sig.agent_index in
          let agent_type1 = site_add1.Cckappa_sig.agent_type in
          let site_type1 = site_add1.Cckappa_sig.site in
          let agent_id2 = site_add2.Cckappa_sig.agent_index in
          let agent_type2 = site_add2.Cckappa_sig.agent_type in
          let site_type2 = site_add2.Cckappa_sig.site in
          Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
            (fun (x, y) (error, store_result) ->
               let (agent_id, _, site_type, site_type') = x in
               let (agent_id', _, site_type', site_type2') = y in
               (*if the first site in the pair is the site that created a bound*)
               if agent_id1 = agent_id && agent_id2 = agent_id' && site_type = site_type1 && site_type2 = site_type' ||
                  agent_id1 = agent_id' && agent_id2 = agent_id &&
                  site_type' = site_type1 && site_type2 = site_type
               then
                 (*let pair =
                   ((agent_id1, agent_type1, site_type1, site_type'),
                   (agent_id2, agent_type2, site_type2, site_type2'))
                   in*)
                 let error, set =
                   Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error (x,y)
                     Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                 in
                 let error, old_set =
                   match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                   | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                   | error, Some s -> error, s
                 in
                 let error', new_set =
                   Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
                 in
                 let error = Exception.check warn parameter error error' (Some "line 540") Exit in
                 let error, store_result =
                   Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
                 in
                 error, store_result
               else error, store_result
            ) store_tuple_pair (error, store_result)
        )(error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind
    in
    error, store_result

  (****************************************************************)  (*collect a set of rule that has site created a bond*)

  let collect_modified_internal_and_bond parameter error rule_id store_tuple_pair store_bonds_rhs store_modified_map store_result =
    let error, store_bond_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_bonds_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, store_modified_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_modified_map with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
      (fun (x, y) (error, store_result) ->
         let (agent_id, _, site_type, site_type') = x in
         let (agent_id', _, site_type', site_type2') = y in
         (*if the second site belong to modified and the first site belong to the bond set*)
         let error, store_result =
           Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun m (error, store_result) ->
               Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
                 (fun (t, r) (error, store_result) ->
                    let (agent_id_m, _, site_type_m, _) = m in
                    let (agent_id_b, _, site_type_b, _) = t in
                    let (agent_id_b', _, site_type_b', _) = r in
                    if (agent_id = agent_id_m && site_type' = site_type_m || agent_id' = agent_id_m && site_type2' = site_type_m)
                       (*if the first site belong to the bond*) &&
                       (agent_id = agent_id_b && site_type = site_type_b &&
                        agent_id' = agent_id_b' && site_type' = site_type_b')
                    then
                      let error, set =
                        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error (x,y)
                          Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                      in
                      let error, old_set =
                        match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                        | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                        | error, Some s -> error, s
                      in
                      let error', new_set =
                        Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
                      in
                      let error = Exception.check warn parameter error error' (Some "line 540") Exit in
                      let error, store_result =
                        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
                      in
                      error, store_result
                    else
                      error, store_result
                 ) store_bond_set (error, store_result)
             ) store_modified_set (error, store_result)
         in
         error, store_result
      ) store_tuple_pair (error, store_result)

  (****************************************************************)

  let collect_explicit_static parameter error store_created_bond store_modified_internal_state_and_bond store_result =
    let add_link parameter error rule_id set store_result =
      let error, old_set =
        match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
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
           Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set1 set2
         in
         let error, store_result =
           add_link parameter error rule_id new_set store_result
         in
         error, store_result
      )
      store_created_bond store_modified_internal_state_and_bond store_result

  (****************************************************************)
  (*implicit rule*)

  let collect_question_marks_rhs parameter error rule_id handler_kappa rule store_modified_map store_result =
    let error, _, question_marks_r =
      Preprocess.translate_mixture parameter error handler_kappa
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.c_mixture
    in
    let error, store_modified_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_modified_map with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, new_set =
      List.fold_left (fun (error, store_set) (agent_id, site_type) ->
          Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun m (error, store_set) ->
              let (agent_id_m, agent_type_m, site_type_m, _) = m in
              let error, agent =
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
              in
              match agent with
              | Some Cckappa_sig.Dead_agent _
              | Some Cckappa_sig.Ghost
              | None | Some Cckappa_sig.Unknown_agent _ -> error, store_set
              | Some Cckappa_sig.Agent agent ->
                let agent_type = agent.Cckappa_sig.agent_name in
                let site_type' = Ckappa_sig.site_name_of_int (Ckappa_sig.int_of_site_name site_type - 1) in
                if agent_type_m = agent_type then
                  let error, set =
                    Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.add_when_not_in parameter error
                      (agent_id, agent_type, site_type', site_type_m)
                      store_set
                  in
                  error, set
                else error, store_set
            ) store_modified_set (error, store_set)
        ) (error, Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.empty) question_marks_r
    in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
        rule_id new_set store_result
    in
    error, store_result

  (*FIXME*)
  let collect_implicit_static parameter error rule_id  store_tuple_pair store_question_marks_rhs store_result =
    let error, question_marks_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_question_marks_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.empty
      | error, Some l -> error, l
    in
    Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold
      (fun (x, y) (error, store_result) ->
         let (agent_id, agent_type, site_type, site_type') = x in
         let (agent_id', agent_type', site_type', site_type2') = y in
         (*if the second site belong to modified and the first site belong to the bond set*)
         let error, store_result =
           Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.fold
             (fun t (error, store_result) ->
                let (agent_id_q, agent_type_q, site_type_q, site_type_m) = t in
                (*if the second site belong to the modified site*)
                (*if site_type' = site_type_m ||
                   site_type2' = site_type_m
                  then*)
                let error, set =
                  Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error
                    ((agent_id_q, agent_type_q, site_type, site_type_m),
                     y)
                    Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                in
                let error, old_set =
                  match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                  | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
                  | error, Some s -> error, s
                in
                let error', new_set =
                  Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.union parameter error set old_set
                in
                let error = Exception.check warn parameter error error' (Some "line 540") Exit in
                let error, store_result =
                  Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
                in
                error, store_result
                (*else
                  error, store_result*)

             ) question_marks_set (error, store_result)
         in
         error, store_result
      ) store_tuple_pair (error, store_result)

  (****************************************************************)
  (*return a set of agent with two different sites on the rhs*)
  (*TO DO*)

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

  (*return a set of pair sites that has the first site is bound and the result of the second site is not bound.*)
(*
let collect_tuple_has_first_site_bound_snd_site_different parameter error rule_id store_bonds_rhs store_pair_sites store_result =
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
         let (agent_id, agent_type, site_type, site_type2, state, _) = x in
         let (agent_id', agent_type', site_type', site_type2', state', _) = y in
         (*check if site_type and site_type' belong to sites that are bound on the rhs *)
         if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
             ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state'))
             store_bonds_set
         then
           let error, store_result =
             Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.add_when_not_in parameter error
               ((agent_id, agent_type, site_type, site_type2),
                (agent_id', agent_type', site_type', site_type2'))
               store_result
             in
             error, store_result
         else
           error, store_result
      ) store_pair_set (error, store_result)
  in
  error, store_result
         *)
  (****************************************************************)
(*
let collect_first_site_y_modified parameter error rule_id store_modified_map store_tuple_has_first_site_bound_snd_site_different store_result =
  let error, modified_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_modified_map with
    | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
    in
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
    (fun (x, y) (error, store_result) ->
       let (agent_id, agent_type, _, site_type2, _, state2) = x in
       (*check the first *)
       if Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.mem (agent_id, agent_type, site_type2, state2) modified_set
       then
         let error, store_set =
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
             (x, y)
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
         in
         let error, old_set =
           match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
           | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
           | error, Some s -> error, s
         in
         let error, new_set = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error store_set old_set
         in
         let error, store_result =
           Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
         in
         error, store_result
       else error, store_result
    ) store_tuple_has_first_site_bound_snd_site_different (error, store_result)

(****************************************************************)
(*t can be modified*)

let collect_snd_site_t_modified parameter error rule_id store_modified_map store_tuple_has_first_site_bound_snd_site_different store_result =
  let error, modified_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_modified_map with
    | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
    (fun (x, y) (error, store_result) ->
       let (agent_id, agent_type, _, site_type2, _, state2) = y in
       (*check the first *)
       if Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.mem (agent_id, agent_type, site_type2, state2) modified_set
       then
         let error, store_set =
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
             (x, y)
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
         in
         let error, old_set =
           match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
           | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
           | error, Some s -> error, s
         in
         let error, new_set = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error store_set old_set
         in
         let error, store_result =
           Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
         in
         error, store_result
       else error, store_result
    ) store_tuple_has_first_site_bound_snd_site_different (error, store_result)

(****************************************************************)
(*site x of A may become bound to a site z of B*)

let collect_first_site_is_bound parameter error rule_id store_tuple_has_first_site_bound_snd_site_different store_bonds_rhs store_result =
  let error, bonds_rhs_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_bonds_rhs with
    | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
    | error, Some s -> error, s
  in
  (*fold over set of tuple pair*)
  let error, store_result =
    Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
      (fun (x, y) (error, store_result) ->
         let (agent_id, agent_type, site_type, _, state, _) = x in
         let (agent_id', agent_type', site_type', _, state', _) = y in
         (*check if the first site belongs to the site that is bound on the rhs*)
         if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
             ((agent_id, agent_type, site_type, state),
            (agent_id', agent_type', site_type', state'))
             bonds_rhs_set
         then
           (*store_the pair*)
           let error, pair_set =
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
               (x, y)
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
           in
           let error, old_set =
             match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
             | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
             | error, Some s -> error, s
           in
           let error, new_set = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error pair_set old_set
           in
           let error, store_result =
             Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id new_set store_result
           in
           error, store_result
         else
           error, store_result
      ) store_tuple_has_first_site_bound_snd_site_different (error, store_result)
  in
  error, store_result

(*todo: remove later*)
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
    *)
  (****************************************************************)

  let collect_rule parameter error rule_id rule static =
    let kappa_handler = get_kappa_handler static in
    (*bonds on the rhs*)
    let store_views_rhs = get_views_rhs static in
    let error, store_views_rhs =
      collect_views_rhs parameter error rule_id rule store_views_rhs
    in
    let static = set_views_rhs store_views_rhs static in
    let store_views_rhs = get_views_rhs static in
    (*--------------------------------------------------------------*)
    let error, store_test =
      collect_pair_sites_aux parameter error rule_id store_views_rhs
    in
    (*let error, store_pair_sites =
      collect_pair_sites parameter error rule_id store_test
        Ckappa_sig.Rule_map_and_set.Map.empty
      in*)
    (*--------------------------------------------------------------*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, store_bonds_rhs =
      collect_bonds_rhs parameter error rule_id rule store_bonds_rhs
    in
    let static = set_bonds_rhs store_bonds_rhs static in
    (*--------------------------------------------------------------*)
    (*first site is bound and the second site is different than the first one*)
    (*let store_tuple_has_first_site_bound_snd_site_different = get_tuple_has_first_site_bound_snd_site_different static in
      let error, store_tuple_has_first_site_bound_snd_site_different =
      collect_tuple_has_first_site_bound_snd_site_different parameter error rule_id store_bonds_rhs store_pair_sites store_tuple_has_first_site_bound_snd_site_different
      in
      let static = set_tuple_has_first_site_bound_snd_site_different store_tuple_has_first_site_bound_snd_site_different static in*)
    (*--------------------------------------------------------------*)
    let store_tuple_pair = get_tuple_pair static in
    let error, store_tuple_pair =
      collect_tuple_pair parameter error rule_id store_test store_tuple_pair
    in
    let static = set_tuple_pair store_tuple_pair static in
    (*--------------------------------------------------------------*)
    let store_modified_map = get_modified_map static in
    let error, store_modified_map =
      collect_site_modified parameter error rule_id rule store_modified_map
    in
    let static = set_modified_map store_modified_map static in
    (*--------------------------------------------------------------*)
    let store_tuple_pair = get_tuple_pair static in
    let store_created_bond = get_created_bond static in
    let error, store_created_bond =
      collect_created_bond parameter error rule_id rule store_tuple_pair store_created_bond
    in
    let static = set_created_bond store_created_bond static in
    (*--------------------------------------------------------------*)
    let store_bonds_rhs = get_bonds_rhs static in
    let store_modified_map = get_modified_map static in
    let store_modified_internal_state_and_bond = get_modified_internal_state_and_bond static in
    let error, store_modified_internal_state_and_bond =
      collect_modified_internal_and_bond parameter error rule_id store_tuple_pair store_bonds_rhs store_modified_map store_modified_internal_state_and_bond
    in
    let static = set_modified_internal_state_and_bond store_modified_internal_state_and_bond static in
    (*--------------------------------------------------------------*)
    let store_explicit_static = get_explicit_rule static in
    let store_created_bond = get_created_bond static in
    let store_modified_internal_state_and_bond = get_modified_internal_state_and_bond static in
    let error, store_explicit_static =
      collect_explicit_static parameter error store_created_bond store_modified_internal_state_and_bond store_explicit_static
    in
    let static = set_explicit_rule store_explicit_static static in
    (*--------------------------------------------------------------*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let error, store_question_marks_rhs =
      collect_question_marks_rhs parameter error rule_id kappa_handler
        rule store_modified_map store_question_marks_rhs
    in
    let static = set_question_marks_rhs store_question_marks_rhs static in
    (*--------------------------------------------------------------*)
    let store_implicit_static = get_implicit_rule static in
    let store_question_marks_rhs = get_question_marks_rhs static in
    let error, store_implicit_static =
      collect_implicit_static parameter error rule_id  store_tuple_pair store_question_marks_rhs store_implicit_static
    in
    let static = set_implicit_rule store_implicit_static static in
    (*let error, _,question_marks_r =
      Preprocess.translate_mixture parameter error kappa_handler
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.c_mixture
      in
      let _ =
      List.iter (fun (agent_id, s) ->
          let s_1 = Ckappa_sig.site_name_of_int (Ckappa_sig.int_of_site_name s - 1) in
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "rule_id:%i:agent_id:%i:site_type:%i\n"
            (Ckappa_sig.int_of_rule_id rule_id)
            (Ckappa_sig.int_of_agent_id agent_id)
            (Ckappa_sig.int_of_site_name s_1)
        ) question_marks_r
      in*)
    (*let store_tuple_has_first_site_bound_snd_site_different = get_tuple_has_first_site_bound_snd_site_different static in*)
    (*--------------------------------------------------------------*)
    (*y of A can be modified*)
    (*let store_first_site_y_modified = get_first_site_y_modified static in
      let error, store_modified_map =
      collect_site_modified parameter error rule_id rule
      in
      let store_tuple_has_first_site_bound_snd_site_different = get_tuple_has_first_site_bound_snd_site_different static in
      let error, store_first_site_y_modified =
      collect_first_site_y_modified parameter error rule_id store_modified_map store_tuple_has_first_site_bound_snd_site_different store_first_site_y_modified
      in
      let static = set_first_site_y_modified store_first_site_y_modified static in
      (*--------------------------------------------------------------*)
      (*t of B can be modified*)
      let store_snd_site_t_modified = get_snd_site_t_modified static in
      let error, store_snd_site_t_modified =
      collect_snd_site_t_modified parameter error rule_id store_modified_map store_tuple_has_first_site_bound_snd_site_different store_snd_site_t_modified
      in
      let static =
      set_snd_site_t_modified store_snd_site_t_modified static in
      (*--------------------------------------------------------------*)
      (*first site can be bound*)
      let store_first_site_is_bound = get_first_site_is_bound static in
      let error, store_first_site_is_bound =
      collect_first_site_is_bound parameter error rule_id store_tuple_has_first_site_bound_snd_site_different store_bonds_rhs store_first_site_is_bound
      in
      let static = set_first_site_is_bound store_first_site_is_bound static in
      (*--------------------------------------------------------------*)
      (**)
      let error, store_result_aux =
      collect_result_aux parameter error rule_id store_bonds_rhs store_pair_sites Ckappa_sig.Rule_map_and_set.Map.empty
      in
      let store_result = get_site_accross_bonds_rhs static in
      let error, store_result =
      collect_result parameter error rule_id store_result_aux store_result
      in
      let static = set_site_accross_bonds_rhs store_result static in*)
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
        store_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_modified_map = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_created_bond = Ckappa_sig.Rule_map_and_set.Map.empty;
        (*store_tuple_has_first_site_bound_snd_site_different = Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty;*)
        store_tuple_pair = Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty;
        store_modified_internal_state_and_bond = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_explicit_static = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_question_marks_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_implicit_static = Ckappa_sig.Rule_map_and_set.Map.empty;
        (*store_first_site_y_modified = Ckappa_sig.Rule_map_and_set.Map.empty;
          store_snd_site_t_modified = Ckappa_sig.Rule_map_and_set.Map.empty;
          store_first_site_is_bound = Ckappa_sig.Rule_map_and_set.Map.empty;
          store_site_accross_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;*)
        dummy = ()
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_local_static_information;
      }
    in
    let init_local_dynamic_information =
      {
        store_init = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty;
        (*store_relation_mvbdu = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
          store_range_site_first_agent = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
          store_range_site_second_agent = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;*)
      }
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = init_local_dynamic_information;
      }
    in
    let error, static, dynamic =
      scan_rule_set
        init_global_static_information
        init_global_dynamic_information
        error
    in
    error, static, dynamic

  (******************************************************************)
  (*initial states*)

  (*collect init where it has the first site can be bound and the second site is different than the first one*)

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
           let error', new_set =
             Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error store_result set
           in
           let error = Exception.check warn parameter error error'
               (Some "line 904") Exit
           in
           error, new_set
      ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty

  let collect_pair_sites_init_aux parameter error store_views_init =
    let error, store_result =
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
          (*fold again views in the initial states*)
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
              ) store_views_init (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
          in
          let error, new_set =
            Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error store_result pair_set
          in
          error, new_set
        ) store_views_init (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
    in
    error, store_result

  let collect_pair_sites_init parameter error store_pair_init_aux =
    let error, store_result =
      Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
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
               ) store_pair_init_aux (error, current_set)
           in
           error, pair_set
        ) store_pair_init_aux (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
    in
    error, store_result

  let collect_pair_of_bonds parameter error site_add agent_id site_type_source views =
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
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_index_target views
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
                  collect_pair_of_bonds
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

  let collect_init parameter error init_state store_result =
    (*a set of site that can be bounds*)
    let error, store_bonds_init =
      collect_bonds_init parameter error init_state in
    (*views in the initial state that has two agents and their sites are different*)
    let error, store_views_init =
      collect_views_init parameter error init_state in
    let error, store_pair_sites_init_aux =
      collect_pair_sites_init_aux parameter error store_views_init in
    let error, store_pair_sites_init =
      collect_pair_sites_init parameter error store_pair_sites_init_aux in
    (*fold over a set of pair and check the first site whether or not it belongs to a set of sites that can be bound*)
    let error, store_result =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
        (fun (x, y) (error, store_result) ->
           let (agent_id, agent_type, site_type, _, state, _) = x in
           let (agent_id', agent_type', site_type', _, state', _) = y in
           if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
               ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state'))
               store_bonds_init
           then
             let error, pair_set =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                 (x, y)
                 Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
             in
             let error', new_set =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error pair_set store_result
             in
             let error = Exception.check warn parameter error error'
                 (Some "line 1060") Exit
             in
             error, new_set
           else
             error, store_result
        ) store_pair_sites_init (error, store_result)
    in
    error, store_result

  (*------------------------------------------------------------*)
  (* take into account bounds that may occur in initial states *)

  let add_initial_state static dynamic error species =
    let parameter = get_parameter static in
    let store_result = get_init dynamic in
    let error, store_init =
      collect_init parameter error species store_result
    in
    let dynamic = set_init store_init dynamic in
    let event_list = [] in
    error, dynamic, event_list

  (*------------------------------------------------------------*)
  (*implementation dynamic information*)

  (*first map, (A,x,y,B,z,t) to a mvbdu: describes the relation between the state of y and the state of t, when both agents are connected via x.z*)
  (*
  let collect_realtion_mvbdu static dynamic error =
    let parameter = get_parameter static in
    let store_result = get_relation_mvbdu dynamic in
    let handler = get_mvbdu_handler dynamic in
    (*get the set of tuple when both agents are connected via x.z*)
    let store_first_site_is_bound = get_first_site_is_bound static in
    let error, handler, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold (fun _ set (error, handler, store_result) ->
          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
            (fun (x, y) (error, handler, store_result)->
               (*build the mvbdu of two sites y(site_type2) and z(site_type2')*)
               let (_, _, _, site_type2, _, state2) = x in
               let (_, _, _, site_type2', _, state2') = y in
               (*build (key * value) list *)
               let pair_list =
                 [(site_type2, state2);(site_type2', state2')]
               in
               let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
               let error, store_result =
                 Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error (x,y) mvbdu store_result
               in
               error, handler, store_result
            ) set (error, handler, store_result)
        ) store_first_site_is_bound (error, handler, store_result)
    in
    (*store handler and the result of the relation *)
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_relation_mvbdu store_result dynamic in
    error, dynamic
           *)
  (*------------------------------------------------------------*)
  (*range of the second site of the first agent, when both agents are connected via the first site*)

  (*
  let collect_range_site_first_agent static dynamic error =
    let parameter = get_parameter static in
    let store_result = get_range_site_first_agent dynamic in
    let handler = get_mvbdu_handler dynamic in
    (*get the set of tuple when both agents are connected via the first site*)
    let store_first_site_is_bound = get_first_site_is_bound static in
    let error, handler, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun _ set (error, handler, store_result) ->
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, handler, store_result) ->
                (*build the mvbdu of the site y (site_type2)*)
                let (_, _, _, site_type2, _, state2) = x in
                (*build (key * value) list*)
                let pair_list = [(site_type2, state2)] in
                let error, handler, mvbdu =
                  Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
                let error, store_result =
                  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error (x, y) mvbdu store_result
                in
                error, handler, store_result
             ) set (error, handler, store_result)
        ) store_first_site_is_bound (error, handler, store_result)
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_range_site_first_agent store_result dynamic in
    error, dynamic
           *)

  (*
  let collect_range_site_second_agent static dynamic error =
    let parameter = get_parameter static in
    let store_result = get_range_site_second_agent dynamic in
    let handler = get_mvbdu_handler dynamic in
    (*get the set of tuple when both agents are connected via the first site*)
    let store_first_site_is_bound = get_first_site_is_bound static in
    let error, handler, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun _ set (error, handler, store_result) ->
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, handler, store_result) ->
                (*build the mvbdu of the site y (site_type2)*)
                let (_, _, _, site_type2, _, state2) = y in
                (*build (key * value) list*)
                let pair_list = [(site_type2, state2)] in
                let error, handler, mvbdu =
                  Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
                let error, store_result =
                  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error (x, y) mvbdu store_result
                in
                error, handler, store_result
             ) set (error, handler, store_result)
        ) store_first_site_is_bound (error, handler, store_result)
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_range_site_second_agent store_result dynamic in
    error, dynamic

           *)
  (* to do *)
  (* check for each bond that occur in the lhs, whether
     the constraints in the lhs are consistent *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (* to do *)
  let apply_rule static dynamic error rule_id precondition =
    (*relation between states *)
    (*let error, dynamic = collect_realtion_mvbdu static dynamic error in
      (*range of the second site of the first agent*)
      let error, dynamic = collect_range_site_first_agent static dynamic error in
      let error, dynamic = collect_range_site_second_agent static dynamic error in*)
    (* look into the static information which tuples may be affected *)
    (* use the method in precondition, to ask about information captured by the view domains *)
    (* take into account all cases  *)
    (* modification of y and/or t and we do not know whether the agents are bound *)
    (* modification of y and/or t accross a bond that is preserved *)
    (* creation of a bond with/without modification of z,t *)

    let event_list = [] in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  (* to do *)
  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (****************************************************************)

  (* to do *)
  let print static dynamic error loggers =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let log = Remanent_parameters.get_logger parameter in    (*--------------------------------------------------------*)
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Loggers.fprintf log
            "------------------------------------------------------------\n";
          Loggers.fprintf log "* Site accross bonds domain\n";
          Loggers.fprintf log
            "------------------------------------------------------------\n"
        in
        error
      else error
    in
    (*--------------------------------------------------------*)
    let store_views_rhs = get_views_rhs static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_views_rhs parameter error handler_kappa log store_views_rhs
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_bonds_rhs parameter error handler_kappa log store_bonds_rhs
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_modified_map = get_modified_map static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_modified_map parameter error handler_kappa log store_modified_map
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_created_bond = get_created_bond static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_created_bond parameter error handler_kappa log store_created_bond
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_modified_internal_state_and_bond = get_modified_internal_state_and_bond static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_modified_internal_state_bond parameter error handler_kappa log store_modified_internal_state_and_bond
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_explicit_static = get_explicit_rule static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_explicit_static parameter error handler_kappa log store_explicit_static
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_question_marks_rhs parameter error handler_kappa log store_question_marks_rhs
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_implicit_static = get_implicit_rule static in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_implicit_static parameter error handler_kappa log store_implicit_static
        in error
      else error
    in
    (*--------------------------------------------------------*)
    (*let store_tuple_pair = get_tuple_pair static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_pair parameter error handler_kappa log store_tuple_pair
        in error
      else error
      in*)
    (*--------------------------------------------------------*)
    (*let store_tuple_has_first_site_bound_snd_site_different = get_tuple_has_first_site_bound_snd_site_different static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_has_first_site_bound_snd_site_different parameter error handler_kappa log store_tuple_has_first_site_bound_snd_site_different
        in error
      else error
      in*)
    (*--------------------------------------------------------*)
    (*let store_first_site_y_modified = get_first_site_y_modified static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_first_site_y_modified parameter error handler_kappa log store_first_site_y_modified
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_snd_site_t_modified = get_snd_site_t_modified static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_snd_site_t_modified parameter error handler_kappa log store_snd_site_t_modified
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_first_site_is_bound = get_first_site_is_bound static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_first_site_is_bound parameter error handler_kappa log store_first_site_is_bound
        in error
      else error
      in*)
    (*--------------------------------------------------------*)
    let store_init = get_init dynamic in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_init parameter error handler_kappa log store_init
        in error
      else error
    in
    (*--------------------------------------------------------*)
    (*dynamic information*)
    (*let store_relation_mvbdu = get_relation_mvbdu dynamic in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_relation_mvbdu parameter error handler_kappa log store_relation_mvbdu
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_range_site_first_agent = get_range_site_first_agent dynamic in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_range_site_first_agent parameter error handler_kappa log store_range_site_first_agent
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_range_site_second_agent = get_range_site_second_agent dynamic in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_range_site_second_agent parameter error handler_kappa log store_range_site_second_agent
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_result = get_site_accross_bonds_rhs static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_site_accross_bonds_domain parameter error handler_kappa log store_result
        in error
      else error
      in*)
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
