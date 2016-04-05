(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to detect whether when two sites of an agent are bound, they must be bound to the same agent.
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

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  (* domain specific info: *)
   (* collect the set of tuples (A,x,y,B,z,t) such that there exists a rule
      with two agent of type A and B and two bonds between A.x and B.z, and A.y
      and B.t *)
   (* for each tuple, collect a map -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list
      RuleIdMap to explain which rules can create a bond of type A.x.z.B (and
      at which position *)
   (* a map -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
      which rules can create a bond of type A.z.t.B (and at which position *)
   (* and a map (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
      which rules can contain parallel bonds in their lhs *)
  
  type local_static_information =
    {
      (*not pair: (agent_type, site_type, old_site_type),(agent_type', old_site_type', site_type') *)
      store_bonds : Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds : Ckappa_sig.PairAgentSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information (* to be replaced *)
    }

  (*--------------------------------------------------------------------*)
  (* one map: for each tuple: Yes, No, Maybe, *)
  (* Yes: to say that when the sites x and y are bound with sites of
   the good type, then they are bound to the same B*)
  (* No: to say that when the sites x and y are bound with sites of the good
   type, then they are never bound to the same B*)
  (* Maybe: both case may happen*)

  type local_dynamic_information = unit (* to be replaced *)

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  let get_local_static_information static = static.local_static_information
    
  let set_local_static_information local static =
    {
      static with
        local_static_information = local
    }

  let get_bonds static = (get_local_static_information static).store_bonds

  let set_bonds bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_bonds = bonds
      }
      static
    
  (*get parallel bonds*)

  let get_parallel_bonds static = (get_local_static_information static).store_parallel_bonds

  let set_parallel_bonds bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds = bonds
      }
      static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  (*--------------------------------------------------------------------*)

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

  (**************************************************************************)
  (*implementation of static information*)

  (**************************************************************************)
  (*store parallel bonds on the rhs*)

  let collect_agent_type_state parameter error agent site_type =
    match agent with
    | Cckappa_sig.Ghost
    | Cckappa_sig.Unknown_agent _
    | Cckappa_sig.Dead_agent _ ->
      warn parameter error (Some "line 199") Exit (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
    | Cckappa_sig.Agent agent1 ->
      let agent_type1 = agent1.Cckappa_sig.agent_name in
      let error, state1 =
        match Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
          parameter
          error
          site_type
          agent1.Cckappa_sig.agent_interface
        with
        | error, None ->  warn parameter error (Some "line 228") Exit Ckappa_sig.dummy_state_index
        | error, Some port ->
          let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
          if (Ckappa_sig.int_of_state_index state) > 0
          then error, state
          else warn parameter error (Some "line 196") Exit Ckappa_sig.dummy_state_index
      in
      error, (agent_type1, state1) 

  (**************************************************************************)

  let add_link_set parameter error rule_id
      ((agent_type1, site_type1, state1), (agent_type2, site_type2, state2)) store_result =
    let error, old_set =
      match 
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error 
          rule_id
          store_result
      with
      | error, None -> error, Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty
      | error, Some p -> error, p
    in
    let error, set =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.add_when_not_in
        parameter
        error
        ((agent_type1, site_type1, state1), (agent_type2, site_type2, state2))
        old_set
    in
    let error'', union_set =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.union parameter error old_set set 
    in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id union_set store_result
    in
    error, store_result

  (**************************************************************************)

  let collect_bonds parameter error rule_id rule store_result =
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error 
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let agent_index_target = site_add.Cckappa_sig.agent_index in
                let site_type_target = site_add.Cckappa_sig.site in
                let error, agent_source =
                  match Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                  with
                  | error, None -> warn parameter error (Some "line 135") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, agent_target =
                  match Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_index_target rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                  with
                  | error, None -> warn parameter error (Some "line 143") Exit Cckappa_sig.Ghost
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
                let pair = ((agent_type1, site_type_source, state1), (agent_type2, site_type_target, state2)) in
                (*search in bonds rhs*)
                let error, store_result =
                  add_link_set parameter error rule_id pair store_result
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result                
        ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds store_result
    in
    error, store_result

  (**************************************************************************)

  let collect_parallel_bonds parameter error rule_id rule store_bonds store_result =
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error 
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let agent_index_target = site_add.Cckappa_sig.agent_index in
                let site_type_target = site_add.Cckappa_sig.site in
                let error, agent_source =
                  match Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                  with
                  | error, None -> warn parameter error (Some "line 135") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, agent_target =
                  match Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_index_target rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                  with
                  | error, None -> warn parameter error (Some "line 143") Exit Cckappa_sig.Ghost
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
                let error, old_bonds_set =
                  match 
                    Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                      parameter
                      error 
                      rule_id
                      store_bonds
                  with
                  | error, None -> error, Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty
                  | error, Some p -> error, p
                in
                let error, store_result =
                  Ckappa_sig.PairAgentSiteState_map_and_set.Set.fold
                    (fun ((old_agent_type1, old_site_type1, _), 
                          (old_agent_type2, old_site_type2, _)) (error, store_result) ->
                      if (agent_type1 = old_agent_type1 && 
                          site_type_source <> old_site_type1 &&
                          agent_type2 = old_agent_type2 &&
                          site_type_target <> old_site_type2
                      )
                      then
                        let error, old_set =
                          match 
                            Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                              parameter
                              error
                              rule_id
                              store_result
                          with
                          | error, None -> error, Ckappa_sig.PairAgentSites_map_and_set.Set.empty
                          | error, Some s -> error, s
                        in
                        let error, set =
                          Ckappa_sig.PairAgentSites_map_and_set.Set.add_when_not_in
                            parameter
                            error
                            ((agent_type1, site_type_source, old_site_type1),
                             (agent_type2, site_type_target, old_site_type2)
                            )
                            old_set
                        in
                        let error'', union_set =
                          Ckappa_sig.PairAgentSites_map_and_set.Set.union parameter error old_set set 
                        in
                        let error, store_result =
                          Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
                            rule_id union_set store_result
                        in
                        error, store_result
                      else
                        error, store_result
                    ) old_bonds_set (error, store_result)
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result                
        ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds store_result
    in
    error, store_result
          
  (**************************************************************************)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun parameter error rule_id rule static ->
          let store_bonds = get_bonds static in
          (*parallel bonds on the rhs*)
          let error, store_bonds =
            collect_bonds
              parameter
              error
              rule_id
              rule.Cckappa_sig.e_rule_c_rule
              store_bonds
          in
          let static = set_bonds store_bonds static in
          let store_bonds = get_bonds static in
          let store_parallel_bonds = get_parallel_bonds static in
          (*parallel bonds on the rhs*)
          let error, store_parallel_bonds =
            collect_parallel_bonds
              parameter
              error
              rule_id
              rule.Cckappa_sig.e_rule_c_rule
              store_bonds
              store_parallel_bonds
          in
          let static = set_parallel_bonds store_parallel_bonds static in
          error, static
        ) compil.Cckappa_sig.rules static
    in
    error, static, dynamic
    

  (**************************************************************************)
  (** [get_scan_rule_set static] *)

  (* todo *)
  let initialize static dynamic error =
    let init_local_static =
      {
        store_bonds = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds = Ckappa_sig.Rule_map_and_set.Map.empty;
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_local_static;
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let parameter = Analyzer_headers.get_parameter static in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = () ;
      }
    in
    let error, static, dynamic =
      scan_rule_set init_global_static_information init_global_dynamic_information error
    in
    error, static, dynamic

  (* to do *)
  (* take into account parallel bounds that may occur in initial states *)
  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* if a parallel boudn occur in a lhs, check that this is possible *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
      error, dynamic, Some precondition

  (* to do, when one bond is created, check in the precondition, whether
     the two other sites may be bound, check whether they must be bound to the
     same agents, whether they cannot be bound to the same agent, whether we
     cannot know, and deal with accordingly *)
  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compil = get_compil static in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)
      
  let print_parallel_bonds store_result =
    Printf.fprintf stdout "Parallel bonds:\n";
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id pair ->
        let _ =
          Printf.fprintf stdout "rule_id:%i:\n" (Ckappa_sig.int_of_rule_id rule_id)
        in
        let _ =
          Ckappa_sig.PairAgentSites_map_and_set.Set.iter
            (fun ((agent_type1, old_site1, site1),(agent_type2, old_site2, site2)) ->
              let _ =
                Printf.fprintf stdout 
                  "agent_type1:%i:old_site_type1:%i:site1:%i -> agent_type2:%i:old_site_type2:%i:site2:%i\n"
                  (Ckappa_sig.int_of_agent_name agent_type1)
                  (Ckappa_sig.int_of_site_name old_site1)
                  (Ckappa_sig.int_of_site_name site1)
                  (Ckappa_sig.int_of_agent_name agent_type2)
                  (Ckappa_sig.int_of_site_name old_site2)
                  (Ckappa_sig.int_of_site_name site2)
              in
              ()
            ) pair
        in
        ()                        
      ) store_result


  (* to do *)
  let print static dynamic error loggers =
    (*let store_parallel_bonds = get_parallel_bonds static in
    let _ =
      print_parallel_bonds store_parallel_bonds
    in*)
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
