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
     at which position (its internal state ~u~p, ...)*)
  (* a map -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
     which rules can create a bond of type A.z.t.B (and at which position *)
  (* and a map (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
     which rules can contain parallel bonds in their lhs *)
  
  type local_static_information =
    {
      store_parallel_full_bonds_rhs: 
        Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t; 
      store_parallel_bonds_rhs : Ckappa_sig.PairAgentSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds_rhs_first_element:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_state)) list
        Ckappa_sig.Rule_map_and_set.Map.t;
      (*the parallel bonds in the left hand side.*)
      store_parallel_full_bonds_lhs : Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.t
        Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds_lhs : Ckappa_sig.PairAgentSites_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds_lhs_first_element:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_state)) list
        Ckappa_sig.Rule_map_and_set.Map.t;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  (*--------------------------------------------------------------------*)
  (* one map: for each tuple: Yes, No, Maybe, *)
  (* Yes: to say that when the sites x and y are bound with sites of
   the good type, then they are bound to the same B*)
  (* No: to say that when the sites x and y are bound with sites of the good
   type, then they are never bound to the same B*)
  (* Maybe: both case may happen*)

  type local_dynamic_information = 
    {
      store_bonds_init : Ckappa_sig.PairAgentSiteState_map_and_set.Set.t;
      store_parallel_init : Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.t;
      (*TODO: change name. a map of each rule*)
      (*store_flat_lattice : bool Usual_domains.flat_lattice*)
    }

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

  (*rhs*)
      
  let get_bonds_rhs static = lift Analyzer_headers.get_bonds_rhs static
    
  let get_bonds_lhs static = lift Analyzer_headers.get_bonds_lhs static
    
  let get_parallel_full_bonds_rhs static =
    (get_local_static_information static).store_parallel_full_bonds_rhs
      
  let set_parallel_full_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_full_bonds_rhs = bonds
      }
      static

  let get_parallel_bonds_rhs static =
    (get_local_static_information static).store_parallel_bonds_rhs

  let set_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds_rhs = bonds
      }
      static

  let get_parallel_bonds_rhs_first_element static =
    (get_local_static_information static).store_parallel_bonds_rhs_first_element

  let set_parallel_bonds_rhs_first_element bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds_rhs_first_element = bonds
      }
      static
      
  (**************************************************************************)
  (*lhs*)
    
  let get_parallel_bonds_lhs static = 
    (get_local_static_information static).store_parallel_bonds_lhs

  let set_parallel_bonds_lhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds_lhs = bonds
      }
      static

  let get_parallel_full_bonds_lhs static =
    (get_local_static_information static).store_parallel_full_bonds_lhs

  let set_parallel_full_bonds_lhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_full_bonds_lhs = bonds
      }
      static

  let get_parallel_bonds_lhs_first_element static =
    (get_local_static_information static).store_parallel_bonds_lhs_first_element

  let set_parallel_bonds_lhs_first_element bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds_lhs_first_element = bonds
      }
      static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

 let get_bonds_init dynamic =
    (get_local_dynamic_information dynamic).store_bonds_init

  let set_bonds_init init_map dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_bonds_init = init_map
      } dynamic

  let get_parallel_init dynamic =
    (get_local_dynamic_information dynamic).store_parallel_init

  let set_parallel_init init_map dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_parallel_init = init_map
      } dynamic

  (*let get_flat_lattice dynamic =
    (get_local_dynamic_information dynamic).store_flat_lattice

  let set_flat_lattice_init map dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_flat_lattice = map
      } dynamic*)
      
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
    | Cckappa_sig.Unknown_agent _ -> error, (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
    | Cckappa_sig.Dead_agent _ ->
      warn parameter error (Some "line 216") Exit (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
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
  (*collect a pair of bonds*)

  let collect_pair_of_bonds parameter error site_add agent_id site_type_source views =
    let error, pair =
      let agent_index_target = site_add.Cckappa_sig.agent_index in
      let site_type_target = site_add.Cckappa_sig.site in
      let error, agent_source = 
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id views
        with
        | error, None -> warn parameter error (Some "line 267") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, agent_target =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_index_target views
        with
        | error, None -> warn parameter error (Some "line 275") Exit Cckappa_sig.Ghost
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

  (**************************************************************************)
  (*collect parallel_bonds*)

  let add_parallel_link parameter error rule_id 
      ((agent_id1, agent_type1, site_type_source, old_site_type1, state1, old_state1),
       (agent_id2, agent_type2, site_type_target, old_site_type2, state2, old_state2))
      store_result =
    let error, old_parallel_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_result
      with
      | error, None -> error, Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error', set =
      Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
        parameter
        error
        ((agent_id1, agent_type1, site_type_source, old_site_type1, state1, old_state1),
         (agent_id2, agent_type2, site_type_target, old_site_type2, state2, old_state2))
        old_parallel_set
    in
    let error = Exception.check warn parameter error error' (Some "line 457") Exit in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
        rule_id 
        set
        store_result
    in
    error, store_result

  (**************************************************************************)

  let collect_parallel_bonds parameter error rule_id views bonds store_bonds_rhs 
      store_result =
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error 
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let error, pair =
                  collect_pair_of_bonds
                    parameter
                    error
                    site_add
                    agent_id
                    site_type_source
                    views
                in
                let ((agent_type1, site_type_source, state1),
                     (agent_type2, site_type_target, state2)) = pair 
                in
                let error, old_bonds_rhs_set =
                  match 
                    Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                      parameter
                      error 
                      rule_id
                      store_bonds_rhs
                  with
                  | error, None -> error, Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty
                  | error, Some p -> error, p
                in
                let error, store_result =
                  Ckappa_sig.PairAgentSiteState_map_and_set.Set.fold
                    (fun ((old_agent_type1, old_site_type1, old_state1), 
                          (old_agent_type2, old_site_type2, old_state2))
                      (error, store_result) ->
                      if (agent_type1 = old_agent_type1 && 
                          site_type_source <> old_site_type1 &&
                          agent_type2 = old_agent_type2 &&
                          site_type_target <> old_site_type2 &&
                          old_state1 = old_state2
                      )
                      then
                        let error, store_result =
                          add_parallel_link
                            parameter
                            error
                            rule_id
                            ((agent_id, agent_type1, site_type_source, old_site_type1, 
                              state1, old_state1),
                             (site_add.Cckappa_sig.agent_index, agent_type2, site_type_target,
                              old_site_type2, state2, old_state2))
                            store_result
                        in
                        error, store_result
                      else
                        error, store_result
                    ) old_bonds_rhs_set (error, store_result)
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result
        ) bonds store_result
    in
    error, store_result

  (**************************************************************************)
  (*collect parallel bonds in the rhs*)

  let collect_parallel_full_bonds_rhs parameter error rule_id rule store_bonds_rhs store_result =
    let error, store_result =
      collect_parallel_bonds
        parameter
        error
        rule_id
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
        store_bonds_rhs
        store_result
    in
    error, store_result

  let collect_parallel_full_bonds_lhs parameter error rule_id rule store_bonds_lhs store_result =
    let error, store_result =
      collect_parallel_bonds
        parameter
        error
        rule_id
        rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
        rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
        store_bonds_lhs
        store_result
    in
    error, store_result

  (**************************************************************************)

  let collect_parallel_bonds_rhs parameter error store_parallel_full_bonds_rhs =
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          let new_set =
            Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold
              (fun
                ((agent_id1, agent_type1, site_type_source, old_site1, state1, old_state1),
                 (agent_id2, agent_type2, site_type_target, old_site2, state2, old_state2)) store ->
                  let error, new_set =
                    Ckappa_sig.PairAgentSites_map_and_set.Set.add_when_not_in
                      parameter
                      error
                      ((agent_type1, site_type_source, old_site1), 
                       (agent_type2, site_type_target, old_site2))
                      store
                  in
                  new_set
              ) set Ckappa_sig.PairAgentSites_map_and_set.Set.empty
          in
          new_set
        ) store_parallel_full_bonds_rhs
    in
    error, store_result

  let collect_parallel_bonds_lhs parameter error store_parallel_full_bonds_lhs =
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          let new_set =
            Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold
              (fun
                ((agent_id1, agent_type1, site_type_source, old_site1, state1, old_state1),
                 (agent_id2, agent_type2, site_type_target, old_site2, state2, old_state2)) store ->
                  let error, new_set =
                    Ckappa_sig.PairAgentSites_map_and_set.Set.add_when_not_in
                      parameter
                      error
                      ((agent_type1, site_type_source, old_site1), 
                       (agent_type2, site_type_target, old_site2))
                      store
                  in
                  new_set
              ) set Ckappa_sig.PairAgentSites_map_and_set.Set.empty
          in
          new_set
        ) store_parallel_full_bonds_lhs
    in
    error, store_result

  (**************************************************************************)
      
  let collect_parallel_bonds_rhs_first_element parameter error store_parallel_full_bonds_rhs =
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          let list =
            Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold
              (fun
                ((agent_id1, agent_type1, site_type_source, old_site1, state1, old_state1),
                 (agent_id2, agent_type2, site_type_target, old_site2, state2, old_state2)) current_list ->
                  let list =
                    ((agent_id1, site_type_source, state1),
                     (agent_id2, site_type_target, state2)) :: current_list
                  in
                  list
              ) set []              
          in
          list
        ) store_parallel_full_bonds_rhs
    in
    error, store_result   

  let collect_parallel_bonds_lhs_first_element parameter error store_parallel_full_bonds_lhs =
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          let list =
            Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold
              (fun
                ((agent_id1, agent_type1, site_type_source, old_site1, state1, old_state1),
                 (agent_id2, agent_type2, site_type_target, old_site2, state2, old_state2)) current_list ->
                  let list =
                    ((agent_id1, site_type_source, state1),
                     (agent_id2, site_type_target, state2)) :: current_list
                  in
                  list
              ) set []              
          in
          list      
        ) store_parallel_full_bonds_lhs
    in
    error, store_result      

  (**************************************************************************)
  (*dynamic map related to flat_lattice*)

  (*let collect_flat_lattice parameter error rule_id rule store_result =
    let error, store_result =
      (*search rule that has bonds in their rhs*)
      let error, parallel_bonds_rhs =
        match
          Ckappa_sig.Rule_map_and_set.find_option_without_logs
            parameter
            error
            rule_id
            store_parallel_bonds
        with
        | error, None -> error, Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty
        | error, Some s -> error, s
      in
      (*search in this set in parallel bonds rhs, if one agent has two sites, and each sites 
        bound to the same agent*)
      

    in
    error, store_result*)
      
  (**************************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let store_bonds_rhs = get_bonds_rhs static in
    let store_parallel_full_bonds_rhs = get_parallel_full_bonds_rhs static in
    let error, store_parallel_full_bonds_rhs =
      collect_parallel_full_bonds_rhs
        parameter
        error
        rule_id
        rule.Cckappa_sig.e_rule_c_rule
        store_bonds_rhs
        store_parallel_full_bonds_rhs
    in
    let static = set_parallel_full_bonds_rhs store_parallel_full_bonds_rhs static in
    let store_parallel_full_bonds_rhs = get_parallel_full_bonds_rhs static in
    let error, store_parallel_bonds_rhs =
      collect_parallel_bonds_rhs 
        parameter
        error
        store_parallel_full_bonds_rhs
    in
    let static = set_parallel_bonds_rhs store_parallel_bonds_rhs static in
    let error, store_parallel_bonds_rhs_first_element =
      collect_parallel_bonds_rhs_first_element
        parameter
        error
        store_parallel_full_bonds_rhs
    in
    let static =
      set_parallel_bonds_rhs_first_element store_parallel_bonds_rhs_first_element static
    in
    error, static

  (**************************************************************************)
  (*parallel bonds on the lhs*)

  let scan_rule_set_bonds_lhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let store_bonds_lhs = get_bonds_lhs static in
    let store_bonds_lhs = get_bonds_lhs static in
    let store_parallel_full_bonds_lhs = get_parallel_full_bonds_lhs static in
    let error, store_parallel_full_bonds_lhs =
      collect_parallel_full_bonds_lhs 
        parameter
        error
        rule_id
        rule.Cckappa_sig.e_rule_c_rule
        store_bonds_lhs
        store_parallel_full_bonds_lhs
    in
    let static = set_parallel_full_bonds_lhs store_parallel_full_bonds_lhs static in
    let store_parallel_full_bonds_lhs = get_parallel_full_bonds_lhs static in
    (*-------------------------------------------------------------------------*)
    let error, store_parallel_bonds_lhs =
      collect_parallel_bonds_lhs
        parameter
        error
        store_parallel_full_bonds_lhs
    in
    let static = set_parallel_bonds_lhs store_parallel_bonds_lhs static in
    let error, store_parallel_bonds_lhs_first_element =
      collect_parallel_bonds_lhs_first_element
        parameter
        error
        store_parallel_full_bonds_lhs
    in
    let static =
      set_parallel_bonds_lhs_first_element store_parallel_bonds_lhs_first_element static
    in
    error, static

  (**************************************************************************)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun parameter error rule_id rule static ->
          (*parallel bonds in the rhs *)
          let error, static =
            scan_rule_set_bonds_rhs
              static
              dynamic
              error
              rule_id
              rule
          in
          (*parallel bonds in the lhs*)
          let error, static =
            scan_rule_set_bonds_lhs
              static
              dynamic
              error
              rule_id
              rule
          in
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
        store_parallel_full_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_rhs_first_element = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_full_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_lhs_first_element = Ckappa_sig.Rule_map_and_set.Map.empty;
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
    let init_local_dynamic =
      {
        store_bonds_init = Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty;
        store_parallel_init = Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.empty;
      }
    in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = init_local_dynamic ;
      }
    in
    let error, static, dynamic =
      scan_rule_set init_global_static_information init_global_dynamic_information error
    in
    error, static, dynamic

  (**************************************************************************)
  (*parallel bonds that may occur in intitial states*)
      
  let collect_bonds_initial static dynamic error init_state store_result =
    let parameter = get_parameter static in
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let error, pair =
                  collect_pair_of_bonds
                    parameter
                    error
                    site_add
                    agent_id
                    site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let error, store_result =
                  Ckappa_sig.PairAgentSiteState_map_and_set.Set.add_when_not_in
                    parameter
                    error
                    pair
                    store_result
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds store_result
    in
    error, store_result

  let collect_parallel_initial static dynamic error init_state store_bonds_init store_result =
    let parameter = get_parameter static in
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let error, pair =
                  collect_pair_of_bonds
                    parameter
                    error
                    site_add
                    agent_id
                    site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let ((agent_type1, site_type_source, state1),
                     (agent_type2, site_type_target, state2)) = pair 
                in
                (*get the old pair in initial bonds*)
                let error, store_result =
                  Ckappa_sig.PairAgentSiteState_map_and_set.Set.fold
                    (fun ((old_agent_type1, old_site_type1, old_state1),
                          (old_agent_type2, old_site_type2, old_state2)) (error, store_result) ->
                      if (agent_type1 = old_agent_type1 && 
                          site_type_source <> old_site_type1 &&
                          agent_type2 = old_agent_type2 &&
                          site_type_target <> old_site_type2 &&
                          old_state1 = old_state2)
                      then
                        let error', store_result =
                          Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                            parameter
                            error
                            ((agent_id, agent_type1, site_type_source, old_site_type1, state1, old_state1), 
                             (site_add.Cckappa_sig.agent_index, agent_type2, 
                              site_type_target, old_site_type2, state2, old_state2))
                            store_result
                        in
                        let error = Exception.check warn parameter error error' (Some "line 907") Exit in
                        error, store_result
                      else
                        error, store_result
                    ) store_bonds_init (error, store_result)
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds store_result
    in
    error, store_result

  (* to do *)
  (* take into account parallel bounds that may occur in initial states *)
  let add_initial_state static dynamic error species =
    let event_list = [] in
    let store_bonds_init = get_bonds_init dynamic in
    let error, store_bonds_init =
      collect_bonds_initial
        static
        dynamic
        error
        species
        store_bonds_init
    in
    let dynamic = set_bonds_init store_bonds_init dynamic in
    let store_bonds_init = get_bonds_init dynamic in
    let store_parallel_init = get_parallel_init dynamic in   
    let error, store_parallel_init = 
      collect_parallel_initial
        static
        dynamic
        error
        species
        store_bonds_init
        store_parallel_init
    in
    let dynamic = set_parallel_init store_parallel_init dynamic in
    error, dynamic, event_list

  (* to do *)
  (* if a parallel bound occur in a lhs, check that this is possible *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    (*let parameter = get_parameter static in
    let store_parallel_bonds_lhs = get_parallel_bonds_lhs static in
    (*check if there is parallel bonds in the lhs*)
    let b =
      Ckappa_sig.Rule_map_and_set.Map.mem
        rule_id
        store_parallel_bonds_lhs
    in
    if b
    then
      (*yes, there is parallel_bonds in the lhs*)
      error, dynamic, Some precondition
    else
      (*no*)*)
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

  let print_bonds static dynamic error store_result =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id set ->
        Ckappa_sig.PairAgentSiteState_map_and_set.Set.iter
          (fun ((agent_type1, site_type1, state1), (agent_type2, site_type2, state2)) ->
            let error, agent_type1_string =
              try Handler.string_of_agent parameter error kappa_handler agent_type1
              with
                _ -> warn parameter error (Some "line 1016") Exit
                  (Ckappa_sig.string_of_agent_name agent_type1)
            in
            let error, site_type1_string =
              try
                Handler.string_of_site parameter error kappa_handler agent_type1 site_type1
              with
                _ -> warn parameter error (Some "line 1023") Exit 
                  (Ckappa_sig.string_of_site_name site_type1)
            in
            let error, state1_string =
              try 
                Handler.string_of_state_fully_deciphered parameter error kappa_handler
                  agent_type1 site_type1 state1
              with
                _ -> warn parameter error (Some "line 1031") Exit
                  (Ckappa_sig.string_of_state_index state1)
            in
            let error, agent_type2_string =
              try Handler.string_of_agent parameter error kappa_handler agent_type2
              with
                _ -> warn parameter error (Some "line 1037") Exit
                  (Ckappa_sig.string_of_agent_name agent_type2)
            in
            let error, site_type2_string =
              try
                Handler.string_of_site parameter error kappa_handler agent_type2 site_type2
              with
                _ -> warn parameter error (Some "line 1044") Exit 
                  (Ckappa_sig.string_of_site_name site_type2)
            in
            let error, state2_string =
              try 
                Handler.string_of_state_fully_deciphered parameter error kappa_handler
                  agent_type2 site_type2 state2
              with
                _ -> warn parameter error (Some "line 1052") Exit
                  (Ckappa_sig.string_of_state_index state2)
            in
            let _ =
              Printf.fprintf stdout 
                "agent_type:%s:site_type:%s:state:%s -> agent_type:%s:site_type:%s:state:%s\n"
                agent_type1_string
                site_type1_string
                state1_string
                agent_type2_string
                site_type2_string
                state2_string
            in
            ()
          ) set
      ) store_result

  (**************************************************************************)
      
  let print_parallel_full_bonds static dynamic error store_parallel_full_bonds =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id pair ->
          let _ =
            Printf.fprintf stdout "rule_id:%i:\n" (Ckappa_sig.int_of_rule_id rule_id)
          in
          let _ =
            Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.iter
              (fun ((agent_id1, agent_type1, site_type1, old_site1, state1, old_state1),
                    (agent_id2, agent_type2, site_type2, old_site2, state2, old_state2)) ->
                let _ =
                  let error, agent_type1_string =
                    try Handler.string_of_agent parameter error kappa_handler agent_type1
                    with
                      _ -> warn parameter error (Some "line 479") Exit
                        (Ckappa_sig.string_of_agent_name agent_type1)
                  in
                  let error, site_type1_string =
                    try
                      Handler.string_of_site parameter error kappa_handler agent_type1 site_type1
                    with
                      _ -> warn parameter error (Some "line 492") Exit 
                        (Ckappa_sig.string_of_site_name site_type1)
                  in
                  let error, old_site1_string =
                    try
                      Handler.string_of_site parameter error kappa_handler agent_type1 old_site1
                    with
                      _ -> warn parameter error (Some "line 492") Exit 
                        (Ckappa_sig.string_of_site_name old_site1)
                  in
                  let error, state1_string =
                    try 
                      Handler.string_of_state_fully_deciphered parameter error kappa_handler
                        agent_type1 site_type1 state1
                    with
                      _ -> warn parameter error (Some "line 657") Exit
                        (Ckappa_sig.string_of_state_index state1)
                  in
                  let error, old_state1_string =
                    try 
                      Handler.string_of_state_fully_deciphered parameter error kappa_handler
                        agent_type1 old_site1 old_state1
                    with
                      _ -> warn parameter error (Some "line 673") Exit
                        (Ckappa_sig.string_of_state_index old_state1)
                  in
                  let error, agent_type2_string =
                    try Handler.string_of_agent parameter error kappa_handler agent_type2
                    with
                      _ -> warn parameter error (Some "line 479") Exit
                        (Ckappa_sig.string_of_agent_name agent_type2)
                  in
                  let error, site_type2_string =
                    try
                      Handler.string_of_site parameter error kappa_handler agent_type2 site_type2
                    with
                      _ -> warn parameter error (Some "line 688") Exit 
                        (Ckappa_sig.string_of_site_name site_type2)
                  in
                  let error, state2_string =
                    try 
                      Handler.string_of_state_fully_deciphered parameter error kappa_handler
                        agent_type2 site_type2 state2
                    with
                      _ -> warn parameter error (Some "line 665") Exit
                        (Ckappa_sig.string_of_state_index state2)
                  in
                  let error, old_site2_string =
                    try
                      Handler.string_of_site parameter error kappa_handler agent_type2 old_site2
                    with
                      _ -> warn parameter error (Some "line 695") Exit 
                        (Ckappa_sig.string_of_site_name old_site2)
                  in
                 
                  let error, old_state2_string =
                    try 
                      Handler.string_of_state_fully_deciphered parameter error kappa_handler
                        agent_type2 old_site2 old_state2
                    with
                      _ -> warn parameter error (Some "line 681") Exit
                        (Ckappa_sig.string_of_state_index old_state2)
                  in
                  Printf.fprintf stdout 
                    "agent_id:%i:agent_type:%s:site_type:%i:%s:site_type:%i:%s:state:%i:%s:state:%i:%s -> agent_id:%i:agent_type:%s:site_type:%i:%s:site_type:%i:%s:state:%i:%s:state:%i:%s\n"
                    (Ckappa_sig.int_of_agent_id agent_id1)
                    agent_type1_string
                    (Ckappa_sig.int_of_site_name site_type1)
                    site_type1_string
                    (Ckappa_sig.int_of_site_name old_site1)
                    old_site1_string
                    (Ckappa_sig.int_of_state_index state1)
                    state1_string
                    (Ckappa_sig.int_of_state_index old_state1)
                    old_state1_string
                    (Ckappa_sig.int_of_agent_id agent_id2)
                    agent_type2_string
                    (Ckappa_sig.int_of_site_name site_type2)
                    site_type2_string
                    (Ckappa_sig.int_of_site_name old_site2)
                    old_site2_string
                    (Ckappa_sig.int_of_state_index state2)
                    state2_string
                    (Ckappa_sig.int_of_state_index old_state2)
                    old_state2_string
                in
                ()
              ) pair
          in
          ()                        
        ) store_parallel_full_bonds
    in
    ()

  (**************************************************************************)

  let print_parallel_full_bonds_rhs static dynamic error =
    let store_parallel_full_bonds_rhs = get_parallel_full_bonds_rhs static in
    Printf.fprintf stdout "Parallel bonds full information in the rhs:\n";
    print_parallel_full_bonds static dynamic error store_parallel_full_bonds_rhs

  let print_parallel_full_bonds_lhs static dynamic error =
    let store_parallel_full_bonds_lhs = get_parallel_full_bonds_lhs static in
    Printf.fprintf stdout "Parallel bonds full information in the lhs:\n";
    print_parallel_full_bonds static dynamic error store_parallel_full_bonds_lhs

  (**************************************************************************)
  (*initial parallel*)

  let print_parallel_init_bonds static dynamic error store_result =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let _ =
      Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.iter
        (fun ((agent_id1, agent_type1, site_type1, old_site1, state1, old_state1),
              (agent_id2, agent_type2, site_type2, old_site2, state2, old_state2)) ->
          let _ =
            let error, agent_type1_string =
              try Handler.string_of_agent parameter error kappa_handler agent_type1
              with
                _ -> warn parameter error (Some "line 479") Exit
                  (Ckappa_sig.string_of_agent_name agent_type1)
            in
            let error, agent_type2_string =
              try Handler.string_of_agent parameter error kappa_handler agent_type2
              with
                _ -> warn parameter error (Some "line 479") Exit
                  (Ckappa_sig.string_of_agent_name agent_type2)
            in
            let error, site_type1_string =
              try
                Handler.string_of_site parameter error kappa_handler agent_type1 site_type1
              with
                _ -> warn parameter error (Some "line 492") Exit 
                  (Ckappa_sig.string_of_site_name site_type1)
            in
            let error, old_site1_string =
              try
                Handler.string_of_site parameter error kappa_handler agent_type1 old_site1
              with
                _ -> warn parameter error (Some "line 492") Exit 
                  (Ckappa_sig.string_of_site_name old_site1)
            in
            let error, state1_string =
              try 
                Handler.string_of_state_fully_deciphered parameter error kappa_handler
                  agent_type1 site_type1 state1
              with
                _ -> warn parameter error (Some "line 657") Exit
                  (Ckappa_sig.string_of_state_index state1)
            in
            let error, state2_string =
              try 
                Handler.string_of_state_fully_deciphered parameter error kappa_handler
                  agent_type2 site_type2 state2
              with
                _ -> warn parameter error (Some "line 665") Exit
                  (Ckappa_sig.string_of_state_index state2)
            in
            let error, old_state1_string =
              try 
                Handler.string_of_state_fully_deciphered parameter error kappa_handler
                  agent_type1 old_site1 old_state1
              with
                _ -> warn parameter error (Some "line 673") Exit
                  (Ckappa_sig.string_of_state_index old_state1)
            in
            let error, old_state2_string =
              try 
                Handler.string_of_state_fully_deciphered parameter error kappa_handler
                  agent_type2 old_site2 old_state2
              with
                _ -> warn parameter error (Some "line 681") Exit
                  (Ckappa_sig.string_of_state_index old_state2)
            in
            let error, site_type2_string =
              try
                Handler.string_of_site parameter error kappa_handler agent_type2 site_type2
              with
                _ -> warn parameter error (Some "line 688") Exit 
                  (Ckappa_sig.string_of_site_name site_type2)
            in
            let error, old_site2_string =
              try
                Handler.string_of_site parameter error kappa_handler agent_type2 old_site2
              with
                _ -> warn parameter error (Some "line 695") Exit 
                  (Ckappa_sig.string_of_site_name old_site2)
            in

            Printf.fprintf stdout 
              "agent_id:%i:agent_type:%s:site_type:%i:%s:site_type:%i:%s:state:%i:%s:state:%i:%s -> agent_id:%i:agent_type:%s:site_type:%i:%s:site_type:%i:%s:state:%i:%s:state:%i:%s\n"
              (Ckappa_sig.int_of_agent_id agent_id1)
              agent_type1_string
              (Ckappa_sig.int_of_site_name site_type1)
              site_type1_string
              (Ckappa_sig.int_of_site_name old_site1)
              old_site1_string
              (Ckappa_sig.int_of_state_index state1)
              state1_string
              (Ckappa_sig.int_of_state_index old_state1)
              old_state1_string
              (Ckappa_sig.int_of_agent_id agent_id2)
              agent_type2_string
              (Ckappa_sig.int_of_site_name site_type2)
              site_type2_string
              (Ckappa_sig.int_of_site_name old_site2)
              old_site2_string
              (Ckappa_sig.int_of_state_index state2)
              state2_string
              (Ckappa_sig.int_of_state_index old_state2)
              old_state2_string
          in
          ()
        ) store_result
    in
    ()

  let print_parallel_bonds_init static dynamic error =
    let store_parallel_init = get_parallel_init dynamic in
    Printf.fprintf stdout "Parallel bonds full information in the initial states:\n";
    print_parallel_init_bonds static dynamic error store_parallel_init

  (**************************************************************************)

  let print_parallel_bonds static dynamic error store_parallel_bonds =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id pair ->
          let _ =
            Printf.fprintf stdout "rule_id:%i:\n" (Ckappa_sig.int_of_rule_id rule_id)
          in
          let _ =
            Ckappa_sig.PairAgentSites_map_and_set.Set.iter
              (fun ((agent_type1, site_type1, old_site1),
                    (agent_type2, site_type2, old_site2)) ->
                let error, agent_type1_string =
                  try Handler.string_of_agent parameter error kappa_handler agent_type1
                  with
                    _ -> warn parameter error (Some "line 479") Exit
                      (Ckappa_sig.string_of_agent_name agent_type1)
                in
                let error, agent_type2_string =
                  try Handler.string_of_agent parameter error kappa_handler agent_type2
                  with
                    _ -> warn parameter error (Some "line 479") Exit
                      (Ckappa_sig.string_of_agent_name agent_type2)
                in
                let error, site_type1_string =
                  try
                    Handler.string_of_site parameter error kappa_handler agent_type1 site_type1
                  with
                    _ -> warn parameter error (Some "line 492") Exit 
                      (Ckappa_sig.string_of_site_name site_type1)
                in
                let error, old_site1_string =
                  try
                    Handler.string_of_site parameter error kappa_handler agent_type1 old_site1
                  with
                    _ -> warn parameter error (Some "line 492") Exit 
                      (Ckappa_sig.string_of_site_name old_site1)
                in
                let error, site_type2_string =
                  try
                    Handler.string_of_site parameter error kappa_handler agent_type2 site_type2
                  with
                    _ -> warn parameter error (Some "line 688") Exit 
                      (Ckappa_sig.string_of_site_name site_type2)
                in
                let error, old_site2_string =
                  try
                    Handler.string_of_site parameter error kappa_handler agent_type2 old_site2
                  with
                    _ -> warn parameter error (Some "line 695") Exit 
                      (Ckappa_sig.string_of_site_name old_site2)
                in
                let _ =
                  Printf.fprintf stdout 
                    "agent_type:%s:site_type:%s:site_type:%s -> agent_type:%s:site_type:%s:site_type:%s\n"
                    agent_type1_string
                    site_type1_string
                    old_site1_string
                    agent_type2_string
                    site_type2_string
                    old_site2_string
                in
                ()               
              )
              pair
          in
          ()
        ) store_parallel_bonds
    in
    ()

  (**************************************************************************)

  let print_parallel_bonds_rhs static dynamic error =
    let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    Printf.fprintf stdout "Parallel bonds in the rhs:\n";
    print_parallel_bonds static dynamic error store_parallel_bonds_rhs

  let print_parallel_bonds_lhs static dynamic error =
    let store_parallel_bonds_lhs = get_parallel_bonds_lhs static in
    Printf.fprintf stdout "Parallel bonds in the lhs:\n";
    print_parallel_bonds static dynamic error store_parallel_bonds_lhs

  (**************************************************************************)
      
  let print_parallel_bonds_first_element static dynamic error store_result =
    let kappa_handler = get_kappa_handler static in
    let store_parallel_bonds_rhs_first_element = get_parallel_bonds_rhs_first_element static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id list ->
          let _ =
            Printf.fprintf stdout "rule_id:%i:\n" (Ckappa_sig.int_of_rule_id rule_id)
          in
          let _ =
            List.iter (fun ((agent_id1, site_type1, state1), (agent_id2, site_type2, state2)) ->
              Printf.fprintf stdout 
                "agent_id:%i:site_type:%i:state:%i -> agent_id:%i:site_type:%i:state:%i\n"
                (Ckappa_sig.int_of_agent_id agent_id1)
                (Ckappa_sig.int_of_site_name site_type1)
                (Ckappa_sig.int_of_state_index state1)
                (Ckappa_sig.int_of_agent_id agent_id2)
                (Ckappa_sig.int_of_site_name site_type2)
                (Ckappa_sig.int_of_state_index state2)              
            ) list  
          in
          ()
        ) store_result
    in
    ()

  (**************************************************************************)

  let print_parallel_bonds_rhs_first_element static dynamic error =
    let store_parallel_bonds_rhs_first_element = get_parallel_bonds_rhs_first_element static in
    Printf.fprintf stdout "Parallel bonds in the first element in the rhs:\n";
    print_parallel_bonds_first_element static dynamic error store_parallel_bonds_rhs_first_element

  let print_parallel_bonds_lhs_first_element static dynamic error =
    let store_parallel_bonds_lhs_first_element = get_parallel_bonds_lhs_first_element static in
    Printf.fprintf stdout "Parallel bonds in the first element in the lhs:\n";
    print_parallel_bonds_first_element static dynamic error store_parallel_bonds_lhs_first_element

  (**************************************************************************)

  (* to do *)
  let print static dynamic error loggers =
    (*let _ =
      print_parallel_full_bonds_rhs static dynamic error
    in
    let _ =
      print_parallel_bonds_rhs static dynamic error
    in
    let _ =
      print_parallel_bonds_rhs_first_element static dynamic error
    in
    (*---------------------------------------------------------*)
    let _ =
      print_parallel_full_bonds_lhs static dynamic error
    in
    let _ =
      print_parallel_bonds_lhs static dynamic error
    in
    let _ =
      print_parallel_bonds_lhs_first_element static dynamic error
    in
    (*---------------------------------------------------------*)
    (*print dynamic information for parallel bonds in the initial states*)
    let _ =
      print_parallel_bonds_init static dynamic error
    in*)
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
