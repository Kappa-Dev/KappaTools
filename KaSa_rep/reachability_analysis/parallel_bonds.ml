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
     RuleIdMap to explain which rules can create a bond of type A.x.z.B
     (and at which position (its internal state ~u~p, ...).*)
  (* a map -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
     which rules can create a bond of type A.y.t.B (and at which position *)
  (* and a map (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
     which rules can contain parallel bonds in their lhs *)
  
  type local_static_information =
    {
      store_bonds_rhs_full : Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t 
      Ckappa_sig.Rule_map_and_set.Map.t;
      store_bonds_lhs_full : Ckappa_sig.PairAgentsSiteState_map_and_set.Set.t 
      Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds_rhs: Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.t;
      store_rule_has_parallel_bonds_rhs: 
        Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_fst_site_create_parallel_bonds_rhs:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * 
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * 
               Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
               Ckappa_sig.c_state * Ckappa_sig.c_state)) list
        Ckappa_sig.PairAgentsSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information  : local_static_information
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
      store_status_of_parallel_bonds :
      bool Usual_domains.flat_lattice Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.t
    }

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information;
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

  let get_action_binding static = lift Analyzer_headers.get_action_binding static
    
  let get_bonds_rhs_full static =
    (get_local_static_information static).store_bonds_rhs_full
      
  let set_bonds_rhs_full bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_bonds_rhs_full = bonds
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
      
  let get_rule_has_parallel_bonds_rhs static =
    (get_local_static_information static).store_rule_has_parallel_bonds_rhs
      
  let set_rule_has_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_rule_has_parallel_bonds_rhs = bonds
      }
      static
      
  let get_fst_site_create_parallel_bonds_rhs static =
    (get_local_static_information static).store_fst_site_create_parallel_bonds_rhs
      
  let set_fst_site_create_parallel_bonds_rhs l static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_fst_site_create_parallel_bonds_rhs = l
      }
      static

  (*lhs*)

  let get_bonds_lhs_full static =
    (get_local_static_information static).store_bonds_lhs_full
      
  let set_bonds_lhs_full bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_bonds_lhs_full = bonds
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

  let get_status_of_parallel_bonds dynamic =
    (get_local_dynamic_information dynamic).store_status_of_parallel_bonds

  let set_status_of_parallel_bonds value dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_status_of_parallel_bonds = value
      } dynamic
      
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
  (*Right hand side bond:
    (agent_id, site_type, state, -> agent_id, site_type, state)*)
  (**************************************************************************)

  let collect_bonds_full parameter error rule_id views bonds store_result =
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
              | error, None -> warn parameter error (Some "line 192") Exit Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            (*get pair agent_type, state*)
            let error, (agent_type1, state1) =
              Common_static.collect_agent_type_state
                parameter
                error
                agent_source
                site_type_source
            in
            (*------------------------------------------------------------------------------*)
            (*second pair*)
            let error, agent_target =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id_target views
              with
              | error, None -> warn parameter error (Some "line 210") Exit Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            let error, (agent_type2, state2) =
              Common_static.collect_agent_type_state
                parameter
                error
                agent_target
                site_type_target
            in
            (*------------------------------------------------------------------------------*)
            (*get old*)
            let error, old_set =
              match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error 
                rule_id store_result
              with
              | error, None -> error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
              | error, Some p -> error, p
            in
            let error', set = 
              Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                parameter error 
                ((agent_id, agent_type1, site_type_source, state1),
                 (agent_id_target, agent_type2, site_type_target, state2))
                old_set
            in    
            let error = Exception.check warn parameter error error' (Some "line 236") Exit in
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
      ) bonds store_result
      
  (**************************************************************************)
      
  let collect_bonds_rhs_full parameter error rule_id rule store_result=
    collect_bonds_full 
      parameter
      error
      rule_id
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
      store_result

  let collect_bonds_lhs_full parameter error rule_id rule store_result =
    collect_bonds_full 
      parameter
      error
      rule_id
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds  
      store_result
      
  (**************************************************************************)
  (*Parallel bonds*)
  (**************************************************************************)

  let collect_parallel_bonds static error rule_id views bonds store_bonds_full store_result =
    let parameter = get_parameter static in
    (*--------------------------------------------*)
    let error, bonds_full_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter 
          error
          rule_id
          store_bonds_full
      with
        | error, None -> error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
        | error, Some s -> error, s
    in
    (*--------------------------------------------*)
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error 
        (fun parameter error agent_id_source bonds_map store_result ->
          Ckappa_sig.Site_map_and_set.Map.fold
            (fun site_type_source site_add (error, store_result) ->
              let agent_id_target = site_add.Cckappa_sig.agent_index in
              let site_type_target = site_add.Cckappa_sig.site in
              (*------------------------------------------------------------------------------*)
              (*first pair*)
              let error, agent_source = 
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None -> warn parameter error (Some "line 302") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                Common_static.collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*------------------------------------------------------------------------------*)
              (*second pair*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None -> warn parameter error (Some "line 319") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_target, state_target) =
                Common_static.collect_agent_type_state
                  parameter
                  error
                  agent_target
                  site_type_target
              in
              (*------------------------------------------------------------------------------*)
              (*parallel bonds*)
              Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
                (fun ((agent_id, agent_type, site_type, state),
                      (agent_id', agent_type', site_type', state'))
                  (error, store_result) ->
                    if agent_id = agent_id_source &&
                      agent_id' = agent_id_target &&
                      site_type <> site_type_source &&
                      site_type' <> site_type_target
                    then
                      let error', set =
                        Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                          parameter
                          error
                          ((agent_id_source, agent_type_source, 
                            site_type_source, site_type, state_source, state),
                           (agent_id_target, agent_type_target, 
                            site_type_target, site_type', state_target,state'))
                          store_result
                      in
                      let error = Exception.check warn parameter error error' (Some "line 350") Exit in
                      error, set
                    else
                      error, store_result
                ) bonds_full_set (error, store_result)
            ) bonds_map (error, store_result)
        ) bonds store_result
    in
    error, store_result

  (**************************************************************************)

  let collect_parallel_bonds_rhs static error rule_id rule =
    let parameter = get_parameter static in
    let store_bonds_rhs_full = get_bonds_rhs_full static in
    let store_result = get_parallel_bonds_rhs static in
    let error, store_result =
      collect_parallel_bonds
        static
        error
        rule_id
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
        store_bonds_rhs_full
        store_result
    in
    let static = set_parallel_bonds_rhs store_result static in
    error, static

let collect_rule_has_parallel_bonds static error rule_id views bonds store_bonds_full store_result =
    let parameter = get_parameter static in
    (*--------------------------------------------*)
    let error, bonds_full_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter 
          error
          rule_id
          store_bonds_full
      with
        | error, None -> error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
        | error, Some s -> error, s
    in
    (*--------------------------------------------*)
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error 
        (fun parameter error agent_id_source bonds_map store_result ->
          Ckappa_sig.Site_map_and_set.Map.fold
            (fun site_type_source site_add (error, store_result) ->
              let agent_id_target = site_add.Cckappa_sig.agent_index in
              let site_type_target = site_add.Cckappa_sig.site in
              (*------------------------------------------------------------------------------*)
              (*first pair*)
              let error, agent_source = 
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None -> warn parameter error (Some "line 335") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                Common_static.collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*------------------------------------------------------------------------------*)
              (*second pair*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None -> warn parameter error (Some "line 352") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_target, state_target) =
                Common_static.collect_agent_type_state
                  parameter
                  error
                  agent_target
                  site_type_target
              in
              (*------------------------------------------------------------------------------*)
              (*parallel bonds*)
              Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
                (fun ((agent_id, agent_type, site_type, state),
                      (agent_id', agent_type', site_type', state')) (error, store_result) ->
                    if agent_id = agent_id_source &&
                      agent_id' = agent_id_target &&
                      site_type <> site_type_source &&
                      site_type' <> site_type_target
                    then
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
                          ((agent_id_source, agent_type_source, 
                            site_type_source, site_type, state_source, state),
                           (agent_id_target, agent_type_target, 
                            site_type_target, site_type', state_target,state'))
                          old_parallel_set
                      in
                      let error = Exception.check warn parameter error error' (Some "line 393") Exit in
                      let error, store_result =
                        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
                          rule_id 
                          set
                          store_result
                      in
                      error, store_result
                    else
                      error, store_result
                ) bonds_full_set (error, store_result)
            ) bonds_map (error, store_result)
        ) bonds store_result
    in
    error, store_result

  (**************************************************************************)

  let collect_rule_has_parallel_bonds_rhs static error rule_id rule =
    let parameter = get_parameter static in
    let store_bonds_rhs_full = get_bonds_rhs_full static in
    let store_result = get_rule_has_parallel_bonds_rhs static in
    let error, store_result =
      collect_rule_has_parallel_bonds
        static
        error
        rule_id
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
        store_bonds_rhs_full
        store_result
    in
    let static = set_rule_has_parallel_bonds_rhs store_result static in
    error, static

 (**************************************************************************)

 let collect_fst_site_create_parallel_bonds static error rule_id store_action_binding
      store_parallel_bonds store_result =
    let parameter = get_parameter static in
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
          (*A.x -> B.x; B.x -> A.x*)
          (fun ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state')) store_result ->
            let error, old_list =
              match
                Ckappa_sig.PairAgentsSiteState_map_and_set.Map.find_option_without_logs
                  parameter
                  error
                  ((agent_id, agent_type, site_type, state),
                   (agent_id', agent_type', site_type', state'))
                  store_result
              with
              | error, None -> error, []
              | error, Some l -> error, l
            in
            let error, new_list =
              Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold_inv
                (fun ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                      (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                  (error, current_list) ->
                    (*Remark only check one direction of the bond: A.x -> B.x*)
                    if 
                      agent_id = agent_id1 &&
                      site_type = site_type1 &&
                      agent_id' = agent_id1' &&
                      site_type' = site_type1'
                  then
                    let new_list =
                      ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                       (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) :: current_list
                    in
                    error, new_list
                  else 
                    error, current_list
                ) store_parallel_bonds (error, old_list)
            in
            let error, store_result =
              Ckappa_sig.PairAgentsSiteState_map_and_set.Map.add_or_overwrite
                parameter
                error
                ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state'))
                new_list
                store_result
            in
            store_result
          ) set Ckappa_sig.PairAgentsSiteState_map_and_set.Map.empty
        ) store_action_binding


  let collect_fst_site_create_parallel_bonds_rhs static error rule_id =
    let store_action_binding = get_action_binding static in
    let store_parallel_bonds = get_parallel_bonds_rhs static in
    let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
    let store_result =
      collect_fst_site_create_parallel_bonds
        static
        error
        rule_id
        store_action_binding
        store_parallel_bonds
        store_fst_site_create_parallel_bonds_rhs
    in
    let static = set_fst_site_create_parallel_bonds_rhs store_result static in
    error, static
  
  (**************************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let store_bonds_rhs_full = get_bonds_rhs_full static in
    let error, store_bonds_rhs_full =
      collect_bonds_rhs_full
        parameter
        error
        rule_id
        rule
        store_bonds_rhs_full
    in
    let static = set_bonds_rhs_full store_bonds_rhs_full static in
    let store_bonds_lhs_full = get_bonds_lhs_full static in
    let error, store_bonds_lhs_full =
      collect_bonds_lhs_full
        parameter
        error
        rule_id
        rule
        store_bonds_lhs_full
    in
    let static = set_bonds_lhs_full store_bonds_lhs_full static in
    let error, static =
      collect_parallel_bonds_rhs
        static        
        error
        rule_id
        rule
    in
    let error, static =
      collect_rule_has_parallel_bonds_rhs
        static        
        error
        rule_id
        rule
    in
    (*rule that created a bond belong to parallel bond*)
    let error, static =
      collect_fst_site_create_parallel_bonds_rhs
        static
        error
        rule_id
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
              rule.Cckappa_sig.e_rule_c_rule
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
        store_bonds_rhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_bonds_lhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_rhs = Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.empty;
        store_rule_has_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_fst_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_local_static;
      }
    in
     let init_local_dynamic_information =
       {
         store_status_of_parallel_bonds = Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.empty
       }
    in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = init_local_dynamic_information ;
      }
    in
    let error, static, dynamic =
      scan_rule_set init_global_static_information init_global_dynamic_information error
    in
    error, static, dynamic

  (**************************************************************************)
  (*parallel bonds in the initial state*)
  (**************************************************************************)
      
  let collect_bonds_initial static error init_state =
    let parameter = get_parameter static in
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let error, ((agent_type_source, site_type_source, state_source),
                            (agent_type_target, site_type_target, state_target)) =
                  Common_static.collect_pair_of_bonds
                    parameter
                    error
                    site_add
                    agent_id
                    site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let pair = ((agent_id, agent_type_source, site_type_source, state_source),
                            (site_add.Cckappa_sig.agent_index, agent_type_target, site_type_target,
                             state_target))
                in
                let error, store_result =
                  Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in
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
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
    in
    error, store_result
      
  let collect_parallel_bonds_init static dynamic error init_state =
    let error, store_bonds_init =
      collect_bonds_initial static error init_state
    in
    let parameter = get_parameter static in
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter error
        (fun parameter error agent_id_source bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let error, pair =
                  Common_static.collect_pair_of_bonds
                    parameter
                    error
                    site_add
                    agent_id_source
                    site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let ((agent_type_source, site_type_source, state_source),
                     (agent_type_target, site_type_target, state_target)) = pair 
                in
                (*get the old pair in initial bonds*)
                let error, store_result =
                  Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
                    (fun ((agent_id, agent_type, site_type, state),
                          (agent_id', agent_type', site_type', state')) (error, store_result) ->
                      if agent_id = agent_id_source &&
                        agent_id' = site_add.Cckappa_sig.agent_index &&
                        site_type_source <> site_type &&
                        site_type_target <> site_type'
                      then
                        let error', store_result =
                          Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                            parameter
                            error
                            ((agent_id, agent_type_source, site_type_source, site_type, state_source, state), 
                             (site_add.Cckappa_sig.agent_index, agent_type_target, 
                              site_type_target, site_type', state_target, state'))
                            store_result
                        in
                        let error = Exception.check warn parameter error error' (Some "line 813") Exit in
                        error, store_result
                      else
                        error, store_result
                    ) store_bonds_init (error, store_result)
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
        Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.empty
    in
    error, store_result

  (**************************************************************************)
  (*PRINT*)
  (**************************************************************************)    

  let print_parallel_bonds static dynamic error store_result =
    let handler_kappa = get_kappa_handler static in
    let parameter = get_parameter static in
    let _ =
      Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.iter
        (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2), 
              (agent_id', agent_type', site_type1', site_type2', state1', state2')) ->
          let error, site_type1_string =
            try
              Handler.string_of_site parameter error handler_kappa
                agent_type site_type1
            with
              _ -> warn parameter error (Some "line 651") Exit
                (Ckappa_sig.string_of_site_name site_type1)
          in
          let error, site_type1_string' =
            try
              Handler.string_of_site parameter error handler_kappa
                agent_type' site_type1'
            with
              _ -> warn parameter error (Some "line 659") Exit
                (Ckappa_sig.string_of_site_name site_type1')
          in
          let error, site_type2_string =
            try
              Handler.string_of_site parameter error handler_kappa
                agent_type site_type2
            with
              _ -> warn parameter error (Some "line 667") Exit
                (Ckappa_sig.string_of_site_name site_type2)
          in
          let error, site_type2_string' =
            try
              Handler.string_of_site parameter error handler_kappa
                agent_type' site_type2'
            with
              _ -> warn parameter error (Some "line 675") Exit
                (Ckappa_sig.string_of_site_name site_type2')
          in
          let error, state1_string =
            try
	      Handler.string_of_state_fully_deciphered parameter error handler_kappa
		agent_type site_type1 state1
	    with
	      _ -> warn parameter error (Some "line 683") Exit
                (Ckappa_sig.string_of_state_index state1)
          in
          let error, state2_string =
            try
	      Handler.string_of_state_fully_deciphered parameter error handler_kappa
		agent_type site_type2 state2
	    with
	      _ -> warn parameter error (Some "line 691") Exit
                (Ckappa_sig.string_of_state_index state2)
          in
          let error, state1_string' =
            try
	      Handler.string_of_state_fully_deciphered parameter error handler_kappa
		agent_type' site_type1' state1'
	    with
	      _ -> warn parameter error (Some "line 699") Exit
                (Ckappa_sig.string_of_state_index state1')
          in
          let error, state2_string' =
            try
	      Handler.string_of_state_fully_deciphered parameter error handler_kappa
		agent_type' site_type2' state2'
	    with
	      _ -> warn parameter error (Some "line 707") Exit
                (Ckappa_sig.string_of_state_index state2')
          in              
          let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_id:%i:site_type:%i:%s.%s:site_type:%i:%s.%s->\
               agent_id:%i:site_type:%i:%s.%s:site_type:%i:%s.%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              (Ckappa_sig.int_of_site_name site_type1)
              site_type1_string
              state1_string
              (Ckappa_sig.int_of_site_name site_type2)
              site_type2_string
              state2_string
              (Ckappa_sig.int_of_agent_id agent_id')
              (Ckappa_sig.int_of_site_name site_type1')
              site_type1_string'
              state1_string'
              (Ckappa_sig.int_of_site_name site_type2')
              site_type2_string'
              state2_string'
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameter)
        ) store_result
    in
    ()
      
  let print_parallel_bonds_rhs static dynamic error =
    let parameter = get_parameter static in
    let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "\nParallel bonds in the rhs:\n";
    print_parallel_bonds static dynamic error store_parallel_bonds_rhs

  (**************************************************************************)

  let print_rule_has_parallel_bonds static dynamic error store_result =
    let handler_kappa = get_kappa_handler static in
    let parameter = get_parameter static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id set ->
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);            
          let _ =
            print_parallel_bonds static dynamic error set
          in
          ()
        ) store_result
    in
    ()
      
  let print_rule_has_parallel_bonds_rhs static dynamic error =
    let parameter = get_parameter static in
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "\nParallel bonds in the rhs with rule:\n";
    print_rule_has_parallel_bonds static dynamic error store_rule_has_parallel_bonds_rhs


  (**************************************************************************)

  let print_action_binding static dynamic error =
    let store_action_binding = get_action_binding static in
    let parameter = get_parameter static in    
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "Action binding:\n";
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id set ->
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);
          Ckappa_sig.PairAgentsSiteState_map_and_set.Set.iter
            (fun ((agent_id, agent_type, site_type, state), 
                  (agent_id', agent_type', site_type', state')) ->
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "agent_id:%i,site_type:%i -> agent_id:%i, site_type:%i\n"
                (Ckappa_sig.int_of_agent_id agent_id)
                (Ckappa_sig.int_of_site_name site_type)
                (Ckappa_sig.int_of_agent_id agent_id')
                (Ckappa_sig.int_of_site_name site_type');
              Loggers.print_newline (Remanent_parameters.get_logger parameter)
            ) set
        ) store_action_binding
    in
    ()

  (**************************************************************************)

 let print_site_create_parallel static dynamic error store_result =
   let handler_kappa = get_kappa_handler static in
   let parameter = get_parameter static in
   let _ =
     Ckappa_sig.Rule_map_and_set.Map.iter
       (fun rule_id map ->
         let _ =
           Loggers.fprintf (Remanent_parameters.get_logger parameter)
             "rule_id:%i\n"
             (Ckappa_sig.int_of_rule_id rule_id)
         in
         Ckappa_sig.PairAgentsSiteState_map_and_set.Map.iter
           (fun ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state')) list ->
             let error, site_type_string =
               try
                 Handler.string_of_site parameter error handler_kappa
                   agent_type site_type
               with
                 _ -> warn parameter error (Some "line 789") Exit
                   (Ckappa_sig.string_of_site_name site_type)
             in
             let error, site_type_string' =
               try
                 Handler.string_of_site parameter error handler_kappa
                   agent_type' site_type'
               with
                 _ -> warn parameter error (Some "line 797") Exit
                   (Ckappa_sig.string_of_site_name site_type')
             in
             let error, state_string =
               try
	         Handler.string_of_state_fully_deciphered parameter error handler_kappa
	           agent_type site_type state
	       with
	         _ -> warn parameter error (Some "line 805") Exit
                   (Ckappa_sig.string_of_state_index state)
             in
             let error, state_string' =
               try
	         Handler.string_of_state_fully_deciphered parameter error handler_kappa
	           agent_type' site_type' state'
	       with
	         _ -> warn parameter error (Some "line 813") Exit
                   (Ckappa_sig.string_of_state_index state')
             in
             Loggers.fprintf (Remanent_parameters.get_logger parameter)
               "(agent_id:%i,site_type:%i:%s.%s, agent_id:%i,site_type:%i:%s.%s):\n"
               (Ckappa_sig.int_of_agent_id agent_id)
               (Ckappa_sig.int_of_site_name site_type)
               site_type_string
               state_string                
               (Ckappa_sig.int_of_agent_id agent_id')
               (Ckappa_sig.int_of_site_name site_type')
               site_type_string'
               state_string'
             ;
             List.iter
               (fun ((agent_id, _, site_type1, site_type2, state1, state2),
                     (agent_id', _, site_type1', site_type2', state1', state2')) ->
                 Loggers.fprintf (Remanent_parameters.get_logger parameter)
                   "agent_id:%i:site_type:%i:site_type:%i:state:%i:state:%i->\
                    agent_id:%i:site_type:%i:site_type:%i:state:%i:state:%i\n"
                   (Ckappa_sig.int_of_agent_id agent_id)
                   (Ckappa_sig.int_of_site_name site_type1)
                   (Ckappa_sig.int_of_site_name site_type2)
                   (Ckappa_sig.int_of_state_index state1)
                   (Ckappa_sig.int_of_state_index state2)
                   (Ckappa_sig.int_of_agent_id agent_id')
                   (Ckappa_sig.int_of_site_name site_type1')
                   (Ckappa_sig.int_of_site_name site_type2')
                   (Ckappa_sig.int_of_state_index state1')
                   (Ckappa_sig.int_of_state_index state2');
                 Loggers.print_newline (Remanent_parameters.get_logger parameter)
               ) list
           ) map
       ) store_result
   in
   ()

 let print_fst_site_create_parallel_rhs static dynamic error =
   let parameter = get_parameter static in
   let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
   Loggers.fprintf (Remanent_parameters.get_logger parameter)
     "Rule create a first bond the parallel bond in the rhs:\n";
   print_site_create_parallel static dynamic error store_fst_site_create_parallel_bonds_rhs

  (**************************************************************************)
  (* to do *)
  (* take into account parallel bounds that may occur in initial states *)

  let print_parallel_bonds_init static error set =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.iter
      (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2), 
            (agent_id', agent_type', site_type1', site_type2', state1', state2')) ->
        let error, site_type1_string =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type site_type1
          with
            _ -> warn parameter error (Some "line 896") Exit
              (Ckappa_sig.string_of_site_name site_type1)
        in
        let error, site_type1_string' =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type' site_type1'
          with
            _ -> warn parameter error (Some "line 904") Exit
              (Ckappa_sig.string_of_site_name site_type1')
        in
        let error, site_type2_string =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type site_type2
          with
            _ -> warn parameter error (Some "line 912") Exit
              (Ckappa_sig.string_of_site_name site_type2)
        in
        let error, site_type2_string' =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type' site_type2'
          with
            _ -> warn parameter error (Some "line 920") Exit
              (Ckappa_sig.string_of_site_name site_type2')
        in
        let error, state1_string =
          try
	    Handler.string_of_state_fully_deciphered parameter error handler_kappa
	      agent_type site_type1 state1
	  with
	    _ -> warn parameter error (Some "line 928") Exit
              (Ckappa_sig.string_of_state_index state1)
        in
        let error, state2_string =
          try
	    Handler.string_of_state_fully_deciphered parameter error handler_kappa
	      agent_type site_type2 state2
	  with
	    _ -> warn parameter error (Some "line 936") Exit
              (Ckappa_sig.string_of_state_index state2)
        in
        let error, state1_string' =
          try
	    Handler.string_of_state_fully_deciphered parameter error handler_kappa
	      agent_type' site_type1' state1'
	  with
	    _ -> warn parameter error (Some "line 944") Exit
              (Ckappa_sig.string_of_state_index state1')
        in
        let error, state2_string' =
          try
	    Handler.string_of_state_fully_deciphered parameter error handler_kappa
	      agent_type' site_type2' state2'
	  with
	    _ -> warn parameter error (Some "line 952") Exit
              (Ckappa_sig.string_of_state_index state2')
        in              
        let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_id:%i:site_type:%i:%s.%s:site_type:%i:%s.%s->\n
             agent_id:%i:site_type:%i:%s.%s:site_type:%i:%s.%s\n"
            (Ckappa_sig.int_of_agent_id agent_id)
            (Ckappa_sig.int_of_site_name site_type1)
            site_type1_string
            state1_string
            (Ckappa_sig.int_of_site_name site_type2)
            site_type2_string
            state2_string
            (Ckappa_sig.int_of_agent_id agent_id')
            (Ckappa_sig.int_of_site_name site_type1')
            site_type1_string'
            state1_string'
            (Ckappa_sig.int_of_site_name site_type2')
            site_type2_string'
            state2_string'
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameter)
      ) set
      
  (**************************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    (*parallel bonds in initial states*)
    let error, parallel_bonds_init =
      collect_parallel_bonds_init
        static
        dynamic
        error
        species        
    in
    (*let parameter = get_parameter static in
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "\nParallel bonds in the initial states:\n";
      print_parallel_bonds_init
        static
        error
        parallel_bonds_init
    in*)
    error, dynamic, event_list

  (* to do *)
  (* if a parallel bound occur in a lhs, check that this is possible *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (**************************************************************************)
  (*print function*)
      
  let print_parallel_pair_triple parameter error handler_kappa (x, y) =
    let (agent_id, agent_type, site_type1, site_type2, state1, state2) = x in
    let (agent_id', agent_type', site_type1', site_type2', state1', state2') = y in
    let error, site_type1_string =
      try
        Handler.string_of_site parameter error handler_kappa
          agent_type site_type1
      with
        _ -> warn parameter error (Some "line 1017") Exit
          (Ckappa_sig.string_of_site_name site_type1)
    in
    let error, state1_string =
      try
	Handler.string_of_state_fully_deciphered parameter error handler_kappa
	  agent_type site_type1 state1
      with
	_ -> warn parameter error (Some "line 1025") Exit
          (Ckappa_sig.string_of_state_index state1)
    in
    let error, site_type2_string =
      try
        Handler.string_of_site parameter error handler_kappa
          agent_type site_type2
      with
        _ -> warn parameter error (Some "line 1033") Exit
          (Ckappa_sig.string_of_site_name site_type2)
    in
    let error, state2_string =
      try
	Handler.string_of_state_fully_deciphered parameter error handler_kappa
	  agent_type site_type2 state2
      with
	_ -> warn parameter error (Some "line 1041") Exit
          (Ckappa_sig.string_of_state_index state2)
    in
    let error, state1_string' =
      try
	Handler.string_of_state_fully_deciphered parameter error handler_kappa
	  agent_type' site_type1' state1'
      with
	_ -> warn parameter error (Some "line 1049") Exit
          (Ckappa_sig.string_of_state_index state1')
    in
    let error, site_type1_string' =
      try
        Handler.string_of_site parameter error handler_kappa
          agent_type' site_type1'
      with
        _ -> warn parameter error (Some "line 1057") Exit
          (Ckappa_sig.string_of_site_name site_type1')
    in
    let error, site_type2_string' =
      try
        Handler.string_of_site parameter error handler_kappa
          agent_type' site_type2'
      with
        _ -> warn parameter error (Some "line 1065") Exit
          (Ckappa_sig.string_of_site_name site_type2')
    in
    let error, state2_string' =
      try
	Handler.string_of_state_fully_deciphered parameter error handler_kappa
	  agent_type' site_type2' state2'
      with
	_ -> warn parameter error (Some "line 1073") Exit
          (Ckappa_sig.string_of_state_index state2')
    in
    error, ((site_type1_string, site_type2_string, state1_string, state2_string),
            (site_type1_string', site_type2_string', state1_string', state2_string'))


  let print_pair_triple parameter error handler_kappa (x, y) =
    let (agent_id, agent_type, site_type, state) = x in
    let (agent_id', agent_type', site_type', state') = y in
    let error, site_type_string =
      try
        Handler.string_of_site parameter error handler_kappa
          agent_type site_type
      with
        _ -> warn parameter error (Some "line 1088") Exit
          (Ckappa_sig.string_of_site_name site_type)
    in
    let error, site_type_string' =
      try
        Handler.string_of_site parameter error handler_kappa
          agent_type' site_type'
      with
        _ -> warn parameter error (Some "line 1096") Exit
          (Ckappa_sig.string_of_site_name site_type')
    in
    let error, state_string =
      try
	Handler.string_of_state_fully_deciphered parameter error handler_kappa
	  agent_type site_type state
      with
	_ -> warn parameter error (Some "line 1103") Exit
          (Ckappa_sig.string_of_state_index state)
    in
    let error, state_string' =
      try
	Handler.string_of_state_fully_deciphered parameter error handler_kappa
	  agent_type' site_type' state'
      with
	_ -> warn parameter error (Some "line 1112") Exit
          (Ckappa_sig.string_of_state_index state')
    in
    error, (site_type_string, site_type_string', state_string, state_string')
    
  (**************************************************************************)
  (* to do, when one bond is created, check in the precondition, whether
     the two other sites may be bound, check whether they must be bound to the
     same agents, whether they cannot be bound to the same agent, whether we
     cannot know, and deal with accordingly *)

  let print_value parameter value =
    match value with
    | Usual_domains.Val b ->
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Val %b\n" b
    | Usual_domains.Any ->
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Any\n"
    | Usual_domains.Undefined -> ()
      (*Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Undefined\n"*)

  let print_result handler_kappa parameter error store_result =
    Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.iter
      (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2),
            (agent_id', agent_type', site_type1', site_type2', state1', state2')) value ->
        let error, ((site_type1_string, site_type2_string, state1_string, state2_string),
                    (site_type1_string', site_type2_string', state1_string', state2_string'))=
          print_parallel_pair_triple parameter error handler_kappa 
            ((agent_id, agent_type, site_type1, site_type2, state1, state2),
             (agent_id', agent_type', site_type1', site_type2', state1', state2'))
        in
        if (Ckappa_sig.int_of_state_index state2) = 0
        then
          ()
          (*Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "Undefined\n"*)
        else
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_id:%i:site_type:%i:%s%s:site_type:%i:%s%s->\
             agent_id:%i:site_type:%i:%s%s:site_type:%i:%s%s\n"
            (Ckappa_sig.int_of_agent_id agent_id)
            (Ckappa_sig.int_of_site_name site_type1)
            site_type1_string
            state1_string
            (Ckappa_sig.int_of_site_name site_type2)
            site_type2_string
            state2_string
            (Ckappa_sig.int_of_agent_id agent_id')
            (Ckappa_sig.int_of_site_name site_type1')
            site_type1_string'
            state1_string'
            (Ckappa_sig.int_of_site_name site_type2')
            site_type2_string'
            state2_string';
        print_value parameter value
      ) store_result
      
  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let compil = get_compil static in
    (*--------------------------------------------------------------------*)
    (*get bonds in the lhs and rhs*)
    let store_bonds_rhs_full = get_bonds_rhs_full static in
    let store_bonds_lhs_full = get_bonds_lhs_full static in
    let error, bonds_rhs_full_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_bonds_rhs_full
      with
      | error, None -> error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, bonds_lhs_full_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_bonds_lhs_full
      with
      | error, None -> error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*--------------------------------------------------------------------*)
    (*rule that has a set of parallel bonds in the rhs*)
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    let error, rule_has_parallel_bonds_rhs_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
          rule_id store_rule_has_parallel_bonds_rhs
      with
      | error, None -> error, Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*--------------------------------------------------------------------*)
    let store_fst_site_create_parallel_bonds_rhs = 
      get_fst_site_create_parallel_bonds_rhs static 
    in
    let error, store_pair_bind_map =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_fst_site_create_parallel_bonds_rhs
      with
      | error, None -> error, Ckappa_sig.PairAgentsSiteState_map_and_set.Map.empty
      | error, Some m -> error, m
    in
    let store_result = get_status_of_parallel_bonds dynamic in
    (*--------------------------------------------------------------------*)
    (*fold over the rule that has action binding, and this binding create a
      parallel bond*)
    let error, store_result =
      Ckappa_sig.PairAgentsSiteState_map_and_set.Map.fold
        (fun ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state')) list (error, store_result) ->
          (*--------------------------------------------------------------------*)
          (*print for test*)
          let error, (site_type_string, site_type_string', state_string, state_string') =
            print_pair_triple parameter error handler_kappa
              ((agent_id, agent_type, site_type, state),
               (agent_id', agent_type', site_type', state'))
          in
          (*let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "\nrule_id:%i: action binding appears\n"
              (Ckappa_sig.int_of_rule_id rule_id);
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_id:%i:site_type:%i:%s%s->\
               agent_id:%i:site_type:%i:%s%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              (Ckappa_sig.int_of_site_name site_type)
              site_type_string
              state_string
              (Ckappa_sig.int_of_agent_id agent_id')
              (Ckappa_sig.int_of_site_name site_type')
              site_type_string'
              state_string'
          in*)
          (*--------------------------------------------------------------------*)
          let error, store_result =
            List.fold_left (fun (error, store_result) 
              ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
               (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) ->
                (*------------------------------------------------------------------*)
                (*print for test*)
                let error, 
                  ((site_type1_string, site_type2_string, state1_string, state2_string),
                   (site_type1_string', site_type2_string', state1_string', state2_string')) =
                  print_parallel_pair_triple parameter error handler_kappa 
                    ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                     (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                in
                (*let _ =
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "\nParallel bonds:\n";
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "agent_id:%i:site_type:%i:%s%s:site_type:%i:%s%s->\
                     agent_id:%i:site_type:%i:%s%s:site_type:%i:%s%s\n"
                    (Ckappa_sig.int_of_agent_id agent_id1)
                    (Ckappa_sig.int_of_site_name site_type1)
                    site_type1_string
                    state1_string
                    (Ckappa_sig.int_of_site_name site_type2)
                    site_type2_string
                    state2_string
                    (Ckappa_sig.int_of_agent_id agent_id1')
                    (Ckappa_sig.int_of_site_name site_type1')
                    site_type1_string'
                    state1_string'
                    (Ckappa_sig.int_of_site_name site_type2')
                    site_type2_string'
                    state2_string'
                in*)
                (*let _ =
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "\n-Check the second site site_type:%i:%s of agent_id:%i in the parallel bond\n\n"
                    (Ckappa_sig.int_of_site_name site_type2)
                    site_type2_string
                    (Ckappa_sig.int_of_agent_id agent_id1)
                in*)
                  (*------------------------------------------------------------------*)
                  (*build a path for the second site*)
                let path =
                  {
                    Communication.agent_id = agent_id;
                    Communication.relative_address = [];
                    Communication.site = site_type2
                  }
                in
                  (*get a list of state of this site in the precondition*)
                let error, global_dynamic, precondition, state_list_flat_lattice =
                  Communication.get_state_of_site
                    error
                    (get_global_dynamic_information dynamic)
                    precondition
                    path
                in
                  (*get a list of precondition state of the the second site*)
                let error, state_list =
                  match state_list_flat_lattice with
                  | Usual_domains.Val l -> error, l
                  | _ -> warn parameter error (Some "line 1487") Exit []
                in
                (*let () =
                  List.iter
                    (fun state ->
                      let error, pstate_string =
                        try
		          Handler.string_of_state_fully_deciphered parameter error handler_kappa
		            agent_type1 site_type2 state
	                with
		          _ -> warn parameter error (Some "line 1500") Exit
                            (Ckappa_sig.string_of_state_index state)
                      in
                      Loggers.fprintf (Remanent_parameters.get_logger parameter)
                        "State of the second site %i:%s in the precondition:%i:%s \n"
                        (Ckappa_sig.int_of_site_name site_type2)
                        site_type2_string
                        (Ckappa_sig.int_of_state_index state)
                        pstate_string
                    )                    
                    state_list
                in*)
                (*------------------------------------------------------------------*)
                  (*check that this second site with this state can be bond
                    with B.x(agent_id1', site_type2', state2'), and remains
                    binding*)
                (*get the information of store_result *)                
                let error, store_result =
                  List.fold_left (fun (error, store_result) pre_state ->
                    let error, old_value =
                      match 
                        Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs
                          parameter
                          error
                          ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                           (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                          store_result
                      with
                      | error, None -> error, Usual_domains.Undefined
                      | error, Some v -> error, v
                    in
                    let error, pre_state_string =
                      try
		        Handler.string_of_state_fully_deciphered parameter error handler_kappa
		          agent_type1 site_type2 pre_state
	              with
		        _ -> warn parameter error (Some "line 1500") Exit
                          (Ckappa_sig.string_of_state_index pre_state)
                    in
                    (*------------------------------------------------------------------*)
                    (*CHECK:if t can be bond with binding type B.t in lhs and remains bond*)
                    if 
                      pre_state = state2' (*check it is bound in the lhs. TODO: check it remains bound*)
                    then
                      (*check if two Bs are necessarily the same*)
                      begin
                        if 
                          Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.mem
                            ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                             (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                            rule_has_parallel_bonds_rhs_set
                        then
                          (*belongs to parallel bond*)
                          let new_value = Usual_domains.lub old_value (Usual_domains.Val true) in
                          let error, store_result =
                            Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                              parameter
                              error
                              ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                               (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                              new_value
                              store_result
                          in
                          error, store_result
                        (*------------------------------------------------------------------*)
                        else
                          let new_value = Usual_domains.lub old_value (Usual_domains.Val false) in
                          let error, store_result =
                            Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                              parameter
                              error
                              ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                               (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                              new_value
                              store_result
                          in
                          error, store_result
                      end
                    (*------------------------------------------------------------------*)
                    else
                      (*they do not bound and do not remain*)
                      let error, store_result =
                        Ckappa_sig.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                          parameter
                          error
                          ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                           (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                          Usual_domains.Undefined
                          store_result
                      in
                      error, store_result
                  ) (error, store_result) state_list
                in
                error, store_result
            ) (error, store_result) list
          in
          error, store_result
        ) store_pair_bind_map (error, store_result)
    in
    (*------------------------------------------------------------------*)
    let dynamic = set_status_of_parallel_bonds store_result dynamic in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  (**************************************************************************)
  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  let print static dynamic error loggers =
    (*let handler_kappa = get_kappa_handler static in
    let parameter = get_parameter static in
    let _ =
      print_parallel_bonds_rhs static dynamic error
    in
    let _ =
      print_fst_site_create_parallel_rhs static dynamic error
    in
    let _ =
      print_rule_has_parallel_bonds_rhs static dynamic error 
    in
    let store_status_of_parallel_bonds = get_status_of_parallel_bonds dynamic in
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Final result:\n";
      print_result handler_kappa parameter error store_status_of_parallel_bonds
    in*)
    (*let _ =
      print_action_binding static dynamic error
    in*)
    (*let _ =
      print_fst_site_create_parallel_lhs static dynamic error
    in*)
    (*let _ =
      print_snd_site_create_parallel_rhs static dynamic error
    in*)
    error, dynamic, ()


  (**************************************************************************)
  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
