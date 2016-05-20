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
     with two agents of type A and B and two bonds between A.x and B.z, and A.y
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
      store_action_binding : Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;
      store_bonds_rhs_full : Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;
      store_bonds_lhs_full : Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
      Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds_rhs: Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t;
      store_rule_has_parallel_bonds_rhs:
        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*a map check the bond between A.x and B.z*)
      store_fst_site_create_parallel_bonds_rhs:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
               Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
               Ckappa_sig.c_state * Ckappa_sig.c_state)
        ) list
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*a map check the bond between A.y and B.t*)
      store_snd_site_create_parallel_bonds_rhs:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
               Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
               Ckappa_sig.c_state * Ckappa_sig.c_state)
        ) list
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t;
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
      store_value_parallel_bonds_init:
      bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
      store_value_non_parallel_bonds_init:
        bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
      store_value_of_init:
        bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
      store_value_of_parallel_bonds:
      bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t *
      (*reverse direction*)
      bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t
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
  (*REMOVE*)
  (*let get_action_binding static = lift Analyzer_headers.get_action_binding static*)
  let get_action_binding static =
    (get_local_static_information static).store_action_binding

  let set_action_binding bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_action_binding = bonds
      }
      static

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

  let get_snd_site_create_parallel_bonds_rhs static =
    (get_local_static_information static).store_snd_site_create_parallel_bonds_rhs

  let set_snd_site_create_parallel_bonds_rhs l static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_snd_site_create_parallel_bonds_rhs = l
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

  let set_global_dynamic_information global dynamic =
    {
      dynamic with global = global
    }

  let get_value_parallel_bonds_init dynamic =
    (get_local_dynamic_information dynamic).store_value_parallel_bonds_init

  let set_value_parallel_bonds_init bonds dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_value_parallel_bonds_init = bonds
      } dynamic

  let get_value_non_parallel_bonds_init dynamic =
    (get_local_dynamic_information dynamic).store_value_non_parallel_bonds_init

  let set_value_non_parallel_bonds_init bonds dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_value_non_parallel_bonds_init = bonds
      } dynamic

  let get_value_of_init dynamic =
    (get_local_dynamic_information dynamic).store_value_of_init

  let set_value_of_init bonds dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_value_of_init = bonds
      } dynamic

  (*value of parallel bonds in the rhs*)
  let get_value_of_parallel_bonds dynamic =
    (get_local_dynamic_information dynamic).store_value_of_parallel_bonds

  let set_value_of_parallel_bonds value dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          store_value_of_parallel_bonds = value
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
        
  (************************************************************************************)
  (*action binding in the rhs*)
        
  let collect_action_binding parameter error rule_id rule store_result =
    List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
      (*get information of a rule that created a bond*)
      let agent_id1 = site_add1.Cckappa_sig.agent_index in
      let site_type1 = site_add1.Cckappa_sig.site in
      let agent_id2 = site_add2.Cckappa_sig.agent_index in
      let site_type2 = site_add2.Cckappa_sig.site in
      let error, agent_source = 
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id1 rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None -> warn parameter error (Some "line 267") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
    (*get pair agent_type, state*)
      let error, (agent_type1, state1) =
        collect_agent_type_state
          parameter
          error
          agent_source
          site_type1
      in
    (*------------------------------------------------------------------------------*)
    (*second pair*)
      let error, agent_target =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id2 rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        with
        | error, None -> warn parameter error (Some "line 275") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, (agent_type2, state2) =
        collect_agent_type_state
          parameter
          error
          agent_target
          site_type2
      in
    (*add the pair inside the set*)
      let error, old_set =
        match 
          Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
            parameter
            error
            rule_id
            store_result
        with
        | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
        | error, Some s -> error, s
      in
      let error', set =
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
          parameter
          error
          ((agent_id1, agent_type1, site_type1, state1), (agent_id2, agent_type2, site_type2, state2))
          old_set
      in
      let error = Exception.check warn parameter error error' (Some "line 358") Exit in
      let error, store_result =
        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
          parameter
          error
          rule_id
          set
          store_result
      in 
      error, store_result
    ) (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind

  (************************************************************************************)

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
              | error, None -> warn parameter error (Some "line 269") Exit Cckappa_sig.Ghost
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
            (*------------------------------------------------------------------------------*)
            (*the second pair*)
            let error, agent_target =
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                  parameter error agent_id_target views
              with
              | error, None -> warn parameter error (Some "line 287") Exit Cckappa_sig.Ghost
              | error, Some agent -> error, agent
            in
            let error, (agent_type2, state2) =
              collect_agent_type_state
                parameter
                error
                agent_target
                site_type_target
            in
            (*------------------------------------------------------------------------------*)
            (*get old set*)
            let error, old_set =
              match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
                rule_id store_result
              with
              | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
              | error, Some p -> error, p
            in
            let error', set =
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                parameter error
                ((agent_id, agent_type1, site_type_source, state1),
                 (agent_id_target, agent_type2, site_type_target, state2))
                old_set
            in
            let error = Exception.check warn parameter error error' (Some "line 312") Exit in
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

  let collect_bonds_rhs_full parameter error rule_id rule store_result =
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
  (**Parallel bonds*)
  (**************************************************************************)

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
        | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
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
              (*the first pair*)
              let error, agent_source =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_source views
                with
                | error, None -> warn parameter error (Some "line 335") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_source, state_source) =
                collect_agent_type_state
                  parameter
                  error
                  agent_source
                  site_type_source
              in
              (*------------------------------------------------------------------------------*)
              (*the second pair*)
              let error, agent_target =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter error agent_id_target views
                with
                | error, None -> warn parameter error (Some "line 352") Exit Cckappa_sig.Ghost
                | error, Some agent -> error, agent
              in
              let error, (agent_type_target, state_target) =
                collect_agent_type_state
                  parameter
                  error
                  agent_target
                  site_type_target
              in
              (*------------------------------------------------------------------------------*)
              (*parallel bonds*)
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
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
                        | error, None -> error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
                        | error, Some s -> error, s
                      in
                      let error', set =
                        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
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
  (*collect a set of parallel bonds in the rhs correspond with its rule*)

  let collect_rule_has_parallel_bonds_rhs static error rule_id rule =
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
 (*collect a set of parallel bonds in the rhs*)

  let collect_parallel_bonds_rhs static error rule_id =
    let parameter = get_parameter static in
    let store_rule_has_parallel_bonds = get_rule_has_parallel_bonds_rhs static in
    let error, parallel_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_rule_has_parallel_bonds
      with
      | error, None -> error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let store_result = get_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold
        (fun (x, y) (error, store_result) ->
          let error, set =
            Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
              parameter
              error
              (x, y)
              store_result
          in
          error, set
        ) parallel_set (error, store_result)
    in
    let static = set_parallel_bonds_rhs store_result static in
    error, static

  (**************************************************************************)
  (*a map (A,x,y, B,z,t) -> (Ag_id, Ag_id) RuleIDMap to explain
    which rules can create a bond of type A.x.z.B (and at which position)*)

  let collect_fst_site_create_parallel_bonds static error rule_id store_action_binding
      store_parallel_bonds store_result =
    let parameter = get_parameter static in
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
          (*A.x -> B.z; B.z -> A.x*)
          (fun ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state')) store_result ->
            let error, old_list =
              match
                Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.find_option_without_logs
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
              Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
                (fun ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                      (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                  (error, current_list) ->
                    if
                      agent_id = agent_id1 &&
                      site_type = site_type1 &&
                      agent_id' = agent_id1' &&
                      site_type' = site_type1'
                    then
                    (*A.x.B.z, B.z.A.x*)
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
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.add_or_overwrite
                parameter
                error
                ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state'))
                new_list
                store_result
            in
            store_result
          ) set Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
        ) store_action_binding

  (**************************************************************************)
  (*in the rhs*)
      
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
  (*the second map (A,x,y, B,z,t) -> A.y.t.B*)

  let collect_snd_site_create_parallel_bonds static error rule_id store_action_binding
      store_parallel_bonds store_result =
    let parameter = get_parameter static in
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
          (*A.y -> B.t; B.t -> A.y*)
          (fun ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state')) store_result ->
            let error, old_list =
              match
                Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.find_option_without_logs
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
              Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
                (fun ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                      (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                  (error, current_list) ->
                    (*check site_type2, and site_type2': A.y -> B.t*)
                    if
                      agent_id = agent_id1 &&
                      site_type = site_type2 &&
                      agent_id' = agent_id1' &&
                      site_type' = site_type2'
                    then
                      let new_list =
                        (*A.x.y.B.z.t, B.z.t.A.x.y*)
                        ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                         (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) :: current_list
                      in
                      error, new_list
                    else
                      error, current_list
                ) store_parallel_bonds (error, old_list)
            in
            let error, store_result =
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.add_or_overwrite
                parameter
                error
                ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state'))
                new_list
                store_result
            in
            store_result
          ) set Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
      ) store_action_binding

  (**************************************************************************)
      
  let collect_snd_site_create_parallel_bonds_rhs static error rule_id =
    let store_parallel_bonds = get_parallel_bonds_rhs static in
    let store_action_binding = get_action_binding static in
    let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in
    let store_result =
      collect_snd_site_create_parallel_bonds
        static
        error
        rule_id
        store_action_binding
        store_parallel_bonds
        store_snd_site_create_parallel_bonds_rhs
    in
    let static = set_snd_site_create_parallel_bonds_rhs store_result static in
    error, static

  (**************************************************************************)
  (**rules*)
  (**************************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let store_action_binding = get_action_binding static in
    let error, store_action_binding =
      collect_action_binding parameter error rule_id rule 
        store_action_binding
    in
    let static = set_action_binding store_action_binding static in
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
      collect_rule_has_parallel_bonds_rhs
        static
        error
        rule_id
        rule
    in
    let error, static =
      collect_parallel_bonds_rhs
        static
        error
        rule_id
    in
    let error, static =
      collect_fst_site_create_parallel_bonds_rhs
        static
        error
        rule_id
    in
    let error, static =
      collect_snd_site_create_parallel_bonds_rhs static error rule_id
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
        store_action_binding = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_bonds_rhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_bonds_lhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_rhs = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty;
        store_rule_has_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_fst_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_snd_site_create_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty
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
         store_value_parallel_bonds_init = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
         store_value_non_parallel_bonds_init = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
         store_value_of_init = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
         store_value_of_parallel_bonds = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty,
         Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty
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
  let collect_pair_of_bonds parameter error site_add agent_id site_type_source views =
    let error, pair =
      let agent_index_target = site_add.Cckappa_sig.agent_index in
      let site_type_target = site_add.Cckappa_sig.site in
      let error, agent_source = 
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameter error agent_id views
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

  (*collect a set of binding sites in the initial states*)

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
                  collect_pair_of_bonds
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
                  Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
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
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.empty
    in
    error, store_result

  (**************************************************************************)
  (*a set of parallel bonds in the initial states*)
      
  let collect_parallel_bonds_init static dynamic error init_state =
    (*collect bonds in the initial states*)
    let error, store_bonds_init = collect_bonds_initial static error init_state in
    let parameter = get_parameter static in
    (*--------------------------------------------------------------*)
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold 
       parameter error
        (fun parameter error agent_id_source bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let error, pair =
                  collect_pair_of_bonds
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
                (*fold over a set of binding sites in the inititial states*)
                let error, store_result =
                  Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
                    (fun ((agent_id, agent_type, site_type, state),
                          (agent_id', agent_type', site_type', state')) (error, store_result) ->
                      if agent_id = agent_id_source &&
                        agent_id' = site_add.Cckappa_sig.agent_index &&
                        site_type_source <> site_type &&
                        site_type_target <> site_type'
                      then
                        let error', store_result =
                          Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in
                            parameter
                            error
                            ((agent_id, agent_type_source, site_type_source, site_type, state_source, state),
                             (site_add.Cckappa_sig.agent_index, agent_type_target,
                              site_type_target, site_type', state_target, state'))
                            store_result
                        in
                        let error = Exception.check warn parameter error error' (Some "line 872") Exit in
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
        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
    in
    error, store_result

  (**************************************************************************)

  let collect_views_init static dynamic error init_state =
    let parameter = get_parameter static in
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
        (fun parameter error init_index agent store_result ->
          match agent with
          | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _ -> error, store_result
          | Cckappa_sig.Dead_agent _ -> warn parameter error (Some "line 957") Exit store_result
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let error, pair_list =
              Ckappa_sig.Site_map_and_set.Map.fold
                (fun site port (error, current_list) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  if (Ckappa_sig.int_of_state_index state) = 0
                  then error, current_list
                  else
                  error, (site, state) :: current_list
                ) agent.Cckappa_sig.agent_interface (error, []) 
            in
            let error, store_result =
              Ckappa_sig.Agents_map_and_set.Map.add_or_overwrite
                parameter error
                (init_index, agent_type)
                pair_list
                store_result
            in
            error, store_result
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views 
        Ckappa_sig.Agents_map_and_set.Map.empty
    in
    error, store_result

  (**************************************************************************)

  let collect_site_pair_list_aux static dynamic error init_state =
    let parameter = get_parameter static in
    let error, store_bonds_init = collect_bonds_initial static error init_state in
    let error, store_result =
     Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
        (fun ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state')) (error, store_result) ->
          (*get old*)
          let error, old_list =
            match Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
              parameter error agent_type' store_result with
              | error, None -> error, []
              | error, Some l -> error, l
          in
          (*get a map of agent_type with a site pair list: (A.x, B.x)*)
          let site_pair_list =
            (*id:0:x:1, id:1:y:1*)
            ((agent_id, site_type, state),
             (agent_id', site_type', state')) :: old_list in
          let error, store_result =
            Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
              parameter error
              agent_type'
              site_pair_list
              store_result
          in
          error, store_result
        ) store_bonds_init (error, Ckappa_sig.Agent_map_and_set.Map.empty)
    in
    error, store_result

  (**************************************************************************)
  (*non parallel bonds in initial state*)

  let collect_site_pair_list static dynamic error init_state =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let error, store_bonds_init = collect_bonds_initial static error init_state in
    let error, store_site_pair_list = collect_site_pair_list_aux static dynamic error init_state in
    let error, store_result =
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
        (*A.x, B.z*)
        (fun ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state')) (error, store_result) ->
          let error, store_result =
            Ckappa_sig.Agent_map_and_set.Map.fold
              (fun agent_type1' pair_list (error, store_result) ->
                let error, store_result =
                  List.fold_left (fun (error, store_result) 
                    ((agent_id1, site_type1, state1), (agent_id1', site_type1', state1')) ->
                      (*B = B, and their id are different*)
                      if agent_id' <> agent_id1' &&  agent_type' = agent_type1' 
                      then
                        (*two elements in the list*)
                        if site_type <> site_type1 (*A.x <> A.y*)
                        then
                          (*non parallel set*)
                          let error, old_list =
                            match Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
                              parameter error agent_type' store_result with
                            | error, None -> error, []
                            | error, Some l -> error, l
                          in
                          (*A.x.y, B.z.t*)
                          let new_list =
                            ((agent_id, agent_type, site_type, state), (*A.x*)
                             (agent_id1, agent_type, site_type1, state1), (*A.y*)
                             (agent_id', agent_type', site_type', state'), (*B.z*)
                             (agent_id1', agent_type', site_type1', state1')) :: old_list
                          in
                          Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
                            parameter error 
                            agent_type'
                            new_list
                            store_result
                        else
                          error, store_result
                      else
                        error, store_result
                  ) (error, store_result) pair_list
                in
                error, store_result
              ) store_site_pair_list (error, store_result)
          in
          error, store_result
        ) store_bonds_init (error, Ckappa_sig.Agent_map_and_set.Map.empty)
    in
    error, store_result

  (**************************************************************************)
  (*collect result of parallel bonds in the intitial state*)

  let collect_value_parallel_bonds static dynamic error init_state =
    let parameter = get_parameter static in
    let error, store_parallel_bonds_init = collect_parallel_bonds_init static dynamic error init_state in
    (*--------------------------------------------------------------------*)
    (*parallel bonds in the initial state:
      (agent_type, site_type, site_type, state, state -> agent_type, site_type, site_type, state, state)
    *)
    let error, parallel_list =
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold
        (fun ((agent_id, agent_type, site_type, site_type1, state, state1),
              (agent_id', agent_type', site_type', site_type1', state', state1')) (error, current_list) ->
          error, ((agent_type, site_type, site_type1, state, state1),
                  (agent_type', site_type', site_type1', state', state1')) :: current_list
        ) store_parallel_bonds_init (error, [])
    in
    (*--------------------------------------------------------------------*)
    let store_result = get_value_parallel_bonds_init dynamic in
    let error, value_parallel_bonds =
      List.fold_left (fun (error, store_result) x ->
        let error, store_result =
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite
            parameter error
            x
            (Usual_domains.Val true)
            store_result
        in
        error, store_result
      ) (error, store_result) parallel_list
    in
    let dynamic = set_value_parallel_bonds_init value_parallel_bonds dynamic in
    error, dynamic

  (**************************************************************************)
  (*collect result of non parallel bonds in the initital state*)

  let collect_value_non_parallel_bonds static dynamic error init_state =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    (*non parallel bonds*)
    let error, store_site_pair_list = collect_site_pair_list static dynamic error init_state in
    (*--------------------------------------------------------------------*)
    (*a map contents a list of non parallel bonds:
      (agent_type, site_type, site_type, state, state) -> (agent_type, site_type, site_type, state, state)
    *)
    let store_non_parallel_init =
      Ckappa_sig.Agent_map_and_set.Map.map
        (fun list ->
          let new_pair_list =
            List.fold_left (fun current_list
              ((agent_id, agent_type, site_type, state),
               (agent_id1, agent_type1, site_type1, state1), 
               (agent_id', agent_type'', site_type', state'),
               (agent_id1', agent_type1', site_type1', state1')) ->
                ((agent_type, site_type, site_type1, state, state1),
                 (agent_type'', site_type', site_type1', state', state1')) :: current_list
            ) [] list
          in
          new_pair_list
        ) store_site_pair_list 
    in
    (*--------------------------------------------------------------------*)
    let store_result = get_value_non_parallel_bonds_init dynamic in
    (*value of a set of non parallel bonds is Val false*)
    let error, value_non_parallel_bonds =
      Ckappa_sig.Agent_map_and_set.Map.fold
        (fun agent_type list (error, store_result) ->
          let error, store_result =
            List.fold_left (fun (error, store_result) x ->
              Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite
                parameter error
                x
                (Usual_domains.Val false)
                store_result
            ) (error, store_result) list
          in
          error, store_result
        ) store_non_parallel_init (error, store_result)
    in
    let dynamic = set_value_non_parallel_bonds_init value_non_parallel_bonds dynamic in
    error, dynamic

  (**************************************************************************)
  (*return a value of initial state, if it has parallel bonds -> yes,
    if non parallel bonds -> no, if it is both -> any, if there is non:
    undefined*)

  let collect_value_of_init static dynamic error init_state =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let value_parallel_bonds = get_value_parallel_bonds_init dynamic in
    let value_non_parallel_bonds = get_value_non_parallel_bonds_init dynamic in
    (*------------------------------------------------------------------------------*)
    (*do the lub in the initial state*)
    let add_link error x value store_result =
      let error, old_value =
        match Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.find_option_without_logs parameter error
          x store_result with
          | error, None -> error, Usual_domains.Undefined
          | error, Some v -> error, v
      in
      let new_value = Usual_domains.lub value old_value in
      let error, store_result =
        Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameter error x new_value 
          store_result
      in
      error, store_result
    in 
    let store_result = get_value_of_init dynamic in
    (*------------------------------------------------------------------------------*)
    let error, store_result =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold2 parameter error
        (fun parameter error x p_value store_result ->
          add_link error x p_value store_result
        )
        (fun parameter error x nonp_value store_result ->
          add_link error x  nonp_value store_result
        )
        (fun parameter error x p_value nonp_value store_result ->
          let new_value = Usual_domains.lub p_value nonp_value in
          add_link error x new_value store_result
        ) value_parallel_bonds value_non_parallel_bonds store_result
    in
    let dynamic = set_value_of_init store_result dynamic in
    error, dynamic

  (**************************************************************************)
  (*a map of parallel bonds in the initial states, if the set
    if empty then return false, if it has parallel bonds return
    true.*)

  (*a set that is not belong to a parallel bonds, both of them : any. if
    it is belong to the parallel bonds then return true, if it is belong to
    not parallel bonds return false, both return any.*)

  let compute_value_init static dynamic error init_state =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let error, dynamic = collect_value_non_parallel_bonds static dynamic error init_state in
    let error, dynamic = collect_value_parallel_bonds static dynamic error init_state in
    let error, dynamic = collect_value_of_init static dynamic error init_state in
    error, dynamic

  (**************************************************************************)
  (*Initial state*)
  (**************************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    (*parallel bonds in the initial states*)
    let error, dynamic =
      compute_value_init static dynamic error species
    in
    error, dynamic, event_list

  (* TODO *)
  (* if a parallel bound occur in a lhs, check that this is possible *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (**************************************************************************)
  (* to do, when one bond is created, check in the precondition, whether
     the two other sites may be bound, check whether they must be bound to the
     same agents, whether they cannot be bound to the same agent, whether we
     cannot know, and deal with accordingly *)

  let get_state_of_site_in_precondition parameter error dynamic agent_id site_type precondition =
  (*binding action: A.x.B.z -> parallel bonds: A.x.y.B.z.t, B.z.t.A.x.y (first bound)*)
  (*build a path for the second site in this bound. A.y*)
    let path =
      {
        Communication.agent_id = agent_id;
        Communication.relative_address = [];
        Communication.site = site_type;
      }
    in
    (*get a list of site_type2 state in the precondition*)
    let error, global_dynamic, precondition, state_list_lattice =
      Communication.get_state_of_site
        error
        (get_global_dynamic_information dynamic)
        precondition
        path
    in
    let error, state_list =
      match state_list_lattice with
      | Usual_domains.Val l -> error, l
      | _ -> warn parameter error (Some "line 1455") Exit []
    in
    error, global_dynamic, precondition, state_list

  (**************************************************************************)

  let compute_result parameter error handler_kappa state_list (x, y)
      rule_has_parallel_bonds_rhs_set store_result =
    let (agent_id1, agent_type1, site_type1, site_type2, state1, state2) = x in
    let (agent_id1', agent_type1', site_type1', site_type2', state1', state2') = y in
    (*--------------------------------------------------------------------*)
    (*print for test*)
    let error, ((site_type1_string, site_type2_string, state1_string, state2_string),
                (site_type1_string', site_type2_string', state1_string', state2_string')) =
      Print_parallel_bonds.print_parallel_pair parameter error handler_kappa
        ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
         (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
    in
    let error, agent_string1 =
      try
        Handler.string_of_agent parameter error handler_kappa agent_type1
      with
        _ -> warn parameter error (Some "line 1441") Exit (Ckappa_sig.string_of_agent_name agent_type1)
    in
    let error, agent_string1' =
      try
        Handler.string_of_agent parameter error handler_kappa agent_type1'
      with
        _ -> warn parameter error (Some "line 1447") Exit (Ckappa_sig.string_of_agent_name agent_type1')
    in
    (*let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "\nList of parallel bonds:\n";
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s->\
         agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s\n"
        (Ckappa_sig.int_of_agent_id agent_id1)
        agent_string1
        (Ckappa_sig.int_of_site_name site_type1)
        site_type1_string
        state1_string
        (Ckappa_sig.int_of_site_name site_type2)
        site_type2_string
        state2_string
        (Ckappa_sig.int_of_agent_id agent_id1')
        agent_string1'
        (Ckappa_sig.int_of_site_name site_type1')
        site_type1_string'
        state1_string'
        (Ckappa_sig.int_of_site_name site_type2')
        site_type2_string'
        state2_string'
    in*)
    (*--------------------------------------------------------------------*)
    let error, store_result =
      List.fold_left (fun (error, store_result) pre_state ->
        (*get the old value*)
        let error, old_value =
          match
            Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs
              parameter
              error
              ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
               (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
              store_result
          with
          | error, None -> error, Usual_domains.Undefined
          | error, Some v -> error, v
        in
        (*--------------------------------------------------------------------*)
        (*print test*)
        let error, pre_state_string =
          try
	    Handler.string_of_state_fully_deciphered parameter error handler_kappa
	      agent_type1 site_type2 pre_state
	  with
	    _ -> warn parameter error (Some "line 1530") Exit
              (Ckappa_sig.string_of_state_index pre_state)
        in
        let error, site_type2_string =
          try
            Handler.string_of_site parameter error handler_kappa
              agent_type1 site_type2
          with
            _ -> warn parameter error (Some "line 1538") Exit
              (Ckappa_sig.string_of_site_name site_type1)
        in
        (*let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "\nThe state of the second site site_type:%i:%s of agent_id:%i:%s in the precondition is:%i:%s\n"
            (Ckappa_sig.int_of_site_name site_type2)
            site_type2_string
            (Ckappa_sig.int_of_agent_id agent_id1)
            agent_string1
            (Ckappa_sig.int_of_state_index pre_state)
            pre_state_string
        in*)
        (*--------------------------------------------------------------------*)
        (*check if it can be bound in the lhs with binding type B@z (check
          their state is enough). TODO: remains bound, it does not change the state*)
        if pre_state = state2'
        then
          (*--------------------------------------------------------------------*)
          (*check if two Bs are necessarily the same*)
          if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.mem
            ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
             (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
            rule_has_parallel_bonds_rhs_set
          then
            (*it belongs to a parallel bonds*)
            let new_value =
              Usual_domains.lub old_value (Usual_domains.Val true)
            in
            let error, store_result =
              Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                parameter
                error
                ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                 (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                new_value
                store_result
            in
            (*print test*)
            (*let _ =
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "\nBelong to a parallel bonds\n";
              Print_parallel_bonds.print_result handler_kappa parameter error store_result
            in*)
            error, store_result
          else
            (*it does not belong to a parallel bonds*)
            let new_value = Usual_domains.lub old_value (Usual_domains.Val false) in
            let error, store_result =
              Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                parameter
                error
                ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                 (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                new_value
                store_result
            in
            (*print test*)
            (*let _ =
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "Do not have a parallel bonds\n";
              Print_parallel_bonds.print_result handler_kappa parameter error store_result
            in*)
            error, store_result
        (*--------------------------------------------------------------------*)
        (*it can not be bound in the lhs, return Undefined*)
        else
          let error, store_result =
            Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
              parameter
              error
              ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
               (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
              Usual_domains.Undefined
              store_result
          in
          (*print test*)
          (*let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Do not care:pre_state:%i:%s:Undefined\n"
              (Ckappa_sig.int_of_state_index pre_state)
              pre_state_string
            ;
            Print_parallel_bonds.print_result handler_kappa parameter error store_result
          in*)
          error, store_result
      ) (error, store_result) state_list
    in
    error, store_result

  (**************************************************************************)

  let collect_result_from_site_create_parallel parameter error dynamic handler_kappa rule_id
      precondition store_pair_bind_map rule_has_parallel_bonds_rhs_set store_result =
    let error, store_result =
      (*fold over a binding action map*)
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.fold
        (*A.x.B.z*)
        (fun ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state')) list (error, store_result) ->
          (*--------------------------------------------------------------------*)
          (*print this binding map for test*)
          (*let _ =
            Print_parallel_bonds.print_action_binding_test parameter error handler_kappa rule_id
              ((agent_id, agent_type, site_type, state),
               (agent_id', agent_type', site_type', state'))
          in*)
          (*--------------------------------------------------------------------*)
          (*fold over a list of parallel bonds*)
          let error, (store_result_direct, store_result_reverse) =
            List.fold_left (fun (error, store_result)
              (*A.x.y.B.z.t, B.z.t.A.x.y*)
              ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
               (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) ->
                let (store_result1, store_result2) = store_result in
                let error, agent_string1 =
                  try
                    Handler.string_of_agent parameter error handler_kappa agent_type1
                  with
                    _ -> warn parameter error (Some "line 1706") Exit
                      (Ckappa_sig.string_of_agent_name agent_type1)
                in
                let error, site_type2_string =
                  try
                    Handler.string_of_site parameter error handler_kappa
                      agent_type1 site_type2
                  with
                    _ -> warn parameter error (Some "line 1714") Exit
                      (Ckappa_sig.string_of_site_name site_type2)
                in
                (*let _ =
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "\n-Check the second site site_type:%i:%s of agent_id:%i:%s in the parallel bonds:\n"
                    (Ckappa_sig.int_of_site_name site_type2)
                    site_type2_string
                    (Ckappa_sig.int_of_agent_id agent_id1)
                    agent_string1
                in*)
                (*get a list of state of the second site site_type2 in the precondition*)
                (*A.x.y.B.z.t*) (*Fixme: precondition'*)
                let error, global_dynamic, precondition', state_list_direct =
                  get_state_of_site_in_precondition
                    parameter
                    error
                    dynamic
                    agent_id1
                    site_type2
                    precondition
                in
                let error, store_result_direct =
                  compute_result
                    parameter
                    error
                    handler_kappa
                    state_list_direct
                    ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                     (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                    rule_has_parallel_bonds_rhs_set
                    store_result1
                in
                (*--------------------------------------------------------------------*)
                (*compute reverse direction: B.z.t.A.x.y*)
                let error, agent_string1' =
                  try
                    Handler.string_of_agent parameter error handler_kappa agent_type1'
                  with
                    _ -> warn parameter error (Some "line 1761") Exit
                      (Ckappa_sig.string_of_agent_name agent_type1')
                in
                let error, site_type2_string' =
                  try
                    Handler.string_of_site parameter error handler_kappa
                      agent_type1' site_type2'
                  with
                    _ -> warn parameter error (Some "line 1769") Exit
                      (Ckappa_sig.string_of_site_name site_type2')
                in
                (*let _ =
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "\n-Reverse direction check the second site site_type:%i:%s of agent_id:%i:%s\
                     in the parallel bonds:\n"
                    (Ckappa_sig.int_of_site_name site_type2')
                    site_type2_string'
                    (Ckappa_sig.int_of_agent_id agent_id1')
                    agent_string1'
                in*)
                (*B.z.t.A.x.y*)
                let error, global_dynamic, precondition'', state_list_reverse =
                  get_state_of_site_in_precondition
                    parameter
                    error
                    dynamic
                    agent_id1'
                    site_type2'
                    precondition
                in
                let error, store_result_reverse =
                  compute_result
                    parameter
                    error
                    handler_kappa
                    state_list_reverse
                    ((agent_id1', agent_type1', site_type1', site_type2', state1', state2') ,
                     (agent_id1, agent_type1, site_type1, site_type2, state1, state2))
                    rule_has_parallel_bonds_rhs_set
                    store_result2
                in
                (*--------------------------------------------------------------------*)
                (*result:return the result of both directions*)
                error, (store_result_direct, store_result_reverse)
            ) (error, store_result) list
          in
          error, (store_result_direct, store_result_reverse)
        ) store_pair_bind_map (error, store_result)
    in
    error, store_result

  (**************************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    (*--------------------------------------------------------------------*)
    (*a map of rule that has a set of parallel bonds in the rhs*)
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    let error, rule_has_parallel_bonds_rhs_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
          rule_id store_rule_has_parallel_bonds_rhs
      with
      | error, None -> error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*--------------------------------------------------------------------*)
    let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
    let error, store_pair_bind_map =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
          rule_id store_fst_site_create_parallel_bonds_rhs
      with
      | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
      | error, Some m -> error, m
    in
    let store_result = get_value_of_parallel_bonds dynamic in
    (*--------------------------------------------------------------------*)
    (*rule that has a binding action, and this binding create a parallel
      bonds*)
    let error, store_result =
      collect_result_from_site_create_parallel
        parameter
        error
        dynamic
        handler_kappa
        rule_id
        precondition
        store_pair_bind_map
        rule_has_parallel_bonds_rhs_set
        store_result
    in
    let dynamic = set_value_of_parallel_bonds store_result dynamic in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  (**************************************************************************)

  let print static dynamic error loggers =
    let handler_kappa = get_kappa_handler static in
    let parameter = get_parameter static in
    (*--------------------------------------------------------------*)
    (*let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    let _ =
      Print_parallel_bonds.print_parallel_bonds_rhs parameter handler_kappa
        store_parallel_bonds_rhs static dynamic error
    in
    (*--------------------------------------------------------------*)
    let store_action_binding = get_action_binding static in
    let _ =
      Print_parallel_bonds.print_action_binding parameter handler_kappa store_action_binding 
        static dynamic error
    in
    (*--------------------------------------------------------------*)
    let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
    let _ =
      Print_parallel_bonds.print_fst_site_create_parallel_rhs parameter handler_kappa
        store_fst_site_create_parallel_bonds_rhs static dynamic error
    in
    (*--------------------------------------------------------------*)
    let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in
    let _ =
      Print_parallel_bonds.print_snd_site_create_parallel_rhs parameter handler_kappa
        store_snd_site_create_parallel_bonds_rhs static dynamic error
    in
    (*--------------------------------------------------------------*)
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    let _ =
      Print_parallel_bonds.print_rule_has_parallel_bonds_rhs parameter handler_kappa
        store_rule_has_parallel_bonds_rhs static dynamic error
    in
    (*--------------------------------------------------------------*)
    (*print value of initial state*)
    let store_value_of_init = get_value_of_init dynamic in
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Value in the initial state:\n";
      Ckappa_sig.PairAgentSitesStates_map_and_set.Map.iter
        (fun ((agent_type, site_type, site_type1, state, state1),
              (agent_type', site_type', site_type1', state', state1')) value ->
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i->\
             agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i\n"
            (Ckappa_sig.int_of_agent_name agent_type)
            (Ckappa_sig.int_of_site_name site_type)
            (Ckappa_sig.int_of_site_name site_type1)
            (Ckappa_sig.int_of_state_index state)
            (Ckappa_sig.int_of_state_index state1)
            (Ckappa_sig.int_of_agent_name agent_type')
            (Ckappa_sig.int_of_site_name site_type')
            (Ckappa_sig.int_of_site_name site_type1')
            (Ckappa_sig.int_of_state_index state')
            (Ckappa_sig.int_of_state_index state1');
          Print_parallel_bonds.print_value parameter value
        ) store_value_of_init;
      Loggers.print_newline (Remanent_parameters.get_logger parameter)
    in
    (*--------------------------------------------------------------*)
    (*print value of parallel in the rhs*)
    let store_value_of_parallel_bonds = get_value_of_parallel_bonds dynamic in
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Value of a parallel bonds:\n";
      Print_parallel_bonds.print_result handler_kappa parameter error (fst store_value_of_parallel_bonds);
      Print_parallel_bonds.print_result handler_kappa parameter error (snd store_value_of_parallel_bonds)
    in*)
    error, dynamic, ()

  (**************************************************************************)

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
