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
      store_parallel_bonds_rhs: 
        Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_fst_site_create_parallel_bonds_rhs:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state)) list
        Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_snd_site_create_parallel_bonds_rhs: (*REMOVE*)
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state) *
            (Ckappa_sig.c_agent_id * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_state * Ckappa_sig.c_state)) list
        Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t
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

  type local_dynamic_information = unit

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
    
  let get_parallel_bonds_rhs static =
    (get_local_static_information static).store_parallel_bonds_rhs
      
  let set_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds_rhs = bonds
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

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

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

  let collect_bonds_rhs_full parameter error rule_id rule =
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
              | error, None -> warn parameter error (Some "line 267") Exit Cckappa_sig.Ghost
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
                  parameter error agent_id_target rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
              with
              | error, None -> warn parameter error (Some "line 275") Exit Cckappa_sig.Ghost
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
            let error = Exception.check warn parameter error error' (Some "line 240") Exit in
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
      ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds Ckappa_sig.Rule_map_and_set.Map.empty 
      

  (*get information of bonds: (agent_id, site_type -> agent_id, site_type)
    set rule_id map *)
      
  let collect_bonds_rhs parameter error rule_id rule =
    let error, store_bonds_rhs_full =
      collect_bonds_rhs_full
        parameter
        error
        rule_id
        rule
    in
    Ckappa_sig.Rule_map_and_set.Map.map
      (fun set ->
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
          (fun ((agent_id, agent_type, site_type, state), (agent_id', agent_type', site_type', state')) 
            store_set ->
              let error, set =
                Ckappa_sig.PairAgentSite_map_and_set.Set.add_when_not_in
                  parameter
                  error
                  ((agent_id, site_type), (agent_id', site_type'))
                  store_set
              in
              set
          ) set Ckappa_sig.PairAgentSite_map_and_set.Set.empty
      ) store_bonds_rhs_full

  (**************************************************************************)
  (*Parallel bonds*)
  (**************************************************************************)

  let collect_parallel_bonds_rhs static error rule_id rule =
    let parameter = get_parameter static in
    let store_result = get_parallel_bonds_rhs static in
    (*--------------------------------------------*)
    let error, store_bonds_rhs_full =
      collect_bonds_rhs_full
        parameter
        error
        rule_id
        rule
    in
    (*--------------------------------------------*)
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
                    parameter error agent_id_source rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                with
                | error, None -> warn parameter error (Some "line 310") Exit Cckappa_sig.Ghost
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
                    parameter error agent_id_target rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                with
                | error, None -> warn parameter error (Some "line 275") Exit Cckappa_sig.Ghost
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
                      let error = Exception.check warn parameter error error' (Some "line 371") Exit in
                      let error, store_result =
                        Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
                          rule_id 
                          set
                          store_result
                      in
                      error, store_result
                    else
                      error, store_result
                ) bonds_rhs_full_set (error, store_result)
            ) bonds_map (error, store_result)
        ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds store_result
    in
    let static = set_parallel_bonds_rhs store_result static in
    error, static

  (**************************************************************************)
  (*collect site that create a parallel bond*)

  let collect_fst_site_create_parallel_bonds_rhs static error rule_id =
    let parameter = get_parameter static in
    (*--------------------------------------------------------------------*)
    (*get parallel set*)
    let store_parallel_bonds = get_parallel_bonds_rhs static in
    let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
    let error, store_pair_map =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_fst_site_create_parallel_bonds_rhs
      with
      | error, None -> error, Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.empty
      | error, Some m -> error, m
    in    
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold
            (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2),
                  (agent_id', agent_type', site_type1', site_type2', state1', state2')) store_result ->
              let error, old_list =
                match
                  Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.find_option_without_logs
                    parameter
                    error
                    ((agent_id, site_type1, state1), (agent_id', site_type1', state1'))
                    store_result
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let new_list =
                ((agent_id, site_type1, site_type2, state1, state2),
                 (agent_id', site_type1', site_type2', state1', state2')) :: old_list
              in
              let error, store_result =
                Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.add_or_overwrite
                  parameter
                  error
                  ((agent_id, site_type1, state1), (agent_id', site_type1', state1'))
                  new_list
                  store_result
              in
              store_result
            ) set store_pair_map
        ) store_parallel_bonds
    in
    let static = set_fst_site_create_parallel_bonds_rhs store_result static in
    error, static
      
  (**************************************************************************)
  (*second binding*)

  let collect_snd_site_create_parallel_bonds_rhs static error rule_id =
    let parameter = get_parameter static in
    (*--------------------------------------------------------------------*)
    (*get parallel set*)
    let store_parallel_bonds = get_parallel_bonds_rhs static in
    let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in
    let error, store_pair_map =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter
          error
          rule_id
          store_snd_site_create_parallel_bonds_rhs
      with
      | error, None -> error, Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.empty
      | error, Some m -> error, m
    in    
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.fold
            (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2),
                  (agent_id', agent_type', site_type1', site_type2', state1', state2')) store_result ->
              let error, old_list =
                match
                  Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.find_option_without_logs
                    parameter
                    error
                    ((agent_id, site_type2, state2), (agent_id', site_type2', state2'))
                    store_result
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let new_list =
                ((agent_id, site_type1, site_type2, state1, state2),
                 (agent_id', site_type1', site_type2', state1', state2')) :: old_list
              in
              let error, store_result =
                Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.add_or_overwrite
                  parameter
                  error
                  ((agent_id, site_type2, state2), (agent_id', site_type2', state2'))
                  new_list
                  store_result
              in
              store_result
            ) set store_pair_map
        ) store_parallel_bonds
    in
    let static = set_snd_site_create_parallel_bonds_rhs store_result static in
    error, static
    

  (**************************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let error, static =
      collect_parallel_bonds_rhs
        static        
        error
        rule_id
        rule.Cckappa_sig.e_rule_c_rule
    in
    (*rule that created a bond belong to parallel bond*)
    let error, static =
      collect_fst_site_create_parallel_bonds_rhs
        static
        error
        rule_id
    in
    let error, static =
      collect_snd_site_create_parallel_bonds_rhs
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
        store_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
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

  (**************************************************************************)
  (* to do *)
  (* take into account parallel bounds that may occur in initial states *)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* if a parallel bound occur in a lhs, check that this is possible *)
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
  (*PRINT*)
  (**************************************************************************)    

  let print_parallel_bonds static dynamic error store_result =
    let handler_kappa = get_kappa_handler static in
    let parameter = get_parameter static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id set ->
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);            
          Ckappa_sig.PairAgentsSitesStates_map_and_set.Set.iter
            (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2), 
                  (agent_id', agent_type', site_type1', site_type2', state1', state2')) ->
              let error, site_type1_string =
                try
                  Handler.string_of_site parameter error handler_kappa
                    agent_type site_type1
                with
                  _ -> warn parameter error (Some "line 658") Exit
                    (Ckappa_sig.string_of_site_name site_type1)
              in
              let error, site_type1_string' =
                try
                  Handler.string_of_site parameter error handler_kappa
                    agent_type' site_type1'
                with
                  _ -> warn parameter error (Some "line 658") Exit
                    (Ckappa_sig.string_of_site_name site_type1')
              in
              let error, site_type2_string =
                try
                  Handler.string_of_site parameter error handler_kappa
                    agent_type site_type2
                with
                  _ -> warn parameter error (Some "line 658") Exit
                    (Ckappa_sig.string_of_site_name site_type2)
              in
              let error, site_type2_string' =
                try
                  Handler.string_of_site parameter error handler_kappa
                    agent_type' site_type2'
                with
                  _ -> warn parameter error (Some "line 658") Exit
                    (Ckappa_sig.string_of_site_name site_type2')
              in
              let error, state1_string =
                try
		   Handler.string_of_state_fully_deciphered parameter error handler_kappa
		     agent_type site_type1 state1
		 with
		   _ -> warn parameter error (Some "line 595") Exit
                     (Ckappa_sig.string_of_state_index state1)
              in
              let error, state2_string =
                try
		   Handler.string_of_state_fully_deciphered parameter error handler_kappa
		     agent_type site_type2 state2
		 with
		   _ -> warn parameter error (Some "line 595") Exit
                     (Ckappa_sig.string_of_state_index state2)
              in
              let error, state1_string' =
                try
		   Handler.string_of_state_fully_deciphered parameter error handler_kappa
		     agent_type' site_type1' state1'
		 with
		   _ -> warn parameter error (Some "line 595") Exit
                     (Ckappa_sig.string_of_state_index state1')
              in
              let error, state2_string' =
                try
		   Handler.string_of_state_fully_deciphered parameter error handler_kappa
		     agent_type' site_type2' state2'
		 with
		   _ -> warn parameter error (Some "line 595") Exit
                     (Ckappa_sig.string_of_state_index state2')
              in              
              let _ =
                Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "agent_id:%i:site_type:%i:%s.%s:site_type:%i:%s.%s->agent_id:%i:site_type:%i:%s.%s:site_type:%i:%s.%s\n"
                  (Ckappa_sig.int_of_agent_id agent_id)
                  (Ckappa_sig.int_of_site_name site_type1)
                  site_type1_string
                  state1_string
                  (Ckappa_sig.int_of_site_name site_type2)
                  site_type2_string
                  state2_string
                  (**)
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
    let parameter = get_parameter static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id map ->
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);
          Ckappa_sig.PairAgentIDSiteState_map_and_set.Map.iter
            (fun ((agent_id, site_type, state), (agent_id', site_type', state')) list ->
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "(agent_id:%i,site_type:%i:state:%i, agent_id:%i,site_type:%i:state:%i):\n"
                (Ckappa_sig.int_of_agent_id agent_id)
                (Ckappa_sig.int_of_site_name site_type)
                (Ckappa_sig.int_of_state_index state)
                (Ckappa_sig.int_of_agent_id agent_id')
                (Ckappa_sig.int_of_site_name site_type')
                (Ckappa_sig.int_of_state_index state')
              ;
              List.iter
                (fun ((agent_id, site_type1, site_type2, state1, state2),
                      (agent_id', site_type1', site_type2', state1', state2')) ->
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "agent_id:%i:site_type:%i:site_type:%i:state:%i:state:%i->agent_id:%i:site_type:%i:site_type:%i:state:%i:state:%i\n"
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

  let print_snd_site_create_parallel_rhs static dynamic error =
    let parameter = get_parameter static in
    let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "\nRule create a second bond the parallel bond in the rhs:\n";
    print_site_create_parallel static dynamic error store_snd_site_create_parallel_bonds_rhs
        
  (**************************************************************************)
  (* to do *)

  let print static dynamic error loggers =
    (*let _ =
      print_parallel_bonds_rhs static dynamic error
    in
    let _ =
      print_action_binding static dynamic error
    in
    let _ =
      print_fst_site_create_parallel_rhs static dynamic error
    in
    let _ =
      print_snd_site_create_parallel_rhs static dynamic error
    in*)
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
