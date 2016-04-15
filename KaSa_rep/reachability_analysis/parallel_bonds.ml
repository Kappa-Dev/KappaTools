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
     (and at which position (its internal state ~u~p, ...).  Return a map
     that detect if one of the site belong to parallel bond, can be bond
     alone, and in which rule.     
  *)
  (* a map -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
     which rules can create a bond of type A.y.t.B (and at which position *)
  (* and a map (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain
     which rules can contain parallel bonds in their lhs *)
  
  type local_static_information =
    {
      store_parallel_bonds_rhs: 
        Ckappa_sig.PairAgentSite_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
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
      
  let get_bonds_rhs static = lift Analyzer_headers.get_bonds_rhs static
    
  let get_bonds_lhs static = lift Analyzer_headers.get_bonds_lhs static
    
  let get_parallel_bonds_rhs static =
    (get_local_static_information static).store_parallel_bonds_rhs
      
  let set_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_parallel_bonds_rhs = bonds
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
  (*implementation of static information*)

  (**************************************************************************)
  (*get information of bonds: (agent_id, site_type -> agent_id, site_type)
    set rule_id map *)

  let collect_bonds_rhs parameter error rule_id rule =
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let agent_id' = site_add.Cckappa_sig.agent_index in
                let site_type_target = site_add.Cckappa_sig.site in
                (*store*)
                let error, old_set =
                  match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                    parameter error
                    rule_id
                    store_result
                  with 
                  | error, None -> error, Ckappa_sig.PairAgentSite_map_and_set.Set.empty
                  | error, Some s -> error, s
                in
                (*agent_id:0:site_type:1 -> agent_id:1:site_type:1*)
                let error', set =
                  Ckappa_sig.PairAgentSite_map_and_set.Set.add_when_not_in
                    parameter 
                    error
                    ((agent_id, site_type_source), (agent_id', site_type_target))
                    old_set
                in
                let error = Exception.check warn parameter error error' (Some "line 181") Exit in
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
          in 
          error, store_result
        ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds Ckappa_sig.Rule_map_and_set.Map.empty
    in
    error, store_result

  (*get information of rule that create a new bond*)

  let collect_parallel_bonds_rhs static error rule_id rule =
    let parameter = get_parameter static in
    let error, store_bonds_rhs = collect_bonds_rhs parameter error rule_id rule in
    let error, bonds_rhs_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
        parameter error rule_id store_bonds_rhs with
        | error, None -> error, Ckappa_sig.PairAgentSite_map_and_set.Set.empty
        | error, Some s -> error, s
    in
    let store_result = get_parallel_bonds_rhs static in
    let error, store_result =
      List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
        let agent_id1 = site_add1.Cckappa_sig.agent_index in
        let site_type1 = site_add1.Cckappa_sig.site in
        let agent_id2 = site_add2.Cckappa_sig.agent_index in
        let site_type2 = site_add2.Cckappa_sig.site in
        let error', binding_set =
          Ckappa_sig.PairAgentSite_map_and_set.Set.add_when_not_in
            parameter
            error
            ((agent_id1, site_type1), (agent_id2, site_type2))
            Ckappa_sig.PairAgentSite_map_and_set.Set.empty
        in
        let error = Exception.check warn parameter error error' (Some "line 223") Exit in
        (*check if the binding belong to a bonds rhs has two sites*)
        let error, parallel_set =
          Ckappa_sig.PairAgentSite_map_and_set.Set.fold
            (fun ((agent_id, site_type), (agent_id', site_type')) (error, store_set) ->
              if agent_id = agent_id1 &&
                agent_id' = agent_id2 &&
                site_type <> site_type1 &&
                site_type' <> site_type2
              then
                let error', parallel_set =
                  Ckappa_sig.PairAgentIDSites_map_and_set.Set.add_when_not_in
                    parameter
                    error
                    ((agent_id, site_type1, site_type), (agent_id', site_type2, site_type'))
                    store_set
                in
                let error = Exception.check warn parameter error error' (Some "line 241") Exit in
                error, parallel_set
              else
                error, store_set                
            ) bonds_rhs_set (error, Ckappa_sig.PairAgentIDSites_map_and_set.Set.empty)
        in
        let error, store_result =
          Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite
            parameter
            error  
            rule_id
            binding_set
            store_result
        in
        (*TODO:check if the binding state belong to the parallel*)
        let error, belong_set =
          Ckappa_sig.PairAgentIDSites_map_and_set.Set.fold
            (fun ((agent_id, site_type, site_type1), (agent_id', site_type', site_type1'))
              (error, store_set) ->
                (*if parallel bonds belong to binding set then return the binding set*)
                if Ckappa_sig.PairAgentSite_map_and_set.Set.mem
                  ((agent_id, site_type), (agent_id', site_type'))
                  binding_set
                then
                  error, store_result
                else
                  error, Ckappa_sig.Rule_map_and_set.Map.empty
            ) parallel_set (error, store_result)
        in
        error, belong_set
      ) (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.bind
    in
    let static = set_parallel_bonds_rhs store_result static in
    error, static
    
  (**************************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let error, static =
      collect_parallel_bonds_rhs
        static        
        error
        rule_id
        rule.Cckappa_sig.e_rule_c_rule
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

  (* to do *)
  let print static dynamic error loggers =
    (*let parameter = get_parameter static in
    let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    let _ =
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id set ->
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);            
          Ckappa_sig.PairAgentSite_map_and_set.Set.iter
            (fun ((agent_id, site_type), (agent_id', site_type')) ->
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "agent_id:%i:site_type:%i->agent_id:%i:site_type:%i\n"
                (Ckappa_sig.int_of_agent_id agent_id)
                (Ckappa_sig.int_of_site_name site_type)
                (Ckappa_sig.int_of_agent_id agent_id')
                (Ckappa_sig.int_of_site_name site_type')
            ) set
    (* Ckappa_sig.PairAgentIDSites_map_and_set.Set.iter
       (fun ((agent_id, site_type1, site_type2), (agent_id', site_type1', site_type2')) ->
       Loggers.fprintf (Remanent_parameters.get_logger parameter)
       "agent_id:%i:site_type:%i:site_type:%i->agent_id:%i:site_type:%i:site_type:%i\n"
       (Ckappa_sig.int_of_agent_id agent_id)
       (Ckappa_sig.int_of_site_name site_type1)
       (Ckappa_sig.int_of_site_name site_type2)
       (Ckappa_sig.int_of_agent_id agent_id')
       (Ckappa_sig.int_of_site_name site_type1')
       (Ckappa_sig.int_of_site_name site_type2')
       ) set*)
       ) store_parallel_bonds_rhs 
    in*)
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
