(*
  * views_domain.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Jul 13 2016>
  *
  * A monolitich domain to deal with all concepts in reachability analysis
  * This module is temporary and will be split according to different concepts
  * thanks to the functor Product
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** Abstract domain to over-approximate the set of reachable views *)
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

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information  : Parallel_bonds_static.local_static_information
    }

  (*--------------------------------------------------------------*)
  (* one map: for each tuple: Yes, No, Maybe, *)
  (* Yes: to say that when the sites x and y are bound with sites of
     the good type, then they are bound to the same B*)
  (* No: to say that when the sites x and y are bound with sites of the good
     type, then they are never bound to the same B*)
  (* Maybe: both case may happen*)

  type local_dynamic_information =
    {
      dummy: unit;
      store_value:
        bool Usual_domains.flat_lattice
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t
    }

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information;
    }

  (*-------------------------------------------------------------*)
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

  let get_action_binding static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_action_binding

  let set_action_binding bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_action_binding = bonds
      }
      static

  let get_views_rhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_views_rhs

  let set_views_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_views_rhs = bonds
      }
      static

  let get_bonds_rhs_full static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_bonds_rhs_full

  let set_bonds_rhs_full bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_bonds_rhs_full = bonds
      }
      static

  (*parallel bonds*)

  let get_parallel_bonds_rhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_parallel_bonds_rhs

  let set_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_parallel_bonds_rhs = bonds
      }
      static

  let get_rule_has_parallel_bonds_rhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_rule_has_parallel_bonds_rhs

  let set_rule_has_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_rule_has_parallel_bonds_rhs = bonds
      }
      static

  (*non parallel bonds*)
  let get_rule_has_non_parallel_bonds_rhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_rule_has_non_parallel_bonds_rhs

  let set_rule_has_non_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_rule_has_non_parallel_bonds_rhs = bonds
      }
      static

  let get_fst_site_create_parallel_bonds_rhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_fst_site_create_parallel_bonds_rhs

  let set_fst_site_create_parallel_bonds_rhs l static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_fst_site_create_parallel_bonds_rhs = l
      }
      static

  let get_snd_site_create_parallel_bonds_rhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_snd_site_create_parallel_bonds_rhs

  let set_snd_site_create_parallel_bonds_rhs l static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_snd_site_create_parallel_bonds_rhs = l
      }
      static

  (*lhs*)

  let get_bonds_lhs_full static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_bonds_lhs_full

  let set_bonds_lhs_full bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_bonds_lhs_full = bonds
      }
      static
      
  let get_rule_has_parallel_bonds_lhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_rule_has_parallel_bonds_lhs
      
  let set_rule_has_parallel_bonds_lhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_rule_has_parallel_bonds_lhs = bonds
      }
      static

  (*non parallel bonds*)
  let get_rule_has_non_parallel_bonds_lhs static =
    (get_local_static_information 
       static).Parallel_bonds_static.store_rule_has_non_parallel_bonds_lhs

  let set_rule_has_non_parallel_bonds_lhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        Parallel_bonds_static.store_rule_has_non_parallel_bonds_lhs = bonds
      }
      static
  (*---------------------------------------------------------------*)
  (*dynamic information*)

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

  (*current value of parallel bonds*)
  let get_value dynamic =
    (get_local_dynamic_information dynamic).store_value

  let set_value value dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value = value
      } dynamic

  (*--------------------------------------------------------------*)

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
  (*rule*)
  (*****************************************************************)

  let scan_rule static _dynamic error rule_id rule =
    let parameter = get_parameter static in
    (*------------------------------------------------------*)
    (*views on the right hand side*)
    let store_views_rhs = get_views_rhs static in
    let error, store_views_rhs =
      Parallel_bonds_static.collect_views_rhs
        parameter error rule_id rule store_views_rhs
    in
    let static = set_views_rhs store_views_rhs static in
    (*------------------------------------------------------*)
    (*action created a binding site*)
    let store_action_binding = get_action_binding static in
    let error, store_action_binding =
      Parallel_bonds_static.collect_action_binding
        parameter error rule_id rule store_action_binding
    in
    let static = set_action_binding store_action_binding static in
    (*------------------------------------------------------*)
    (*binding on the right hand side*)
    let store_bonds_rhs_full = get_bonds_rhs_full static in
    let error, store_bonds_rhs_full =
      Parallel_bonds_static.collect_bonds_rhs_full
        parameter
        error
        rule_id
        rule
        store_bonds_rhs_full
    in
    let static = set_bonds_rhs_full store_bonds_rhs_full static in
    (*------------------------------------------------------*)
    (*binding on the left hand side*)
    let store_bonds_lhs_full = get_bonds_lhs_full static in
    let error, store_bonds_lhs_full =
      Parallel_bonds_static.collect_bonds_lhs_full
        parameter
        error
        rule_id
        rule
        store_bonds_lhs_full
    in
    let static = set_bonds_lhs_full store_bonds_lhs_full static in
       (*------------------------------------------------------*)
    (*binding on the left hand side*)
    let store_bonds_lhs_full = get_bonds_lhs_full static in
    let error, store_bonds_lhs_full =
      Parallel_bonds_static.collect_bonds_lhs_full
        parameter
        error
        rule_id
        rule
        store_bonds_lhs_full
    in
    let static = set_bonds_lhs_full store_bonds_lhs_full static in
    (*------------------------------------------------------*)
    (*a set of rules that has a potential double binding on the lhs*)
    let store_bonds_lhs_full = get_bonds_lhs_full static in
    let store_result = get_rule_has_parallel_bonds_lhs static in
    let error, store_result =
      Parallel_bonds_static.collect_rule_has_parallel_bonds_lhs
        parameter
        store_bonds_lhs_full
        error
        rule_id
        rule
        store_result
    in
    let static = set_rule_has_parallel_bonds_lhs store_result static in
    (*------------------------------------------------------*)
    (*a set of rules that has a potential double bindings on the rhs*)
    let store_bonds_rhs_full = get_bonds_rhs_full static in
    let store_result = get_rule_has_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_rule_has_parallel_bonds_rhs
        parameter
        store_bonds_rhs_full
        error
        rule_id
        rule
        store_result
    in
    let static = set_rule_has_parallel_bonds_rhs store_result static in
    (*------------------------------------------------------*)
    (*a set of rules that has a potential non double bindings on the rhs*)
    let store_result = get_rule_has_non_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_rule_has_non_parallel_bonds_rhs
        parameter
        error
        rule_id
        rule
        store_bonds_rhs_full
        store_result
    in
    let static = set_rule_has_non_parallel_bonds_rhs store_result static in
    (*------------------------------------------------------*)
    (*a set of double bindings on the rhs*)
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    let store_result = get_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_parallel_bonds_rhs
        parameter
        store_rule_has_parallel_bonds_rhs
        error
        rule_id
        store_result
    in
    let static = set_parallel_bonds_rhs store_result static in
      error, static

  (****************************************************************)

  let scan_rules static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun _ error rule_id rule static ->
           let error, static =
             scan_rule
               static
               dynamic
               error
               rule_id
               rule.Cckappa_sig.e_rule_c_rule
           in
           error, static
        ) compil.Cckappa_sig.rules static
    in
    (*------------------------------------------------------*)
    (*A(x!1, y), B(x!1, y): first site is an action binding*)
    let store_action_binding = get_action_binding static in
    let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_fst_site_create_parallel_bonds_rhs
        parameter
        error
        store_action_binding
        store_parallel_bonds_rhs
    in
    let static = set_fst_site_create_parallel_bonds_rhs store_result static in
    (*------------------------------------------------------*)
    (*A(x, y!1), B(x, y!1): second site is an action binding *)
    let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_snd_site_create_parallel_bonds_rhs
        parameter error
        store_action_binding
        store_parallel_bonds_rhs
    in
    let static = set_snd_site_create_parallel_bonds_rhs store_result static in
    (*------------------------------------------------------*)
    error, static, dynamic

  (***************************************************************)

  let initialize static dynamic error =
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = Parallel_bonds_static.init_local_static;
      }
    in
    let init_local_dynamic_information =
      {
        dummy = ();
        store_value = 
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty
      }
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = init_local_dynamic_information ;
      }
    in
    let error, static, dynamic =
      scan_rules
        init_global_static_information
        init_global_dynamic_information error
    in
    error, static, dynamic

  (***************************************************************)
  (*Initial state*)
  (***************************************************************)

  (***************************************************************)
  (*a map of parallel bonds in the initial states, if the set
    if empty then return false, if it has parallel bonds return
    true.*)

  let compute_value_init static dynamic error init_state =
        let parameter = get_parameter static in
        let kappa_handler = get_kappa_handler static in
        (*--------------------------------------------------------*)
        (*bonds in the initial states*)
        let error, store_bonds_init =
          Parallel_bonds_init.collect_bonds_initial
            parameter
            error
            init_state
        in
        (*--------------------------------------------------------*)
        (*a set of potential non parallel bonds*)
        let error, store_site_pair_list =
          Parallel_bonds_init.collect_non_parallel_init_aux
            parameter
            store_bonds_init
            error
        in
        let error, store_non_parallel_init =
          Parallel_bonds_init.collect_non_parallel_init
            parameter
            store_bonds_init
            store_site_pair_list
            error
        in
        (*---------------------------------------------------------*)
        (*value of non parallel bonds*)
        let store_result = get_value dynamic in
        let error, store_result =
          Parallel_bonds_init.collect_value_non_parallel_bonds
            parameter
            store_non_parallel_init
            error
            kappa_handler
            store_result
        in   
        let dynamic = set_value store_result dynamic in
        (*---------------------------------------------------------*)
        (*a set of potential parallel bonds*)
        let error, store_parallel_bonds_init =
          Parallel_bonds_init.collect_parallel_bonds_init
            parameter
            store_bonds_init
            error
            init_state
        in
        (*---------------------------------------------------------*)
        (*value of parallel bonds*)
        let store_result = get_value dynamic in
        let error, store_result =
          Parallel_bonds_init.collect_value_parallel_bonds
            parameter
            store_parallel_bonds_init
            error
            kappa_handler
            store_result
        in
        let dynamic = set_value store_result dynamic in
        error, dynamic

  (*************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    (*parallel bonds in the initial states*)
    let error, dynamic =
      compute_value_init static dynamic error species
    in
    error, dynamic, event_list

  (*************************************************************)
  (* if a parallel bound occur in a lhs, check that this is possible *)

  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    let parameter = get_parameter static in
    (*-----------------------------------------------------------*)
    (*a set of rules has parallel bonds on the lhs*)
    let store_rule_has_parallel_bonds_lhs = get_rule_has_parallel_bonds_lhs static in
    let error, (b, parallel_set) =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id store_rule_has_parallel_bonds_lhs
      with
      | error, None ->
        error,
        (false, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty)
      | error, Some (b, s) -> error, (b, s)
    in
    (*-----------------------------------------------------------*)
    let store_non_parallel_map = get_rule_has_non_parallel_bonds_lhs static in
    let error, (b', non_parallel_list) =
      match 
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_non_parallel_map
      with
      | error, None -> error, (false, [])
      | error, Some (b, l) -> error, (b, l)
    in
    (*-----------------------------------------------------------*)
    let store_value = get_value dynamic in
    let error, dynamic, op =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold
        (fun (x,y) value (error, dynamic, op) ->
           (*with/without parallel bonds or non parallel bonds*)
          if b || b' || (b && b') 
          then
            match value with        
            | Usual_domains.Val _
            | Usual_domains.Any -> error, dynamic, op
            | Usual_domains.Undefined -> error, dynamic, None
          else
            error, dynamic, op
        ) store_value (error, dynamic, Some precondition)
    in
    error, dynamic, op

  (***************************************************************)
  (* when one bond is created, check in the precondition, whether the two
     other sites may be bound, check whether they must be bound to the same
     agents, whether they cannot be bound to the same agent, whether we cannot
     know, and deal with accordingly *)

  let get_state_of_site_in_precondition parameter error dynamic agent_id site_type precondition =
    (*binding action: A.x.B.z -> parallel bonds: A.x.y.B.z.t, B.z.t.A.x.y
      (first bound)*)
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
      | Usual_domains.Any | Usual_domains.Undefined ->
        warn parameter error (Some "line 512") Exit []
    in
    let dynamic = set_global_dynamic_information global_dynamic dynamic in
    error, dynamic, precondition, state_list

  (***********************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    (*-----------------------------------------------------------*)
    let parameter = Remanent_parameters.update_prefix parameter "                " in
    let dump_title () =
      if local_trace || Remanent_parameters.get_dump_reachability_analysis_diff parameter
      then
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameter)
            "%sUpdate information about potential double bindings"
            (Remanent_parameters.get_prefix parameter)
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameter)
      else
        ()
    in
    (*------------------------------------------------------------------*)
    (*the first site in a potential double bindings is an action binding*)
    let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
    let error, store_pair_bind_map =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error
          rule_id
          store_fst_site_create_parallel_bonds_rhs
      with
      | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
      | error, Some m -> error, m
    in
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    let error, rule_has_parallel_bonds_rhs_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
          rule_id store_rule_has_parallel_bonds_rhs
      with
      | error, None -> error, Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*-----------------------------------------------------------*)
    let error, dynamic, precondition, store_value1 =
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.fold
        (fun (_, _) parallel_list (error, dynamic, precondition, store_result) ->
           (*let (agent_id, agent_type, site_type, state) = x in
           let (agent_id', agent_type', site_type', state') = y in*)
           let error, dynamic, precondition, store_result =
             List.fold_left (fun (error, dynamic, precondition, store_result) (z, t) ->
                 let (agent_id1, agent_type1, site_type1, site_type2, state1, state2) = z in
                 let (agent_id1', agent_type1', site_type1', site_type2', state1', state2') = t in
                 (*to be checked the agent_type, and site_type1*)
                 let error, old_value =
                   match
                     Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs
                       parameter error
                       (z, t)
                       store_result
                   with
                   | error, None -> error, Usual_domains.Undefined
                   | error, Some value -> error, value
                 in
                 (*------------------------------------------------------*)
                 (*get a list of potential states of the second site*)
                 let error, dynamic, precondition, state_list =
                   get_state_of_site_in_precondition
                     parameter error
                     dynamic
                     agent_id1 (*A*)
                     site_type2
                     precondition
                 in
                 let error, dynamic, precondition, state_list' =
                   get_state_of_site_in_precondition
                     parameter error
                     dynamic
                     agent_id1' (*B*)
                     site_type2'
                     precondition
                 in
                 let error, potential_list =
                   List.fold_left (fun (error, current_list) pre_state ->
                       List.fold_left (fun (error, current_list) pre_state' ->
                           let potential_list =
                             ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
                             (agent_id1', agent_type1', site_type1', site_type2', state1', pre_state'))
                             :: current_list
                           in
                           error, potential_list
                         ) (error, current_list) state_list'
                     ) (error, []) state_list
                 in
                 (*------------------------------------------------------*)
                 (*fold over a potential list and compare with parallel list*)
                 let error, value =
                   List.fold_left (fun (error, value) (x', y') ->
                       let (_, _, _, s_type2, _, pre_state2) = x' in
                       let (_, _, _, s_type2', _, pre_state2') = y' in
                       (*check if the pre_state2 and pre_state2' of the second site are
                         bound and if yes which the good state?  - Firstly check that if the
                         parallel bonds is an empty set then depend on the state of the
                         second site, it will give a different value: whether Undefined or
                         Any, (question 1 and 2)*)
                       if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.is_empty
                            rule_has_parallel_bonds_rhs_set
                       then
                         begin
                           (*question 1: if the pre_state2/pre_state2' of A or B is free -> undefined*)
                           if Ckappa_sig.int_of_state_index pre_state2 = 0 (*||
                              Ckappa_sig.int_of_state_index pre_state2' = 0*)
                           then
                             (*answer of question 1: the second site is free*)
                             let new_value = Usual_domains.lub value Usual_domains.Undefined in
                             error, new_value
                           else
                             (* the pre_state2 is bound or pre_state2' is bound. Question
                                2: both sites are bound with the good sites, then return Any,
                                if not return false*)
                             begin
                               if s_type2 = s_type2' && pre_state2 = pre_state2' &&
                                  not (Ckappa_sig.int_of_state_index pre_state2' = 0)
                               then
                                 (*both question1 and 2 are yes: return any*)
                                 let new_value = Usual_domains.lub value Usual_domains.Any in
                                 error, new_value
                               else
                                 (*the question1 is true but the question 2 is false -> false*)
                                 let new_value = Usual_domains.lub value (Usual_domains.Val false) in
                                 error, new_value
                             end
                         end
                       else
                         (*the set of parallel bonds is not empty. Check
                           the second site of A and B and its states
                           whether or not it belongs to a parallel set*)
                         begin
                           if s_type2 = site_type2 && pre_state2 = state2 &&
                              s_type2' = site_type2' && pre_state2' = state2'
                           then
                             (*it belongs to parallel set, the answer is yes*)
                             let new_value = Usual_domains.lub value (Usual_domains.Val true) in
                             error, new_value
                           else
                             (*no*)
                             let new_value = Usual_domains.lub value Usual_domains.Undefined in
                             error, new_value
                         end
                     ) (error, old_value) potential_list
                 in
                 (*------------------------------------------------------*)
                 (*call the symmetric add *)
                 let error, store_result =
                   Parallel_bonds_type.add_symmetric_tuple_pair
                     (fun parameter error t map ->
                        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                          parameter error
                          t
                          value
                          map)
                     parameter
                     error
                     (z, t)
                     store_result
                 in
                 error, dynamic, precondition, store_result
               ) (error, dynamic, precondition, store_result) parallel_list
           in
           error, dynamic, precondition, store_result
        ) store_pair_bind_map
        (error, dynamic, precondition,
         Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty)
    in
    (*-----------------------------------------------------------*)
    (*revert*)
    let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in
    let error, store_pair_bind_map2 =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error
          rule_id
          store_snd_site_create_parallel_bonds_rhs
      with
      | error, None ->
        error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
      | error, Some  m -> error, m
    in
    let error, dynamic, precondition, store_value2 =
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.fold
        (fun (x, y) parallel_list (error, dynamic, precondition, store_result) ->
           let (agent_id, agent_type, site_type, state) = x in
           let (agent_id', agent_type', site_type', state') = y in
           List.fold_left (fun (error, dynamic, precondtion, store_result) (z, t) ->
               let (agent_id1, _, site_type1, _, _, _) = z in
               let (agent_id1', _, site_type1', _, _, _) = t in
               (*to be check agent_type and site_type1*)
               let error, old_value =
                 match
                   Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs
                     parameter error
                     (z, t)
                     store_result
                 with
                 | error, None -> error, Usual_domains.Undefined
                 | error, Some value -> error, value
               in
               (*-----------------------------------------------------------*)
               let error, dynamic, precondition, state_list =
                 get_state_of_site_in_precondition
                   parameter error
                   dynamic
                   agent_id1
                   site_type1
                   precondition
               in
               let error, dynamic, precondition, state_list' =
                 get_state_of_site_in_precondition
                   parameter error
                   dynamic
                   agent_id1'
                   site_type1'
                   precondition
               in
               (*-----------------------------------------------------------*)
               let error, potential_list =
                 List.fold_left (fun (error, current_list) pre_state ->
                     List.fold_left (fun (error, current_list) pre_state' ->
                         let potential_list =
                           ((agent_id, agent_type, site_type1, site_type, pre_state, state),
                            (agent_id', agent_type', site_type1', site_type', pre_state', state'))
                           :: current_list
                         in
                         error, potential_list
                       ) (error, current_list) state_list'
                   ) (error, []) state_list
               in
               (*-----------------------------------------------------------*)
               let error, value =
                 List.fold_left (fun (error, value) (x', y') ->
                     let (_, _, s_type, _, pre_state, _) = x' in
                     let (_, _, s_type', _, pre_state', _) = y' in
                     if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.is_empty
                         rule_has_parallel_bonds_rhs_set
                     then
                       begin
                         if (Ckappa_sig.int_of_state_index pre_state = 0)
                         then
                           let new_value = Usual_domains.lub value Usual_domains.Undefined in
                           error, new_value
                         else
                           begin
                             if s_type = s_type' && pre_state = pre_state' &&
                                not (Ckappa_sig.int_of_state_index pre_state' = 0)
                             then
                               let new_value = Usual_domains.lub value Usual_domains.Any in
                               error, new_value
                             else
                               let new_value = Usual_domains.lub value (Usual_domains.Val false) in
                               error, new_value
                           end
                       end
                     else
                       (*check inside the parallel bonds*)
                       begin
                         if s_type = site_type && pre_state = state &&
                            s_type' = site_type' && pre_state' = state'
                         then
                           let new_value = Usual_domains.lub value (Usual_domains.Val true) in
                           error, new_value
                         else
                           let new_value = Usual_domains.lub value Usual_domains.Undefined in
                           error, new_value
                       end
                   ) (error, old_value) potential_list
               in
               (*-----------------------------------------------------------*)
               let error, store_result =
                 Parallel_bonds_type.add_symmetric_tuple_pair
                   (fun parameter error t map ->
                      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                        parameter error
                        t value map)
                   parameter error
                   (z, t)
                   store_result
               in
               error, dynamic, precondition, store_result
             ) (error, dynamic, precondition, store_result) parallel_list
        ) store_pair_bind_map2
        (error, dynamic, precondition,
         Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty)
    in
    (*-----------------------------------------------------------*)
    (*combine two value above*)
    let bool =
      if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.is_empty store_value1
      && Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.is_empty store_value2
      then
        false
      else
        let () = dump_title () in
        true
    in
     let error, store_value =
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.fold2
        parameter error
        (fun parameter error x value store_result ->
           let error, store_result =
             Parallel_bonds_type.add_value_from_refined_tuple
               parameter error kappa_handler x value store_result
           in
           error, store_result
        )
        (fun parameter error x value store_result ->
           let error, store_result =
             Parallel_bonds_type.add_value_from_refined_tuple
               parameter error kappa_handler
               x value store_result
           in
           error, store_result
        )
        (fun parameter error x value1 value2 store_result ->
            let new_value = Usual_domains.lub value1 value2 in
           let error, store_result =
             Parallel_bonds_type.add_value_from_refined_tuple
               parameter error kappa_handler
               x new_value store_result
           in
           error, store_result
        )
        store_value1
        store_value2
        Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty
    in
    (*--------------------------------------------------------------*)
    (*if it belongs to non parallel bonds then false*)
    let store_non_parallel_map = get_rule_has_non_parallel_bonds_rhs static in
    let error, non_parallel_rhs_list =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter
          error rule_id store_non_parallel_map
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, store_non_parallel =
      List.fold_left (fun (error, store_result) (x, y, z, t) ->
          let (_, agent_type, site_type, state) = x in
          let (_, _, site_type', state') = y in
          let (_, agent_type1, site_type1, state1) = z in
          let (_, _, site_type1', state1') = t in
          let error, store_result =
            Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite
              parameter error
              ((agent_type, site_type, site_type', state, state'),
               (agent_type1, site_type1, site_type1', state1, state1'))
              (Usual_domains.Val false)
             store_result
          in
          error, store_result
        )
        (error, Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty)
        non_parallel_rhs_list
    in
    (*--------------------------------------------------------------*)
    (*fold with store_value above*)
    let error, map_value = error, store_value in
    let () =
      if not bool &&
         Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.is_empty
           store_non_parallel
      then
        ()
      else
        dump_title ()
    in
    let store_result = get_value dynamic in
    let error, store_result =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold2
        parameter error
        (fun parameter error x value store_result ->
           Parallel_bonds_type.add_value parameter error kappa_handler x value store_result
        )
        (fun parameter error x value store_result ->
           Parallel_bonds_type.add_value parameter error kappa_handler x value store_result
        )
        (fun parameter error x value1 value2 store_result ->
           let new_value = Usual_domains.lub value1 value2 in
           Parallel_bonds_type.add_value parameter error kappa_handler x new_value store_result
        )
        map_value
        store_non_parallel
        store_result
    in
    let dynamic = set_value store_result dynamic in
    (*--------------------------------------------------------------*)
    (*if it belongs to parallel bonds then true*)
    let error, store_parallel =
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold
        (fun (x, y) (error, store_result) ->
           let pair = Parallel_bonds_type.project2 (x, y) in
           let error, store_result =
             Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite
               parameter error
               pair
               (Usual_domains.Val true)
               store_result
           in
           error, store_result
        ) rule_has_parallel_bonds_rhs_set
        (error, Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty)
    in
    (*--------------------------------------------------------------*)
    (*fold over the store_value and parallel bond value *)
    let () =
      if not bool &&
         Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.is_empty
           store_parallel
      then
        ()
      else
        dump_title ()
    in
    let store_result = get_value dynamic in
    let error, store_result =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold2
        parameter error
        (fun parameter error x value store_result ->
           Parallel_bonds_type.add_value parameter error
             kappa_handler x value store_result
        )
        (fun parameter error x value store_result ->
           Parallel_bonds_type.add_value parameter error
             kappa_handler x value store_result
        )
        (fun parameter error x value1 value2 store_result ->
           let new_value = Usual_domains.lub value1 value2 in
           Parallel_bonds_type.add_value parameter error
             kappa_handler x new_value store_result
        )
        map_value
        store_parallel
        store_result
    in
    let dynamic = set_value store_result dynamic in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  (****************************************************************)

  let print static dynamic (error:Exception.method_handler) loggers =
    let kappa_handler = get_kappa_handler static in
    let parameter = get_parameter static in
    let log = loggers in
   (*-------------------------------------------------------*)
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_result
          parameter
      then
        let () =
          Loggers.fprintf log
            "------------------------------------------------------------\n";
          Loggers.fprintf log "* Parallel bonds domain\n";
          Loggers.fprintf log
            "------------------------------------------------------------\n"
        in
        let store_value = get_value dynamic in
        let error =
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold
            (fun tuple value error ->
               Parallel_bonds_type.print_parallel_constraint
                 ~verbose:true
                 ~sparse:true
                 ~final_resul:true
                 ~dump_any:true parameter error kappa_handler tuple value
            ) store_value error
        in error
      else
        error
    in
    error, dynamic, ()

  (****************************************************************)

  let export _static dynamic error kasa_state =
    error, dynamic, kasa_state

  let lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
