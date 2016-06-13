(*
  * views_domain.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification:
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

  type local_static_information =
    {
      store_views_rhs : Parallel_bonds_type.AgentsSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_action_binding : Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
          Ckappa_sig.Rule_map_and_set.Map.t;
      store_bonds_rhs_full : Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
          Ckappa_sig.Rule_map_and_set.Map.t;
      store_bonds_lhs_full : Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.t
          Ckappa_sig.Rule_map_and_set.Map.t;
      store_parallel_bonds_rhs: Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t;
      store_rule_has_parallel_bonds_rhs:
        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*a map check a bond between A.x and B.z*)
      store_fst_site_create_parallel_bonds_rhs:
        ((Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
          Ckappa_sig.c_state * Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.c_site_name * Ckappa_sig.c_site_name *
          Ckappa_sig.c_state * Ckappa_sig.c_state)
        ) list
          Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.t Ckappa_sig.Rule_map_and_set.Map.t;
      (*rule has non parallel bonds in the rhs*)
      store_rule_has_non_parallel_bonds_rhs:
        ((Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.key *
          Ckappa_sig.c_agent_name * Ckappa_sig.Site_map_and_set.Map.elt *
          Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.Site_map_and_set.Map.elt * Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
         (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
          Ckappa_sig.c_site_name * Ckappa_sig.c_state))
          list
          Ckappa_sig.Rule_map_and_set.Map.t;
      (*a map check a bond between A.y and B.t*)
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

  (*--------------------------------------------------------------*)
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
      (*value of parallel bonds in the rhs*)
      store_value_parallel_bonds_rhs :
        bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
      (*value of non parallel bonds in the rhs*)
      store_value_non_parallel_bonds_rhs :
        bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
      (*the final value of parallel bonds and non parallel bonds in the rhs *)
      store_value_bonds_rhs :
        bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t;
      (*final result*)
      store_value_of_parallel_bonds:
        bool Usual_domains.flat_lattice
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.t
(*Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t*) (*
        (*reverse direction*)
        bool Usual_domains.flat_lattice Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.t*)
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

  (*rhs*)
  let get_action_binding static =
    (get_local_static_information static).store_action_binding

  let set_action_binding bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_action_binding = bonds
      }
      static

  let get_views_rhs static =
    (get_local_static_information static).store_views_rhs

  let set_views_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_views_rhs = bonds
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

  (*parallel bonds*)
  let get_rule_has_parallel_bonds_rhs static =
    (get_local_static_information static).store_rule_has_parallel_bonds_rhs

  let set_rule_has_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_rule_has_parallel_bonds_rhs = bonds
      }
      static

  (*non parallel bonds*)
  let get_rule_has_non_parallel_bonds_rhs static =
    (get_local_static_information static).store_rule_has_non_parallel_bonds_rhs

  let set_rule_has_non_parallel_bonds_rhs bonds static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_rule_has_non_parallel_bonds_rhs = bonds
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

  (*---------------------------------------------------------------*)
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

  (*initial states*)
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

  (*value in the rhs*)

  let get_value_parallel_bonds_rhs dynamic =
    (get_local_dynamic_information dynamic).store_value_parallel_bonds_rhs

  let set_value_parallel_bonds_rhs bonds dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value_parallel_bonds_rhs = bonds
      } dynamic

  let get_value_non_parallel_bonds_rhs dynamic =
    (get_local_dynamic_information dynamic).store_value_non_parallel_bonds_rhs

  let set_value_non_parallel_bonds_rhs bonds dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value_non_parallel_bonds_rhs = bonds
      } dynamic

  let get_value_bonds_rhs dynamic =
    (get_local_dynamic_information dynamic).store_value_bonds_rhs

  let set_value_bonds_rhs bonds dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value_bonds_rhs = bonds
      } dynamic

  (*final value of parallel bonds*)
  let get_value_of_parallel_bonds dynamic =
    (get_local_dynamic_information dynamic).store_value_of_parallel_bonds

  let set_value_of_parallel_bonds value dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value_of_parallel_bonds = value
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

  (******************************************************************)
  (** [get_scan_rule_set static] *)

  (****************************************************************)
  (**rules*)
  (*****************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let store_views_rhs = get_views_rhs static in
    let error, store_views_rhs =
      Parallel_bonds_static.collect_views_rhs
        parameter error rule_id rule store_views_rhs
    in
    let static = set_views_rhs store_views_rhs static in
    (*------------------------------------------------------*)
    let store_action_binding = get_action_binding static in
    let error, store_action_binding =
      Parallel_bonds_static.collect_action_binding
        parameter error rule_id rule store_action_binding
    in
    let static = set_action_binding store_action_binding static in
    (*------------------------------------------------------*)
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
    (*------------------------------------------------------*)
    let store_action_binding = get_action_binding static in
    let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
    let store_result = get_fst_site_create_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_fst_site_create_parallel_bonds_rhs
        parameter
        error
        rule_id
        store_action_binding
        store_parallel_bonds_rhs
        store_result
    in
    let static = set_fst_site_create_parallel_bonds_rhs store_result static in
    (*------------------------------------------------------*)
    let store_result = get_snd_site_create_parallel_bonds_rhs static in
    let error, store_result =
      Parallel_bonds_static.collect_snd_site_create_parallel_bonds_rhs
        parameter error rule_id
        store_action_binding
        store_parallel_bonds_rhs
        store_result
    in
    let static = set_snd_site_create_parallel_bonds_rhs store_result static in
    error, static

  (****************************************************************)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun _ error rule_id rule static ->
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

  (* todo *)
  let initialize static dynamic error =
    let init_local_static =
      {
        store_views_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_action_binding = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_bonds_rhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_bonds_lhs_full = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_parallel_bonds_rhs = Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.empty;
        store_rule_has_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_rule_has_non_parallel_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
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
        store_value_parallel_bonds_rhs = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
        store_value_non_parallel_bonds_rhs = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
        store_value_bonds_rhs = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty;
        store_value_of_parallel_bonds = Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty(*,
        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty*)
      }
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = init_local_dynamic_information ;
      }
    in
    let error, static, dynamic =
      scan_rule_set init_global_static_information
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

  (*a set that is not belong to a parallel bonds, both of them : any. if
    it is belong to the parallel bonds then return true, if it is belong to
    not parallel bonds return false, both return any.*)

  let compute_value_init static dynamic error init_state =
    let parameter = get_parameter static in
    let error, store_bonds_init =
      Parallel_bonds_init.collect_bonds_initial
        parameter
        error
        init_state
    in
    let error, store_site_pair_list =
      Parallel_bonds_init.collect_non_parallel_init_aux
        parameter
        store_bonds_init
        error
        init_state
    in
    let error, store_non_parallel_init =
      Parallel_bonds_init.collect_non_parallel_init
        parameter
        store_bonds_init
        store_site_pair_list
        error
        init_state
    in
    let store_result = get_value_non_parallel_bonds_init dynamic in
    let error, store_result =
      Parallel_bonds_init.collect_value_non_parallel_bonds
        parameter
        store_non_parallel_init
        error
        init_state
        store_result
    in
    let dynamic = set_value_non_parallel_bonds_init store_result dynamic in
    (*------------------------------------------------------------*)
    let error, store_parallel_bonds_init =
      Parallel_bonds_init.collect_parallel_bonds_init
        parameter
        store_bonds_init
        error
        init_state
    in
    let store_result = get_value_parallel_bonds_init dynamic in
    let error, store_result =
      Parallel_bonds_init.collect_value_parallel_bonds
        parameter
        store_parallel_bonds_init
        error
        init_state
        store_result
    in
    let dynamic = set_value_parallel_bonds_init store_result dynamic in
    (*-------------------------------------------------------------*)
    let value_parallel_bonds = get_value_parallel_bonds_init dynamic in
    let value_non_parallel_bonds = get_value_non_parallel_bonds_init dynamic in
    let store_result = get_value_of_init dynamic in
    let error, store_result =
      Parallel_bonds_init.collect_value_of_init
        parameter
        value_parallel_bonds
        value_non_parallel_bonds
        error
        init_state
        store_result
    in
    let dynamic = set_value_of_init store_result dynamic in
    error, dynamic

  let add_initial_state static dynamic error species =
    let event_list = [] in
    (*parallel bonds in the initial states*)
    let error, dynamic =
      compute_value_init static dynamic error species
    in
    error, dynamic, event_list

  (*************************************************************)
  (* TODO *)
  (* if a parallel bound occur in a lhs, check that this is possible *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (***************************************************************)
  (* to do, when one bond is created, check in the precondition, whether the two other sites may be bound, check whether they must be bound to the same agents, whether they cannot be bound to the same agent, whether we cannot know, and deal with accordingly *)

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
      | Usual_domains.Any | Usual_domains.Undefined -> warn parameter error (Some "line 1455") Exit []
    in
    let dynamic = set_global_dynamic_information global_dynamic dynamic in
    error, dynamic, precondition, state_list

(****************************************************************)
(*return the first site that created a parallel sites *)

  let compute_value_fst_aux error site_type2 state2 site_type2' state2' rule_has_parallel_bonds_rhs_set old_value potential_list =
  List.fold_left (fun (error, value) (x, y) ->
      (*the first site s_type and s_type' are bound together, the action binding appears*)
      let (_, _, _, s_type2, _, p_state2) = x in
      let (_, _, _, s_type2', _, p_state2') = y in
      (*check if the pre_state2 and pre_state2' of the second site are bound and if yes which the good state?
        - Firstly check that if the parallel bonds is an empty set then depend on the state of the second site, it will give a different value: whether Undefined or Any, (question 1 and 2)*)
      if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.is_empty rule_has_parallel_bonds_rhs_set
      then
        begin
          (*question 1: if the pre_state2/pre_state2' of A or B is free -> undefined*)
          if (Ckappa_sig.int_of_state_index p_state2 = 0) || (Ckappa_sig.int_of_state_index p_state2') = 0
          then
            (*answer of question 1: the second site is free*)
            (*let error =
              if local_trace ||
                 Remanent_parameters.get_dump_reachability_analysis_parallel parameter
              then
                let _ = Loggers.fprintf log "Undefined_free\n"
                in
                error
              else error
            in*)
            let new_value = Usual_domains.lub value Usual_domains.Undefined
            in
            error, new_value
          else
            (* the pre_state2 is bound or pre_state2' is bound. Question 2: both sites are bound with the good sites, then return Any, if not return false*)
          begin
            if s_type2 = s_type2' && p_state2 = p_state2'
            then
              (*both question1 and 2 are yes: return any*)
              (*let error =
                if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
                then
                  let _ = Loggers.fprintf log "Any\n" in
                  error
                else error
              in*)
              let new_value = Usual_domains.lub value Usual_domains.Any in
              error, new_value
            else
              (*the question1 is true but the question 2 is false -> false*)
              (*let error =
                if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
                then
                  let _ =
                    Loggers.fprintf log "False\n"
                  in
                  error
                else
                  error
              in*)
              let new_value = Usual_domains.lub value (Usual_domains.Val false) in
              error, new_value
          end
        end
      else
        (*the set of parallel bonds is not empty. Check the second sites of A and B and its states whether or not it belongs to parallel set*)
      if s_type2 = site_type2 && p_state2 = state2 && s_type2' = site_type2' && p_state2' = state2'
      then
        (*it belongs to parallel set, the answer is yes*)
        (*let error =
          if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
          then
        let _ =
          Loggers.fprintf log "True\n"
        in
        error
          else error
        in*)
        let new_value = Usual_domains.lub value (Usual_domains.Val true) in
        error, new_value
      else
        (*it does not belogn to the parallel set, Todo: continue to check with the non parallel bonds*)
        (*let error =
          if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
          then
            let _ =
              Loggers.fprintf log "Undefined\n"
            in
            error
          else error
        in*)
        let new_value = Usual_domains.lub value Usual_domains.Undefined in
      error, new_value
      ) (error, old_value) potential_list

  let collect_result_from_site_create_parallel parameter error dynamic precondition store_pair_bind_map rule_has_parallel_bonds_rhs_set store_result =
    let error, dynamic, precondition, store_result =
      (*fold over a binding action map*)
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.fold
        (*A.x.B.z*)
        (fun ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state'))
          parallel_list (error, dynamic, precondition, store_result) ->
          (*------------------------------------------------------*)
          (*fold over a list of parallel bonds*)
          let error, dynamic, precondition, store_result =
            List.fold_left
              (fun (error, dynamic, precondition, store_result)
                (*A.t.z.B.t.z, B.t.z.A.t.z*)
                ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),                              (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) ->
                if agent_type = agent_type1 && site_type1 = site_type
                then
                  let error, old_value =
                  match
                    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs parameter error
                      ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                       (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                    store_result
                  with
                  | error, None -> error, Usual_domains.Undefined
                  | error, Some value -> error, value
                in
                (*get a list of state of the second site site_type2 in the precondition*)
                (*A.x.y.B.z.t*)
                let error, dynamic, precondition, state_list =
                  get_state_of_site_in_precondition
                    parameter
                    error
                    dynamic
                    agent_id1 (*A*)
                    site_type2
                    precondition
                in
                (*get pre_state for B*)
                let error, dynamic, precondition, state_list' =
                  get_state_of_site_in_precondition
                    parameter
                    error
                    dynamic
                    agent_id1' (*B*)
                    site_type2'
                    precondition
                in
                (*a map of potential sites*)
                let error, potential_list =
                  List.fold_left (fun (error, store_list) pre_state ->
                      let error, potential_list =
                        List.fold_left (fun (error, current_list) pre_state' ->
                          let potential_list =
                            ((agent_id, agent_type, site_type, site_type2, state, pre_state),
                             (agent_id', agent_type', site_type', site_type2', state', pre_state')) :: current_list
                          in
                          error, potential_list
                          ) (error, store_list) state_list'
                      in
                      error, potential_list
                    ) (error, []) state_list
                in
                (*print for test, list of potential_list*)
                (*let error =
                  if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
                  then
                    begin
                      let () = Print_parallel_bonds.print_action_binding_test parameter error handler_kappa rule_id
                    ((agent_id, agent_type, site_type, state),
                     (agent_id', agent_type', site_type', state'));
                        Loggers.fprintf log "List of potential sites:\n";
                        List.iter
                          (fun
                            ((ag_id, ag_type, s_type, s_type2, p_state, p_state2),
                             (ag_id', ag_type', s_type', s_type2', p_state', p_state2')) ->
                            let error, ag_string =
                              try
                                Handler.string_of_agent parameter error handler_kappa ag_type
                              with
                                _ -> warn parameter error (Some "line 1706") Exit (Ckappa_sig.string_of_agent_name ag_type)
                            in
                            let error, s_type_string =
                              try                          Handler.string_of_site parameter error
                            handler_kappa ag_type s_type
                              with
                                _ -> warn parameter error (Some "line 1714") Exit
                                       (Ckappa_sig.string_of_site_name s_type)
                            in
                            let error, p_state_string =
                        try
                          Handler.string_of_state_fully_deciphered parameter error handler_kappa
                            ag_type s_type p_state
                        with
                          _ -> warn parameter error (Some "line 1530") Exit
                                 (Ckappa_sig.string_of_state_index p_state)
                      in
                      let error, s_type2_string =
                        try
                          Handler.string_of_site parameter error
                            handler_kappa ag_type s_type2
                        with
                          _ -> warn parameter error (Some "line 1714") Exit
                                 (Ckappa_sig.string_of_site_name s_type2)
                      in
                      let error, p_state2_string =
                        try
                          Handler.string_of_state_fully_deciphered parameter error handler_kappa
                            ag_type s_type2 p_state2
                        with
                          _ -> warn parameter error (Some "line 1530") Exit
                                 (Ckappa_sig.string_of_state_index p_state2)
                      in
                      (**)
                      let error, ag_string' =
                        try
                          Handler.string_of_agent parameter error handler_kappa ag_type'
                        with
                          _ -> warn parameter error (Some "line 1706") Exit
                                 (Ckappa_sig.string_of_agent_name ag_type')
                      in
                      let error, s_type'_string =
                        try
                          Handler.string_of_site parameter error
                            handler_kappa ag_type' s_type'
                        with
                          _ -> warn parameter error (Some "line 1714") Exit
                                 (Ckappa_sig.string_of_site_name s_type')
                      in
                      let error, p_state'_string =
                        try
                          Handler.string_of_state_fully_deciphered parameter error handler_kappa
                            ag_type' s_type' p_state'
                        with
                          _ -> warn parameter error (Some "line 1530") Exit
                                 (Ckappa_sig.string_of_state_index p_state')
                      in
                      let error, s_type2'_string =
                        try
                          Handler.string_of_site parameter error
                            handler_kappa ag_type' s_type2'
                        with
                          _ -> warn parameter error (Some "line 1714") Exit
                                 (Ckappa_sig.string_of_site_name s_type2')
                      in
                      let error, p_state2'_string =
                        try
                          Handler.string_of_state_fully_deciphered parameter error handler_kappa
                            ag_type' s_type2' p_state2'
                        with
                          _ -> warn parameter error (Some "line 1530") Exit
                                 (Ckappa_sig.string_of_state_index p_state2')
                      in
                      Loggers.fprintf log
                      "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:%s:site_type:%i:%s:%s ->\
                       agent_id:%i:agent_type:%i:%s:site_type:%i:%s:%s:site_type:%i:%s:%s\n"
                      (Ckappa_sig.int_of_agent_id ag_id)
                      (Ckappa_sig.int_of_agent_name ag_type)
                      ag_string
                      (Ckappa_sig.int_of_site_name s_type)
                      s_type_string
                      p_state_string
                      (Ckappa_sig.int_of_site_name s_type2)
                      s_type2_string
                      p_state2_string
                      (**)
                      (Ckappa_sig.int_of_agent_id ag_id')
                      (Ckappa_sig.int_of_agent_name ag_type')
                      ag_string'
                      (Ckappa_sig.int_of_site_name s_type')
                      s_type'_string
                      p_state'_string
                      (Ckappa_sig.int_of_site_name s_type2')
                      s_type2'_string
                      p_state2'_string) potential_list
                      in error
                    end
                  else error
                in*)
                (*fold over a potential list and compare with parallel_list*)
                let error, value =
                  compute_value_fst_aux error site_type2 state2
                    site_type2' state2' rule_has_parallel_bonds_rhs_set old_value potential_list
                in
                let error, store_result =
                  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error
                    ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),                              (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                    value
                    store_result
                  in
                  error, dynamic, precondition, store_result
                else
                  error, dynamic, precondition, store_result
              )(error, dynamic, precondition, store_result) parallel_list
          in
          error, dynamic, precondition, store_result
        ) store_pair_bind_map (error, dynamic, precondition, store_result)
    in
    error, dynamic, precondition, store_result

(****************************************************************)

  let compute_value_snd_aux error site_type state site_type' state' rule_has_parallel_bonds_rhs_set old_value potential_list =
  List.fold_left (fun (error, value) (x, y) ->
      (*parallel_list*)
      let (_, _, s_type, _, p_state, _) = x in
      let (_, _, s_type', _, p_state', _) = y in
      (*first check if the parallel set is empty*)
      if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.is_empty rule_has_parallel_bonds_rhs_set
      then
        begin
          (*question 1*)
          if (Ckappa_sig.int_of_state_index p_state) = 0 || (Ckappa_sig.int_of_state_index p_state') = 0
          then
            (**)
            (*let error =
              if local_trace ||
                 Remanent_parameters.get_dump_reachability_analysis_parallel parameter
              then
                let _ = Loggers.fprintf log "Undefined_free\n"
                in
                error
              else error
            in*)
            let new_value = Usual_domains.lub value Usual_domains.Undefined
            in error, new_value
          else
            begin
              if s_type = s_type' && p_state = p_state'
              then
                (*let error =
                  if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
                  then
                    let _ = Loggers.fprintf log "Any\n" in
                    error
                  else error
                in*)
                let new_value = Usual_domains.lub value Usual_domains.Any in
                error, new_value
              else
                (*let error =
                  if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
                  then
                    let _ =
                      Loggers.fprintf log "False\n"
                    in
                    error
                  else
                    error
                in*)
                let new_value = Usual_domains.lub value (Usual_domains.Val false)
                in
                error, new_value
            end
        end
      else
      (*check inside the parallel bonds*)
      if s_type = site_type && p_state = state &&
         s_type' = site_type' && p_state' = state'
      then
        (*let error =
          if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
          then
            let _ =
              Loggers.fprintf log "True\n"
            in
            error
          else error
        in*)
        let new_value = Usual_domains.lub value (Usual_domains.Val true)
        in
        error, new_value
      else
        (*it does not belong to parallel list*)
        let new_value = Usual_domains.lub value Usual_domains.Undefined
        in
        error, new_value
    ) (error, old_value) potential_list

  let collect_result_from_snd_site_create_parallel parameter error dynamic precondition store_snd_pair_bind_map rule_has_parallel_bonds_rhs_set store_result =
    let error, dynamic, precondition, store_result =
      Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.fold
        (fun ((agent_id, agent_type, site_type, state),
              (agent_id', agent_type', site_type', state')) parallel_list
          (error, dynamic, precondition, store_result) ->
          (*fold over a list of parallel bonds*)
          let error, dynamic, precondition, store_result =
            List.fold_left
              (fun (error, dynamic, precondition, store_result)
                (*A.z.t.B.z.t, B.z.t.A.z.t*)
                ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                 (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) ->
                if agent_type1 = agent_type && site_type = site_type1
                then
                let error, old_value =
                match
                  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs parameter error
                    ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                     (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                  store_result
                with
                | error, None -> error, Usual_domains.Undefined
                | error, Some value -> error, value
                in
                (*get a list of a state of the first site site_type1 in the precondition of agent_id1*)
                let error, dynamic, precondition, state_list =
                  get_state_of_site_in_precondition parameter error dynamic agent_id1 site_type1 precondition
                in
                (*get a pre_state for B*)
                let error, dynamic, precondition, state_list' =
                  get_state_of_site_in_precondition parameter error dynamic agent_id1' site_type1' precondition
                in
                (*build a potential sites*)
                let error, potential_list =
                  List.fold_left (fun (error, store_list) pre_state ->
                      let error, potential_list =
                        List.fold_left (fun (error, current_list) pre_state' ->
                            let potential_list =
                              ((agent_id, agent_type, site_type1, site_type, pre_state, state),
                               (agent_id', agent_type', site_type1', site_type', pre_state', state')) :: current_list
                            in
                            error, potential_list
                          ) (error, store_list) state_list'
                      in
                      error, potential_list) (error, []) state_list
                  in
                  (*print the potential list*)
                  (* let error =
                    if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
                    then
                      begin
                        let () = Print_parallel_bonds.print_action_binding_test parameter error handler_kappa rule_id
                      ((agent_id, agent_type, site_type, state),
                       (agent_id', agent_type', site_type', state'));
                          Loggers.fprintf log "List of potential sites:\n";
                          List.iter (fun (
                        (ag_id, ag_type, s_type, s_type2, p_state, p_state2),
                        (ag_id', ag_type', s_type', s_type2', p_state', p_state2')) ->
                        let error, ag_string =
                          try
                            Handler.string_of_agent parameter error handler_kappa ag_type
                          with
                            _ -> warn parameter error (Some "line 1706") Exit
                                   (Ckappa_sig.string_of_agent_name ag_type)
                        in
                        let error, s_type_string =
                          try
                            Handler.string_of_site parameter error
                              handler_kappa ag_type s_type
                          with
                            _ -> warn parameter error (Some "line 1714") Exit
                                   (Ckappa_sig.string_of_site_name s_type)
                        in
                        let error, p_state_string =
                          try
                            Handler.string_of_state_fully_deciphered parameter error handler_kappa
                              ag_type s_type p_state
                          with
                            _ -> warn parameter error (Some "line 1530") Exit
                                   (Ckappa_sig.string_of_state_index p_state)
                        in
                        let error, s_type2_string =
                          try
                            Handler.string_of_site parameter error
                              handler_kappa ag_type s_type2
                          with
                            _ -> warn parameter error (Some "line 1714") Exit
                                   (Ckappa_sig.string_of_site_name s_type2)
                        in
                        let error, p_state2_string =
                          try
                            Handler.string_of_state_fully_deciphered parameter error handler_kappa
                              ag_type s_type2 p_state2
                          with
                            _ -> warn parameter error (Some "line 1530") Exit
                                   (Ckappa_sig.string_of_state_index p_state2)
                        in
                        (**)
                        let error, ag_string' =
                          try
                            Handler.string_of_agent parameter error handler_kappa ag_type'
                          with
                            _ -> warn parameter error (Some "line 1706") Exit
                                   (Ckappa_sig.string_of_agent_name ag_type')
                        in
                        let error, s_type'_string =
                          try
                            Handler.string_of_site parameter error
                              handler_kappa ag_type' s_type'
                          with
                            _ -> warn parameter error (Some "line 1714") Exit
                                   (Ckappa_sig.string_of_site_name s_type')
                        in
                        let error, p_state'_string =
                          try
                            Handler.string_of_state_fully_deciphered parameter error handler_kappa
                              ag_type' s_type' p_state'
                          with
                            _ -> warn parameter error (Some "line 1530") Exit
                                   (Ckappa_sig.string_of_state_index p_state')
                        in
                        let error, s_type2'_string =
                          try
                            Handler.string_of_site parameter error
                              handler_kappa ag_type' s_type2'
                          with
                            _ -> warn parameter error (Some "line 1714") Exit
                                   (Ckappa_sig.string_of_site_name s_type2')
                        in
                        let error, p_state2'_string =
                          try
                            Handler.string_of_state_fully_deciphered parameter error handler_kappa
                              ag_type' s_type2' p_state2'
                          with
                            _ -> warn parameter error (Some "line 1530") Exit
                                   (Ckappa_sig.string_of_state_index p_state2')
                        in
                        Loggers.fprintf log
                        "NEW:agent_id:%i:agent_type:%i:%s:site_type:%i:%s:%s:site_type:%i:%s:%s ->\
                         agent_id:%i:agent_type:%i:%s:site_type:%i:%s:%s:site_type:%i:%s:%s\n"
                        (Ckappa_sig.int_of_agent_id ag_id)
                        (Ckappa_sig.int_of_agent_name ag_type)
                        ag_string
                        (Ckappa_sig.int_of_site_name s_type)
                        s_type_string
                        p_state_string
                        (Ckappa_sig.int_of_site_name s_type2)
                        s_type2_string
                        p_state2_string
                        (**)
                        (Ckappa_sig.int_of_agent_id ag_id')
                        (Ckappa_sig.int_of_agent_name ag_type')
                        ag_string'
                        (Ckappa_sig.int_of_site_name s_type')
                        s_type'_string
                        p_state'_string
                        (Ckappa_sig.int_of_site_name s_type2')
                        s_type2'_string
                        p_state2'_string) potential_list
                        in error
                      end
                    else error
                  in*)
                  (*fold over the potential_list and compare with parallel_list*)
                  let error, value =
                    compute_value_snd_aux error site_type state site_type' state' rule_has_parallel_bonds_rhs_set old_value potential_list
                  in
                  let error, store_result =
                    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error
                      ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                       (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                      value
                      store_result
                    in
                  error, dynamic, precondition, store_result
              else
                error, dynamic, precondition, store_result
              ) (error, dynamic, precondition, store_result) parallel_list
          in
          error, dynamic, precondition, store_result
        )
        store_snd_pair_bind_map (error, dynamic, precondition, store_result)
    in
    error, dynamic, precondition, store_result

(****************************************************************)

let collect_result_of_non_parallel parameter error rule_id non_parallel_rhs_list store_result =
  List.fold_left (fun (error, store_result) (x, y, z, t) ->
  let (_, ag_type, s_type, state) = x in
  let (_, _, s_type', state') = y in (*A*)
  let (_, ag_type1, s_type1, state1) = z in (*B*)
  let (_, _, s_type1', state1') = t (*B'*) in
  let error, store_result =
    Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameter error
      ((ag_type, s_type, s_type', state, state'),
       (ag_type1, s_type1, s_type1', state1, state1'))
      (Usual_domains.Val false)
      store_result
  in
  error, store_result
  ) (error, store_result) non_parallel_rhs_list

(****************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    (*-----------------------------------------------------------*)
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
    (*-----------------------------------------------------------*)
    let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
    let error, store_pair_bind_map =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error
          rule_id store_fst_site_create_parallel_bonds_rhs
      with
      | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
      | error, Some m -> error, m
    in
    (*-----------------------------------------------------------*)
    (*compute value, check the second sites binding in the parallel bonds*)
    let error, dynamic, precondition, store_value1 =
    collect_result_from_site_create_parallel
      parameter
      error
      dynamic
      precondition
      store_pair_bind_map
      rule_has_parallel_bonds_rhs_set
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty
    in
    (*-----------------------------------------------------------*)
    let store_non_parallel_map = get_rule_has_non_parallel_bonds_rhs static
    in
    let error, non_parallel_rhs_list =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_non_parallel_map with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    (*-----------------------------------------------------------*)
    (*check the first binding site in the parallel bonds*)
    let store_snd_site_create_parallel_bonds_rhs =
      get_snd_site_create_parallel_bonds_rhs static in
    let error, store_snd_pair_bind_map =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_snd_site_create_parallel_bonds_rhs with
      | error, None -> error, Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.empty
      | error, Some  m -> error, m
    in
    let error, dynamic, precondition, store_value2 =
      collect_result_from_snd_site_create_parallel parameter error
        dynamic precondition store_snd_pair_bind_map rule_has_parallel_bonds_rhs_set
        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty
    in
    (*-----------------------------------------------------------*)
    (*fold2*)
    let add_link error x value store_result =
      let error, old_value =
        match Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs parameter error x store_result
        with
        | error, None -> error, Usual_domains.Undefined
        | error, Some v -> error, v
      in
      let new_value = Usual_domains.lub old_value value in
      let error, store_result =
        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error x new_value store_result
      in
      error, store_result
    in
    (*-----------------------------------------------------------*)
    let error, store_value =
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.fold2
        parameter error
        (fun _ error x value store_result ->
           let error, store_result =
             add_link error x value store_result
           in
           error, store_result
        )
        (fun _ error x value store_result ->
          let error, store_result =
            add_link error x value store_result
          in
          error, store_result
        )
        (fun _ error x value1 value2 store_result ->
           let new_value = Usual_domains.lub value1 value2 in
           let error, store_result =
             add_link error x new_value store_result
           in
           error, store_result
        )
        store_value1 store_value2 Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.empty
    in
    (*--------------------------------------------------------------*)
    let error, map_value =
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.fold (fun (x,y) value (error, store_result)->
          let (_, agent_type, site_type, site_type1, state, state1) = x in
          let (_, agent_type', site_type', site_type1', state', state1') = y in
          let error, store_result =
          Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameter error
            ((agent_type, site_type, site_type1, state, state1),
             (agent_type', site_type', site_type1', state', state1'))
            value
            store_result
          in
          error, store_result
        ) store_value (error, Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty)
    in
    let error, store_non_parallel =
      collect_result_of_non_parallel parameter error rule_id
        non_parallel_rhs_list Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.empty
    in
    (*--------------------------------------------------------------*)
    (*fold2*)
    let store_result = get_value_of_parallel_bonds dynamic in
    let add_map error x value store_result =
      let error, old_value =
        match Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.find_option_without_logs parameter error x store_result with
        | error, None -> error, Usual_domains.Undefined
        | error, Some v -> error, v
      in
      let new_value = Usual_domains.lub value old_value in
      let error, store_result =
        Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameter error x new_value store_result
      in
      error, store_result
    in
    let error, store_result =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold2
        parameter error
        (fun _ error x value store_result ->
           let error, store_result =
             add_map error x value store_result
           in
           error, store_result
        )
        (fun _ error x value store_result ->
           let error, store_result =
             add_map error x value store_result
           in
           error, store_result
        )
        (fun _ error x value1 value2 store_result ->
           let new_value = Usual_domains.lub value1 value2 in
           let error, store_result =
             add_map error x new_value store_result
           in
           error, store_result) map_value store_non_parallel store_result
    in
    let dynamic = set_value_of_parallel_bonds store_result dynamic in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  (****************************************************************)

  let print static dynamic error loggers =
    let handler_kappa = get_kappa_handler static in
    let parameter = get_parameter static in
    (*--------------------------------------------------------------*)
    (*let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in
      let _ =
      Print_parallel_bonds.print_parallel_bonds_rhs parameter handler_kappa
        store_parallel_bonds_rhs static dynamic error
      in
      (*----------------------------------------------------------*)
      let store_action_binding = get_action_binding static in
      let _ =
      Print_parallel_bonds.print_action_binding parameter handler_kappa store_action_binding
        static dynamic error
      in
      (*----------------------------------------------------------*)
      let store_fst_site_create_parallel_bonds_rhs = get_fst_site_create_parallel_bonds_rhs static in
      let _ =
      Print_parallel_bonds.print_fst_site_create_parallel_rhs parameter handler_kappa
        store_fst_site_create_parallel_bonds_rhs static dynamic error
      in
      (*----------------------------------------------------------*)
      let store_snd_site_create_parallel_bonds_rhs = get_snd_site_create_parallel_bonds_rhs static in
      let _ =
      Print_parallel_bonds.print_snd_site_create_parallel_rhs parameter handler_kappa
        store_snd_site_create_parallel_bonds_rhs static dynamic error
      in
      (*----------------------------------------------------------*)
      let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
      let _ =
      Print_parallel_bonds.print_rule_has_parallel_bonds_rhs parameter handler_kappa
        store_rule_has_parallel_bonds_rhs static dynamic error
      in*)
      (*---------------------------------------------------------*)
    let store_rule_has_non_parallel_bonds_rhs = get_rule_has_non_parallel_bonds_rhs static in
    let error =
      if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
      then
        let () =
          Print_parallel_bonds.print_rule_has_non_parallel_bonds_rhs parameter store_rule_has_non_parallel_bonds_rhs
        in
        error
      else error
    in
      (*-----------------------------------------------------------*)
      (*print value of initial state*)
      (*let store_value_of_init = get_value_of_init dynamic in
      let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "Value in the initial state:\n";
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.iter
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
      in*)
      (*--------------------------------------------------------------*)
      (*print value of parallel in the rhs*)
      let store_value_of_parallel_bonds = get_value_of_parallel_bonds dynamic in
      let error =
        if local_trace || Remanent_parameters.get_dump_reachability_analysis_parallel parameter
        then
          begin
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameter) "The value of a list of parallel bonds:\n";
              Print_parallel_bonds.print_result handler_kappa parameter error store_value_of_parallel_bonds(*;                                                                                                     Print_parallel_bonds.print_result handler_kappa parameter error (snd store_value_of_parallel_bonds)*)
            in error
          end
        else
          error
      in
      error, dynamic, ()

  (****************************************************************)

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
