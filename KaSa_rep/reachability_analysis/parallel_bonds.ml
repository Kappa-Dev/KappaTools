(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to detect whether when two sites of an agent are bound,
   * they must be bound to the same agent.
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
  (** [get_scan_rule_set static] *)

  (**************************************************************************)
  (**rules*)
  (**************************************************************************)

  let scan_rule_set_bonds_rhs static dynamic error rule_id rule =
    let parameter = get_parameter static in
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

  (* todo *)
  let initialize static dynamic error =
    let init_local_static =
      {
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
      scan_rule_set init_global_static_information
        init_global_dynamic_information error
    in
    error, static, dynamic

  (**************************************************************************)
  (*Initial state*)
  (**************************************************************************)

  (**************************************************************************)
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
    (*------------------------------------------------------------------------*)
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
    (*------------------------------------------------------------------------*)
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

  (**************************************************************************)
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
  (*return the value true if it belongs to the set of parallel bonds*)

  let collect_value_parallel_bonds_rhs static dynamic error =
    let parameter = get_parameter static in
    let store_rule_has_parallel_bonds_rhs = get_rule_has_parallel_bonds_rhs static in
    let parallel_list =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun set ->
          let error, new_list =
            Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.fold
              (fun ((agent_id, agent_type, site_type, site_type1, state, state1),
                    (agent_id', agent_type', site_type', site_type1', state', state1')) (error, current_list) ->
                error,
                ((agent_type, site_type, site_type1, state, state1),
                 (agent_type', site_type', site_type1', state', state1')) :: current_list
              ) set (error, [])
          in
          new_list
        ) store_rule_has_parallel_bonds_rhs
    in
    let store_result = get_value_parallel_bonds_rhs dynamic in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun rule_id list (error, store_result) ->
          let error, store_result =
            List.fold_left (fun (error, store_result) x ->
              Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite
                parameter error x
                (Usual_domains.Val true)
                store_result
            ) (error, store_result) list
          in
          error, store_result
        ) parallel_list (error, store_result)
    in
    let dynamic = set_value_parallel_bonds_rhs store_result dynamic in
    error, dynamic

  (**************************************************************************)
  (*return the value false if it belongs to the set of non parallel bonds*)

  let collect_value_non_parallel_bonds_rhs static dynamic error =
    let parameter = get_parameter static in
    let store_rule_has_non_parallel_bonds_rhs = get_rule_has_non_parallel_bonds_rhs static in
    let non_parallel_bonds_list =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun list ->
          let error, new_list =
            List.fold_left (fun (error, current_list)
            ((agent_id, agent_type, site_type, state),
             (agent_id', agent_type', site_type', state'),
             (agent_id1, agent_type1, site_type1, state1),
             (agent_id1', agent_type1', site_type1', state1')) ->
              error,
              ((agent_type, site_type, site_type1, state, state'),
               (agent_type1, site_type1, site_type1', state1, state1')) :: current_list
            ) (error, []) list
          in
          new_list
        ) store_rule_has_non_parallel_bonds_rhs
    in
    let store_result = get_value_non_parallel_bonds_rhs dynamic in
    let error, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun rule_id list (error, store_result) ->
          let error, store_result =
            List.fold_left (fun (error, store_result) x ->
              Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite
                parameter error x
                (Usual_domains.Val false)
                store_result
            ) (error, store_result) list
          in
          error, store_result
        ) non_parallel_bonds_list (error, store_result)
    in
    let dynamic = set_value_non_parallel_bonds_rhs store_result dynamic in
    error, dynamic

  (**************************************************************************)
  (*value in the rhs, if it belongs to parallel bonds -> true, belongs to
    non parallel bonds -> false, both exists -> any, if not Undefined.*)

  let collect_value_bonds_rhs static dynamic error =
    let parameter = get_parameter static in
    let value_parallel_bonds_rhs = get_value_parallel_bonds_rhs dynamic in
    let value_non_parallel_bonds_rhs = get_value_non_parallel_bonds_rhs dynamic in
    (*do the lub in the old result*)
    let add_link error x value store_result =
      let error, old_value =
        match Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.find_option_without_logs
          parameter error x store_result with
          | error, None -> error, Usual_domains.Undefined
          | error, Some v -> error, v
      in
      let new_value = Usual_domains.lub value old_value in
      let error, store_result =
        Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameter error
          x new_value store_result
      in
      error, store_result
    in
    let store_result = get_value_bonds_rhs dynamic in
    let error, store_result =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.fold2 parameter error
        (fun parameter error x p_value store_result ->
           (*print for test*)
           (*let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Value of parallel bonds in the rhs:\n";
            Print_parallel_bonds.print_value parameter p_value
          in*)
          add_link error x p_value store_result
        )
        (fun parameter error x nonp_value store_result ->
           (*print for test*)
           (*let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Value of non parallel bonds in the rhs:\n";
            Print_parallel_bonds.print_value parameter nonp_value
          in*)
          add_link error x  nonp_value store_result
        )
        (fun parameter error x p_value nonp_value store_result ->
          let new_value = Usual_domains.lub p_value nonp_value in
          (*print for test*)
          (*let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Value bonds in the rhs:\n";
            Print_parallel_bonds.print_value parameter new_value
          in*)
          add_link error x new_value store_result
        ) value_parallel_bonds_rhs value_non_parallel_bonds_rhs store_result
    in
    let dynamic = set_value_bonds_rhs store_result dynamic in
    error, dynamic

  (**************************************************************************)

  let compute_result parameter error handler_kappa state_list (x, y)
      rule_has_parallel_bonds_rhs_set value_bonds_rhs store_result =
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
    (*print for test*)
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
    (*fold over a list of pre_state*)
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
        (*print for test*)
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
        (*print for test*)
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
        (*TODO: check with parallel bonds and non parallel bonds*)
        (*--------------------------------------------------------------------*)
        (*check if it can be bound in the lhs with binding type B@z (check
          their state is enough). TODO: remains bound, it does not change the state*)
        if pre_state = state2'
        then
          (*--------------------------------------------------------------------*)
          (*check if two Bs are necessarily the same, it belongs to parallel bonds*)
          (*let error, value =
            match Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.find_option_without_logs
              parameter error
              ((agent_type1, site_type1, site_type2, state1, pre_state),
               (agent_type1', site_type1', site_type2', state1', state2'))
              value_bonds_rhs with
              | error, None -> error, Usual_domains.Undefined
              | error, Some v -> error, v
          in
          let new_value = Usual_domains.lub old_value value in
          let error, store_result =
            Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
              parameter
              error
              ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state),
               (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
              new_value
              store_result
          in
          error, store_result*)
          if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.mem
            ((agent_id1, agent_type1, site_type1, site_type2, state1, pre_state), (*A.x.y: pre_state = y*)
             (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) (*B.z.t*)
            rule_has_parallel_bonds_rhs_set
          then
            (*it belongs to a parallel bonds in the rhs*)
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
            (*print for test*)
            (*let _ =
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "\nBelong to a parallel bonds\n";
              Print_parallel_bonds.print_result handler_kappa parameter error store_result
            in*)
            error, store_result
          else
            (*TODO: if two Bs are necessarily different -> No, belong to non parallel bonds*)
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
            (*print for test*)
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
          (*print for test*)
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
      precondition store_pair_bind_map rule_has_parallel_bonds_rhs_set value_bonds_rhs store_result =
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
                (*print for test*)
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
                    value_bonds_rhs
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
                (*print for test*)
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
                    value_bonds_rhs
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
(* TO DO *)

  let collect_value parameter error dynamic hanlder_kappa rule_id precondition
      store_bonds_rhs_full store_pair_bind_map store_result =
    let error, store_result =
      (*fold over a binding in the rhs*)
      let error, store_result =
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.fold
          (fun ((agent_id, agent_type, site_type, state), (*A.x*)
                (agent_id', agent_type', site_type', state')(*B.z*)) (error, store_result) ->
            (*fold over binding action*)
            let error, store_result =
              Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.fold
                (fun ((agent_id1, agent_type1, site_type1, state1), (*A.y*)
                      (agent_id1', agent_type1', site_type1', state1') (*B.t*))
                  list_parallel (error, store_result) ->
                  (*check if there is a second site of this agent A*)
                  if site_type <> site_type1 (*x <> y of agent A*)
                  then
                    (*get the previous state of the second site*)
                    let error, global_dynamic, precondition, state_list =
                      get_state_of_site_in_precondition parameter error dynamic
                        agent_id site_type1 precondition
                    in
                    (*fold over a list of pre_state of second site*)
                    let error, store_result =
                      List.fold_left (fun (error, store_result) pre_state ->
                          (*--------------------------------------------------------------------*)
                          let error, store_result =
                            (*fold over a list of parallel bonds*)
                            List.fold_left (fun (error, store_result) (x, y) ->
                                let (a_id, a_type, s_type, s_type1, state_index, state_index1) = x in
                                let (a_id', a_type', s_type', s_type1', state_index', state_index1') = y in
                                (*get the old value*)
                                let error, old_value =
                                  match
                                    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.find_option_without_logs
                                      parameter
                                      error
                                      ((a_id, a_type, s_type, s_type1, state_index, pre_state),
                                       (a_id', a_type', s_type', s_type1', state_index', state_index1'))
                                      store_result
                                  with
                                  | error, None -> error, Usual_domains.Undefined
                                  | error, Some v -> error, v
                                in
                                (*check if it can be bound to the lhs with binding type B@z, second site of B. TODO: remains bound, it does not change its state*)
                                if pre_state = state_index1' (*second state of B*)
                                then
                                  (*if two Bs are nessecarily the same, it belongs to parallel bonds, then return true*)
                                  if agent_id' = agent_id1' (*B has the same identity*)
                                  then
                                    (*return true*)
                                    let new_value = Usual_domains.lub old_value (Usual_domains.Val false)
                                    in
                                    let error, store_result =
                                    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                                      parameter error
                                      ((a_id, a_type, s_type, s_type1, state_index, pre_state),
                                       (a_id', a_type', s_type', s_type1', state_index', state_index1'))
                                      new_value
                                      store_result
                                    in
                                    error, store_result
                                  else
                                    (*B has different identity*)
                                    begin
                                      if agent_type' = agent_type1' (*but their type is B*)                                  then
                                        (*return false*)
                                        let new_value = Usual_domains.lub old_value (Usual_domains.Val true)
                                        in
                                        let error, store_result =
                                        Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                                          parameter error
                                          ((a_id, a_type, s_type, s_type1, state_index, pre_state),
                                           (a_id', a_type', s_type', s_type1', state_index', state_index1'))
                                          new_value
                                          store_result
                                        in
                                        error, store_result
                                      else
                                        error, store_result
                                    end
                                else
                                  (*it can not be bound in the lhs; return Undefined*)
                                  let error, store_result =
                                    Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite
                                      parameter error
                                      ((a_id, a_type, s_type, s_type1, state_index, pre_state),
                                       (a_id', a_type', s_type', s_type1', state_index', state_index1'))
                                      Usual_domains.Undefined
                                      store_result
                                  in
                                  error, store_result
                              ) (error, store_result) list_parallel
                          in
                          error, store_result
                        ) (error, store_result) state_list
                    in
                    error, store_result
                  else
                    (*there is no second site*)
                    error, store_result
                ) store_pair_bind_map (error, store_result)
            in
            error, store_result
          ) store_bonds_rhs_full (error, store_result)
      in
      error, store_result
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
    (*get value from the rhs*)
    let error, dynamic = collect_value_parallel_bonds_rhs static dynamic error in
    let error, dynamic = collect_value_non_parallel_bonds_rhs static dynamic error in
    let error, dynamic = collect_value_bonds_rhs static dynamic error in
    let store_value_bonds_rhs = get_value_bonds_rhs dynamic in
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
        store_value_bonds_rhs
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
    let store_rule_has_non_parallel_bonds_rhs = get_rule_has_non_parallel_bonds_rhs static in
    let _ =
      Print_parallel_bonds.print_rule_has_non_parallel_bonds_rhs parameter handler_kappa
        store_rule_has_non_parallel_bonds_rhs static dynamic error
    in
    (*--------------------------------------------------------------*)
    (*print value of initial state*)
    let store_value_of_init = get_value_of_init dynamic in
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
