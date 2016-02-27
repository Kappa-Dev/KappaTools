(**
   * contact_map_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 22th of February
   * Last modification:
   *
   * Abstract domain to record live rules
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

  module Set_triple =
    Map_wrapper.Make
      (SetMap.Make (
        struct
          type t = int * int * int
          let compare = compare
        end))


  type local_static_information =
    {
      bond_rhs : Set_triple.Set.t * Set_triple.Set.t ; (*TODO: a triple_set of map*)
      bond_lhs : Set_triple.Set.t * Set_triple.Set.t
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  type local_dynamic_information = Set_triple.Set.t * Set_triple.Set.t

  type dynamic_information =
    {
      local  : local_dynamic_information;
      global : Analyzer_headers.global_dynamic_information
    }

  (**************************************************************************)
  (*local static information*)

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

  let get_bond_rhs static =
    (get_local_static_information static).bond_rhs

  let set_bond_rhs bond static =
    set_local_static_information
      {
        (get_local_static_information static) with
          bond_rhs = bond
      } static

  let get_bond_lhs static =
    (get_local_static_information static).bond_lhs

  let set_bond_lhs bond static =
    set_local_static_information
      {
        (get_local_static_information static) with
          bond_lhs = bond
      } static

  (*--------------------------------------------------------------------*)
  (** dynamic information*)

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  (**************************************************************************)
  (*implementations*)

  let collect_set parameter error agent site_type store_result =
    match agent with
    | Cckappa_sig.Ghost
    | Cckappa_sig.Unknown_agent _
    | Cckappa_sig.Dead_agent _ ->
      warn parameter error (Some "line 199") Exit Set_triple.Set.empty
    | Cckappa_sig.Agent agent1 ->
      let agent_type1 = agent1.Cckappa_sig.agent_name in
      let error, state1 =
        match Cckappa_sig.Site_map_and_set.Map.find_option_without_logs
          parameter error site_type
          agent1.Cckappa_sig.agent_interface
        with
        | error, None ->  warn parameter error (Some "line 120") Exit 0
        | error, Some port ->
          let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
          if state > 0
          then error, state
          else warn parameter error (Some "line 125") Exit 0
      in
      let error', set1 =
        Set_triple.Set.add_when_not_in
          parameter error (agent_type1, site_type, state1) store_result
      in
      let error =
        Exception.check warn parameter error error' (Some "line 132") Exit in
      error, set1

  let collect_bonds parameter error rule views bonds store_result =
    let error, store_result =
      Int_storage.Quick_Nearly_inf_Imperatif.fold parameter error
        (fun parameter error agent_id bonds_map (store_result1, store_result2) ->
          let error, (store_result1, store_result2) =
            Cckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, (store_result1, store_result2)) ->
                let agent_index_target = site_add.Cckappa_sig.agent_index in
                let site_type_target = site_add.Cckappa_sig.site in
                let error, agent_source =
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_id views
                  with
                  | error, None -> warn parameter error (Some "line 148") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, agent_target =
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_index_target views
                  with
                  | error, None -> warn parameter error (Some "line 156") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, set1 =
                  collect_set
                    parameter
                    error
                    agent_source
                    site_type_source
                    store_result1
                in
                let error, set2 =
                  collect_set
                    parameter
                    error
                    agent_target
                    site_type_target
                    store_result2
                in
                error, (set1, set2)
              ) bonds_map (error, (store_result1, store_result2))
          in
          error, (store_result1, store_result2)
        ) bonds store_result
    in
    error, store_result

  (*collect bonds rhs*)
  let collect_bonds_rhs parameter error rule store_result =
    let error, store_result =
      collect_bonds
        parameter
        error
        rule
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds
        store_result
    in
    error, store_result

  (*collect bonds lhs*)

  let collect_bonds_lhs parameter error rule store_result =
    let error, store_result =
      collect_bonds
        parameter
        error
        rule
        rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
        rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
        store_result
    in
    error, store_result

  (**************************************************************************)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Int_storage.Nearly_inf_Imperatif.fold
        parameter
        error
        (fun parameter error rule_id rule static ->
          let store_rhs = get_bond_rhs static in
          (*bond rhs*)
          let error, store_rhs =
            collect_bonds_rhs
              parameter
              error
              rule.Cckappa_sig.e_rule_c_rule
              store_rhs
          in
          let static = set_bond_rhs store_rhs static in
          (*bond lhs*)
          let store_lhs = get_bond_lhs static in
          let error, store_lhs =
            collect_bonds_lhs
              parameter
              error
              rule.Cckappa_sig.e_rule_c_rule
              store_lhs
          in
          let static = set_bond_lhs store_lhs static in
          error, static
        ) compil.Cckappa_sig.rules static
    in
    error, static, dynamic

  (**************************************************************************)

  let initialize static dynamic error =
    let init_domain_static =
      {
        bond_rhs = (Set_triple.Set.empty, Set_triple.Set.empty);
        bond_lhs = (Set_triple.Set.empty, Set_triple.Set.empty);
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_domain_static
      }
    in
    let init_global_dynamic_information =
      {
        local  = (Set_triple.Set.empty, Set_triple.Set.empty);
        global = dynamic
      }
    in
    let error, static, dynamic =
      scan_rule_set init_global_static_information init_global_dynamic_information error
    in
    error, static, dynamic

  (**************************************************************************)

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
  (*Implementation*)

  (**bond occurs in the initial state*)

  let collect_bonds_initial static dynamic error init_state =
    let parameter = get_parameter static in
    let store_result = get_local_dynamic_information dynamic in
    let error, store_result =
      collect_bonds
        parameter
        error
        init_state
        init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
        init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
        store_result
    in
    let dynamic = set_local_dynamic_information store_result dynamic in
    error, dynamic

  (**************************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    let error, dynamic =
      collect_bonds_initial static dynamic error species
    in
    error, dynamic, event_list

  (**************************************************************************)

  let is_enabled static dynamic error rule_id precondition =
    (*test if the bond in the lhs has already in the contact map, if not
      None, *)
    (*let (contact_map1, contact_map2) = get_local_dynamic_information dynamic in
    let (bonds_lhs_set1, bonds_lhs_set2) = get_bond_lhs static in
    let b1 =
      Set_triple.Set.for_all (fun (agent_type, site_type, state) ->
        if Set_triple.Set.mem (agent_type, site_type, state) contact_map1
        then true
        else false
      ) bonds_lhs_set1
    in
    let b2 =
      Set_triple.Set.for_all (fun (agent_type', site_type', state') ->
        if Set_triple.Set.mem (agent_type', site_type', state') contact_map2
        then true
        else false
      ) bonds_lhs_set2
    in
    if b1 && b2
    then
      error, dynamic, Some precondition
    else*)
      error, dynamic, Some precondition

  (**************************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    (*let (contact_map1, contact_map2) = get_local_dynamic_information dynamic in
    let (bonds_rhs_set1, bonds_rhs_set2) = get_bond_rhs static in
    let parameter = get_parameter static in
    (*add the bonds in the rhs into the contact map*)
    let error', union1 = Set_triple.Set.union parameter error contact_map1 bonds_rhs_set1 in
    let error = Exception.check warn parameter error error' (Some "line 371") Exit in
    let error'', union2 = Set_triple.Set.union parameter error contact_map2 bonds_rhs_set2 in
    let error = Exception.check warn parameter error error'' (Some "line 373") Exit in
    let dynamic = set_local_dynamic_information (union1, union2) dynamic in
    let (contact_map_update1, contact_map_update2) = get_local_dynamic_information dynamic in
    (*add the bond of the rhs in the contact map (in dynamic), if it is not
      see for the first time, update the contact map and raise an event
      (event_list) *)
    (*set dynamic contact map set_diff,
      add event_list for set_diff.
    *)
    let b = is_the_bond_seen_for_the_first_time precondition in
    if not b
    then
      let event_list =
        Set_triple.Set.fold (fun (agent_type, site_type, state) event_list ->
          Set_triple.Set.fold (fun (agent_type', site_type', state') event_list ->
            (Analyzer_headers.See_a_new_bond
               ((agent_type, site_type, state),
                (agent_type', site_type', state'))
            ) :: event_list
          ) contact_map_update2 event_list
        ) contact_map_update1 event_list
      in
      error, dynamic, (precondition, event_list)
    else*)
      error, dynamic, (precondition, event_list)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  let print static dynamic error loggers =
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
