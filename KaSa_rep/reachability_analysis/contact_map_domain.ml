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

  module Int2Map_CM =
    Map_wrapper.Make 
      (SetMap.Make
         (struct 
           type t = Set_triple.Set.t
           let compare = compare
          end))

  type local_static_information =
    {
      bond_rhs : Set_triple.Set.t Int2Map_CM.Map.t;
      bond_lhs : Set_triple.Set.t Int2Map_CM.Map.t
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  type local_dynamic_information = Set_triple.Set.t Int2Map_CM.Map.t
    
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
      
  let collect_set parameter error agent site_type =
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
        | error, None ->
          warn parameter error (Some "line 138") Exit 0
        | error, Some port ->
          let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
          if state > 0
          then error, state
          else warn parameter error (Some "line 142") Exit 0
      in
      let error, set1 = 
        Set_triple.Set.add_when_not_in parameter error 
          (agent_type1, site_type, state1) Set_triple.Set.empty
      in
      error, set1

  let add_link_bonds parameter error set1 set2 store_result =
    let error, old_set =
      match Int2Map_CM.Map.find_option_without_logs parameter error set1 store_result
      with
      | error, None -> error, Set_triple.Set.empty
      | error, Some s -> error, s
    in
    let error, union = Set_triple.Set.union parameter error set2 old_set in
    let error, store_result =
      Int2Map_CM.Map.add_or_overwrite parameter error set1 union store_result
    in
    error, store_result

  let collect_bonds parameter error rule views bonds store_result =
    let error, store_result =
      Int_storage.Quick_Nearly_inf_Imperatif.fold parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Cckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let agent_index_target = site_add.Cckappa_sig.agent_index in
                let site_type_target = site_add.Cckappa_sig.site in
                let error, agent_source =
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_id views
                  with
                  | error, None -> warn parameter error (Some "line 304") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, agent_target = 
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_index_target views
                  with
                  | error, None -> warn parameter error (Some "line 313") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, set1 =
                  collect_set
                    parameter
                    error
                    agent_source
                    site_type_source
                in
                let error, set2 =
                  collect_set
                    parameter
                    error
                    agent_target
                    site_type_target
                in
                (*let _ =
                  Set_triple.Set.iter (fun (agent_type, site_type, state) ->
                    Set_triple.Set.iter (fun (agent_type', site_type', state') ->
                      Printf.fprintf stdout "agent_type:%i:site_type:%i:state:%i -> agent_type':%i:site_type':%i:state':%i\n"
                        agent_type site_type state
                        agent_type' site_type' state'
                    ) set2
                  ) set1
                in*)
                let error, store_result =
                  add_link_bonds
                    parameter
                    error
                    set1
                    set2
                    store_result
                in
                error, store_result
              ) bonds_map (error, store_result)           
          in
          error, store_result
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
        bond_rhs = Int2Map_CM.Map.empty;
        bond_lhs = Int2Map_CM.Map.empty;
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
        local  = Int2Map_CM.Map.empty;
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

  let is_enabled static dynamic error rule_id precondition =
    error, dynamic, Some precondition

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    error, dynamic, (precondition, event_list)
      
  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state
      
  let print static dynamic error loggers =
    error, dynamic, ()

end
