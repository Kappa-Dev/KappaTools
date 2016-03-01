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
  Exception.warn parameters mh (Some "Contact map domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  type rule_id = int
  type agent_type = int
  type site_type = int
  type state_index = int

  type pair_triple =
    (agent_type * site_type * state_index)*(agent_type * site_type * state_index)

  module Set_pair_triple =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = pair_triple
           let compare = compare
          end))

  module Map_pair_triple =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = rule_id
           let compare = compare
          end))
      
  type local_static_information =
    {
      bond_rhs : Set_pair_triple.Set.t Map_pair_triple.Map.t;
      bond_lhs : Set_pair_triple.Set.t Map_pair_triple.Map.t
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information  : local_static_information
    }

  module Sites_map =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = (agent_type * site_type)
           let compare = compare
          end))
      
  module State_map =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = state_index
           let compare = compare
          end))
      
      
  type local_dynamic_information = 
    {
      contact_map_dynamic : Set_pair_triple.Set.t;
      contact_map_communicate :
        (agent_type * site_type * state_index) State_map.Map.t Sites_map.Map.t
    }

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

  let get_bond_rhs static = (get_local_static_information static).bond_rhs

  let set_bond_rhs bond static =
    set_local_static_information
      {
        (get_local_static_information static) with
          bond_rhs = bond
      } static

  let get_bond_lhs static = (get_local_static_information static).bond_lhs

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

  let get_contact_map_dynamic dynamic =
    (get_local_dynamic_information dynamic).contact_map_dynamic

  let set_contact_map_dynamic contact_map dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          contact_map_dynamic = contact_map
      } dynamic
  
  let get_contact_map_communicate dynamic =
    (get_local_dynamic_information dynamic).contact_map_communicate

  let set_contact_map_communicate contact_map dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          contact_map_communicate = contact_map
      } dynamic
      
  (**************************************************************************)
  (*implementations*)

  (*dual: contact map including initial state, use in views_domain*)
  
  module Map_dual =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = agent_type * site_type * state_index
           let compare = compare
          end))

  module Set_triple =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = agent_type * site_type * state_index
           let compare = compare
          end))

  let collect_dual_map parameter error handler store_result =
    let error, store_result =
      Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
        parameter error
        (fun parameter error (agent_type, (site_type, state))
          (agent_type', site_type', state') store_result ->
            let error, old_set =
              match Map_dual.Map.find_option_without_logs parameter error
                (agent_type, site_type, state) store_result
              with
              | error, None -> error, Set_triple.Set.empty
              | error, Some s -> error, s
            in
            let error', set =
              Set_triple.Set.add_when_not_in parameter error
                (agent_type', site_type', state') Set_triple.Set.empty
            in
            let error = Exception.check warn parameter error error'
              (Some "line 446") Exit 
            in
            let error'', new_set = 
              Set_triple.Set.union parameter error set old_set
            in
            let error = Exception.check warn parameter error error''
              (Some "line 446") Exit 
            in
            let error, store_result = 
              Map_dual.Map.add_or_overwrite parameter error
                (agent_type, site_type, state) new_set store_result
            in
            error, store_result
        ) handler.Cckappa_sig.dual store_result
    in
    error, store_result

  let collect_agent_type_state parameter error agent site_type =
    match agent with
    | Cckappa_sig.Ghost
    | Cckappa_sig.Unknown_agent _
    | Cckappa_sig.Dead_agent _ ->
      warn parameter error (Some "line 199") Exit (0, 0)
    | Cckappa_sig.Agent agent1 ->
      let agent_type1 = agent1.Cckappa_sig.agent_name in
      let error, state1 =
        match Cckappa_sig.Site_map_and_set.Map.find_option_without_logs
          parameter error site_type
          agent1.Cckappa_sig.agent_interface
        with
        | error, None ->  warn parameter error (Some "line 228") Exit 0
        | error, Some port ->
          let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
          if state > 0
          then error, state
          else warn parameter error (Some "line 234") Exit 0
      in
      error, (agent_type1, state1) 

  let add_link_set parameter error rule_id ((a, b, c), (d, e, f)) store_result =
    let error, old_set =
      match Map_pair_triple.Map.find_option_without_logs parameter error 
        rule_id store_result
      with
      | error, None -> error, Set_pair_triple.Set.empty
      | error, Some p -> error, p
    in
    let error', set = 
      Set_pair_triple.Set.add_when_not_in parameter error ((a, b, c), (d, e, f))
        Set_pair_triple.Set.empty
    in    
    let error = Exception.check warn parameter error error' (Some "line 202") Exit in
    let error'', union_set = Set_pair_triple.Set.union parameter error set old_set in
    let error = Exception.check warn parameter error error'' (Some "line 204") Exit in
    let error, store_result =
      Map_pair_triple.Map.add_or_overwrite parameter error rule_id union_set store_result
    in
    error, store_result

   let collect_bonds parameter error (rule_id:int) bonds views store_result =
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
                  | error, None -> warn parameter error (Some "line 271") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, agent_target =
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_index_target views
                  with
                  | error, None -> warn parameter error (Some "line 279") Exit
                    Cckappa_sig.Ghost
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
                let error, store_result =
                  add_link_set parameter error rule_id pair store_result 
                in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result
        ) bonds store_result
    in
    error, store_result

  (*collect bonds lhs*)
   let collect_bonds_rhs parameter error rule_id rule store_result =
    let views = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
    let bonds = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
    let error, store_result =
      collect_bonds parameter error rule_id bonds views store_result
    in
    error, store_result

   let collect_bonds_lhs parameter error rule_id rule store_result =
    let views = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
    let bonds = rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds in
    let error, store_result =
      collect_bonds parameter error rule_id bonds views store_result
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
              rule_id
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
              rule_id
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
        bond_rhs = Map_pair_triple.Map.empty;
        bond_lhs = Map_pair_triple.Map.empty;
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_domain_static
      }
    in
    let init_local =
      {
        contact_map_dynamic = Set_pair_triple.Set.empty;
        contact_map_communicate = Sites_map.Map.empty
      }
    in
    let init_global_dynamic_information =
      {
        local  = init_local;
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
     
  let add_bond static dynamic error 
      ((agent_type, site_type, state), (agent_type', site_type', state')) =
    let parameter = get_parameter static in
    let pair_triple =
      ((agent_type, site_type, state), (agent_type', site_type', state'))
    in
    (*current value of the variable dynamic*)
    let contact_map_dynamic = get_contact_map_dynamic dynamic in
    let contact_map_communicate = get_contact_map_communicate dynamic in
    (*add a bond (a pair of triples) of does what is neccessary *)
    let error, contact_map_dynamic =
      Set_pair_triple.Set.add_when_not_in parameter error pair_triple
        contact_map_dynamic
    in
    let dynamic = set_contact_map_dynamic contact_map_dynamic dynamic in
    (*contact map communicate*)
    let error, state_map =
      State_map.Map.add_or_overwrite parameter error
        state
        (agent_type', site_type', state')
        State_map.Map.empty (*FIXME: empty*)
    in
    let error, contact_map_communicate =
      Sites_map.Map.add_or_overwrite parameter error
        (agent_type, site_type)
        state_map
        contact_map_communicate
    in
    (*add apply_rule and add_initial apply this function over each new bond*)
    let dynamic = set_contact_map_communicate contact_map_communicate dynamic in
    error, dynamic

  (**bond occurs in the initial state*)

  let collect_bonds_initial static dynamic error init_state =
    let parameter = get_parameter static in
    let views = init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views in
    let bonds = init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds in
    let error, dynamic =
      Int_storage.Quick_Nearly_inf_Imperatif.fold parameter error
        (fun parameter error agent_id bonds_map dynamic ->
          let error, store_result =
            Cckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, dynamic) ->
                let agent_index_target = site_add.Cckappa_sig.agent_index in
                let site_type_target = site_add.Cckappa_sig.site in
                let error, agent_source =
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_id views
                  with
                  | error, None -> warn parameter error (Some "line 473") Exit
                    Cckappa_sig.Ghost
                  | error, Some agent -> error, agent
                in
                let error, agent_target =
                  match Int_storage.Quick_Nearly_inf_Imperatif.get
                    parameter error agent_index_target views
                  with
                  | error, None -> warn parameter error (Some "line 480") Exit
                    Cckappa_sig.Ghost
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
                let pair_triple = ((agent_type1, site_type_source, state1),
                            (agent_type2, site_type_target, state2)) 
                in
                let error, dynamic = add_bond static dynamic error pair_triple in
                error, dynamic
              ) bonds_map (error, dynamic)
          in
          error, dynamic
        ) bonds dynamic
    in
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
    let parameter = get_parameter static in
    let bond_lhs = get_bond_lhs static in
    let contact_map = get_contact_map_dynamic dynamic in
    let error, bond_lhs_set =
      match Map_pair_triple.Map.find_option_without_logs parameter error
        rule_id bond_lhs
      with
      | error, None -> error, Set_pair_triple.Set.empty
      | error, Some l -> error, l
    in
    let error, inter =
      Set_pair_triple.Set.inter parameter error contact_map bond_lhs_set
    in
    if Set_pair_triple.Set.is_empty inter
    then error, dynamic, Some precondition
    else error, dynamic, None
    
  (**************************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    (*add the bonds in the rhs into the contact map, by doing union set
      with contact map*)
    let contact_map = get_contact_map_dynamic dynamic in
    let bond_rhs_map = get_bond_rhs static in
    let error, bond_rhs_set =
      match Map_pair_triple.Map.find_option_without_logs parameter error rule_id
        bond_rhs_map
      with
      | error, None -> error, Set_pair_triple.Set.empty
      | error, Some s -> error, s
    in
    let error', union =
      Set_pair_triple.Set.union parameter error contact_map bond_rhs_set
    in
    let error = Exception.check warn parameter error error' (Some "line 561") Exit in
    let dynamic = set_contact_map_dynamic union dynamic in
    let new_contact_map = get_contact_map_dynamic dynamic in
    (*check if it is seen for the first time, if not update the contact
      map, and raise an event*)
    let error', map_diff =
      Set_pair_triple.Set.diff parameter error new_contact_map contact_map
    in
    let error = Exception.check warn parameter error error' (Some "line 569") Exit in
    let dynamic = set_contact_map_dynamic new_contact_map dynamic in
    let event_list =
      Set_pair_triple.Set.fold (fun pair event_list ->
        (Communication.See_a_new_bond pair) :: event_list
      ) map_diff event_list
    in
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
