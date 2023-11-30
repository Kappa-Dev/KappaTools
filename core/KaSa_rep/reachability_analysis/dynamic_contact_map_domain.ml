(**
   * contact_map_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2016, the 22th of February
   * Last modification: Time-stamp: <Dec 04 2018>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

module Domain = struct
  type static_information = {
    global_static_information: Analyzer_headers.global_static_information;
    bonds_to_rules:
      Ckappa_sig.Rule_map_and_set.Set.t
      Ckappa_sig.PairAgentSiteState_map_and_set.Map.t;
  }

  type local_dynamic_information = {
    contact_map_dynamic: Ckappa_sig.PairAgentSiteState_map_and_set.Set.t;
    bonds_per_site:
      (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
      Ckappa_sig.State_map_and_set.Map.t
      Ckappa_sig.AgentSite_map_and_set.Map.t;
  }

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
  }

  (**************************************************************************)
  (*local static information*)

  let get_global_static_information static = static.global_static_information
  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static
  let get_compil static = lift Analyzer_headers.get_cc_code static
  let get_bond_rhs static = lift Analyzer_headers.get_bonds_rhs static
  let get_bond_lhs static = lift Analyzer_headers.get_bonds_lhs static
  let get_bounds_to_rule static = static.bonds_to_rules

  (*--------------------------------------------------------------------*)
  (** dynamic information*)

  let get_local_dynamic_information dynamic = dynamic.local
  let set_local_dynamic_information local dynamic = { dynamic with local }
  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  let get_contact_map_dynamic dynamic =
    (get_local_dynamic_information dynamic).contact_map_dynamic

  let set_contact_map_dynamic contact_map dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        contact_map_dynamic = contact_map;
      }
      dynamic

  let get_bonds_per_site dynamic =
    (get_local_dynamic_information dynamic).bonds_per_site

  let set_bonds_per_site bonds dynamic =
    set_local_dynamic_information
      { (get_local_dynamic_information dynamic) with bonds_per_site = bonds }
      dynamic

  (**************************************************************************)
  (*implementations*)

  let add_oriented_relation parameter error r_id site1 site2 map =
    let error, old_set =
      match
        Ckappa_sig.PairAgentSiteState_map_and_set.Map.find_option_without_logs
          parameter error (site1, site2) map
      with
      | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, new_set =
      Ckappa_sig.Rule_map_and_set.Set.add_when_not_in parameter error r_id
        old_set
    in
    Ckappa_sig.PairAgentSiteState_map_and_set.Map.add_or_overwrite parameter
      error (site1, site2) new_set map

  let add_relation parameter error r_id site1 site2 map =
    let error, map =
      add_oriented_relation parameter error r_id site1 site2 map
    in
    add_oriented_relation parameter error r_id site2 site1 map

  let initialize static dynamic error =
    let init_local =
      {
        contact_map_dynamic =
          Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty;
        bonds_per_site = Ckappa_sig.AgentSite_map_and_set.Map.empty;
      }
    in
    let init_global_dynamic_information =
      { local = init_local; global = dynamic }
    in
    let parameter = Analyzer_headers.get_parameter static in
    let bonds_lhs = Analyzer_headers.get_bonds_lhs static in
    let bonds_to_rules = Ckappa_sig.PairAgentSiteState_map_and_set.Map.empty in
    let p (_, a, b, c) = a, b, c in
    let error, bonds_to_rules =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun r_id set (error, bonds_to_rules) ->
          Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
            (fun (site1, site2) (error, bonds_to_rules) ->
              add_relation parameter error r_id (p site1) (p site2)
                bonds_to_rules)
            set (error, bonds_to_rules))
        bonds_lhs (error, bonds_to_rules)
    in
    let init_global_static_information =
      { global_static_information = static; bonds_to_rules }
    in
    error, init_global_static_information, init_global_dynamic_information, []

  let complete_wake_up_relation _static error wake_up = error, wake_up

  (**************************************************************************)

  type 'a zeroary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * dynamic_information * 'c

  type ('a, 'b, 'c, 'd) ternary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * dynamic_information * 'd

  (**************************************************************************)
  (*Implementation*)

  let add_oriented_bond_in_set_of_bonds static dynamic error (x, y) =
    let parameters = get_parameter static in
    let contact_map_dynamic = get_contact_map_dynamic dynamic in
    let error, contact_map_dynamic =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.add_when_not_in parameters
        error (x, y) contact_map_dynamic
    in
    let dynamic = set_contact_map_dynamic contact_map_dynamic dynamic in
    error, dynamic

  let add_oriented_bond_in_map_of_bonds static dynamic error (x, y) =
    let agent_type, site_type, state = x in
    let agent_type', site_type', state' = y in
    let parameters = get_parameter static in
    let bonds_per_site = get_bonds_per_site dynamic in
    let error, old_map =
      match
        Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameters
          error (agent_type, site_type) bonds_per_site
      with
      | error, None -> error, Ckappa_sig.State_map_and_set.Map.empty
      | error, Some m -> error, m
    in
    let error, state_map =
      Ckappa_sig.State_map_and_set.Map.add_or_overwrite parameters error state
        (agent_type', site_type', state')
        old_map
    in
    let error, bonds_per_site =
      Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite parameters error
        (agent_type, site_type) state_map bonds_per_site
    in
    let dynamic = set_bonds_per_site bonds_per_site dynamic in
    error, dynamic

  let add_bond_in_map_of_bonds static dynamic error (x, y) =
    let error, dynamic =
      add_oriented_bond_in_map_of_bonds static dynamic error (x, y)
    in
    add_oriented_bond_in_map_of_bonds static dynamic error (y, x)

  let add_oriented_bond static dynamic error bond =
    let error, dynamic =
      add_oriented_bond_in_set_of_bonds static dynamic error bond
    in
    add_oriented_bond_in_map_of_bonds static dynamic error bond

  (* make sure the appropriate version among oriented and unoriented, is
     used each one (the result must be unonriented) *)
  (* basically, either the input is unoriented, which means that each time
     the bond (x,y) is given, the bond (y,x) is given as well, and we can use
     the oriented version *)
  (* but if this is not the case, we have to use the unoriented version *)

  (**bond occurs in the initial state*)

  let collect_bonds_initial static dynamic error init_state =
    let parameters = get_parameter static in
    let error, dynamic =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
        error
        (fun parameters error agent_id bonds_map dynamic ->
          let error, dynamic =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, dynamic) ->
                let error, pair =
                  Common_static.collect_fingerprint_of_bond parameters error
                    site_add agent_id site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                (*use the oriented bonds, when given the bond (x, y), the
                  bond (y, x) is given as well*)
                let error, dynamic =
                  add_oriented_bond static dynamic error pair
                in
                error, dynamic)
              bonds_map (error, dynamic)
          in
          error, dynamic)
        init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds dynamic
    in
    error, dynamic

  (**************************************************************************)

  let collect_events static error map_diff event_list =
    let bonds_to_rules = get_bounds_to_rule static in
    let parameter = get_parameter static in
    Ckappa_sig.PairAgentSiteState_map_and_set.Set.fold
      (fun pair (error, event_list) ->
        let error, event_list =
          match
            Ckappa_sig.PairAgentSiteState_map_and_set.Map
            .find_option_without_logs parameter error pair bonds_to_rules
          with
          | error, None -> error, event_list
          | error, Some r_set ->
            let error =
              if
                local_trace
                || Remanent_parameters.get_dump_reachability_analysis_wl
                     parameter
              then (
                let log = Remanent_parameters.get_logger parameter in
                (*---------------------------------------------------------------*)
                let error =
                  Ckappa_sig.Rule_map_and_set.Set.fold
                    (fun rule_id error ->
                      let compiled = get_compil static in
                      let error, rule_id_string =
                        try
                          Handler.string_of_rule parameter error compiled
                            rule_id
                        with _ ->
                          Exception.warn parameter error __POS__ Exit
                            (Ckappa_sig.string_of_rule_id rule_id)
                      in
                      let title = "" in
                      let tab =
                        if title = "" then
                          "\t\t\t\t"
                        else
                          "\t\t\t"
                      in
                      let () =
                        Loggers.fprintf log "%s%s(%s) should be investigated "
                          (Remanent_parameters.get_prefix parameter)
                          tab rule_id_string
                      in
                      let () = Loggers.print_newline log in
                      error)
                    r_set error
                in
                let () = Loggers.print_newline log in
                error
              ) else
                error
            in
            ( error,
              Ckappa_sig.Rule_map_and_set.Set.fold
                (fun r_id event_list ->
                  Communication.Check_rule r_id :: event_list)
                r_set event_list )
        in
        error, Communication.See_a_new_bond pair :: event_list)
      map_diff (error, event_list)

  let add_initial_state static dynamic error species =
    let parameter = get_parameter static in
    let set_before = get_contact_map_dynamic dynamic in
    (*------------------------------------------------------*)
    let error, dynamic = collect_bonds_initial static dynamic error species in
    let set_after = get_contact_map_dynamic dynamic in
    (*------------------------------------------------------*)
    let error, set_diff =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.diff parameter error
        set_after set_before
    in
    let error, event_list = collect_events static error set_diff [] in
    error, dynamic, event_list

  (**************************************************************************)

  module Proj_bonds =
    Map_wrapper.Proj
      (Ckappa_sig.PairAgentsSiteState_map_and_set)
      (*potential tuple pair set*)
      (Ckappa_sig.PairAgentSiteState_map_and_set)
  (*use to search the set in bonds rhs*)

  let proj (_, b, c, d) = b, c, d
  let proj2 (x, y) = proj x, proj y

  let proj_bonds_aux parameters error bonds_set =
    Proj_bonds.proj_set (fun (x, y) -> proj2 (x, y)) parameters error bonds_set

  let is_enabled static dynamic error rule_id precondition =
    (*test if the bond in the lhs has already in the contact map, if not
      None*)
    let parameters = get_parameter static in
    let bond_lhs = get_bond_lhs static in
    let contact_map = get_contact_map_dynamic dynamic in
    (*------------------------------------------------------*)
    let error, bond_lhs_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameters
          error rule_id bond_lhs
      with
      | error, None ->
        error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, bond_lhs_set = proj_bonds_aux parameters error bond_lhs_set in
    (*------------------------------------------------------*)
    if
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.subset bond_lhs_set
        contact_map
    then (
      (* use the function Communication.overwrite_potential_partners_map to
         fill the two fields related to the dynamic contact map *)
      (* then use the functions get_potential_partner and/or
         fold_over_potential_partners in the views domain to use the incremental
         (dynamic) contact map *)
      (* instead of the static one *)
      let error, precondition =
        Communication.overwrite_potential_partners_map parameters error
          precondition
          (fun error agent_type site_type state ->
            (* Here you should fetch the partner in the dynamic contact
               map, if defined, *)
            let error, statemap_bottop =
              (* JF: error should be propagated, Please correct !!! *)
              Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                parameters error (agent_type, site_type)
                dynamic.local.bonds_per_site
            in
            match statemap_bottop with
            | None ->
              (*error, Usual_domains.Val (agent_type, site_type, state) *)
              Exception.warn parameters error __POS__
                ~message:"state map bottop is empty" Exit
                (Usual_domains.Val (agent_type, site_type, state))
            (* I think you should raise an error here *)
            | Some statemap ->
              (match
                 Ckappa_sig.State_map_and_set.Map.find_option parameters error
                   state statemap
               with
              | error, None -> error, Usual_domains.Undefined
              | error, Some tuple -> error, Usual_domains.Val tuple))
          {
            Communication.fold =
              (fun parameters error agent_type site_type ->
                let error, statemap_bottop =
                  Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                    parameters error (agent_type, site_type)
                    dynamic.local.bonds_per_site
                in
                match statemap_bottop with
                | None ->
                  error, Usual_domains.Val (fun _f error init -> error, init)
                | Some statemap ->
                  ( error,
                    Usual_domains.Val
                      (fun f error init ->
                        Ckappa_sig.State_map_and_set.Map.fold (f parameters)
                          statemap (error, init)) ));
          }
      in
      error, dynamic, Some precondition
    ) else
      error, dynamic, None

  (***********************************************************)

  (* TO DO *)
  (* ignore the flag *)
  (* Please check that any bond in the pattern is reachable *)
  let maybe_reachable _static dynamic error _flag _pattern precondition =
    error, dynamic, Some precondition

  (**************************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameters = get_parameter static in
    (*add the bonds in the rhs into the contact map*)
    let contact_map = get_contact_map_dynamic dynamic in
    let bond_rhs_map = get_bond_rhs static in
    (*------------------------------------------------------*)
    let error, bond_rhs_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameters
          error rule_id bond_rhs_map
      with
      | error, None ->
        error, Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, bond_rhs_set = proj_bonds_aux parameters error bond_rhs_set in
    (*------------------------------------------------------*)
    let error', union =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.union parameters error
        contact_map bond_rhs_set
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    let dynamic = set_contact_map_dynamic union dynamic in
    (*------------------------------------------------------*)
    let new_contact_map = get_contact_map_dynamic dynamic in
    let error', map_diff =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.diff parameters error
        new_contact_map contact_map
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    (*------------------------------------------------------*)
    (*update the second field*)
    let error, dynamic =
      Ckappa_sig.PairAgentSiteState_map_and_set.Set.fold
        (fun bond (error, dynamic) ->
          add_bond_in_map_of_bonds static dynamic error bond)
        map_diff (error, dynamic)
    in
    (*check if it is seen for the first time, if not update the contact
      map, and raise an event*)
    let error, event_list = collect_events static error map_diff event_list in
    error, dynamic, (precondition, event_list)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  let apply_one_side_effect _static dynamic error _ _ precondition =
    error, dynamic, (precondition, [])
  (* this domain ignores side effects *)

  let stabilize _static dynamic error = error, dynamic, ()
  let export _static dynamic error kasa_state = error, dynamic, kasa_state

  let print ?dead_rules _static dynamic error _loggers =
    let _ = dead_rules in
    error, dynamic, ()

  let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules
  let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
end
