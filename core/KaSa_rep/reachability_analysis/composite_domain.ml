(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Dec 04 2018>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Composite_domain = sig
  type static_information
  type dynamic_information

  val initialize :
    Analyzer_headers.global_static_information ->
    Analyzer_headers.global_dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * static_information * dynamic_information

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

  val next_rule : Ckappa_sig.c_rule_id option zeroary
  val add_initial_state : (Analyzer_headers.initial_state, unit) unary

  val is_enabled :
    (Ckappa_sig.c_rule_id, Communication.precondition option) unary

  val apply_rule :
    (Ckappa_sig.c_rule_id, Communication.precondition, unit) binary

  val stabilize : unit zeroary

  val export :
    ( ('static, 'dynamic) Analyzer_headers.kasa_state,
      ('static, 'dynamic) Analyzer_headers.kasa_state )
    unary

  val print : (Loggers.t, unit) unary

  val maybe_reachable :
    ( Analyzer_headers.pattern_matching_flag,
      Cckappa_sig.mixture,
      Communication.precondition option )
    binary

  val get_global_dynamic_information :
    dynamic_information -> Analyzer_headers.global_dynamic_information

  val set_global_dynamic_information :
    Analyzer_headers.global_dynamic_information ->
    dynamic_information ->
    dynamic_information
end

(****************************************************************************)
(*Analyzer is a functor takes a module Domain as its parameters.*)

module Make (Domain : Analyzer_domain_sig.Domain) = struct
  type static_information =
    Analyzer_headers.global_static_information * Domain.static_information

  type working_list = Ckappa_sig.Rule_FIFO.t

  type dynamic_information = {
    modified_sites_blackboard: Communication.site_working_list;
    rule_working_list: working_list;
    bonds:
      Ckappa_sig.AgentSite_map_and_set.Set.t
      Ckappa_sig.AgentSite_map_and_set.Map.t;
    domain: Domain.dynamic_information;
  }

  let get_modified_sites_blackboard dynamic = dynamic.modified_sites_blackboard
  let get_bonds dynamic = dynamic.bonds
  let set_bonds bonds dynamic = { dynamic with bonds }
  let get_global_static_information = fst
  let get_domain_static_information = snd
  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static
  let get_compil static = lift Analyzer_headers.get_cc_code static

  let get_wake_up_relation static =
    lift Analyzer_headers.get_wake_up_relation static

  let empty_working_list = Ckappa_sig.Rule_FIFO.empty

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

  (** push r_id in the working_list *)

  let get_working_list dynamic = dynamic.rule_working_list

  let set_working_list rule_working_list dynamic =
    { dynamic with rule_working_list }

  let set_domain domain dynamic = { dynamic with domain }

  let get_global_dynamic_information dynamic =
    Domain.get_global_dynamic_information dynamic.domain

  let set_global_dynamic_information gdynamic dynamic =
    {
      dynamic with
      domain = Domain.set_global_dynamic_information gdynamic dynamic.domain;
    }

  let push_rule static dynamic error r_id =
    let working_list = get_working_list dynamic in
    let parameters = get_parameter static in
    let compiled = get_compil static in
    let error, rule_working_list =
      Ckappa_sig.Rule_FIFO.push parameters error r_id working_list
    in
    let error =
      if
        (not (rule_working_list == working_list))
        && Remanent_parameters.get_dump_reachability_analysis_wl parameters
      then (
        let error, rule_id_string =
          try Handler.string_of_rule parameters error compiled r_id
          with _ ->
            Exception.warn parameters error __POS__ Exit
              (Ckappa_sig.string_of_rule_id r_id)
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "\t\t\t(%s) should be investigated\n" rule_id_string
        in
        error
      ) else
        error
    in
    let dynamic = set_working_list rule_working_list dynamic in
    error, dynamic

  let push_modified_site static dynamic error agent site =
    let wake_up = get_wake_up_relation static in
    let parameter = get_parameter static in
    let error, rules_list =
      Common_static.wake_up parameter error agent site wake_up
    in
    List.fold_left
      (fun (error, dynamic) r_id -> push_rule static dynamic error r_id)
      (error, dynamic) rules_list

  (**[next_rule static dynamic] returns a rule_id inside a working list
     if it is not empty*)

  let next_rule static dynamic error =
    let working_list = get_working_list dynamic in
    (* see if the working list is empty, if not pop an element *)
    if Ckappa_sig.Rule_FIFO.is_empty working_list then
      error, dynamic, None
    else (
      let parameters = get_parameter static in
      let error, (rule_id_op, working_list_tail) =
        Ckappa_sig.Rule_FIFO.pop parameters error working_list
      in
      let dynamic = set_working_list working_list_tail dynamic in
      error, dynamic, rule_id_op
    )

  (*for each rule with no lhs, push the rule in the working list *)
  let push_rule_creation static dynamic error rule_id rule =
    let parameters = get_parameter static in
    let error, dynamic =
      List.fold_left
        (fun (error, dynamic) (agent_id, _agent_type) ->
          let error, agent =
            Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
              parameters error agent_id
              rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
          in
          match agent with
          | Some (Cckappa_sig.Dead_agent _) | Some Cckappa_sig.Ghost ->
            error, dynamic
          | None -> Exception.warn parameters error __POS__ Exit dynamic
          | Some (Cckappa_sig.Unknown_agent _) | Some (Cckappa_sig.Agent _) ->
            let error, dynamic = push_rule static dynamic error rule_id in
            error, dynamic)
        (error, dynamic) rule.Cckappa_sig.actions.Cckappa_sig.creation
    in
    let error, dynamic =
      if
        rule.Cckappa_sig.actions.Cckappa_sig.creation = []
        && Ckappa_sig.skip_only rule.Cckappa_sig.rule_lhs.Cckappa_sig.c_mixture
      then
        push_rule static dynamic error rule_id
      else
        error, dynamic
    in
    error, dynamic

  let scan_rule_creation static dynamic error =
    let parameters = get_parameter static in
    let compil = get_compil static in
    let rules = compil.Cckappa_sig.rules in
    let error, dynamic =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
        (fun _parameters error rule_id rule dynamic ->
          let error, dynamic =
            push_rule_creation static dynamic error rule_id
              rule.Cckappa_sig.e_rule_c_rule
          in
          error, dynamic)
        rules dynamic
    in
    error, dynamic

  (**[lift_unary f static dynamic] is a function lifted of unary type,
     returns information of dynamic information and its output*)

  let lift_zeroary f static dynamic error =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error
    in
    let dynamic = set_domain domain_dynamic dynamic in
    error, dynamic, output

  let lift_unary f static dynamic error a =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a
    in
    let dynamic = set_domain domain_dynamic dynamic in
    error, dynamic, output

  (**[pre_add_initial_state static dynamic error a] returns a pair of
     type unary where the output of static information [a] is an initial state
     of analyzer header, and the dynamic output [a list of event] is unit. *)

  let pre_add_initial_state static dynamic error a =
    lift_unary Domain.add_initial_state static dynamic error a

  let lift_binary f static dynamic error a b =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a b
    in
    let dynamic = set_domain domain_dynamic dynamic in
    error, dynamic, output

  let lift_ternary f static dynamic error a b c =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a b c
    in
    let dynamic = set_domain domain_dynamic dynamic in
    error, dynamic, output

  (**[is_enabled static dynamic error a] returns a triple of type binary
     when given a rule_id [a], check that if this rule is enable or not *)

  let is_enabled static dynamic error a =
    lift_binary Domain.is_enabled static dynamic error a
      Communication.dummy_precondition

  (***********************************************************)

  (**[pre_apply_rule static dynamic error a b] *)
  let pre_apply_rule static dynamic error a b =
    lift_binary Domain.apply_rule static dynamic error a b

  (**apply a list of event if it is empty then do nothing, otherwise go
     through this list and at each event push rule_id inside a working
     list and apply this list of event with new dynamic information*)

  let get_partner parameter error site bonds =
    match
      Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameter
        error site bonds
    with
    | error, None -> error, Ckappa_sig.AgentSite_map_and_set.Set.empty
    | error, Some set -> error, set

  let p (a, b, _) = a, b

  let add_oriented_bond parameter error (site, site') bonds =
    let site = p site in
    let site' = p site' in
    let error, old = get_partner parameter error site bonds in
    let error, newset =
      Ckappa_sig.AgentSite_map_and_set.Set.add_when_not_in parameter error site'
        old
    in
    Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite parameter error site
      newset bonds

  let add_bond parameter error (site, site') bonds =
    let error, bonds = add_oriented_bond parameter error (site, site') bonds in
    add_oriented_bond parameter error (site', site) bonds

  let rec apply_event_list (static : static_information) dynamic error
      event_list =
    let parameter = get_parameter static in
    if event_list = [] then
      error, dynamic, ()
    else (
      let split event_list =
        List.fold_left
          (fun (check_rules, modified_sites, bonds, others) event ->
            match event with
            | Communication.Check_rule r_id ->
              r_id :: check_rules, modified_sites, bonds, others
            | Communication.Modified_sites site ->
              check_rules, site :: modified_sites, bonds, others
            | Communication.See_a_new_bond bond ->
              check_rules, modified_sites, bond :: bonds, event :: others
            | Communication.Dummy ->
              check_rules, modified_sites, bonds, event :: others)
          ([], [], [], []) event_list
      in
      let check_rules, modified_sites, bonds, event_list = split event_list in
      let dyn_bonds = get_bonds dynamic in
      let error, dyn_bonds =
        List.fold_left
          (fun (error, dyn_bonds) bond ->
            add_bond parameter error bond dyn_bonds)
          (error, dyn_bonds) bonds
      in
      let dynamic = set_bonds dyn_bonds dynamic in
      let error, dynamic, event_list' =
        lift_unary Domain.apply_event_list static dynamic error event_list
      in
      let modified_sites_blackboard = get_modified_sites_blackboard dynamic in
      let error, modified_sites_blackboard =
        List.fold_left
          (fun (error, modified_sites_blackboard) (agent, site) ->
            let error, modified_sites_blackboard =
              Communication.add_site parameter error agent site
                modified_sites_blackboard
            in
            let (error : Exception.method_handler), set =
              get_partner parameter error (agent, site) dyn_bonds
            in
            Ckappa_sig.AgentSite_map_and_set.Set.fold
              (fun (agent, site) (error, modified_sites_blackboard) ->
                Communication.add_site parameter error agent site
                  modified_sites_blackboard)
              set
              (error, modified_sites_blackboard))
          (error, modified_sites_blackboard)
          modified_sites
      in
      let f l error dynamic =
        List.fold_left
          (fun (error, dynamic) r_id -> push_rule static dynamic error r_id)
          (error, dynamic) l
      in
      let error, dynamic = f check_rules error dynamic in
      let wake_up = Analyzer_headers.get_wake_up_relation (fst static) in
      let error, dynamic =
        Communication.fold_sites parameter error
          (fun parameter error (agent, site) () dynamic ->
            let error, list_r_id =
              Common_static.wake_up parameter error agent site wake_up
            in
            f list_r_id error dynamic)
          modified_sites_blackboard dynamic
      in
      let error, dynamic =
        List.fold_left
          (fun (error, dynamic) event ->
            match event with
            | Communication.Check_rule rule_id ->
              push_rule static dynamic error rule_id
            | Communication.Modified_sites (agent, site) ->
              push_modified_site static dynamic error agent site
            | Communication.See_a_new_bond _ | Communication.Dummy ->
              error, dynamic)
          (error, dynamic) event_list
      in
      apply_event_list static dynamic error event_list'
    )

  let initialize static dynamic error =
    let error, domain_static, domain_dynamic, event_list =
      Domain.initialize static dynamic error
    in
    let parameters = get_parameter (static, domain_static) in
    let error, wake_up_tmp =
      Common_static.empty_site_to_rules parameters error
    in
    let error, wake_up_tmp =
      Domain.complete_wake_up_relation domain_static error wake_up_tmp
    in
    let error, wake_up =
      Common_static.consolidate_site_rule_dependencies parameters error
        wake_up_tmp
    in
    let static =
      Analyzer_headers.add_wake_up_relation static wake_up, domain_static
    in
    let working_list = empty_working_list in
    let error, sites_blackboard =
      Communication.init_sites_working_list parameters error
    in
    let dynamic =
      {
        rule_working_list = working_list;
        domain = domain_dynamic;
        modified_sites_blackboard = sites_blackboard;
        bonds = Ckappa_sig.AgentSite_map_and_set.Map.empty;
      }
    in
    let error, dynamic = scan_rule_creation static dynamic error in
    let error, dynamic, () = apply_event_list static dynamic error event_list in
    error, static, dynamic

  (** add initial state then apply a list of event starts from this new
      list*)

  let add_initial_state static dynamic error initial_state =
    let error, dynamic, event_list =
      pre_add_initial_state static dynamic error initial_state
    in
    apply_event_list static dynamic error event_list

  let get_rule parameter error static r_id =
    let compil = get_compil static in
    let error, rule =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameter error r_id
        compil.Cckappa_sig.rules
    in
    error, rule

  (**if it has a precondition for this rule_id then apply a list of event
     starts from this new list*)
  let apply_one_side_effect static dynamic error r_id (source, target)
      precondition event_list =
    let error, dynamic, (precondition, event_list') =
      lift_ternary Domain.apply_one_side_effect static dynamic error r_id
        (source, target) precondition
    in
    error, dynamic, (precondition, event_list' @ event_list)

  let check_side_effect static dynamic error precondition event_list r_id rule
      source target =
    let agent_id, _, site, state = source in
    let error, dynamic, precondition, state_list =
      Communication.get_state_of_site_in_precondition
        get_global_static_information get_global_dynamic_information
        set_global_dynamic_information error static dynamic (r_id, rule)
        agent_id (*A*)
        site precondition
    in
    if List.mem state state_list then
      apply_one_side_effect static dynamic error r_id (Some source, target)
        precondition event_list
    else
      error, dynamic, (precondition, event_list)

  let apply_side_effect static dynamic error r_id precondition event_list =
    let parameters =
      Analyzer_headers.get_parameter (get_global_static_information static)
    in
    let error, side_effects =
      Ckappa_sig.Rule_map_and_set.Map.find_default_without_logs parameters error
        [] r_id
        (Analyzer_headers.get_potential_side_effects_per_rule
           (get_global_static_information static))
    in
    let error, rule_opt = get_rule parameters error static r_id in
    match rule_opt with
    | Some rule ->
      let error, side_effects_opt =
        Domain.get_side_effects
          (get_domain_static_information static)
          dynamic.domain parameters error r_id
      in
      (match side_effects_opt with
      | None ->
        List.fold_left
          (fun (error, dynamic, (precondition, event_list)) (source, target) ->
            check_side_effect static dynamic error precondition event_list r_id
              rule source target)
          (error, dynamic, (precondition, event_list))
          side_effects
      | Some side_effects ->
        let error, dynamic, (precondition, event_list) =
          Ckappa_sig.AgentSiteState_map_and_set.Set.fold
            (fun target (error, dynamic, (precondition, event_list)) ->
              apply_one_side_effect static dynamic error r_id (None, target)
                precondition event_list)
            side_effects.Ckappa_sig.seen
            (error, dynamic, (precondition, event_list))
        in
        let error, dynamic, (precondition, event_list) =
          Ckappa_sig.AgentsSiteState_map_and_set.Map.fold
            (fun source target (error, dynamic, (precondition, event_list)) ->
              apply_one_side_effect static dynamic error r_id
                (Some source, target) precondition event_list)
            side_effects.Ckappa_sig.not_seen_yet
            (error, dynamic, (precondition, event_list))
        in
        error, dynamic, (precondition, event_list))
    | None ->
      let error, () = Exception.warn parameters error __POS__ Exit () in
      error, dynamic, (precondition, event_list)

  let apply_rule static dynamic error r_id precondition =
    let error, dynamic, (precondition, event_list) =
      pre_apply_rule static dynamic error r_id precondition
    in
    let error, dynamic, (_precondition, event_list) =
      apply_side_effect static dynamic error r_id precondition event_list
    in
    apply_event_list static dynamic error event_list

  let stabilize static dynamic error =
    lift_zeroary Domain.stabilize static dynamic error

  let export static dynamic error kasa_state =
    lift_unary Domain.export static dynamic error kasa_state

  let print static dynamic error loggers =
    let dead_rules =
      Domain.get_dead_rules
        (get_domain_static_information static)
        dynamic.domain
    in
    lift_unary (Domain.print ~dead_rules) static dynamic error loggers

  let maybe_reachable (static : static_information) dynamic error flag mixture =
    lift_ternary Domain.maybe_reachable static dynamic error flag mixture
      Communication.dummy_precondition
end
