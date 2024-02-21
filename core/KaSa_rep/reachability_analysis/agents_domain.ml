(**
   * agent_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2016, the 30th of January
   * Last modification: Time-stamp: <Sep 27 2018>
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
    domain_static_information:
      (Ckappa_sig.c_agent_name list Usual_domains.bot_or_not
      * Ckappa_sig.c_agent_name list)
      Ckappa_sig.Rule_map_and_set.Map.t;
    agents_without_interface:
      Ckappa_sig.c_rule_id list Ckappa_sig.Agent_map_and_set.Map.t;
  }

  (*--------------------------------------------------------------------*)
  (* array that indicates whether an agent type is already discovered, or
     not: from the beginning everything will be set to false*)

  (* This array is statically allocated *)
  (* Why do you use extensive arrays ? *)
  type local_dynamic_information =
    bool Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
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

  (**domain *)

  let get_domain_static_information static = static.domain_static_information

  let set_domain_static_information domain static =
    { static with domain_static_information = domain }

  let get_agents_without_interface static = static.agents_without_interface

  let set_agents_without_interface agents static =
    { static with agents_without_interface = agents }

  (*--------------------------------------------------------------------*)
  (** global static/dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  (** dead rule local dynamic information*)
  let get_seen_agent dynamic = dynamic.local

  let set_seen_agent seen_agent dynamic = { dynamic with local = seen_agent }

  (*--------------------------------------------------------------------*)

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
  (**initialize*)
  (*you should use the following type constructor:
    type 'a Bot_or_not = Bot | Not_bot 'a
    if a dead agent occurs in a lhs, output Bot
    otherwise Not_bot list *)
  (*In is_enable, Bot is unsatisfiable *)
  (*Be careful, dead agents and Ghosts agents are different *)
  (*Ghost agents must be ignored *)
  (*Dead agents, generates Bot elements *)
  (*I think that unknown agents must be dealt with as dead agents *)

  (*convert a fold into a list*)

  let map_to_list parameters error map =
    let error, list =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
        error
        (fun _ error _ a current_list ->
          let list = a :: current_list in
          error, list)
        map []
    in
    error, list

  let collect_agents parameters error rule =
    let error, list_lhs_views =
      map_to_list parameters error rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    in
    let error, agents_test_list =
      let rec aux l (error, output) =
        match l with
        | [] -> error, Usual_domains.Not_bot output
        | agent :: tl ->
          (match agent with
          | Cckappa_sig.Ghost -> aux tl (error, output)
          | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Dead_agent _ ->
            error, Usual_domains.Bot
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            aux tl (error, agent_type :: output))
      in
      aux list_lhs_views (error, [])
    in
    let error, agents_created_list =
      List.fold_left
        (fun (error, current_list) (_agent_id, agent_type) ->
          let agent_list = agent_type :: current_list in
          error, agent_list)
        (error, []) rule.Cckappa_sig.actions.Cckappa_sig.creation
    in
    error, (agents_test_list, agents_created_list)

  let collect_agents_without_interface parameters error rule_id rule map =
    let error, agents_lhs_list =
      map_to_list parameters error rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    in
    let rec aux l (error, output) =
      match l with
      | [] -> error, Usual_domains.Not_bot output
      | agent :: tl ->
        (match agent with
        | Cckappa_sig.Ghost -> aux tl (error, output)
        | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Dead_agent _ ->
          error, Usual_domains.Bot
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          aux tl (error, agent_type :: output))
    in
    match aux agents_lhs_list (error, []) with
    | error, Usual_domains.Bot -> error, map
    | error, Usual_domains.Not_bot l ->
      List.fold_left
        (fun (error, map) agent_type ->
          let error, old_list =
            match
              Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
                parameters error agent_type map
            with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let rule_id_list = rule_id :: old_list in
          Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite parameters error
            agent_type rule_id_list map)
        (error, map) l

  let scan_rule_set static dynamic error =
    let parameters = get_parameter static in
    let compil = get_compil static in
    let error, (result, agents) =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
        (fun parameters error rule_id rule
             (store_result, agents_without_interface) ->
          let error, (agents_test_list, agents_created_list) =
            collect_agents parameters error rule.Cckappa_sig.e_rule_c_rule
          in
          (*add rule_id in map*)
          let error, result =
            Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameters error
              rule_id
              (agents_test_list, agents_created_list)
              store_result
          in
          (*agents without interface*)
          let error, agents_without_interface =
            collect_agents_without_interface parameters error rule_id
              rule.Cckappa_sig.e_rule_c_rule agents_without_interface
          in
          error, (result, agents_without_interface))
        compil.Cckappa_sig.rules
        ( Ckappa_sig.Rule_map_and_set.Map.empty,
          Ckappa_sig.Agent_map_and_set.Map.empty )
    in
    let static = set_agents_without_interface agents static in
    let static = set_domain_static_information result static in
    error, static, dynamic

  (**************************************************************************)

  let initialize static dynamic error =
    let parameters = Analyzer_headers.get_parameter static in
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = Ckappa_sig.Rule_map_and_set.Map.empty;
        agents_without_interface = Ckappa_sig.Agent_map_and_set.Map.empty;
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let nagents =
      Ckappa_sig.int_of_agent_name
        (Handler.nagents parameters error kappa_handler)
      - 1
    in
    let error, init_seen_agents_array =
      if nagents < 0 then
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      else
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.init parameters
          error nagents (fun _ error _ -> error, false)
    in
    let init_global_dynamic_information =
      { global = dynamic; local = init_seen_agents_array }
    in
    let error, static, dynamic =
      scan_rule_set init_global_static_information
        init_global_dynamic_information error
    in
    error, static, dynamic, []

  let complete_wake_up_relation _static error wake_up = error, wake_up

  (***************************************************************************)
  (*JF: Here, you should add in the event list, each rule that test for an
    agent with a type among the ones you have newly see, and with an empty
    interface (no test).  For instance, if you see an agent of type A for
    the first time, then a rule like B(x~u),A() -> B(x~p),A() should be
    awaken. You also have to do this, when you add an initial state, thus
    it is worth using an auxilliary function, that check whether an agant
    type has been already seen, if not, declared it as seen, then add all
    the rules that requires this agent in their lhs and with no tests, in
    the working list, via the event_list argument For this, you need to add
    a map in the struct static, to map each agent type to the list of rules
    an agent of this type and with an empty interface occur in the lhs of
    the rule *)

  let add_event_list static dynamic error (agent_type : Ckappa_sig.c_agent_name)
      event_list =
    let parameters = get_parameter static in
    let map = get_agents_without_interface static in
    let log = Remanent_parameters.get_logger parameters in
    let local = get_seen_agent dynamic in
    let error, bool =
      Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters
        error agent_type local
    in
    match bool with
    | Some false ->
      let error, local =
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters
          error agent_type true local
      in
      let dynamic = set_seen_agent local dynamic in
      let error, rule_id_list =
        match
          Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs parameters
            error agent_type map
        with
        | error, None -> error, []
        | error, Some l -> error, l
      in
      let error, bool =
        if
          local_trace
          || Remanent_parameters.get_dump_reachability_analysis_wl parameters
        then
          List.fold_left
            (fun (error, _) rule_id ->
              let compiled = get_compil static in
              let error, rule_id_string =
                try Handler.string_of_rule parameters error compiled rule_id
                with _ ->
                  Exception.warn parameters error __POS__ Exit
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
                  (Remanent_parameters.get_prefix parameters)
                  tab rule_id_string
              in
              let () = Loggers.print_newline log in
              error, true)
            (error, false) rule_id_list
        else
          error, false
      in
      let () = if bool then Loggers.print_newline log in
      let event_list =
        List.fold_left
          (fun event_list rule_id ->
            Communication.Check_rule rule_id :: event_list)
          event_list rule_id_list
      in
      error, (dynamic, event_list)
    | Some true -> error, (dynamic, event_list)
    | None -> Exception.warn parameters error __POS__ Exit (dynamic, event_list)

  (**************************************************************************)
  (** collect the agent type of the agents of the species and declare
      them seen *)

  let init_agents static dynamic error init_state event_list =
    let parameters = get_parameter static in
    let error, (dynamic, event_list) =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
        error
        (fun parameters error _ agent (dynamic, event_list) ->
          match agent with
          (*JF: warn: dead,unknown,ghost should not occur in initial states *)
          | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost
          | Cckappa_sig.Dead_agent _ ->
            Exception.warn parameters error __POS__ Exit (dynamic, event_list)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let error, (dynamic, event_list) =
              add_event_list static dynamic error agent_type event_list
            in
            error, (dynamic, event_list))
        init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
        (dynamic, event_list)
    in
    error, (dynamic, event_list)

  (**************************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    let error, (dynamic, event_list) =
      init_agents static dynamic error species event_list
    in
    error, dynamic, event_list

  (************************************************************************************)
  (** check that the type of each agent in the lhs has been already seen
      forall to test *)

  (* JF: The fold should start with the value Some Precondition *)
  (* If one requested agent type has not been seen, then this value
     should be replaced with None *)
  (* JF: agent_id test that is not seen, update to seen*)
  (* No, the primitive is_enabled aims at checking that required
     agent types have already been seen by the domain *)
  (* This primitive should not change the list of the agent type
     which have been seen*)

  let is_enabled static dynamic error rule_id precondition =
    let parameters = get_parameter static in
    let domain_static = get_domain_static_information static in
    let error, (bot_or_not, _) =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameters
          error rule_id domain_static
      with
      | error, None -> error, (Usual_domains.Not_bot [], [])
      | error, Some (l1, l2) -> error, (l1, l2)
    in
    match bot_or_not with
    | Usual_domains.Bot -> error, dynamic, None
    | Usual_domains.Not_bot l ->
      List.fold_left
        (fun (error, dynamic, s) agent_type ->
          let local = get_seen_agent dynamic in
          let error, bool =
            Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
              parameters error agent_type local
          in
          match bool with
          | Some true -> error, dynamic, s
          | Some false -> error, dynamic, None
          | None ->
            let error, () = Exception.warn parameters error __POS__ Exit () in
            error, dynamic, None)
        (error, dynamic, Some precondition)
        l

  (***********************************************************)

  (* ignore the flag *)
  (* Please check that each agent type occuring in the pattern is reachable *)
  exception False of Exception.method_handler * dynamic_information

  let maybe_reachable static dynamic error _flag pattern precondition =
    let parameters = get_parameter static in
    try
      let error, dynamic =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error _agent_id agent dynamic ->
            match agent with
            | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost
            | Cckappa_sig.Dead_agent (_, _, _, _) ->
              error, dynamic
            | Cckappa_sig.Agent agent ->
              let local = get_seen_agent dynamic in
              let agent_type = agent.Cckappa_sig.agent_name in
              let error, bool =
                Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
                  parameters error agent_type local
              in
              (match bool with
              | Some true -> error, dynamic
              | Some false -> raise (False (error, dynamic))
              | None ->
                let error, () =
                  Exception.warn parameters error __POS__ Exit ()
                in
                error, dynamic))
          pattern.Cckappa_sig.views dynamic
      in
      error, dynamic, Some precondition
    with False (error, dynamic) -> error, dynamic, None

  (*********************************************************************)
  (** fold a list of creation each time update the array when agent is
      seen for the first time, add list of rule inside event list that
      contain the list of rules in the lhs.  a map from agent to
      rule. event_list need to be updated, add rules that this agents
      apply. *)

  (*JF: just declare each agent types in that list to be seen *)

  let apply_rule static dynamic error rule_id precondition =
    let parameters = get_parameter static in
    let event_list = [] in
    let domain_static = get_domain_static_information static in
    let error, list_created =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameters
          error rule_id domain_static
      with
      | error, None -> error, []
      | error, Some (_, l2) -> error, l2
    in
    let error, (dynamic, event_list) =
      List.fold_left
        (fun (error, (dynamic, event_list)) agent_type ->
          let error, (dynamic, event_list) =
            add_event_list static dynamic error agent_type event_list
          in
          error, (dynamic, event_list))
        (error, (dynamic, event_list))
        list_created
    in
    error, dynamic, (precondition, event_list)

  (************************************************************************************)
  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event_list _static dynamic error _event_list =
    let event_list' = [] in
    error, dynamic, event_list'

  let export static dynamic error kasa_state =
    let parameters = get_parameter static in
    let handler = get_kappa_handler static in
    let compil = get_compil static in
    let array = get_seen_agent dynamic in
    let error, list =
      Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.fold parameters
        error
        (fun _parameters error agent bool list ->
          if bool then
            error, list
          else (
            let error, info =
              Handler.info_of_agent parameters error handler compil agent
            in
            let agent = Remanent_state.info_to_agent info in
            error, agent :: list
          ))
        array []
    in
    error, dynamic, Remanent_state.set_dead_agents list kasa_state

  let apply_one_side_effect _static dynamic error _ _ precondition =
    error, dynamic, (precondition, [])
  (* this domain ignores side effects *)

  (**************************************************************************)
  let stabilize _static dynamic error = error, dynamic, ()

  let print_dead_agent loggers static dynamic error =
    let parameters = get_parameter static in
    let result = get_seen_agent dynamic in
    let handler = get_kappa_handler static in
    if Remanent_parameters.get_dump_reachability_analysis_result parameters then (
      let error, bool =
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.fold parameters
          error
          (fun _parameters error _k bool bool' -> error, bool && bool')
          result true
      in
      if not bool then (
        let parameters = Remanent_parameters.update_prefix parameters "" in
        let () = Loggers.print_newline loggers in
        let () =
          Loggers.fprintf loggers
            "------------------------------------------------------------"
        in
        let () = Loggers.print_newline loggers in
        let () =
          Loggers.fprintf loggers "* There are some non creatable agents "
        in
        let () = Loggers.print_newline loggers in
        let () =
          Loggers.fprintf loggers
            "------------------------------------------------------------"
        in
        let () = Loggers.print_newline loggers in
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.iter parameters
          error
          (fun parameters error k bool ->
            if bool then
              error
            else (
              let error', agent_string =
                try Handler.string_of_agent parameters error handler k
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_agent_name k)
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let () =
                Loggers.fprintf loggers "%s cannot occur in the model"
                  agent_string
              in
              let () = Loggers.print_newline loggers in
              error
            ))
          result
      ) else (
        let () =
          Loggers.fprintf loggers
            "------------------------------------------------------------"
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let () = Loggers.fprintf loggers "every agent may occur in the model" in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error
      )
    ) else
      error

  let print ?dead_rules static dynamic error loggers =
    let _ = dead_rules in
    let error = print_dead_agent loggers static dynamic error in
    error, dynamic, ()

  let _lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let _cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules
  let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
end
