(**
   * agent_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 30th of January
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

  (*type of agents in the lhs after minus created agents: type (tested *
    created\tested): use list less space, because the array below will
    control everything*)
  (* TO DO, for each rule, the set of agent types of agents in the lhs,
     and the set of the agent types of created agents minus the set of the
     agent types of agents in the lhs *)

  module Int2Map_Agent =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = int (*rule_id*)
           let compare = compare
          end
         ))

  type store_agents_test = (int * int) list Int2Map_Agent.Map.t

  type store_agents_test_without_created = (int * int) list Int2Map_Agent.Map.t

  type store_agents = store_agents_test * store_agents_test_without_created

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : store_agents
    }

  (*--------------------------------------------------------------------*)
  (* array that indicates whether an agent type is already discovered, or
     not: from the beginning everything will be set to false*)

  type local_dynamic_information = bool array

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  let get_domain_static_information static = static.domain_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  (** dead rule local dynamic information*)
  let get_seen_agent dynamic = dynamic.local

  let set_seen_agent seen_agent dynamic =
    {
      dynamic with local = seen_agent
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

  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    (*global static information*)
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = Int2Map_Agent.Map.empty, Int2Map_Agent.Map.empty
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    (*global dynamic information*)
    let nagent_types = Handler.nagents parameter error kappa_handler in
    let init_seen_agents_array = Array.make nagent_types false in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = init_seen_agents_array;
      }
    in
    error, init_global_static_information, init_global_dynamic_information

  (*collect agents test*)
  let get_store_agents_test static = fst (get_domain_static_information static)

  let get_store_agents_test_without_created static = snd (get_domain_static_information static)

  let set_store_agents_test agents static =
    let agents_test' = get_store_agents_test_without_created static in
    {
      static with
        domain_static_information = agents, agents_test'
    }

  let set_store_agents_test_without_created agents static =
    let agents_test' = get_store_agents_test static in
    {
      static with
        domain_static_information = agents_test', agents
    }

  let add_link rule_id (agent_id, agent_type) static error =
    let parameter = get_parameter static in
    let result = get_store_agents_test static in
    let error, old_list =
      match Int2Map_Agent.Map.find_option_without_logs parameter error rule_id result with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let list = (agent_id, agent_type) :: old_list in
    let error, result =
      Int2Map_Agent.Map.add_or_overwrite parameter error rule_id list result
    in
    let static = set_store_agents_test result static in
    error, static

  let collect_agents_test static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let error, static =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error agent_id agent static ->
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost -> error, static
          | Cckappa_sig.Dead_agent (agent, _, _, _)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let error, static =
              add_link rule_id (agent_id, agent_type) static error
            in
            error, static
        ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views static
    in
    error, static

  (* to do: collect the agent type of the agents of the species and declare
     them seen *)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  let is_enabled static dynamic error rule_id precondition =
    (* TO DO, check that the type of each agent in the lhs has been already seen
       forall to test
    *)
    error, dynamic, Some precondition

  let apply_rule static dynamic error rule_id precondition =
    (*TO DO: fold list of creation each time update the array when agent is
      seen for the first time, add list of rule inside event list that
      contain the list of rules in the lhs.  a map from agent to
      rule. event_list need to be updated, add rules that this agents
      apply. *)
    let event_list = [] in
    error, dynamic, event_list

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list' = [] in
    error, dynamic, event_list'

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)

  let print static dynamic error loggers =
    (* TO DO: as in dead rules. dead agents *)
    error, dynamic, ()

end
