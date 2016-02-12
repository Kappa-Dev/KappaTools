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

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  (*type of agents in the lhs after minus created agents: type (tested *
    created\tested): use list less space, because the array below will
    control everything*)

  type agents_lhs = (int list * int list) Bdu_analysis_type.Int2Map_Modif.Map.t
    
  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : agents_lhs

    (* TO DO, for each rule, the set of agent types of agents in the lhs,
       and the set of the agent types of created agents minus the set of the
       agent types of agents in the lhs *)
    }

  (*--------------------------------------------------------------------*)
  (* put here the type of the struct that contains the rest of the dynamic
     information, including the result of the analysis *)

  type local_dynamic_information = bool array 
  (* array that indicates whether an agent type is already discovered, or
     not: from the beginning everything will be set to false*)

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

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

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
        domain_static_information = Bdu_analysis_type.Int2Map_Modif.Map.empty;
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

  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list
  (* to do: collect the agent type of the agents of the species and declare them seen *)

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
