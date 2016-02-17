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

  module Int2Map_Agent =
    Map_wrapper.Make
      (SetMap.Make
         (struct
           type t = int
           let compare = compare
          end
         ))

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : (int list * int list) Int2Map_Agent.Map.t
    }

  (*--------------------------------------------------------------------*)
  (* array that indicates whether an agent type is already discovered, or
     not: from the beginning everything will be set to false*)

  type local_dynamic_information = bool array

  type dynamic_information =
    {
      local  : local_dynamic_information;
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

  (**domain *)

  let get_domain_static_information static = static.domain_static_information

  let set_domain_static_information domain static =
    {
      static with
        domain_static_information = domain
    }

  (*--------------------------------------------------------------------*)
  (** global static/dynamic information*)

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
  (**initialize*)

  let collect_agents parameter error rule =
    let error, agents_test_list =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error agent_id agent current_list ->
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost -> error, current_list
          | Cckappa_sig.Dead_agent _ ->
            warn parameter error (Some "line 49") Exit current_list
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let agent_list = agent_type :: current_list in
            error, agent_list
        ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views []
    in
    let error, agents_created_list =
      List.fold_left (fun (error, current_list) (agent_id, agent_type) ->
        let agent_list = agent_type :: current_list in
        error, agent_list
      ) (error, []) rule.Cckappa_sig.actions.Cckappa_sig.creation
    in
    error, (agents_test_list, agents_created_list)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, result =
      Int_storage.Nearly_inf_Imperatif.fold
        parameter error
        (fun parameter error rule_id rule store_result ->
          let error, (agents_test_list, agents_created_list) =
            collect_agents
              parameter
              error
              rule.Cckappa_sig.e_rule_c_rule
          in
          (*add rule_id in map*)
          let error, map =
            Int2Map_Agent.Map.add_or_overwrite parameter error rule_id 
              (agents_test_list, agents_created_list) store_result
          in
          error, map
        ) compil.Cckappa_sig.rules Int2Map_Agent.Map.empty
    in
    let static = set_domain_static_information result static in
    error, static, dynamic
    
  (**************************************************************************)

  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = Int2Map_Agent.Map.empty
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let nagents = Handler.nagents parameter error kappa_handler in
    let init_seen_agents_array = Array.make (nagents+1) false in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local  = init_seen_agents_array;
      }
    in
    scan_rule_set init_global_static_information init_global_dynamic_information error

  (**************************************************************************)
  (** collect the agent type of the agents of the species and declare
     them seen *)

  let init_agents static dynamic error init_state =
    let parameter = get_parameter static in
    let error, dynamic =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error agent_id agent dynamic ->
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost -> error, dynamic
          | Cckappa_sig.Dead_agent _ -> warn parameter error (Some "line 331") Exit dynamic
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let local = get_seen_agent dynamic in
            let local =
              local.(agent_type) <- true;
              local
            in
            let dynamic = set_seen_agent local dynamic in
            error, dynamic
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views dynamic
    in
    error, dynamic

  (************************************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    let error, dynamic = init_agents static dynamic error species in
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
    let parameter = get_parameter static in
    let domain_static = get_domain_static_information static in
    let error, (l, _) =
      match Int2Map_Agent.Map.find_option_without_logs parameter error
        rule_id domain_static
      with
      | error, None -> error, ([], [])
      | error, Some (l1, l2) -> error, (l1, l2)
    in
    List.fold_left (fun (error, dynamic, s) agent_type ->
      let local = get_seen_agent dynamic in
      let bool = Array.get local agent_type in
      if bool
      then
        error, dynamic, s
      else
        error, dynamic, None
    ) (error, dynamic, Some precondition) l

  (************************************************************************************)
  (** fold a list of creation each time update the array when agent is
      seen for the first time, add list of rule inside event list that
      contain the list of rules in the lhs.  a map from agent to
      rule. event_list need to be updated, add rules that this agents
      apply. *)

  (*JF: just declare each agent types in that list to be seen *)

  let apply_rule static dynamic error rule_id precondition =
    let parameter = get_parameter static in
    let event_list = [] in
    let domain_static = get_domain_static_information static in
    let error, (_, l) =
      match Int2Map_Agent.Map.find_option_without_logs
        parameter error rule_id domain_static
      with
      | error, None -> error, ([], [])
      | error, Some (l1, l2) -> error, (l1, l2)
    in
    let dynamic, event_list =
      List.fold_left (fun (dynamic, event_list) agent_type ->
        let local = get_seen_agent dynamic in
        let bool = Array.get local agent_type in
        if not bool
        then
          let local =
            local.(agent_type) <- true;
            local
          in
          let dynamic = set_seen_agent local dynamic in
          let event_list =
            Analyzer_headers.Check_rule rule_id :: event_list
          in
          dynamic, event_list
        else
          dynamic, event_list
      ) (dynamic, event_list) l
    in
    (*JF: Here, you should add in the event list, each rule that test for
      an agent with a type among the ones you have newly see, and with an empty
      interface (no test) *)
    (*let agents_test = get_agents_test static in
    let error, list =
      match Agents_domain_test.Int2Map_Agent.Map.find_option_without_logs
        parameter error rule_id agents_test
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let local = get_seen_agent dynamic in
    let event_list =
      List.fold_left (fun event_list agent_type ->
        let bool = Array.get local agent_type in
        if bool
        then
        (*seen*)
          Analyzer_headers.Check_rule rule_id :: event_list
        else
          event_list
      ) event_list list
    in*)
    error, dynamic, event_list

  (************************************************************************************)
  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list' = [] in
    error, dynamic, event_list'

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)

  let print_dead_agent static dynamic error =
    let parameter = get_parameter static in
    let local = get_seen_agent dynamic in
    let handler = get_kappa_handler static in
    if Remanent_parameters.get_dump_reachability_analysis_result parameter
    then
      let parameter = Remanent_parameters.update_prefix parameter "" in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () = 
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "* Dead agents :"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------" 
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let size = Array.length local in
      let rec aux k error =
        if k = size then error
        else
	  let bool = Array.get local k in
	  let error =
	    if bool
	    then
	      error
	    else
	      let error', agent_string =
                try
                  Handler.string_of_agent parameter error handler k
                with
                  _ -> warn parameter error (Some "line 238") Exit (string_of_int k)
	      in
	      let error =
                Exception.check warn parameter error error' (Some "line 234") Exit
              in
              let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "%s is a dead agent." agent_string
	      in
	      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
	      error
	  in aux (k+1) error
      in aux 0 error
    else
      error

  let print static dynamic error loggers =
    let error =
      print_dead_agent static dynamic error
    in
    error, dynamic, ()

end
