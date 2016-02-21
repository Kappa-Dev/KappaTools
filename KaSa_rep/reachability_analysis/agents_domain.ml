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
      domain_static_information :
        (int list Analyzer_headers.bot_or_not * int list) Int2Map_Agent.Map.t;
      agents_without_interface  : int list Int2Map_Agent.Map.t 
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

  let get_agents_without_interface static = static.agents_without_interface

  let set_agents_without_interface agents static =
    {
      static with
        agents_without_interface = agents
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

  let map_to_list parameter error map =
    let error, list =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error key a current_list ->
          let list = a :: current_list in
          error, list
        ) map []
    in
    error, list

  let collect_agents parameter error rule =
    let error, list_lhs_views = 
      map_to_list parameter error rule.Cckappa_sig.rule_lhs.Cckappa_sig.views 
    in
    let error, agents_test_list =
      let rec aux l (error, output) =
        match l with
        | [] -> error, Analyzer_headers.Not_bot output
        | agent :: tl ->
          match agent with
          | Cckappa_sig.Ghost -> aux tl (error, output)
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Dead_agent _ -> error, Analyzer_headers.Bot
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            aux tl (error, agent_type :: output)
      in
      aux list_lhs_views (error, [])
    in
    let error, agents_created_list =
      List.fold_left (fun (error, current_list) (agent_id, agent_type) ->
        let agent_list = agent_type :: current_list in
        error, agent_list
      ) (error, []) rule.Cckappa_sig.actions.Cckappa_sig.creation
    in
    error, (agents_test_list, agents_created_list)

  (*FIXME*)
  let collect_agents_without_interface parameter error rule_id rule map =
    let error, agents_lhs_list =
      map_to_list parameter error rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    in
    let rec aux l (error, output) =
      match l with
      | [] -> error, Analyzer_headers.Not_bot output
      | agent :: tl ->
	 begin
	   match agent with
           | Cckappa_sig.Ghost -> aux tl (error, output)
           | Cckappa_sig.Unknown_agent _
           | Cckappa_sig.Dead_agent _ -> error, Analyzer_headers.Bot
           | Cckappa_sig.Agent agent ->
              let agent_type = agent.Cckappa_sig.agent_name in
	      let agent_interface = agent.Cckappa_sig.agent_interface in
              if Cckappa_sig.Site_map_and_set.Map.is_empty agent_interface
	      then
		aux tl (error, agent_type::output)
	      else
		aux tl (error, output)
	 end
    in
    match
      aux agents_lhs_list (error, []) 
    with
    | error, Analyzer_headers.Bot -> error, map
    | error, Analyzer_headers.Not_bot l ->
       List.fold_left
	 (fun (error,map) agent_type ->
	  let error, old_list =
            match
	      Int2Map_Agent.Map.find_option_without_logs
		parameter
		error
		agent_type
		map
            with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let rule_id_list = rule_id :: old_list in
          Int2Map_Agent.Map.add_or_overwrite parameter error agent_type
					     rule_id_list map
	 )
         (error,map) l
	 
  (*let collect_agents_without_interface' parameter error rule_id rule store_result =
    let error, store_result =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error agent_id agent store_result ->
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost
          | Cckappa_sig.Dead_agent _ -> error, store_result (*output bot*)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let agent_interface = agent.Cckappa_sig.agent_interface in
            if Cckappa_sig.Site_map_and_set.Map.is_empty agent_interface
            then
              (*empty interface*)
              let error, old_list =
                match Int2Map_Agent.Map.find_option_without_logs parameter error
                  agent_type store_result
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let rule_id_list = rule_id :: old_list in
              let error, store_result =
                Int2Map_Agent.Map.add_or_overwrite parameter error
                  agent_type rule_id_list store_result
              in
              error, store_result
            else
              error, store_result
        ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
    in
    error, store_result*)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, (result, agents) =
      Int_storage.Nearly_inf_Imperatif.fold
        parameter error
        (fun parameter error rule_id rule (store_result, agents_without_interface) ->
          let error, (agents_test_list, agents_created_list) =
            collect_agents
              parameter
              error
              rule.Cckappa_sig.e_rule_c_rule
          in
          (*add rule_id in map*)
          let error, result =
            Int2Map_Agent.Map.add_or_overwrite parameter error rule_id
              (agents_test_list, agents_created_list) store_result
          in
          (*agents without interface*)
          let error, agents_without_interface =
            collect_agents_without_interface
              parameter
              error
              rule_id
              rule.Cckappa_sig.e_rule_c_rule
	      agents_without_interface
          in
          error, (result, agents_without_interface)
        ) compil.Cckappa_sig.rules (Int2Map_Agent.Map.empty, Int2Map_Agent.Map.empty)
    in
    let static = set_agents_without_interface agents static in
    let static = set_domain_static_information result static in
    error, static, dynamic

  (**************************************************************************)

  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = Int2Map_Agent.Map.empty;
        agents_without_interface  = Int2Map_Agent.Map.empty;
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
    let error, static, dynamic =
      scan_rule_set init_global_static_information init_global_dynamic_information error
    in
    error, static, dynamic

  (************************************************************************************)
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

  (*FIXME*)
  let add_event_list static dynamic error agent_type event_list =
    let parameter = get_parameter static in
    let map = get_agents_without_interface static in
    let local = get_seen_agent dynamic in
    let bool = Array.get local agent_type in
    if not bool
    then
      let local = 
        local.(agent_type) <- true;
        local
      in
      let dynamic = set_seen_agent local dynamic in
      let error, rule_id_list = 
            match Int2Map_Agent.Map.find_option_without_logs parameter error
							     agent_type map
            with
            | error, None -> error, []
            | error, Some l -> error, l
      in
      let event_list =          
        List.fold_left (fun event_list rule_id ->
			Analyzer_headers.Check_rule rule_id :: event_list
		       ) event_list rule_id_list
      in
      error, (dynamic, event_list)
    else
      error, (dynamic, event_list)
	       
  (**************************************************************************)
  (** collect the agent type of the agents of the species and declare
     them seen *)

  let init_agents static dynamic error init_state event_list =
    let parameter = get_parameter static in
    let error, (dynamic, event_list) =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error index_init agent (dynamic, event_list) ->
          match agent with
	  (*JF: warn: dead,unknown,ghost should not occur in initial states *)
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost
          | Cckappa_sig.Dead_agent _ ->
            warn parameter error (Some "line 275") Exit (dynamic, event_list)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let error, (dynamic, event_list) =
              add_event_list
                static
                dynamic
                error
                agent_type
                event_list
            in
            error, (dynamic, event_list)
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views (dynamic, event_list)
    in
    error, (dynamic, event_list)

  (************************************************************************************)

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
    let parameter = get_parameter static in
    let domain_static = get_domain_static_information static in
    let error, (bot_or_not, _) =
      match Int2Map_Agent.Map.find_option_without_logs parameter error
        rule_id domain_static
      with
      | error, None -> error, (Analyzer_headers.Not_bot [], [])
      | error, Some (l1, l2) -> error, (l1, l2)
    in
    match bot_or_not with
    | Analyzer_headers.Bot ->
       let _ = Printf.fprintf stdout "BOT\n" in
       error, dynamic, None
    | Analyzer_headers.Not_bot l ->
       List.fold_left
	 (fun (error, dynamic, s) agent_type ->
	  let _ = Printf.fprintf stdout "NOT BOT\n" in
	  let local = get_seen_agent dynamic in
	  let bool = Array.get local agent_type in
	  if bool
	  then
	    error, dynamic, s
	  else
	    error, dynamic, None
	 )
	 (error, dynamic, Some precondition) l

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
    let error, (_, list_created) =
      match Int2Map_Agent.Map.find_option_without_logs
        parameter error rule_id domain_static
      with
      | error, None -> error, (Analyzer_headers.Bot, [])
      | error, Some (l1, l2) -> error, (l1, l2)
    in
    let error, (dynamic, event_list) =
      List.fold_left (fun (error, (dynamic, event_list)) agent_type ->
        let error, (dynamic, event_list) =
          add_event_list
            static
            dynamic
            error
            agent_type
            event_list
        in
        error, (dynamic, event_list)
      ) (error, (dynamic, event_list)) list_created
    in
    error, dynamic, (precondition, event_list)

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
