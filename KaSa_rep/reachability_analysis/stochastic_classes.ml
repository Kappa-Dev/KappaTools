(**
  * stochastic_classes.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of March
  * Last modification: 
  * 
  * Compute the relations between sites in an agent.
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic_classes") message exn
                 (fun () -> default)

let trace = false
              
let empty_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, stochastic_classes =
    Stochastic_classes_type.AgentMap.create parameter error n_agents in
  error,
  {
    Stochastic_classes_type.stochastic_classes_lhs = stochastic_classes;
    Stochastic_classes_type.stochastic_classes_rhs = stochastic_classes
  }

let agent_sites parameter error agent =
   let sites_list =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_list ->
        site :: current_list)
      agent.Cckappa_sig.agent_interface []
   in sites_list
 
let get_agent_sites parameter error agent views =
  let error, get_agent =
    Stochastic_classes_type.AgentMap.get
      parameter
      error
      agent.Cckappa_sig.agent_name
      views
  in
  match get_agent with
    | None 
    | Some Cckappa_sig.Ghost -> []
    | Some Cckappa_sig.Agent agent1 ->
      let sites_list1 =
	agent_sites parameter error agent1
      in
      let sites_list =
	Cckappa_sig.Site_map_and_set.fold_map
	  (fun site _ current_class ->
	    site :: current_class)
	  agent.Cckappa_sig.agent_interface sites_list1
      in
      sites_list

let scan_rule parameter error handler rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let viewsrhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let creation = rule.Cckappa_sig.actions.Cckappa_sig.creation in
  let stochastic_classes_lhs = classes.Stochastic_classes_type.stochastic_classes_lhs in
  let stochastic_classes_rhs = classes.Stochastic_classes_type.stochastic_classes_rhs in
  (*creation*)
  let error, stochastic_classes_rhs =
    List.fold_left
      (fun (error, stochastic_classes_rhs) (agent_id, agent_type) ->
      (*get agent on the right hand side*)
	let error, agent =
	  Stochastic_classes_type.AgentMap.get
	    parameter
	    error
	    agent_id
	    viewsrhs
	in
	match agent with
	  | None
	  | Some Cckappa_sig.Ghost -> error, stochastic_classes_rhs
	  | Some Cckappa_sig.Agent agent ->
	    let sites_classes =
	      get_agent_sites parameter error agent viewsrhs
	    in
	    (*get the sites of this agent*)
	    let error, stochastic_classes_rhs =
	      Stochastic_classes_type.AgentMap.set
		parameter
		error
		agent_type
		(List.rev sites_classes :: [])
		stochastic_classes_rhs
	    in
	    error, stochastic_classes_rhs
      ) (error, stochastic_classes_rhs) creation
  in
  (*Left hand side*)
  let error, stochastic_classes_lhs =
    Stochastic_classes_type.AgentMap.fold
      parameter
      error
      (fun parameter error agent_id agent stochastic_classes_lhs ->
	match agent with
	  | Cckappa_sig.Ghost -> error, stochastic_classes_lhs
	  | Cckappa_sig.Agent agent ->
	    let sites_classes =
	      agent_sites parameter error agent
	    (*get_agent_sites parameter error agent viewslhs*)
	    in
	    (*get agent in stochastic classes *)
	    let agent_type = agent.Cckappa_sig.agent_name in
            (*store all sites associated with agent_type*)
	    let error, stochastic_classes_lhs =
	      Stochastic_classes_type.AgentMap.set
		parameter
		error
		agent_type
		(List.rev sites_classes :: [])
		stochastic_classes_lhs
            in
            error, stochastic_classes_lhs
      ) viewslhs stochastic_classes_lhs
  in
  error,
  {
    Stochastic_classes_type.stochastic_classes_lhs = stochastic_classes_lhs;
    Stochastic_classes_type.stochastic_classes_rhs = stochastic_classes_rhs
  }

let result parameter error =
  let error, init = Stochastic_classes_type.AgentMap.create
    parameter error 0 in
  Stochastic_classes_type.AgentMap.fold
    parameter
    error
    (fun parameter error id classes init ->
      let error, eq_classes =
        Union_find.union_dic parameter error classes
      in
      Stochastic_classes_type.AgentMap.set
        parameter error id eq_classes init)
     
let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  (*map each agent to a stochastic classes*)
  let error, agent_map =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
       scan_rule
         parameter
         error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         classes
      ) rules init
  in
  let error, init = Stochastic_classes_type.AgentMap.create
    parameter error 0 in
  let error, result_lhs =
    result parameter error
      agent_map.Stochastic_classes_type.stochastic_classes_lhs
      init
  in
  let error, result_rhs =
    result parameter error
      agent_map.Stochastic_classes_type.stochastic_classes_rhs
      init
  in
  let _ = error, result_lhs in
  error, result_rhs
             
let print parameter error result =
  Stochastic_classes_type.AgentMap.print
    error
    (fun error parameter l ->
      let _ =
	print_string "site_type:{";
	Union_find.print_list l ;
	print_string "}"; print_newline ()
      in error)
    parameter
    result
    
let stochastic_classes parameter error handler cc_compil =
  let parameter =  Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print parameter error result in
  error, result
