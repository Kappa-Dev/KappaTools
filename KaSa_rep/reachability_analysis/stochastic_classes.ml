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
                   
let add_generic parameter error agent_id key map = (*REMOVE*)
  let get_agent =
    Stochastic_classes_type.AgentMap.unsafe_get
      parameter
      error
      key
      map
  in
  let error, old_agent =
    match get_agent with
      | error, None ->
         Stochastic_classes_type.AgentMap.create parameter error 0
      | error, Some a -> error, a
  in
  let error, new_agent=
    Stochastic_classes_type.AgentMap.set
      parameter
      error
      agent_id
      old_agent
      map
  in new_agent

let add_agent parameter error agent_id agent_type = (*REMOVE*)
  add_generic
    parameter
    error
    agent_id
    agent_type

let agent_creation parameter error viewsrhs creation = (*REMOVE*)
  let error, agent_modif_plus =
    Stochastic_classes_type.AgentMap.create parameter error 0
  in
  List.fold_left (fun (error, agent_modif_plus) (agent_id, agent_type) ->
    let error, get_agent =
      Stochastic_classes_type.AgentMap.get
        parameter
        error
        agent_id
        viewsrhs
    in
    match get_agent with
      | None -> warn parameter error (Some "line 242") Exit agent_modif_plus
      | Some Cckappa_sig.Ghost -> error, agent_modif_plus
      | Some Cckappa_sig.Agent agent ->
         error,
	 add_agent parameter error
                   agent_id
                   agent_type
                   agent_modif_plus)
                 (error, agent_modif_plus) creation
                 
let agent_sites parameter error agent =
   let sites_list =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_list ->
        site :: current_list)
      agent.Cckappa_sig.agent_interface []
  in sites_list

let scan_rule parameter error handler rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let viewsrhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let creation = rule.Cckappa_sig.actions.Cckappa_sig.creation in
  let error, agent_modif_plus =
    Stochastic_classes_type.AgentMap.create parameter error 0 in
  let stochastic_classes_lhs = classes.Stochastic_classes_type.stochastic_classes_lhs in
  let stochastic_classes_rhs = classes.Stochastic_classes_type.stochastic_classes_rhs in
  (*creation*)
  let error, stochastic_classes_rhs =
    List.fold_left
      (fun (error, stochastic_classes_rhs) (agent_id, agent_type) ->
      (*get agent on the right hand side*)
	let error, agent =
	  Int_storage.Quick_Nearly_inf_Imperatif.get
	    parameter
	    error
	    agent_id
	    viewsrhs
	in
	match agent with
	  | None -> Stochastic_classes_type.AgentMap.create parameter error 0
	  | Some Cckappa_sig.Ghost -> error, stochastic_classes_rhs
	  | Some Cckappa_sig.Agent agent ->
	    (*get the sites of this agent*)
	    let sites_list =
	      agent_sites parameter error agent in
	    let error, stochastic_classes_rhs =
	      Stochastic_classes_type.AgentMap.set
		parameter
		error
		agent_type
		(List.rev sites_list :: [])
		stochastic_classes_rhs
	    in
	    error, stochastic_classes_rhs
      ) (error, stochastic_classes_rhs) creation
  in
  (*Left hand side*)
  let error, stochastic_classes_lhs =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent stochastic_classes_lhs ->
        match agent with
       | Cckappa_sig.Ghost -> error, stochastic_classes_lhs
       | Cckappa_sig.Agent agent ->
         (*get agent in stochastic classes *)
         let agent_type = agent.Cckappa_sig.agent_name in
         let sites_list =
           agent_sites parameter error agent  in
         (*store all sites associated with agent_type*)
         let error, stochastic_classes_lhs =
	   Stochastic_classes_type.AgentMap.set
	     parameter
             error
             agent_type
             (List.rev sites_list :: [])
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
    Stochastic_classes_type.AgentMap.fold
      parameter
      error
      (fun parameter error id classes init ->
       	let error, eq_classes =
          Union_find.union_dic parameter error classes in
        (*store the result*)
	Stochastic_classes_type.AgentMap.set
          parameter error id eq_classes init)
      agent_map.Stochastic_classes_type.stochastic_classes_lhs
      init
  in
  let error, result_rhs =
    Stochastic_classes_type.AgentMap.fold
      parameter
      error
      (fun parameter error id classes init ->
       	let error, eq_classes =
          Union_find.union_dic parameter error classes in
        (*store the result*)
	Stochastic_classes_type.AgentMap.set
          parameter error id eq_classes init)
      agent_map.Stochastic_classes_type.stochastic_classes_rhs
      init
  in
  error, result_lhs;
  error, result_rhs
             
let print_remanent parameter error result =
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
  (*let _ = print_remanent parameter error result in*)
  error, result
