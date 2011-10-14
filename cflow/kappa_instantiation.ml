(**
  * kappa_instantiation.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 29/08/2011
  * Last modification: 29/09/2011
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let compose f g = (fun x -> f (g x))

module type Cflow_signature =
sig
  type agent_name
  type site_name 
  type agent_id 
  type agent 
  type site 
  type internal_state 
  type binding_type 
  type binding_state
  type kappa_sig 

  val agent_id_of_agent: agent -> agent_id 
  val agent_name_of_agent: agent -> agent_name
  val agent_of_site: site -> agent 
  val agent_id_of_site: site -> agent_id 
  val agent_name_of_site: site -> agent_name 
  val site_name_of_site: site -> site_name 
  val agent_name_of_binding_type: binding_type -> agent_name
  val site_name_of_binding_type: binding_type -> site_name 

  val build_agent: agent_id -> agent_name -> agent 
  val build_site: agent -> site_name -> site 

  val get_binding_sites: kappa_sig -> agent_name -> site_name list 

  type test
  type action  
  type event 
  type refined_event
  type rule 
  type embedding
  type fresh_map 

  val rule_of_event: event -> rule 
  val embedding_of_event: event -> embedding
  val fresh_map_of_event: event -> fresh_map
  val event_of_refined_event: refined_event -> event
  val rule_of_refined_event: refined_event -> rule 
  val embedding_of_refined_event: refined_event -> embedding
  val fresh_map_of_refined_event: refined_event -> fresh_map

  val tests_of_refined_event: refined_event -> test list 
  val actions_of_refined_event: refined_event -> kappa_sig -> action list * (site*binding_state) list 

  
end 



module Cflow_linker = 
(struct 
  type agent_name = int
  type site_name = int 
  type agent_id = int 
  type agent = agent_id * agent_name 
  type site = agent * site_name 
  type rule = Dynamics.rule
  type embedding = int Mods.IntMap.t 
  type fresh_map = int Mods.IntMap.t 

  type event = rule * embedding * fresh_map
  type refined_event = event

  type kappa_sig = site_name  list Mods.IntMap.t

  let get_binding_sites map x = 
    try 
      Mods.IntMap.find x map 
    with 
      | Not_found -> 
	  failwith "Kappa_instantiation, line 94" 

  let fresh_map_of_event (_,_,x) = x 
  let event_of_refined_event x = x 
  let fresh_map_of_refined_event = compose fresh_map_of_event event_of_refined_event
  let embedding_of_event (_,x,_) = x 
  let rule_of_event (x,_,_) = x 
  let rule_of_refined_event = compose rule_of_event event_of_refined_event
  let embedding_of_refined_event = compose embedding_of_event event_of_refined_event 


  type internal_state  = int 
  type binding_type = agent_name * site_name 
  type binding_state = 
    | ANY 
    | FREE 
    | BOUND 
    | BOUND_TYPE of binding_type 
    | BOUND_to of site 

  let agent_id_of_agent = fst 
  let agent_name_of_agent = snd 
  let agent_of_site = fst 
  let agent_id_of_site = compose agent_id_of_agent agent_of_site
  let agent_name_of_site = compose agent_name_of_agent agent_of_site 
  let site_name_of_site = snd 
  let agent_name_of_binding_type = fst
  let site_name_of_binding_type = snd 

  let lhs_of_rule rule = rule.Dynamics.lhs 
  let lhs_of_refined_event = compose lhs_of_rule rule_of_refined_event
    
  let get_agent agent_id refined_event fresh_map = 
      let i,map = 
      match agent_id 
      with 
	| Dynamics.KEPT i -> i,
	     let lhs = lhs_of_refined_event refined_event in 
	       Mixture.agents lhs 
	| Dynamics.FRESH i -> 
	    i,fresh_map 
    in 
	try 
	  Mods.IntMap.find i map 
	with 
	  | Not_found -> failwith "kappa_instantiation, line 130"

  let name_of_agent agent_id refined_event fresh_map = 
    let agent = get_agent agent_id refined_event fresh_map in 
      Mixture.name agent

  let get_binding_state_of_site agent_id site_name refined_event fresh_map =
    let agent = get_agent agent_id refined_event fresh_map in 
      try 
	let interface = Mixture.interface agent in 
	  match snd (Mods.IntMap.find site_name interface)
	  with 
	    | Node.WLD -> ANY
	    | Node.FREE -> FREE
	    | Node.BND -> BOUND
	    | Node.TYPE(agent_name,site_name) -> BOUND_TYPE(agent_name,site_name)
      with 
	  Not_found -> ANY

  let build_site a b = (a,b)
  let build_agent a b = (a,b)

  let order_site site1 site2 = 
    let agent_id1,agent_id2 = agent_id_of_site site1,agent_id_of_site site2 in
    match compare  agent_id1 agent_id2 
    with 
      | -1  -> site1,site2
      | +1 -> site2,site1
      | 0 -> 
	  begin
	    let agent_name1,agent_name2 = agent_name_of_site site1,agent_name_of_site site2 in 
	      match compare agent_name1 agent_name2 
	      with 
		| -1 -> site1,site2
		| +1 -> site2,site1
		| 0 -> 
		    begin
		      let site_name1,site_name2 = site_name_of_site site1,site_name_of_site site2 in 
			match compare site_name1 site_name2 
			with 
			  | -1 -> site1,site2 
			  | _  -> site2,site1
		    end
		| _ -> failwith "ERROR, kappa_instantiation, line 109"
	  end
      | _ -> failwith "ERROR, kappa_instantiation, line 111"
      

  type test = 
    | Is_Here of agent
    | Has_Internal of site * internal_state 
    | Is_Free of site 
    | Is_Bound of site
    | Has_Binding_type of site * binding_type 
    | Is_Bound_to of site * site 

  type action = 
    | Create of agent 
    | Mod_internal of site * internal_state 
    | Bind of site * site 
    | Unbind of site * site  
    | Free of site 
    | Destroy of site 
    | Remove of agent 
	
  let apply_fun f refined_event id = 
    let embedding = f refined_event in 
    try 
      Mods.IntMap.find id embedding 
    with 
	Not_found -> failwith "Kappa_instantiation.ml/apply_fun/93"
	  (* WARNING, to complete with newly created agents *)

  let apply_embedding = apply_fun embedding_of_refined_event 
  let apply_fresh_map = apply_fun fresh_map_of_refined_event 

  let apply_embedding_on_action event id = 
    match id 
    with 
      | Dynamics.KEPT i -> apply_embedding event i 
      | Dynamics.FRESH i -> apply_fresh_map event i 

   

  let tests_of_refined_event refined_event  = 
    let rule = rule_of_refined_event refined_event in 
    let lhs = rule.Dynamics.lhs in 
      Mods.IntMap.fold 
	(fun lhs_id ag list -> 
	   let agent_id = apply_embedding refined_event lhs_id in 
	   let agent_name = Mixture.name ag in 
	   let agent = build_agent agent_id agent_name in 
	   let list = Is_Here(agent)::list in 
	     Mixture.fold_interface 
	       (fun site_id (int,lnk) list -> 
		  let site = build_site agent site_id in 
		  let list = 
		    match int with 
		      | Some i -> Has_Internal(site,i)::list
		      | None -> list
		  in 
		    match lnk with 
		      | Node.WLD -> list 
		      | Node.FREE -> Is_Free(site)::list 
		      | Node.BND -> Is_Bound(site)::list
		      | Node.TYPE(agent_name,site_name) -> Has_Binding_type(site,(agent_name,site_name))::list 
	       )
	       ag list
	)
	(Mixture.agents lhs) []
	
  let actions_of_refined_event refined_event kappa_sig = 
    let a,b,_ = 
      List.fold_left
	(fun (list_actions,side_sites,(fresh:Mixture.agent Mods.IntMap.t)) action -> 
	   match action 
	   with 
	     | Dynamics.BND((lhs_id1,site1),(lhs_id2,site2)) ->
		 let agent_id1 = apply_embedding_on_action refined_event lhs_id1 in 
		 let agent_name1 = name_of_agent lhs_id1 refined_event fresh in 
		 let agent1 = build_agent agent_id1 agent_name1 in
		 let site1 = build_site agent1 site1 in 
		 let agent_id2 = apply_embedding_on_action refined_event lhs_id2 in 
		 let agent_name2 = name_of_agent lhs_id2 refined_event fresh in 
		 let agent2 = build_agent agent_id2 agent_name2 in
		 let site2 = build_site agent2 site2 in 
		 let site1,site2 = order_site site1 site2 in 
		   (
		     Bind(site1,site2)::list_actions,
		     side_sites,
		     fresh
		   )
	     | Dynamics.FREE((lhs_id,site_name),bool) ->
		 let agent_id = apply_embedding_on_action refined_event lhs_id in 
		 let agent_name = name_of_agent lhs_id refined_event fresh in
		 let agent = build_agent agent_id agent_name in 
		 let site = build_site agent site_name in 
		   (
		     Free (site)::list_actions,
		     (if bool 
		      then side_sites 
		      else 
			let state = get_binding_state_of_site lhs_id site_name refined_event fresh in 
			  (site,state)::side_sites
		     ),
		     fresh
		   )
	     | Dynamics.MOD((lhs_id,site),internal) -> 
		 let agent_id = apply_embedding_on_action refined_event lhs_id in 
		 let agent_name = name_of_agent lhs_id refined_event fresh in 
		 let agent = build_agent agent_id agent_name in 
		 let site = build_site agent site in 
		   Mod_internal(site,internal)::list_actions,
		   side_sites,
		   fresh
		   
	     | Dynamics.DEL(lhs_id) -> 
		 let fake_id = Dynamics.KEPT lhs_id in 
		 let agent_id = apply_embedding refined_event lhs_id in 
		 let agent_name = name_of_agent fake_id refined_event fresh in 
		 let agent = build_agent agent_id agent_name in 
		 let interface = get_binding_sites kappa_sig agent_name in 
		   Remove(agent)::list_actions,
		   List.fold_left 
		     (fun list site -> 
			let state = get_binding_state_of_site fake_id  site refined_event fresh in 
			  if state = FREE 
			  then list
			  else (build_site agent site,state)::list
		     )
		     side_sites interface,
		 fresh 
		
	     | Dynamics.ADD(rhs_id,agenttype) -> 
		 let agent_id = apply_embedding_on_action refined_event (Dynamics.FRESH rhs_id) in 
		   list_actions,side_sites,fresh)
	([],[],Mods.IntMap.empty)
	(rule_of_refined_event refined_event).Dynamics.script 
    in a,b
	
end:Cflow_signature)
