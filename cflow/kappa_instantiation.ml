(**
  * kappa_instantiation.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 29/08/2011
  * Last modification: 02/08/2015
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let debug_mode = false
let compose_with_handler f g parameter handler error x = 
  let error,y = g parameter handler error x in 
  f parameter handler error y 

module type Cflow_signature =
sig
  module H:Cflow_handler.Cflow_handler
  module P:StoryProfiling.StoryStats
  module PI = Instantiation

  type agent_id = int 
  module AgentIdSet:Set.S with type elt = agent_id

  type internal_state = int 

  (*
  type event 
  type embedding
  type fresh_map 
  type obs  
  type step *)
  type side_effect = PI.concrete PI.site list
(*  type kappa_rule *)
  type refined_event = Causal.event_kind * PI.concrete PI.event
  type refined_obs =
      Causal.event_kind * PI.concrete PI.test list * unit Mods.simulation_info
  type refined_step
(*  type obs_from_rule_app = (int * int Mods.IntMap.t) list 
  type r = Primitives.elementary_rule 
  type counter = int 
  type rule_info = obs_from_rule_app * r * counter * kasim_side_effect*)

  val empty_side_effect: side_effect
  val dummy_refined_step: string -> refined_step
  (*  val type_of_refined_step: refined_step -> (unit,unit,unit,unit) choice*)
  val agent_name_of_binding_type: PI.binding_type -> PI.agent_name
  val site_name_of_binding_type: PI.binding_type -> PI.site_name
  val agent_id_of_agent:
    PI.concrete -> int
  val agent_name_of_agent: PI.concrete -> PI.agent_name
  val agent_of_site: PI.concrete PI.site -> PI.concrete
  val agent_id_of_site: PI.concrete PI.site -> int
  val agent_name_of_site: PI.concrete PI.site -> PI.agent_name
  val site_name_of_site: PI.concrete PI.site -> PI.site_name
(*  val agent_name_of_binding_type: binding_type -> PI.agent_name
  val site_name_of_binding_type: binding_type -> PI.site_name
  val build_agent: agent_id -> agent_name -> agent 
  val build_site: agent -> site_name -> site 
  val get_binding_sites: (agent_name -> H.error_channel * site_name list) H.with_handler 
  val get_default_state: (agent_name -> H.error_channel * (site_name*internal_state option) list) H.with_handler 
  val rule_of_event: event -> kappa_rule 
  val btype_of_names : agent_name -> site_name -> binding_type 
  val embedding_of_event: event -> embedding
  val fresh_map_of_event: event -> fresh_map
  val refine_step: (step -> H.error_channel * refined_step) H.with_handler *)
  val build_subs_refined_step: int -> int -> refined_step
(*  val step_of_refined_step: (refined_step -> H.error_channel * step) H.with_handler
  val rule_of_refined_event: (refined_event -> H.error_channel * kappa_rule) H.with_handler*)
  val tests_of_refined_step: (refined_step -> H.error_channel * PI.concrete PI.test list) H.with_handler
  val actions_of_refined_step:
    (refined_step ->
     H.error_channel *
       (PI.concrete PI.action list *
	  (PI.concrete PI.site*PI.concrete PI.binding_state) list)) H.with_handler
  val is_obs_of_refined_step: refined_step -> bool
  val is_init_of_refined_step: refined_step -> bool
  val is_subs_of_refined_step: refined_step -> bool
  val is_event_of_refined_step: refined_step -> bool
  val simulation_info_of_refined_step:
    refined_step -> unit Mods.simulation_info option
(*
  val print_test: (string -> test -> H.error_channel) H.with_handler
  val print_action: (string -> action -> H.error_channel) H.with_handler *)
  val print_side:
    Format.formatter -> H.handler -> string  ->
    (PI.concrete PI.site*PI.concrete PI.binding_state) -> unit

  val print_refined_step:
    ?handler:H.handler -> Format.formatter -> refined_step -> unit

  val store_event:
    P.log_info -> refined_event -> refined_step list -> P.log_info * refined_step list
  val store_obs :
    P.log_info -> refined_obs  -> refined_step list -> P.log_info * refined_step list

  val build_grid:
    (refined_step * PI.concrete PI.site list * bool)  list -> bool ->
    H.handler -> Causal.grid
  val print_side_effect: Format.formatter -> side_effect -> unit
  val side_effect_of_list: PI.concrete PI.site list -> side_effect
(*  val no_obs_found: step list -> bool 

  val subs_agent_in_test: agent_id -> agent_id -> test -> test
  val subs_agent_in_action: agent_id -> agent_id -> action -> action
  val subs_agent_in_side_effect:
    agent_id -> agent_id -> (PI.concrete PI.site*binding_state) ->
    (PI.concrete PI.site*binding_state)

  val subs_map_agent_in_test: (agent_id -> agent_id) -> test -> test
  val subs_map_agent_in_action: (agent_id -> agent_id) -> action -> action
  val subs_map_agent_in_side_effect:
    (agent_id -> agent_id) -> (PI.concrete PI.site*binding_state) ->
    (PI.concrete PI.site*binding_state)*)

  val get_kasim_side_effects: refined_step -> side_effect

  val level_of_event: (refined_step -> (agent_id -> bool) -> H.error_channel * Priority.level) H.with_handler
  val disambiguate: refined_step list -> refined_step list
  val clean_events: refined_step list -> refined_step list
  val agent_id_in_obs: (refined_step -> H.error_channel * AgentIdSet.t) H.with_handler 

  val fill_siphon: refined_step list -> refined_step list

(*  val print_step: (refined_step -> H.error_channel) H.with_handler *)
  val agent_id_in_obs: (refined_step -> H.error_channel * AgentIdSet.t) H.with_handler
end



module Cflow_linker =
  (struct
    module H = Cflow_handler.Cflow_handler
    module P = StoryProfiling.StoryStats
    module PI = Instantiation

  type site_name = int

  type agent_id = int

  module AgIdSet = Set.Make (struct type t = agent_id let compare = compare end)
(*
  type kappa_rule = Primitives.elementary_rule

  type embedding = agent_id Mods.IntMap.t *)
  type side_effect = PI.concrete PI.site list
(*  type fresh_map = int Mods.IntMap.t 
  type obs_from_rule_app = (int * int Mods.IntMap.t) list 
  type r = Primitives.elementary_rule 
  type counter = int  
  type rule_info = (obs_from_rule_app * r  * counter * kasim_side_effect) 
  type obs = int * Mixture.t * embedding 
      
  let get_causal (_,d) = d *)

  module AgentIdMap = Map.Make (struct type t = agent_id let compare=compare end)
  module AgentIdSet = Set.Make (struct type t = agent_id let compare=compare end)
  module SiteMap = Map.Make (struct type t = site_name let compare=compare end)
  module SiteSet = Set.Make (struct type t = site_name let compare = compare end) 
  type internal_state  = int 

(*
  type test = 
    | Is_Here of agent
    | Has_Internal of site * internal_state 
    | Is_Free of site 
    | Is_Bound of site
    | Has_Binding_type of site * binding_type 
    | Is_Bound_to of site * site 

  type action = 
    | Create of agent * (site_name * internal_state option) list 
    | Mod_internal of site * internal_state 
    | Bind of site * site 
    | Free of site 
    | Remove of agent *)

  type refined_event =
      Causal.event_kind * PI.concrete PI.event
  type refined_obs =
      Causal.event_kind *
	PI.concrete PI.test list *
	  unit Mods.simulation_info
  type refined_step =
  | Subs of (agent_id * agent_id)
  | Event of refined_event
  | Init of string * PI.concrete PI.action list
  | Obs of refined_obs
  | Dummy  of string

(*
  let btype_of_names a b = (a,b)*)
  let build_subs_refined_step a b = Subs (a,b)
  let get_kasim_side_effects = function
    | Event ((_,(_,(_,_,a)))) -> a
    | Subs _ | Obs _ | Dummy _ | Init _ -> []

  let dummy_refined_step x = Dummy x
  let empty_side_effect = []
(*  let type_of_refined_step c = 
    match c 
    with 
      | Event _ -> Event ()
      | Subs (_,_) -> Subs ((),())
      | Obs _ -> Obs () 
      | Dummy x -> Dummy x *)

  let site_name_of_binding_type = snd
  let agent_name_of_binding_type = fst
(*  let map_sites parameter handler error f map x = 
      try 
        let rec aux k list = 
          if k=0 
          then list
          else aux (k-1) ((f x k (Environment.signatures map))::list)
        in error,aux (Signature.arity (Environment.signatures map) x -1) []
      with 
	  Not_found -> 
	    let error_list,error = 
              H.create_error
		parameter handler error (Some "kappa_instantiation.ml") None
		(Some "map_sites") (Some "265") (Some "Kappa_instantiation")
		(failwith "Kappa instantiation, line 265")  in
            H.raise_error parameter handler error_list error []


  let get_binding_sites parameter handler error = 
    map_sites parameter handler error 
      (fun _ k _ -> k)
      handler.H.env

  let get_default_state parameter handler error = 
    map_sites parameter handler error 
      (fun x k sigs -> (k,Signature.default_internal_state x k sigs))
      handler.H.env
  
  let fresh_map_of_event ((_,_,x),_) = x 
  
  let embedding_of_event ((_,x,_),_) = x 
  
  let rule_of_event ((x,_,_),_) = x
 *)
  let agent_id_of_agent = fst

  let agent_name_of_agent = snd

  let agent_of_site = fst

  let agent_id_of_site x = agent_id_of_agent @@ agent_of_site x

  let agent_name_of_site x = agent_name_of_agent @@ agent_of_site x

  let site_name_of_site = snd

(*  let agent_name_of_binding_type = fst

  let site_name_of_binding_type = snd

  let string_of_agent env agent =
    Format.asprintf "%a_%i" (Environment.print_agent ~env:env.H.env)
		    (agent_name_of_agent agent) (agent_id_of_agent agent)
 
  let string_of_site_name _env = string_of_int 
				 *)
  let print_site ?env f ((ag_id,ag),s) =
    Format.fprintf f "%a_%i.%a"
		   (Environment.print_agent ?env) ag ag_id
		   (match env with
		    | Some env ->
		       Signature.print_site (Environment.signatures env) ag
		    | None -> Format.pp_print_int) s

(*  let string_of_internal_state _env int = string_of_int int 
  
  let string_of_btype _env (agent_name,site_name) = (string_of_int agent_name)^"!"^(string_of_int site_name)
 *)
  let print_binding_state ?env f state =
    match state with
      | PI.ANY -> Format.fprintf f"*"
      | PI.FREE -> ()
      | PI.BOUND -> Format.fprintf f "!_"
      | PI.BOUND_TYPE (s,a) ->
	 Format.fprintf f "!%a.%a"
		   (match env with
		    | Some env ->
		       Signature.print_site (Environment.signatures env) a
		    | None -> Format.pp_print_int) s
			(Environment.print_agent ?env) a
      | PI.BOUND_to site ->
	 Format.fprintf f "!%a" (print_site ?env) site
  (*
  let print_event log env ((rule,emb,fresh),(rule_info)) =
    Kappa_printer.elementary_rule ~env log rule

  let print_obs log env obs = ()
   *)
  let print_side log hand prefix (s,binding_state) =
    let env = hand.H.env in
    Format.fprintf log "%s(%a,%a)\n" prefix (print_site ~env) s
		   (print_binding_state ~env) binding_state

(*  let lhs_of_rule rule = rule.Primitives.lhs 
  let lhs_of_event = compose lhs_of_rule rule_of_event
    
  let get_agent parameter handler error agent_id lhs fresh_map = 
    let i,map = 
      match agent_id 
      with 
	| Primitives.KEPT i -> (i,Mixture.agents lhs)
	| Primitives.FRESH i -> (i,fresh_map)
    in 
      try 
	error,Mods.IntMap.find i map 
      with 
	| Not_found -> 
            let error_list,error = 
              H.create_error
		parameter handler error (Some "kappa_instantiation.ml") None
		(Some "get_agent") (Some "401") (Some "Kappa_instantiation")
		(failwith "Kappa instantiation, line 401")  in
            H.raise_error parameter handler error_list error Mixture.dummy_agent
 	    
  let name_of_agent parameter handler error agent_id event fresh_map = 
    let error,agent = get_agent parameter handler error agent_id event fresh_map in 
    error,Mixture.name agent

  let build_kappa_agent name interface = 
    Mixture.create_agent 
      name
      (List.fold_left
	  (fun map (a,b) -> Mods.IntMap.add a (b,Mixture.FREE) map)
	  Mods.IntMap.empty interface)

  let build_site a b = (a,b)

  let build_agent a b = (a,b)

  let subs_agent id1 id2 agent = 
    if agent_id_of_agent agent = id1 then 
      build_agent id2 (agent_name_of_agent agent)
    else 
      agent
        
  let subs_site id1 id2 site = 
    let agent = agent_of_site site in 
    let agent' = subs_agent id1 id2 agent in 
    if agent==agent'
    then site
    else 
      build_site agent' (site_name_of_site site)

  let subs_map_agent f agent = 
    let agent_id = agent_id_of_agent agent in 
    try 
      build_agent (f agent_id) (agent_name_of_agent agent)
    with 
    | Not_found -> agent
    
      
        
  let subs_map_site f site = 
    let agent = agent_of_site site in 
    let agent' = subs_map_agent f agent in 
    if agent==agent'
    then site
    else 
      build_site agent' (site_name_of_site site)

  let subs_agent_in_test id1 id2 test = 
    match 
      test
    with 
    | Is_Here agent -> Is_Here (subs_agent id1 id2 agent)
    | Has_Internal (site,internal_state) -> Has_Internal (subs_site id1 id2 site,internal_state)
    | Is_Free site -> Is_Free (subs_site id1 id2 site)
    | Is_Bound site -> Is_Bound (subs_site id1 id2 site)
    | Has_Binding_type (site,binding_type) -> Has_Binding_type (subs_site id1 id2 site,binding_type)
    | Is_Bound_to (site1,site2) -> Is_Bound_to (subs_site id1 id2 site1,subs_site id1 id2 site2)

  let subs_map_agent_in_test f test = 
    match 
      test
    with 
    | Is_Here agent -> Is_Here (subs_map_agent f agent)
    | Has_Internal (site,internal_state) -> Has_Internal (subs_map_site f site,internal_state)
    | Is_Free site -> Is_Free (subs_map_site f site)
    | Is_Bound site -> Is_Bound (subs_map_site f site)
    | Has_Binding_type (site,binding_type) -> Has_Binding_type (subs_map_site f site,binding_type)
    | Is_Bound_to (site1,site2) -> Is_Bound_to (subs_map_site f site1,subs_map_site f site2)

  let subs_agent_in_action id1 id2 action = 
    match
      action
    with 
    | Create (agent,list) -> Create(subs_agent id1 id2 agent,list)
    | Mod_internal (site,i) -> Mod_internal(subs_site id1 id2 site,i)
    | Bind (s1,s2) -> Bind(subs_site id1 id2 s1,subs_site id1 id2 s2)
    | Free site -> Free (subs_site id1 id2 site)
    | Remove agent -> Remove (subs_agent id1 id2 agent)

   let subs_map_agent_in_action f action = 
    match
      action
    with 
    | Create (agent,list) -> Create(subs_map_agent f agent,list)
    | Mod_internal (site,i) -> Mod_internal(subs_map_site f site,i)
    | Bind (s1,s2) -> Bind(subs_map_site f s1,subs_map_site f s2)
    | Free site -> Free (subs_map_site f site)
    | Remove agent -> Remove (subs_map_agent f agent)
 *)
(*
  let apply_map id phi = 
    try 
      Mods.IntMap.find id phi 
    with 
      | Not_found -> 
        failwith "Kappa_instantiation.ml/apply_embedding/321"
 
  let apply_fun f event id = 
    apply_map id (f event)
     
  let apply_embedding = apply_fun embedding_of_event 
  let apply_fresh_map = apply_fun fresh_map_of_event 

  let apply_embedding_on_action event id = 
    match id 
    with 
      | Primitives.KEPT i -> apply_embedding event i 
      | Primitives.FRESH i -> apply_fresh_map event i 

  let get_binding_state_of_site parameter handler error agent_id site_name mixture embedding fresh_map =
    match agent_id 
    with 
      | Primitives.KEPT(id) ->
	  begin 
	    match 
	      Mixture.follow (id,site_name) mixture
	    with 
	      | Some (ag,site) -> 
		  let fake_id = Primitives.KEPT ag in 
		  let agent_id = apply_map ag embedding in 
		  let error,kappa_agent = get_agent parameter handler error fake_id mixture fresh_map in 
		  let agent_name = Mixture.name kappa_agent in 
		  let agent =  build_agent agent_id agent_name in 
		  let site = build_site agent site in 
		    error,BOUND_to (site)
	      | None -> 
		  begin 
		    let error,agent = get_agent parameter handler error agent_id mixture fresh_map in 
                    error,
		      try 
			let interface = Mixture.interface agent in 
			  match snd (Mods.IntMap.find site_name interface)
			  with
			    | Mixture.WLD -> ANY
			    | Mixture.FREE -> FREE
			    | Mixture.BND -> BOUND
			    | Mixture.TYPE(site_name,agent_name) ->
			       BOUND_TYPE(agent_name,site_name)
		      with 
			  Not_found -> ANY
		  end 
	  end
      | Primitives.FRESH _ -> error,ANY 



  let compare_site site1 site2 = 
    let agent_id1,agent_id2 = agent_id_of_site site1,agent_id_of_site site2 in
    match compare  agent_id1 agent_id2 
    with 
      | 0 -> 
	  begin 
	   let agent_name1,agent_name2 = agent_name_of_site site1,agent_name_of_site site2 in 
	      match compare agent_name1 agent_name2 
	      with 
		| 0 -> 
		    begin 
		       let site_name1,site_name2 = site_name_of_site site1,site_name_of_site site2 in 
			 compare site_name1 site_name2 
		    end
		| x -> x 
	  end
      | x -> x 

  let order_site site1 site2 = 
    match compare_site site1 site2
    with
      | -1 -> site2,site1
      | _ -> site1,site2
	
 
  let add_asso i j map = Mods.IntMap.add i j map 

  let add_bound_to site1 site2 list = 
    if compare_site site1 site2 = 1 
    then 
      (Is_Bound_to (site1,site2))::list
    else
      list

  let refine_bound_state parameter handler error site list list' fake_id lhs embedding = 
    let site_id = site_name_of_site site in 
    let error,state = get_binding_state_of_site parameter handler error fake_id site_id lhs embedding (Mods.IntMap.empty) in 
      begin
	match state 
	with 
	    BOUND_to (site2) -> 
	      error,add_bound_to site site2 list 
	  | _ -> error,list'
      end

  let tests_of_lhs parameter handler error lhs embedding =
    Mods.IntMap.fold 
	(fun lhs_id ag (error,list) -> 
	   let fake_id = Primitives.KEPT lhs_id in 
	   let agent_id = apply_map lhs_id embedding in 
	   let agent_name = Mixture.name ag in 
	   let agent = build_agent agent_id agent_name in 
	   Mixture.fold_interface 
	     (fun site_id (int,lnk) (error,list) -> 
	       if site_id = 0 
	       then error,Is_Here(agent)::list
	       else 
		 let site = build_site agent site_id in 
		 let list = 
		   match int with 
		   | Some i -> Has_Internal(site,i)::list
		   | None -> list
		 in 
		 let list' = 
		   match lnk with
		   | Mixture.WLD -> list
		   | Mixture.FREE -> Is_Free(site)::list
		   | Mixture.BND -> Is_Bound(site)::list
		   | Mixture.TYPE(site_name,agent_name) ->
		      Has_Binding_type(site,(agent_name,site_name))::list
		 in
		 refine_bound_state parameter handler error site list list' fake_id lhs embedding 
	     )
	     ag (error,list)
	)
      (Mixture.agents lhs) 
      (error,[])

  let tests_of_event parameter handler error event = 
    let rule = rule_of_event event in 
    let lhs = rule.Primitives.lhs in 
    let embedding = embedding_of_event event in 
    tests_of_lhs parameter handler error lhs embedding 
      
  let tests_of_obs = tests_of_lhs 

  let agent_of_node n = build_agent (Node.get_address n) (Node.name n) 

  let actions_of_event parameter handler error event = 
    let rule = rule_of_event event in 
    let lhs = rule.Primitives.lhs in
    let embedding = embedding_of_event event in 
    let a,b,_,error = 
      List.fold_left
	(fun (list_actions,side_sites,fresh,error) action -> 
	   match action 
	   with 
	     | Primitives.BND((lhs_id1,site1),(lhs_id2,site2)) ->
		 let agent_id1 = apply_embedding_on_action event lhs_id1 in 
		 let error,agent_name1 = name_of_agent parameter handler error lhs_id1  lhs fresh in 
		 let agent1 = build_agent agent_id1 agent_name1 in
		 let site1 = build_site agent1 site1 in 
		 let agent_id2 = apply_embedding_on_action event lhs_id2 in 
		 let error,agent_name2 = name_of_agent parameter handler error lhs_id2 lhs fresh in 
		 let agent2 = build_agent agent_id2 agent_name2 in
		 let site2 = build_site agent2 site2 in 
		 let site1,site2 = order_site site1 site2 in 
		   (
		     Bind(site1,site2)::list_actions,
		     side_sites,
		     fresh,
                     error
		   )
	     | Primitives.FREE((lhs_id,site_name),bool) ->
		 let agent_id = apply_embedding_on_action event lhs_id in 
		 let error,agent_name = name_of_agent parameter handler error lhs_id lhs fresh in
		 let agent = build_agent agent_id agent_name in 
		 let site = build_site agent site_name in 
		 let list_actions = (Free site)::list_actions in 
                 let error,state = get_binding_state_of_site parameter handler error lhs_id site_name lhs embedding fresh in 
                 if bool 
		 then 
                   match state 
                   with 
                     | BOUND_to site  -> 
                       (Free site)::list_actions,
                       side_sites,
                       fresh,
                       error
                     | _ -> raise (invalid_arg "actions_of_event") 
		 else 
		   let error,state = get_binding_state_of_site parameter handler error lhs_id site_name lhs embedding fresh in 
		   list_actions,
                   (site,state)::side_sites,
                   fresh,
                   error
		     
	     | Primitives.MOD((lhs_id,site),internal) ->
		 let agent_id = apply_embedding_on_action event lhs_id in 
		 let error,agent_name = name_of_agent parameter handler error lhs_id lhs fresh in 
		 let agent = build_agent agent_id agent_name in 
		 let site = build_site agent site in 
		 Mod_internal(site,internal)::list_actions,
		 side_sites,
		 fresh,
                 error
		   
	     | Primitives.DEL(lhs_id) ->
		 let fake_id = Primitives.KEPT lhs_id in
		 let agent_id = apply_embedding event lhs_id in 
		 let error,agent_name = name_of_agent parameter handler error fake_id lhs fresh in 
		 let agent = build_agent agent_id agent_name in 
		 let error,interface = get_binding_sites parameter handler error agent_name in 
                 let error,list = 
                   List.fold_left 
		     (fun (error,list) site -> 
		       let error,state = get_binding_state_of_site parameter handler error fake_id  site lhs embedding fresh in 
		       begin 
		         match state with 
		         | FREE | BOUND_to _ -> error,list 
		         | _ -> error,(build_site agent site,state)::list
		       end
		     )
		     (error,side_sites) interface
                 in 
		 Remove(agent)::list_actions,
	         list,
		 fresh,
                 error
		
	     | Primitives.ADD(rhs_id,agent_name) ->
		let agent_id =
		  apply_embedding_on_action event (Primitives.FRESH rhs_id) in
		 let error,interface = get_default_state parameter handler error agent_name in 
		 let agent = build_agent agent_id agent_name in 
		 let kappa_agent = build_kappa_agent agent_name interface in 
		 let list_actions' = Create(agent,interface)::list_actions in 
		 let fresh' = add_asso rhs_id kappa_agent fresh in 
		   list_actions',side_sites,fresh',error)
	([],[],Mods.IntMap.empty,error)
	rule.Primitives.script
    in error,(List.rev a,b)

      

  let refine_event parameter handler error event = 
    let error,tests = tests_of_event parameter handler error event in 
    let error,actions = actions_of_event parameter handler error event in 
    error,(event,tests,actions)
    
  let refine_obs parameter handler error obs = 
    let _,lhs,embedding,info = obs in 
    let error,tests = tests_of_obs parameter handler error lhs embedding in 
    error,(obs,tests)

  let refine_subs parameter handler error a b = error,(a,b)

  let obs_of_refined_obs _ _ error a = error,fst a

  let event_of_refined_event _ _ error (a,_,_) = error,a

  let subs_of_refined_subs _ _ error a b = error,(a,b)

  let tests_of_obs (i,mixture,phi) = 
    tests_of_lhs mixture phi 
 *)
  let tests_of_refined_obs _ _ error (_,x,_) = error, x
  let tests_of_refined_event _ _ error (_,(y,(_,_,_))) =  error,y
  let tests_of_refined_init _ _ error _ =  error,[]
  let tests_of_refined_subs _ _ error _ _ = error,[]
  let actions_of_refined_event _ _ error (_,(_,(x,y,_))) = error,(x,y)
  let actions_of_refined_init _ _ error y = error,(y,[])
  let actions_of_refined_obs _ _ error _ = error,([],[])
  let actions_of_refined_subs _ _ error _ _ = error,([],[])
(*  let rule_of_refined_event parameter handler error r_event = 
    let error,event = event_of_refined_event parameter handler error r_event in 
    error,rule_of_event event *)

  let print_side_effects ?handler =
    let env = Tools.option_map (fun x -> x.H.env) handler in
    Pp.list Pp.space
	    (fun f (site,state) ->
	     Format.fprintf
	       f "Side_effects(%a:%a)@,"
	       (print_site ?env) site (print_binding_state ?env) state)

  let print_refined_obs ?handler f _refined_obs =
    Format.fprintf f "***Refined obs***"

  let print_refined_subs _f (_a,_b)  = ()

  let print_refined_init ?handler log actions =
    let sigs = match handler with
      | None -> None
      | Some handler -> Some (Environment.signatures handler.H.env) in
    Format.fprintf log "@[<1>INIT:%a@]"
		   (Pp.list Pp.space (PI.print_concrete_action ?sigs)) actions

  let print_refined_event ?handler log (ev_kind,(tests,(actions,side_sites,_))) =
    let sigs = match handler with
      | None -> None
      | Some handler -> Some (Environment.signatures handler.H.env) in
    let () = Format.fprintf log "@[***Refined event:***@,* Kappa_rule %s@,"
			    (Causal.label ?env:(Tools.option_map (fun x -> x.H.env) handler) ev_kind) in
    Format.fprintf log "Story encoding:@[<1>@,%a%a%a@]@,***@]"
		   (Pp.list ~trailing:Pp.space Pp.space (PI.print_concrete_test?sigs))
		   tests
		   (Pp.list ~trailing:Pp.space Pp.space (PI.print_concrete_action ?sigs))
		   actions
		   (print_side_effects ?handler) side_sites

  let gen f0 f1 f2 f3 f4 (p:H.parameter) h e step =
    match step with
    | Subs (a,b) -> f0 p h e a b
    | Event (x,y) -> f1 p h e (x,y)
    | Init (_,a) -> f2 p h e a
    | Obs a -> f3 p h e a
    | Dummy x  -> f4 p h e x

  let print_refined_step ?handler f = function
    | Subs (a,b) -> print_refined_subs f (a,b)
    | Event (x,y) -> print_refined_event ?handler f (x,y)
    | Init (_,a) -> print_refined_init ?handler f a
    | Obs a -> print_refined_obs ?handler f a
    | Dummy _  -> ()

  let tests_of_refined_step =
    gen
      tests_of_refined_subs
      tests_of_refined_event
      tests_of_refined_init
      tests_of_refined_obs
      (fun _ _ error _ -> error,[])

  let is_obs_of_refined_step x =
    match x with
    | Obs _ -> true
    | Event _ | Subs _ | Dummy _ | Init _ -> false
  let is_init_of_refined_step x =
    match x with
    | Init _ -> true
    | Event _ | Subs _ | Dummy _ | Obs _ -> false
  let is_subs_of_refined_step x =
    match x with
    | Subs _ -> true
    | Event _ | Init _ | Dummy _ | Obs _ -> false
  let is_event_of_refined_step x =
    match x with
    | Event _ -> true
    | Init _ | Subs _ | Dummy _ | Obs _ -> false

  let simulation_info_of_refined_step x =
    match x with
    | Obs (_,_,info) -> Some info
    | Event _ | Subs _ | Dummy _ | Init _ -> None

(*  let (refine_step:(step->H.error_channel * refined_step) H.with_handler) =
    genbis
      refine_subs
      refine_event
      refine_obs
      (fun _ _ error x -> error,x)

  let (step_of_refined_step: (refined_step -> H.error_channel*step) H.with_handler) = 
    genbis
      subs_of_refined_subs
      event_of_refined_event
      obs_of_refined_obs
      (fun _ _ error x -> error,x)
 *)
  let actions_of_refined_step =
    gen
      actions_of_refined_subs
      actions_of_refined_event
      actions_of_refined_init
      actions_of_refined_obs
      (fun _ _ error _ -> error,([],[]))
  let store_event log_info (event:refined_event) (step_list:refined_step list) =
    match event with
    | Causal.INIT sort,(_,(actions,_,_)) ->
       P.inc_n_kasim_events log_info,(Init (sort,actions))::step_list
    | Causal.OBS _,_ -> assert false
    | (Causal.RULE _ | Causal.PERT _ as k),x ->
       P.inc_n_kasim_events log_info,(Event (k,x))::step_list
  let store_obs log_info (i,x,c) step_list =
    P.inc_n_obs_events log_info,Obs(i,x,c)::step_list

  let build_grid list bool handler =
    let env = handler.H.env in
    let empty_set = [] in
    let grid = Causal.empty_grid () in
    let grid,_,_,_ =
      List.fold_left
        (fun (grid,side_effect,counter,subs) (k,side,is_weak) ->
	 let maybe_side_effect =
	   if bool then fun se -> se
	   else fun _ -> List.rev_append side_effect side in
	 let translate y = try Mods.IntMap.find y subs with Not_found -> y in
         match (k:refined_step) with
         | Event (id,event) ->
	    let (tests,(actions,side_effects,kappa_side)) =
	      PI.subst_map_agent_in_concrete_event translate event in
            let kasim_side_effect = maybe_side_effect kappa_side in
            Causal.record
	      (id,(tests,(actions,side_effects,kasim_side_effect)))
	      is_weak counter env grid,
            empty_set,counter+1,Mods.IntMap.empty
         | Obs (id,tests,info) ->
	    let tests' =
	      Tools.list_smart_map
		(PI.subst_map_agent_in_concrete_test translate) tests in
	    Causal.record_obs
	      (id,tests',info) side_effect is_weak counter grid,
	    maybe_side_effect empty_set,counter+1,Mods.IntMap.empty
         | Subs (a,b) ->
            grid, side_effect, counter, Mods.IntMap.add a b subs
	 | Init (so,actions) ->
	    let actions' =
	      Tools.list_smart_map
		(PI.subst_map_agent_in_concrete_action translate) actions in
            Causal.record_init (so,actions') is_weak counter env grid,
	    side_effect,counter+1,Mods.IntMap.empty
         | Dummy _ ->
            grid, maybe_side_effect empty_set, counter, subs
        )
        (grid,empty_set,1,Mods.IntMap.empty) list
    in grid

  let clean_events =
    List.filter
      (function Event _ | Obs _ -> true | Dummy _ | Init _ | Subs _ -> false)

  let print_side_effect =
    Pp.list Pp.comma (fun f ((a,_),b) -> Format.fprintf f "(%i,%i)," a b)
  let side_effect_of_list l = l

(*  let rec no_obs_found l = 
    match l 
    with 
      | Obs(_)::_ -> false
      | _::q -> no_obs_found q
      | [] -> true 

 *)
  let level_of_event parameter handler error e set =
    match H.get_priorities parameter with
    | None -> error,0
    | Some priorities ->
       match e with
       | Obs _ -> error,priorities.Priority.other_events
       | Event _ ->
          begin
            let error,actions =
	      actions_of_refined_step parameter handler error e in
            let priority =
              List.fold_left
                (fun priority ->
		 function
		 | PI.Create (ag,_)  ->
                    let ag_id = agent_id_of_agent ag in
                    if set ag_id then priority
                    else
                      Priority.min_level priority priorities.Priority.creation
                 | PI.Remove _ ->
		    Priority.min_level priority priorities.Priority.removal
                 | PI.Mod_internal _ -> priority
                 | PI.Free _ ->
		    Priority.min_level priority priorities.Priority.unbinding
                 | PI.Bind(_,_) | PI.Bind_to(_,_) -> priority
                )
                priorities.Priority.other_events
                (fst (actions))
            in
            error,priority
          end
       | (Dummy _ | Subs _ | Init _) ->
	  error,priorities.Priority.substitution

  let creation_of_event = function
    | Event (_,(_,(actions,_,_))) ->
       List.fold_left
	 (fun l -> function
		| PI.Create (x,_) -> fst x :: l
		| PI.Mod_internal _ | PI.Bind _ | PI.Bind_to _ | PI.Free _
		| PI.Remove _ -> l) [] actions
    | Obs _ | Dummy _ | Init _ | Subs _ -> []

  let subs_agent_in_event mapping = function
    | Event (a,event) ->
       Event
	 (a,
	  PI.subst_map_agent_in_concrete_event
	    (fun x -> try AgentIdMap.find x mapping with Not_found -> x)
	    event)
    | Obs (a,b,c) ->
       Obs(a,
	   Tools.list_smart_map
	     (PI.subst_map_agent_in_concrete_test
		(fun x -> try AgentIdMap.find x mapping with Not_found -> x)) b,
	   c)
    | Init _ | Dummy _ | Subs _ as event -> event

  let disambiguate event_list =
    let _,_,_,event_list_rev =
      List.fold_left
        (fun (max_id,used,mapping,event_list) event ->
         let max_id,used,mapping =
           List.fold_left
             (fun (max_id,used,mapping) x ->
              if AgentIdSet.mem x used
              then
                (max_id+1,AgentIdSet.add (max_id+1) used,
		 AgentIdMap.add x (max_id+1) mapping)
              else (max x max_id,AgentIdSet.add x used,mapping))
             (max_id,used,mapping) (creation_of_event event) in
         let list = (subs_agent_in_event mapping event)::event_list in
         max_id,used,mapping,list)
        (0,AgentIdSet.empty,AgentIdMap.empty,[])
        (List.rev event_list)
    in event_list_rev

  type agent_info = 
    {
      initial_step: refined_step ;
      internal_states: internal_state SiteMap.t ;
      bound_sites: SiteSet.t ;
      sites_with_wrong_internal_state: SiteSet.t 
    }

  let convert_init remanent action_list =
    let rec aux recur = function
      | [] -> recur
      | PI.Free _ :: t -> aux recur t
      | (PI.Bind _ | PI.Remove _ | PI.Bind_to _ | PI.Mod_internal _) :: _ -> remanent
      | PI.Create ((id,sort),site_list) :: t ->
	   let map =
	     List.fold_left
	       (fun map -> function
			| s, Some u -> SiteMap.add s u map
			| _, None -> map)
	       SiteMap.empty site_list in
	   let agent_info =
	     {
	       initial_step=Init (Format.asprintf "Intro #%i" sort,action_list);
	       internal_states=map;
	       bound_sites=SiteSet.empty;
	       sites_with_wrong_internal_state=SiteSet.empty
	     }
	   in aux (AgentIdMap.add id agent_info remanent) t
    in aux remanent action_list

  let as_init agent_info = 
    SiteSet.is_empty agent_info.bound_sites 
      && SiteSet.is_empty agent_info.sites_with_wrong_internal_state 

  let mod_site site state (remanent,set) = 
    let agid = agent_id_of_site site in 
    let s_name = site_name_of_site site in 
    try 
      let ag_info = AgentIdMap.find agid remanent in 
      let state_ref =  SiteMap.find s_name ag_info.internal_states in 
      if state_ref = state 
      then 
	begin
	  if SiteSet.mem s_name ag_info.sites_with_wrong_internal_state 
	  then 
	    let ag_info = 
	      { 
		ag_info with 
		  sites_with_wrong_internal_state = 
		  SiteSet.remove s_name ag_info.sites_with_wrong_internal_state}
	    in 
	    let remanent = AgentIdMap.add agid ag_info remanent in 
	    begin
	      if as_init ag_info 
	      then 
		remanent,
		AgentIdSet.add agid set
	      else 
		remanent, 
		set
	    end 
	  else remanent,set 
	end 
      else 
	begin
	  if SiteSet.mem s_name ag_info.sites_with_wrong_internal_state 
	  then
	    remanent,set 
	  else 
	    let ag_info = 
	      { 
		ag_info with 
		  sites_with_wrong_internal_state = 
		  SiteSet.add s_name ag_info.sites_with_wrong_internal_state}
	    in 
	    let remanent = AgentIdMap.add agid ag_info remanent in 
	    begin
	      if as_init ag_info 
	      then 
		remanent,set 
	      else 
		remanent, 
		AgentIdSet.remove agid set
	    end 
	end 
    with 
  Not_found -> 
    remanent,set 
	
  let unbind_side (agid,s_name) (remanent,set) = 
     try 
      let ag_info = AgentIdMap.find agid remanent in 
      begin
	if SiteSet.mem s_name ag_info.bound_sites 
	then 
	  let ag_info = 
	    { 
	      ag_info with 
		bound_sites = 
		SiteSet.remove s_name ag_info.bound_sites}
	  in 
	  let remanent = AgentIdMap.add agid ag_info remanent in 
	  begin
	    if as_init ag_info 
	    then 
	      remanent,
		AgentIdSet.add agid set
	    else 
	      remanent, 
	      set
	  end 
	else remanent,set 
      end 
     with
  Not_found -> 
    remanent,set 


  let unbind site rem  = 
    let agid = agent_id_of_site site in 
    let s_name = site_name_of_site site in 
     unbind_side (agid,s_name) rem 

  let bind site (remanent,set) = 
    let agid = agent_id_of_site site in 
    let s_name = site_name_of_site site in 
    try 
      let ag_info = AgentIdMap.find agid remanent in 
      begin
	if SiteSet.mem s_name ag_info.bound_sites
	then
	  remanent,set 
	else 
	  let ag_info = 
	    { 
	      ag_info with 
		bound_sites = 
		SiteSet.add s_name ag_info.bound_sites}
	  in 
	  let remanent = AgentIdMap.add agid ag_info remanent in 
	    begin
	      if as_init ag_info 
	      then 
		remanent,set 
	      else 
		remanent, 
		AgentIdSet.remove agid set
	    end 
      end 
    with 
      Not_found -> 
	remanent,set 

  let fill_siphon refined_step_list =
    let remanent = AgentIdMap.empty in 
    let a,_ =
      List.fold_left
	(fun (step_list,remanent) refined_step -> 
	 match refined_step with
	 | Init (_,init) -> (refined_step::step_list, convert_init remanent init)
	 | Event (_,(_,(action,_,kasim_side))) ->
	    let remanent,set =
	      List.fold_left
		(fun recur ->
		 function
		 | PI.Create _ -> recur
		 | PI.Mod_internal (site,state) ->
		    mod_site site state recur
		 | (PI.Bind (site1,site2) | PI.Bind_to (site1,site2)) ->
		    bind site1 (bind site2 recur)
		 | PI.Free site -> unbind site recur
		 | PI.Remove _ -> recur )
		(remanent,AgentIdSet.empty) action in
	    let remanent,set =
	      List.fold_right unbind kasim_side (remanent,set)
	    in
	    ((AgentIdSet.fold
		(fun id list ->
		 try (AgentIdMap.find id remanent).initial_step::list
		 with Not_found -> list)
		set
		(refined_step::step_list)),remanent)
	 | Subs _ | Obs _ | Dummy _ -> (refined_step::step_list,remanent))
	([],remanent)
	refined_step_list in
    a

(*  let print_step parameter handler error refined_event = 
    let log = parameter.H.out_channel in 
    match 
      step_of_refined_step 
        parameter 
        handler 
        error 
        refined_event 
    with 
    | error,Subs (a,b) -> 
      let _ = Format.fprintf log "Subs: %s/%s" (string_of_int b) (string_of_int a) in error 
    | error,Event event ->
       let () = print_event log handler.Cflow_handler.Cflow_handler.env event in
       error
    | error,Obs obs -> 
      let _ = Format.fprintf log "Obs: " in 
      let _ = print_obs log handler obs in 
      error 
    | error,Dummy x -> 
      let _ = Format.fprintf log "%s" x in error 
 *)
  let agent_id_in_obs _parameter _handler error = function
    | Subs _ | Event _ | Init _ | Dummy _ -> error,AgentIdSet.empty
    | Obs (_,tests,_) ->
       error,
       List.fold_left
         (fun l x ->
          match x with
          | PI.Is_Here x ->
             AgentIdSet.add (agent_id_of_agent x) l
          | PI.Is_Bound _ | PI.Is_Free _ | PI.Has_Binding_type _
	  | PI.Is_Bound_to _ | PI.Has_Internal _ -> l)
         AgentIdSet.empty
         tests
end:Cflow_signature)
