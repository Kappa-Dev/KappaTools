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

let compose f g = (fun x -> f (g x))

module type Cflow_signature =
sig
  module H:Cflow_handler.Cflow_handler 
  module P:StoryProfiling.StoryStats 
  type agent_name = int
  type site_name = int 
  type agent_id = int 
  module AgentIdSet:Set.S with type elt = agent_id
  type agent 
  type site 
  type internal_state = int 
  type binding_type 
  type binding_state = 
    | ANY 
    | FREE 
    | BOUND 
    | BOUND_TYPE of binding_type 
    | BOUND_to of site 

  type ('a,'b,'c,'d,'e) choice = 
    | Subs of ('d*'e)
    | Event of 'a 
    | Init of 'b
    | Obs of 'c  
    | Dummy of string  
    

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
    | Bind_to of site * site (*used when initial agents are bound*)
    | Unbind of site * site  
    | Free of site 
    | Remove of agent 

  type event 
  type init 
  type embedding
  type fresh_map 
  type obs  
  type step 
  type side_effect 
  type kasim_side_effect = Mods.Int2Set.t 
  type kappa_rule 
  type refined_event 
  type refined_step
  type obs_from_rule_app = (int * int Mods.IntMap.t) list 
  type r = Primitives.rule 
  type counter = int 
  type rule_info = obs_from_rule_app * r * counter * kasim_side_effect
                   
  val empty_side_effect: side_effect
  val dummy_refined_step: string -> refined_step
  val type_of_refined_step: refined_step -> (unit,unit,unit,unit,unit) choice 
  val agent_of_binding_type: binding_type -> agent_name 
  val site_of_binding_type: binding_type -> site_name
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
  val get_binding_sites: (agent_name -> H.error_channel * site_name list) H.with_handler 
  val get_default_state: (agent_name -> H.error_channel * (site_name*internal_state option) list) H.with_handler 
  val rule_of_event: event -> kappa_rule 
  val btype_of_names : agent_name -> site_name -> binding_type 
  val embedding_of_event: event -> embedding
  val fresh_map_of_event: event -> fresh_map
  val refine_step: (step -> H.error_channel * refined_step) H.with_handler 
  val build_subs_refined_step: int -> int -> refined_step 
  val step_of_refined_step: (refined_step -> H.error_channel * step) H.with_handler 
  val rule_of_refined_event: (refined_event -> H.error_channel * kappa_rule) H.with_handler
  val tests_of_refined_step: (refined_step -> H.error_channel * test list) H.with_handler 
  val actions_of_refined_step: (refined_step -> H.error_channel * (action list * (site*binding_state) list)) H.with_handler  
  val is_obs_of_refined_step: refined_step -> bool 
  val is_init_of_refined_step: refined_step -> bool 
  val simulation_info_of_refined_step: refined_step -> unit Mods.simulation_info option 

  val print_test: (string -> test -> H.error_channel) H.with_handler
  val print_action: (string -> action -> H.error_channel) H.with_handler 
  val print_side:
    Format.formatter -> H.handler -> string  -> (site*binding_state) -> unit

  val print_refined_step: (refined_step -> H.error_channel) H.with_handler 


  val import_event:  (Primitives.rule * int Mods.IntMap.t * int Mods.IntMap.t) * rule_info -> event 
  val store_event: P.log_info -> event -> step list -> P.log_info * step list 
  val store_init : P.log_info -> State.t -> step list -> P.log_info * step list 
  val store_obs :  P.log_info -> int * Mixture.t * int Mods.IntMap.t * unit Mods.simulation_info -> step list -> P.log_info * step list 
  val build_grid: (refined_step * side_effect * bool)  list -> bool -> H.handler -> Causal.grid 
  val print_side_effect: Format.formatter -> side_effect -> unit
  val side_effect_of_list: (int*int) list -> side_effect 
  val no_obs_found: step list -> bool 

  val subs_agent_in_test: agent_id -> agent_id -> test -> test
  val subs_agent_in_action: agent_id -> agent_id -> action -> action 
  val subs_agent_in_side_effect: agent_id -> agent_id -> (site*binding_state) -> (site*binding_state) 

  val subs_map_agent_in_test: (agent_id -> agent_id) -> test -> test
  val subs_map_agent_in_action: (agent_id -> agent_id) -> action -> action 
  val subs_map_agent_in_side_effect: (agent_id -> agent_id) -> (site*binding_state) -> (site*binding_state) 

  val get_kasim_side_effects: refined_step -> kasim_side_effect 

  val level_of_event: (refined_step -> (agent_id -> bool) -> H.error_channel * Priority.level) H.with_handler
  val disambiguate: step list -> H.handler -> step list 
  val clean_events: (refined_step list -> H.error_channel * step list) H.with_handler 

  val print_step: (refined_step -> H.error_channel) H.with_handler 
  val agent_id_in_obs: (refined_step -> H.error_channel * AgentIdSet.t) H.with_handler 

  val fill_siphon: (refined_step list -> H.error_channel * step list) H.with_handler 
end 



module Cflow_linker = 
(struct 
  module H = Cflow_handler.Cflow_handler 
  module P = StoryProfiling.StoryStats 

  type agent_name = int
 
  type site_name = int 

  type agent_id = int 

  module AgIdSet = Set.Make (struct type t = agent_id let compare = compare end)
 
  type agent = agent_id * agent_name 

  type site = agent * site_name 

  type kappa_rule = Primitives.rule

  type embedding = agent_id Mods.IntMap.t 
  type side_effect = (agent_id*int) list 
  type kasim_side_effect = Mods.Int2Set.t 
  type fresh_map = int Mods.IntMap.t 
  type obs_from_rule_app = (int * int Mods.IntMap.t) list 
  type r = Primitives.rule 
  type counter = int  
  type rule_info = (obs_from_rule_app * r  * counter * kasim_side_effect) 
  type init = agent * (site_name * (int option * Node.ptr)) list
  type event = (kappa_rule * embedding * fresh_map) * (rule_info)
  type obs = int * Mixture.t * embedding * unit Mods.simulation_info 
      
  let get_causal (_,d) = d 

  module AgentIdSet = Set.Make (struct type t = agent_id let compare=compare end)
  module AgentIdMap = Map.Make (struct type t = agent_id let compare=compare end)
  module SiteMap = Map.Make (struct type t = site_name let compare=compare end)
  module SiteSet = Set.Make (struct type t = site_name let compare = compare end) 
  type internal_state  = int 

  type binding_type = agent_name * site_name 

  type binding_state = 
    | ANY 
    | FREE 
    | BOUND 
    | BOUND_TYPE of binding_type 
    | BOUND_to of site 

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
    | Bind_to of site * site 
    | Unbind of site * site  
    | Free of site 
    | Remove of agent 

 
  type ('a,'b,'c,'d,'e) choice = 
  | Subs of ('d * 'e) 
  | Event of 'a 
  | Init of 'b
  | Obs of 'c  
  | Dummy  of string 

  type refined_event = event * test list * (action list * ((site * binding_state) list))
  type refined_init = init * action list
  type refined_obs =  obs * test list 

          

  type side_effects = (int*int) list 
  type step = (event,init,obs,agent_id,agent_id) choice 
  type refined_step = (refined_event,refined_init,refined_obs,agent_id,agent_id) choice 

  let btype_of_names a b = (a,b)
  let build_subs_refined_step a b = Subs (a,b)
  let get_kasim_side_effects a = 
    match a 
    with 
      | Event ((_,(_,_,_,a)),_,_) -> a
      | _ -> Mods.Int2Set.empty 
      
  let dummy_refined_step x = Dummy x
  let empty_side_effect = []
  let type_of_refined_step c = 
    match c 
    with 
      | Event _ -> Event ()
      | Init _ -> Init () 
      | Subs (_,_) -> Subs ((),())
      | Obs _ -> Obs () 
      | Dummy x -> Dummy x 

  let site_of_binding_type = snd
  let agent_of_binding_type = fst
  let map_sites parameter handler error f map x = 
      try 
        let rec aux k list = 
          if k=0 
          then list
          else aux (k-1) ((f x k map.Environment.signatures)::list)
        in error,aux (Signature.arity map.Environment.signatures x -1) []
      with 
	  Not_found -> 
	    let error_list,error = 
              H.create_error parameter handler error (Some "kappa_instantiation.ml") None (Some "map_sites") (Some "265") (Some "Kappa_instantiation") (failwith "Kappa instantiation, line 265")  in 
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

  let agent_id_of_agent = fst 
  
  let agent_name_of_agent = snd 
  
  let agent_of_site = fst 
  
  let agent_id_of_site = compose agent_id_of_agent agent_of_site
  
  let agent_name_of_site = compose agent_name_of_agent agent_of_site 
  
  let site_name_of_site = snd 
  
  let agent_name_of_binding_type = fst
  
  let site_name_of_binding_type = snd 
    
  let string_of_agent env agent =
    Format.asprintf "%a_%i" (Environment.print_agent env.H.env)
		    (agent_name_of_agent agent) (agent_id_of_agent agent)
 
  let string_of_site_name env = string_of_int 
  
  let string_of_site env site = (string_of_agent env (agent_of_site site))^"."^(string_of_site_name env (site_name_of_site site))
  
  let string_of_internal_state env int = string_of_int int 
  
  let string_of_btype env (agent_name,site_name) = (string_of_int agent_name)^"!"^(string_of_int site_name)

  let string_of_binding_state env state = 
    match state with
      | ANY -> "*"
      | FREE -> ""
      | BOUND -> "!_"
      | BOUND_TYPE btype -> "!"^(string_of_btype env btype)
      | BOUND_to site -> "!"^(string_of_site env site)

  let print_init log env ((agent,list):init) =
    let ag = (string_of_agent env agent) in
    let _ = Format.fprintf log "%s(" ag in
    let _ =
      List.fold_left
        (fun bool (s,(i,l)) ->
         let _ =
           Format.fprintf log "%s%s%s%s"
			  (if bool then "," else "")
			  (string_of_int s)
			  (match i with
			   | None -> ""
			   | Some i -> "~"^(string_of_int i))
			  (match l with
			   | Node.Null -> ""
			   | Node.Ptr (t,i) ->
			      "!"^(string_of_int t.Node.name)^"."^(string_of_int i)
			   | Node.FPtr (i,j) ->
			      "!T"^(string_of_int i)^"."^(string_of_int j))
         in true)
        false (List.rev list)
    in Format.fprintf log ")"

  let print_event log env ((rule,emb,fresh),(rule_info)) =
    Format.fprintf log "%a->%a"
		   (Kappa_printer.mixture false env) rule.Primitives.lhs
		   (Kappa_printer.mixture false env) rule.Primitives.rhs

  let print_obs log env obs = ()

  let print_test parameter handler error prefix test =
    let log = parameter.H.out_channel in
    let _ =
      match test with
      | Is_Here agent ->
	 Format.fprintf log "%sIs_Here(%s)\n" prefix (string_of_agent handler agent)
      | Has_Internal (site,int) ->
	 Format.fprintf log "%sHas_Internal(%s~%s)\n" prefix
			(string_of_site handler site) (string_of_internal_state handler int)
      | Is_Free site ->
	 Format.fprintf log "%sIs_Free(%s)\n" prefix (string_of_site handler site)
      | Is_Bound site ->
	 Format.fprintf log "%sIs_Bound(%s)\n" prefix (string_of_site handler site)
      | Has_Binding_type (site,btype) ->
	 Format.fprintf log "%sBtype(%s,%s)\n" prefix
			(string_of_site handler site) (string_of_btype handler btype)
      | Is_Bound_to (site1,site2) ->
	 Format.fprintf log "%sIs_Bound(%s,%s)\n" prefix
			(string_of_site handler site1) (string_of_site handler site2)
    in error
  let print_action parameter handler error prefix action =
    let log = parameter.H.out_channel in 
    let _ = 
      match action with 
      | Create (agent,list) -> 
	let _ = Format.fprintf log "%sCreate(%s[" prefix (string_of_agent handler agent) in 
	let _ = 
	    List.fold_left 
              (fun bool (x,y) -> 
		let _ = 
		  Format.fprintf 
		    log 
		    "%s%s%s" 
		    (if bool then "," else "")
		    (string_of_site_name handler x)
		     (match y with 
		     | None -> ""
		     | Some y -> "~"^(string_of_int y))
		in true)
	      false list in
	let _ = Format.fprintf log "])\n" in
	()
      | Mod_internal (site,int) ->
	 Format.fprintf log "%sMod(%s~%s)\n" prefix (string_of_site handler site) (string_of_internal_state handler int)
      | (Bind (site1,site2) | Bind_to (site1,site2)) ->
	 Format.fprintf log "%sBind(%s,%s)\n" prefix (string_of_site handler site1) (string_of_site handler site2)
      | Unbind (site1,site2)  ->
	 Format.fprintf log "%sUnBind(%s,%s)\n" prefix (string_of_site handler site1) (string_of_site handler site2)
      | Free site ->
	 Format.fprintf log "%sFree(%s)\n" prefix (string_of_site handler site)
      | Remove agent ->
	 Format.fprintf log "%sRemove(%s)\n" prefix (string_of_agent handler agent)
    in error

  let print_side log env prefix (s,binding_state) = 
    Format.fprintf log "%s(%s,%s)\n" prefix (string_of_site env s) (string_of_binding_state env binding_state)

  let lhs_of_rule rule = rule.Primitives.lhs 
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
              H.create_error parameter handler error (Some "kappa_instantiation.ml") None (Some "get_agent") (Some "401") (Some "Kappa_instantiation") (failwith "Kappa instantiation, line 401")  in 
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
    | Bind_to (s1,s2) -> Bind_to(subs_site id1 id2 s1,subs_site id1 id2 s2)
    | Unbind (s1,s2) -> Unbind (subs_site id1 id2 s1,subs_site id1 id2 s2)
    | Free site -> Free (subs_site id1 id2 site)
    | Remove agent -> Remove (subs_agent id1 id2 agent)

   let subs_map_agent_in_action f action = 
    match
      action
    with 
    | Create (agent,list) -> Create(subs_map_agent f agent,list)
    | Mod_internal (site,i) -> Mod_internal(subs_map_site f site,i)
    | Bind (s1,s2) -> Bind(subs_map_site f s1,subs_map_site f s2)
    | Bind_to (s1,s2) -> Bind_to(subs_map_site f s1,subs_map_site f s2)
    | Unbind (s1,s2) -> Unbind (subs_map_site f s1,subs_map_site f s2)
    | Free site -> Free (subs_map_site f site)
    | Remove agent -> Remove (subs_map_agent f agent)

  let subs_agent_in_side_effect id1 id2 (site,bstate) = (subs_site id1 id2 site,bstate)
  let subs_map_agent_in_side_effect f (site,bstate) = (subs_map_site f site,bstate)

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

  let create_init state log_info event_list = 
    State.fold_graph
	  (fun node_id node (log_info,list)  ->
            let interface = 
		Node.fold_status
		  (fun site_id (int,lnk) list -> 
                    if site_id = 0 
                    then list 
                    else 
                      (site_id,(int,lnk))::list)
                  node 
	          []
            in 
            let agent = build_agent node_id (Node.name node) in 
            let log_info = P.inc_n_init_events log_info in 
            (log_info,(Init (agent,interface))::list))
          state
          (log_info,event_list) 

  let actions_of_init parameter handler error (init:init) = 
    let agent,list_sites = init in 
    let list = [Create(agent,List.rev_map (fun (x,(y,z)) -> (x,y)) (List.rev list_sites))] in 
    let list = 
      List.fold_left 
        (fun list (x,(y,z)) -> 
          match z 
          with 
            | Node.Null -> 
              Free(build_site agent x)::list
            | Node.Ptr (node,site) -> 
              let agent2 = agent_of_node node in 
              let site1 = build_site agent x in 
              let site2 = build_site agent2 site in 
                Bind_to(site1,site2)::list
            | Node.FPtr _ -> raise (invalid_arg "actionS_of_init")
        )
        list list_sites 
    in 
    error,List.rev list 

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

  let refine_init parameter handler error init = 
    let error,actions = actions_of_init parameter handler error init in 
    error,(init,actions)

  let init_of_refined_init _ _ error x = error,fst x

  let tests_of_obs (i,mixture,phi) = 
    tests_of_lhs mixture phi 

  let tests_of_refined_obs parameter handler error x = error,snd x 

  let tests_of_refined_init _ _ error _ = error,[]
  let tests_of_refined_event _ _ error (_,y,_) =  error,y
  let tests_of_refined_subs _ _ error _ _ = error,[]
  let actions_of_refined_event _ _ error (_,_,y) = error,y
  let actions_of_refined_init _ _ error (_,x) = error,(x,[])
  let actions_of_refined_obs _ _ error _ = error,([],[])
  let actions_of_refined_subs _ _ error _ _ = error,([],[])
  let rule_of_refined_event parameter handler error r_event = 
    let error,event = event_of_refined_event parameter handler error r_event in 
    error,rule_of_event event 

  let print_side_effects parameter handler error prefix (site,state) = 
    let _ = 
      Format.fprintf 
        parameter.H.out_channel
        "%sSide_effects(%s:%s)\n" 
        prefix 
        (string_of_site handler site)
        (string_of_binding_state handler state)
    in error 

  let print_refined_obs parameter handler error refined_obs = 
    let _ = Format.fprintf parameter.H.out_channel "***Refined obs***" 
    in error 

  let print_refined_subs parameter handler error a b  = error

  let print_refined_event parameter handler error refined_event = 
    let log = parameter.H.out_channel in 
    let _ = Format.fprintf log "***Refined event:***\n" in 
    let _ = Format.fprintf log "* Kappa_rule \n" in 
    let error,rule = rule_of_refined_event parameter handler error refined_event in 
    let _ = Dynamics.dump Format.err_formatter rule  handler.H.env in
    let _ = Format.fprintf log "\n" in 
    let error = 
      let _ = Format.fprintf log "Story encoding: \n" in 
      let error,tests = tests_of_refined_event parameter handler error refined_event in 
      let error = List.fold_left (fun error -> print_test parameter handler error " ") error (List.rev tests) in 
      let error,actions = actions_of_refined_event parameter handler error refined_event in 
      let error = List.fold_left (fun error -> print_action parameter handler error " ") error (fst actions) in 
      let error = List.fold_left (fun error -> print_side_effects parameter handler error " ") error (List.rev (snd actions)) in 
      let _ = Format.fprintf log "***\n"  in 
      error
    in 
    error

  let print_refined_init parameter handler error (refined_init:refined_init) = 
    let log = parameter.H.out_channel in 
    let ((agent_name,agent_id),_),actions = refined_init in 
    let _ = Format.fprintf log "INIT: Agent %i_%i\n" agent_id agent_name in
    let error = 
      List.fold_left  
        (fun error -> print_action parameter handler error " ") 
        error 
        actions in 
    let _ = Format.fprintf log "\n" in 
    error
      
  let gen f0 f1 f2 f3 f4 (p:H.parameter) h e step = 
    match step
    with 
    | Subs (a,b) -> f0 p h e a b 
    | Event a -> f1 p h e a 
    | Init a -> f2 p h e a
    | Obs a -> f3 p h e a 
    | Dummy x  -> f4 p h e x

  let genbis f0 f1 f2 f3 f4 = 
    gen 
      (fun p h e a b -> 
        let e,output = f0 p h e a b in 
        e,Subs output)
      (fun p h e a -> 
        let e,output = f1 p h e a in 
        e,Event output)
      (fun p h e a -> 
        let e,output = f2 p h e a in 
        e,Init output) 
      (fun p h e a -> 
        let e,output = f3 p h e a in 
        e,Obs output)
      (fun p h e a -> 
        let e,output = f4 p h e a in 
        e,Dummy output)
  
  let print_refined_step  = 
    gen print_refined_subs print_refined_event print_refined_init print_refined_obs (fun _ _ error _   -> error) 

  let tests_of_refined_step =
    gen 
      tests_of_refined_subs 
      tests_of_refined_event 
      tests_of_refined_init 
      tests_of_refined_obs 
      (fun _ _ error _ -> error,[])

  let is_obs_of_refined_step x = 
    match x 
    with 
      | Obs _ -> true
      | _ -> false

  let is_init_of_refined_step x = 
    match x 
    with 
      | Init _ -> true
      | _ -> false

  let simulation_info_of_refined_step x = 
    match x
    with 
      | Obs ((_,_,_,info),_) -> Some info
      | _ -> None 
      
  let (refine_step:(step->H.error_channel * refined_step) H.with_handler) = 
    genbis 
      refine_subs  
      refine_event  
      refine_init  
      refine_obs  
      (fun _ _ error x -> error,x) 
  
  let (step_of_refined_step: (refined_step -> H.error_channel*step) H.with_handler) = 
    genbis 
      subs_of_refined_subs 
      event_of_refined_event 
      init_of_refined_init 
      obs_of_refined_obs 
      (fun _ _ error x -> error,x)

  let actions_of_refined_step = 
    gen 
      actions_of_refined_subs 
      actions_of_refined_event 
      actions_of_refined_init 
      actions_of_refined_obs (fun _ _ error _ -> error,([],[]))

 
  let import_event x = x 
  let import_env x = x
  let store_event log_info (event:event) (step_list:step list) = 
    P.inc_n_kasim_events log_info,(Event event)::step_list    
  let store_init log_info init step_list = 
    create_init init log_info step_list  
  let store_obs log_info (i,a,x,c) step_list = 
    P.inc_n_obs_events log_info,Obs(i,a,x,c)::step_list 

  let build_grid list bool handler = 
    let env = handler.H.env in 
    let empty_set = Mods.Int2Set.empty in 
    let grid = Causal.empty_grid () in 
    let grid,_,_,_ = 
      List.fold_left 
        (fun (grid,side_effect,counter,subs) (k,(side:side_effect),is_weak) ->
          match (k:refined_step) 
          with 
            | Event (a,_,_) -> 
              begin 
                let obs_from_rule_app,r,_,kappa_side = get_causal a in 
                let side_effect =
                  if bool 
                  then 
                    kappa_side 
                  else 
                    List.fold_left
                      (fun set i -> Mods.Int2Set.add i set)
                      side_effect 
                      side 
                in 
                let phi = embedding_of_event a in 
                let psi = fresh_map_of_event a in
                let phi = 
                  Mods.IntMap.map 
                    (fun y -> 
                      try 
                        Mods.IntMap.find y subs 
                      with 
                      | Not_found -> y)
                    phi 
                in 
                Causal.record ~decorate_with:obs_from_rule_app r side_effect (phi,psi) is_weak counter grid env,
                Mods.Int2Set.empty,counter+1,Mods.IntMap.empty
              end
            | Init b -> 
               Causal.record_init b is_weak counter grid env,side_effect,counter+1,Mods.IntMap.empty
            | Obs c  -> 
              let ((r_id,state,embedding,x),test) = c in 
              let embedding = 
                Mods.IntMap.map 
                  (fun y -> 
                    try 
                      Mods.IntMap.find y subs 
                    with 
                    | Not_found -> y)
                  embedding 
              in 
              let c = ((r_id,state,embedding,x),test) in 
              let side_effect =
                if bool 
                then 
                  Mods.Int2Set.empty 
                else 
                  List.fold_left
                    (fun set i -> Mods.Int2Set.add i set)
                    side_effect 
                    side 
              in 
              Causal.record_obs side_effect c is_weak counter grid env,side_effect,counter+1,Mods.IntMap.empty
            | Subs (a,b) -> 
              grid, 
              side_effect,
              counter,
              Mods.IntMap.add a b subs 
            | Dummy _ -> 
              grid,
              (if bool 
               then 
                  empty_set 
               else 
                  (
                    List.fold_left 
                      (fun side_effect x -> Mods.Int2Set.add x side_effect)
                      side_effect side)),
              counter,
              subs 
        ) 
        (grid,Mods.Int2Set.empty,1,Mods.IntMap.empty) list 
    in grid 
    
  let clean_events (parameter:H.parameter) handler error list = 
    List.fold_left 
      (fun (error,list) a -> 
        match a
        with 
        | Event _ | Init _ | Obs _ -> 
          let error,head = step_of_refined_step parameter 
            handler 
            error 
            a 
          in 
          error,head::list
        | _ -> error,list)
      (error,[]) 
      (List.rev list) 

  let print_side_effect log l = 
    List.iter (fun (a,b) -> Format.fprintf log "(%i,%i)," a b) l 
  let side_effect_of_list l = l 

  let rec no_obs_found l = 
    match l 
    with 
      | Obs(_)::_ -> false
      | _::q -> no_obs_found q
      | [] -> true 

 
  
  let level_of_event parameter handler error e set = 
    let priorities = H.get_priorities parameter in 
    match 
      priorities 
    with 
    | None -> error,0
    | Some priorities -> 
      begin 
        match e 
        with 
        | Init _ | Obs _ -> error,priorities.Priority.other_events
        | Event _ -> 
          begin 
            let error,actions = actions_of_refined_step parameter handler error e in 
            let priority = 
              List.fold_left 
                (fun priority action -> 
                  match 
                    action 
                  with 
                  | Create (ag,_)  -> 
                    let ag_id = agent_id_of_agent ag in 
                    if 
                    set ag_id 
                    then 
                      priority
                    else 
                      Priority.min_level priority priorities.Priority.creation
                  | Remove _ -> Priority.min_level priority priorities.Priority.removal 
                  | Mod_internal _ -> priority 
                  | Free _ -> Priority.min_level priority priorities.Priority.unbinding 
                  | Bind(site1,site2) | Bind_to (site1,site2) | Unbind (site1,site2) -> priority 
                )
                priorities.Priority.other_events 
                (fst (actions))
            in 
            error,priority
          end 
        | Dummy _ | Subs _ -> error,priorities.Priority.substitution
      end 

  let creation_of_event event handler = 
    match 
      event 
    with 
    | Event ((_,_,f),_) -> 
        Mods.IntMap.fold 
          (fun a b list -> 
            b::list)
          f []
    | Init (agent,_) -> 
      begin 
        [agent_id_of_agent agent]
      end 
    | _ -> []

  let compose f g = 
    Mods.IntMap.map 
      (fun x -> 
        (try AgentIdMap.find x f
       with Not_found -> x))
      g

  let subs_rule_info mapping (a,b,c,d) = 
    (a,b,c,
     Mods.Int2Set.fold 
       (fun (agent_id,y) -> 
         Mods.Int2Set.add 
           ((try 
               AgentIdMap.find agent_id mapping 
             with 
               Not_found -> agent_id),
            y))
       d
       Mods.Int2Set.empty 
       )


  let subs_agent_in_event mapping event = 
    match 
      event 
    with 
    | Event ((a,f,g),b) -> Event ((a,compose mapping f,compose mapping g),
                                  subs_rule_info mapping b)
      
    | Obs (a,b,f,c) -> Obs(a,b,compose mapping f,c)
    | Init (agent,interface) -> 
      Init (let agent_id=agent_id_of_agent agent in 
            let agent_id' = 
              try 
                AgentIdMap.find agent_id mapping
              with 
              | Not_found -> agent_id 
            in
            let agent = 
              if agent_id==agent_id'
              then 
                agent 
              else 
                build_agent agent_id' (agent_name_of_agent agent)
            in 
            agent,interface)
    | _ -> event

  type agent_info = 
    {
      initial_step: refined_step ;
      internal_states: internal_state SiteMap.t ;
      bound_sites: SiteSet.t ;
      sites_with_wrong_internal_state: SiteSet.t 
    }

  let convert_init x = 
    let ((agent,site_list),action_list) = x in
    let b = 
      List.for_all 
	(fun action -> 
	  match 
	    action 
	  with 
	  | Create _ 
	  | Free _ -> true
	  | _ -> false)
	action_list 
    in 
    if b then
      let map = 
	List.fold_left
	  (fun map (s,(i,_)) -> 
	    match i 
	    with 
	    | Some u -> SiteMap.add s u map
	    | None -> map)
	  SiteMap.empty 
	  site_list
      in 
      let agent_info = 
	{
	  initial_step=Init x;
	  internal_states=map;
	  bound_sites=SiteSet.empty;
	  sites_with_wrong_internal_state=SiteSet.empty
	}
      in
      Some ((agent_id_of_agent agent),agent_info)
    else
      None 

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

  let add_step parameter handler refined_step (step_list,remanent,error) = 
      let error,step = step_of_refined_step parameter handler error refined_step in 
      (step::step_list,remanent,error)
	  
  let fill_siphon parameter handler error refined_step_list  = 
    let remanent = AgentIdMap.empty in 
    let a,_,c = 
      List.fold_left
	(fun ((step_list:step list),remanent,error) refined_step -> 
	  match
	    (refined_step:refined_step)
	  with 
	  | Init init -> 
	    let remanent = 
	      match 
		convert_init init 
	      with
	      | None -> remanent
	      | Some (id,info) -> 
		AgentIdMap.add id info remanent
	    in 
	    add_step parameter handler refined_step (step_list,remanent,error) 
	  | Event (_,_,(action,side)) -> 
	    let remanent,set = 
	      List.fold_left 
		(fun recur action -> 
		    match action
		    with 
		    | Create _ -> recur
		    | Mod_internal (site,state) -> 
		      mod_site site state recur
		    | Bind (site1,site2) | Bind_to (site1,site2)-> 
		      bind site1 (bind site2 recur)
		    | Unbind (site1,site2) -> 
		      unbind site1 (unbind site2 recur)
		    | Free site -> 
		      unbind site recur
		    | Remove agent -> recur )
		(remanent,AgentIdSet.empty) action 
	    in 
	    let remanent,set = 
	      Mods.Int2Set.fold 
		unbind_side
		(get_kasim_side_effects refined_step)
		(remanent,set)
	    in 
	    AgentIdSet.fold 
	      (fun id list -> 
		try
		  let list = add_step parameter handler (AgentIdMap.find id remanent).initial_step list
		  in 
		  list
		with 
		  Not_found -> list)
	      set
	      (add_step parameter handler refined_step (step_list,remanent,error))
	  | Subs _ | Obs _ | Dummy _ -> 
	    add_step parameter handler refined_step (step_list,remanent,error))
	([],remanent,error) 
	refined_step_list
    in 
    c,a 
      
  let disambiguate event_list handler = 
    let max_id = 0 in 
    let used = AgentIdSet.empty in 
    let mapping = AgentIdMap.empty in 
      (let _,_,_,event_list_rev = 
         List.fold_left 
           (fun (max_id,used,mapping,event_list) event -> 
             let max_id,used,mapping = 
               List.fold_left 
                 (fun (max_id,used,mapping) x -> 
                   if AgentIdSet.mem x used
                   then
                      (max_id+1,AgentIdSet.add (max_id+1) used, AgentIdMap.add x (max_id+1) mapping)
                   else (max x max_id,AgentIdSet.add x used,mapping)
                 )
                 (max_id,used,mapping) (creation_of_event event handler )
             in 
             let list = (subs_agent_in_event mapping event)::event_list in 
             max_id,used,mapping,list)
           (max_id,used,mapping,[]) 
           (List.rev event_list)
       in event_list_rev)


  let print_step parameter handler error refined_event = 
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
    | error,Init init -> 
      let _ = Format.fprintf log "Init: " in 
      let _ = print_init log handler init in 
      error
    | error,Obs obs -> 
      let _ = Format.fprintf log "Obs: " in 
      let _ = print_obs log handler obs in 
      error 
    | error,Dummy x -> 
      let _ = Format.fprintf log "%s" x in error 

  let agent_id_in_obs parameter handler error step = 
    match step 
    with 
    | Subs _ | Event _ | Init _ | Dummy _ -> error,AgentIdSet.empty
    | Obs (x,_) -> 
      begin 
        let error,tests = tests_of_refined_step parameter handler error step in 
          
        error,
        List.fold_left 
          (fun l x -> 
            match x with 
            | Is_Here x -> 
              AgentIdSet.add (agent_id_of_agent x) l
            | _ -> l)
          AgentIdSet.empty 
          tests
      end
        
end:Cflow_signature)
