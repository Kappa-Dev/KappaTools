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
  * Last modification: 17/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let debug_mode = false
let compose f g = (fun x -> f (g x))

module type Cflow_signature =
sig
  type agent_name = int
  type site_name = int 
  type agent_id = int 
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

  type kappa_sig 
  
  type ('a,'b,'c(*,'d*)) choice = 
    | Event of 'a 
    | Init of 'b
    | Obs of 'c  
    | Dummy (*of 'd *)
  
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
  type kappa_rule 
  type refined_event 
  type refined_step
  type obs_from_rule_app = (int * int Mods.IntMap.t) list 
  type r = Dynamics.rule 
  type counter = int 
  type rule_info = obs_from_rule_app * r * counter
                   
  val dummy_refined_step: refined_step
  val type_of_refined_step: refined_step -> (unit,unit,unit) choice 
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
  val get_binding_sites: kappa_sig -> agent_name -> site_name list 
  val get_default_state: kappa_sig -> agent_name -> (site_name*internal_state option) list 
  val rule_of_event: event -> kappa_rule 
  val embedding_of_event: event -> embedding
  val fresh_map_of_event: event -> fresh_map
  val refine_step: kappa_sig -> step -> refined_step
  val step_of_refined_step: refined_step -> step
  val rule_of_refined_event: refined_event -> kappa_rule
  val tests_of_refined_step: refined_step -> test list 
  val actions_of_refined_step: refined_step -> action list * (site*binding_state) list 
  val is_obs_of_refined_step: refined_step -> bool 

  val print_refined_step: out_channel -> kappa_sig -> refined_step -> unit 

  val import_event:  (Dynamics.rule * int Mods.IntMap.t * int Mods.IntMap.t) * rule_info -> event 
  val import_env: Environment.t -> kappa_sig 
  val store_event: event -> step list -> step list 
  val store_init : State.implicit_state -> step list -> step list 
  val store_obs :  int * Mixture.t * int Mods.IntMap.t -> step list -> step list 
  val build_grid: (refined_step * unit list) list -> Environment.t -> Causal.grid 
end 



module Cflow_linker = 
(struct 

  type agent_name = int

  type site_name = int 

  type agent_id = int 

  type agent = agent_id * agent_name 

  type site = agent * site_name 

  type kappa_rule = Dynamics.rule

  type embedding = agent_id Mods.IntMap.t 

  type fresh_map = int Mods.IntMap.t 
  type obs_from_rule_app = (int * int Mods.IntMap.t) list 
  type r = Dynamics.rule 
  type counter = int  
  type rule_info = (obs_from_rule_app * r  * counter) 
  type init = agent * (site_name * (int option * Node.ptr)) list
  type event = (kappa_rule * embedding * fresh_map) * (rule_info)
  type obs = int * Mixture.t * embedding 
      
  let get_causal (_,d) = d 


  type kappa_sig = Environment.t 

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

  type ('a,'b,'c) choice = 
    | Event of 'a 
    | Init of 'b
    | Obs of 'c  
    | Dummy   

  type refined_event = event * test list * (action list * ((site * binding_state) list))
  type refined_init = init * action list
  type refined_obs =  obs * test list 

  type side_effects = (int*int) list 
  type step = (event,init,obs) choice 
  type refined_step = (refined_event,refined_init,refined_obs) choice 

  let dummy_refined_step = Dummy 
  let type_of_refined_step c = 
    match c 
    with 
      | Event _ -> Event ()
      | Init _ -> Init () 
      | Obs _ -> Obs () 
      | Dummy -> Dummy 

  let site_of_binding_type = snd
  let agent_of_binding_type = fst
  let map_sites f map x = 
     let sign = 
      try 
	Environment.get_sig x map 
      with 
	  Not_found -> 
	    failwith "Kappa_instantiation, line 89"
    in 
     let rec aux k list = 
       if k=0 
       then list
       else aux (k-1) ((f k sign)::list)
     in aux (Signature.arity sign -1) []

  let get_binding_sites = 
    map_sites 
      (fun k _ -> k)

  let get_default_state = 
    map_sites 
      (fun k sign -> (k,Signature.default_num_value k sign))

  
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
    
  let string_of_agent env agent = (string_of_int (agent_name_of_agent agent))^"_"^(string_of_int (agent_id_of_agent agent))
 
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

  let print_test log env prefix test = 
    match test with 
      | Is_Here agent -> Printf.fprintf log "%sIs_Here(%s)\n" prefix (string_of_agent env agent)
      | Has_Internal (site,int) -> Printf.fprintf log "%sHas_Internal(%s~%s)\n" prefix (string_of_site env site) (string_of_internal_state env int)
      | Is_Free site -> Printf.fprintf log "%sIs_Free(%s)\n" prefix (string_of_site env site)
      | Is_Bound site -> Printf.fprintf log "%sIs_Bound(%s)\n" prefix (string_of_site env site)
      | Has_Binding_type (site,btype) -> Printf.fprintf log "%sBtype(%s,%s)\n" prefix (string_of_site env site) (string_of_btype env btype)
      | Is_Bound_to (site1,site2) -> Printf.fprintf log "%sIs_Bound(%s,%s)\n" prefix (string_of_site env site1) (string_of_site env site2)
     
  let print_action log env prefix action =
    match action with 
      | Create (agent,list) -> 
	  let _ = Printf.fprintf log "%sCreate(%s[" prefix (string_of_agent env agent) in 
	  let _ = 
	    List.fold_left 
              (fun bool (x,y) -> 
		 let _ = 
		   Printf.fprintf 
		     log 
		     "%s%s%s" 
		     (if bool then "," else "")
		     (string_of_site_name env x)
		     (match y with 
			| None -> ""
			| Some y -> "~y")
		 in true)
	      false list in
	  let _ = Printf.fprintf log "])\n" in
	    ()
      | Mod_internal (site,int) -> Printf.fprintf log "%sMod(%s~%s)\n" prefix (string_of_site env site) (string_of_internal_state env int)
      | Bind (site1,site2) | Bind_to (site1,site2) -> Printf.fprintf log "%sBind(%s,%s)\n" prefix (string_of_site env site1) (string_of_site env site2)
      | Unbind (site1,site2)  -> Printf.fprintf log "%sUnBind(%s,%s)\n" prefix (string_of_site env site1) (string_of_site env site2)
      | Free site ->  Printf.fprintf log "%sFree(%s)\n" prefix (string_of_site env site)
      | Remove agent -> Printf.fprintf log "%sRemove(%s)\n" prefix (string_of_agent env agent)


  let lhs_of_rule rule = rule.Dynamics.lhs 
  let lhs_of_event = compose lhs_of_rule rule_of_event
    
  let get_agent agent_id lhs fresh_map = 
    let i,map = 
      match agent_id 
      with 
	| Dynamics.KEPT i -> 
	    i,Mixture.agents lhs
	| Dynamics.FRESH i -> 
	    i,fresh_map 
    in 
      try 
	Mods.IntMap.find i map 
      with 
	| Not_found -> failwith "kappa_instantiation, line 130"
	    
  let name_of_agent agent_id event fresh_map = 
    let agent = get_agent agent_id event fresh_map in 
      Mixture.name agent

  let build_kappa_agent name interface = 
    Mixture.create_agent 
      name
      (List.fold_left
	  (fun map (a,b) -> Mods.IntMap.add a (b,Node.FREE) map)
	  Mods.IntMap.empty interface)

  let build_site a b = (a,b)

  let build_agent a b = (a,b)

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
      | Dynamics.KEPT i -> apply_embedding event i 
      | Dynamics.FRESH i -> apply_fresh_map event i 

  let get_binding_state_of_site agent_id site_name mixture embedding fresh_map =
    match agent_id 
    with 
      | Dynamics.KEPT(id) ->
	  begin 
	    match 
	      Mixture.follow (id,site_name) mixture
	    with 
	      | Some (ag,site) -> 
		  let fake_id = Dynamics.KEPT ag in 
		  let agent_id = apply_map ag embedding in 
		  let kappa_agent = get_agent fake_id mixture fresh_map in 
		  let agent_name = Mixture.name kappa_agent in 
		  let agent =  build_agent agent_id agent_name in 
		  let site = build_site agent site in 
		    BOUND_to (site)
	      | None -> 
		  begin 
		    let agent = get_agent agent_id mixture fresh_map in 
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
		  end 
	  end
      | Dynamics.FRESH _ -> ANY 



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

  let refine_bound_state site list list' fake_id lhs embedding = 
    let site_id = site_name_of_site site in 
    let state = get_binding_state_of_site fake_id site_id lhs embedding (Mods.IntMap.empty) in 
      begin
	match state 
	with 
	    BOUND_to (site2) -> 
	      add_bound_to site site2 list 
	  | _ -> list'
      end

  let tests_of_lhs lhs embedding =
    Mods.IntMap.fold 
	(fun lhs_id ag list -> 
	   let fake_id = Dynamics.KEPT lhs_id in 
	   let agent_id = apply_map lhs_id embedding in 
	   let agent_name = Mixture.name ag in 
	   let agent = build_agent agent_id agent_name in 
	     Mixture.fold_interface 
	       (fun site_id (int,lnk) list -> 
		  if site_id = 0 
		  then Is_Here(agent)::list
		  else 
		    let site = build_site agent site_id in 
		    let list = 
		      match int with 
			| Some i -> Has_Internal(site,i)::list
			| None -> list
		    in 
		    let list' = 
		      match lnk with 
			| Node.WLD -> list 
			| Node.FREE -> Is_Free(site)::list 
			| Node.BND -> Is_Bound(site)::list 
			| Node.TYPE(agent_name,site_name) -> Has_Binding_type(site,(agent_name,site_name))::list
		    in 
		      refine_bound_state site list list' fake_id lhs embedding 
	       )
	       ag list
	)
	(Mixture.agents lhs) []

  let tests_of_event event = 
    let rule = rule_of_event event in 
(*    let event = short_of event in *)
    let lhs = rule.Dynamics.lhs in 
    let embedding = embedding_of_event event in 
    tests_of_lhs lhs embedding 
      
  let tests_of_obs = tests_of_lhs 

  let agent_of_node n = build_agent (Node.get_address n) (Node.name n) 

  let create_init state event_list = 
    Graph.SiteGraph.fold
	  (fun node_id node list  ->
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
            (Init (agent,interface))::list)
          state.State.graph 
          event_list 

  let actions_of_init (init:init) kappa_sig  = 
    let agent,list_sites = init in 
    let list = [Create(agent,List.map (fun (x,(y,z)) -> (x,y)) list_sites)] in 
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
    list 

  let actions_of_event event kappa_sig = 
    let rule = rule_of_event event in 
    let lhs = rule.Dynamics.lhs in
    let embedding = embedding_of_event event in 
    let a,b,_ = 
      List.fold_left
	(fun (list_actions,side_sites,fresh) action -> 
	   match action 
	   with 
	     | Dynamics.BND((lhs_id1,site1),(lhs_id2,site2)) ->
		 let agent_id1 = apply_embedding_on_action event lhs_id1 in 
		 let agent_name1 = name_of_agent lhs_id1 lhs fresh in 
		 let agent1 = build_agent agent_id1 agent_name1 in
		 let site1 = build_site agent1 site1 in 
		 let agent_id2 = apply_embedding_on_action event lhs_id2 in 
		 let agent_name2 = name_of_agent lhs_id2 lhs fresh in 
		 let agent2 = build_agent agent_id2 agent_name2 in
		 let site2 = build_site agent2 site2 in 
		 let site1,site2 = order_site site1 site2 in 
		   (
		     Bind(site1,site2)::list_actions,
		     side_sites,
		     fresh
		   )
	     | Dynamics.FREE((lhs_id,site_name),bool) ->
		 let agent_id = apply_embedding_on_action event lhs_id in 
		 let agent_name = name_of_agent lhs_id lhs fresh in
		 let agent = build_agent agent_id agent_name in 
		 let site = build_site agent site_name in 
		 let list_actions = (Free site)::list_actions in 
                 let state = get_binding_state_of_site lhs_id site_name lhs embedding fresh in 
                 if bool 
		 then 
                   match state 
                   with 
                     | BOUND_to site  -> 
                       (Free site)::list_actions,
                       side_sites,
                       fresh
                     | _ -> raise (invalid_arg "actions_of_event") 
		 else 
		   let state = get_binding_state_of_site lhs_id site_name lhs embedding fresh in 
		   list_actions,
                   (site,state)::side_sites,
                   fresh
		     
	     | Dynamics.MOD((lhs_id,site),internal) -> 
		 let agent_id = apply_embedding_on_action event lhs_id in 
		 let agent_name = name_of_agent lhs_id lhs fresh in 
		 let agent = build_agent agent_id agent_name in 
		 let site = build_site agent site in 
		   Mod_internal(site,internal)::list_actions,
		   side_sites,
		   fresh
		   
	     | Dynamics.DEL(lhs_id) -> 
		 let fake_id = Dynamics.KEPT lhs_id in 
		 let agent_id = apply_embedding event lhs_id in 
		 let agent_name = name_of_agent fake_id lhs fresh in 
		 let agent = build_agent agent_id agent_name in 
		 let interface = get_binding_sites kappa_sig agent_name in 
		   Remove(agent)::list_actions,
		   List.fold_left 
		     (fun list site -> 
			let state = get_binding_state_of_site fake_id  site lhs embedding fresh in 
			  begin 
			    match state with 
			      | FREE | BOUND_to _ -> list 
			      | _ -> (build_site agent site,state)::list
			  end
		     )
		     side_sites interface,
		 fresh 
		
	     | Dynamics.ADD(rhs_id,agent_name) -> 
		 let agent_id = apply_embedding_on_action event (Dynamics.FRESH rhs_id) in 
		 let interface = get_default_state kappa_sig agent_name in 
		 let agent = build_agent agent_id agent_name in 
		 let kappa_agent = build_kappa_agent agent_name interface in 
		 let list_actions' = Create(agent,interface)::list_actions in 
		 let fresh' = add_asso rhs_id kappa_agent fresh in 
		   list_actions',side_sites,fresh')
	([],[],Mods.IntMap.empty)
	rule.Dynamics.script 
    in a,b

      

  let refine_event env event = (event,tests_of_event event,actions_of_event event env)
    
  let refine_obs env obs = 
    let _,lhs,embedding = obs in 
    obs,tests_of_obs lhs embedding 

  let obs_of_refined_obs = fst 

  let event_of_refined_event (a,_,_) = a

  let refine_init env init = (init,actions_of_init init env)

  let init_of_refined_init = fst 

  let tests_of_obs (i,mixture,phi) = 
    tests_of_lhs mixture phi 

  let tests_of_refined_obs = snd 

  let tests_of_refined_init _ = []
  let tests_of_refined_event (_,y,_) =  y
  let actions_of_refined_event (_,_,y) = y
  let actions_of_refined_init (_,x) = x,[]
  let actions_of_refined_obs _ = [],[]
  let rule_of_refined_event x = (compose rule_of_event event_of_refined_event) x 

  let print_side_effects log env prefix (site,state) = 
    Printf.fprintf 
      log 
      "%sSide_effects(%s:%s)\n" 
      prefix 
      (string_of_site env site)
      (string_of_binding_state env state)
      
  let print_refined_obs log env refined_obs = () 

  let print_refined_event log env refined_event = 
    let _ = Printf.fprintf log "***Refined event:***\n" in 
    let _ = Printf.fprintf log "* Kappa_rule \n" in 
    let _ = Dynamics.dump (rule_of_refined_event refined_event) env in 
    let _ = 
      if debug_mode 
      then 
        let _ = Printf.fprintf log "Story encoding: \n" in 
	let _ = List.iter (print_test log env " ") (tests_of_refined_event refined_event) in 
	let actions = actions_of_refined_event refined_event in 
	let _ = List.iter (print_action log env " ") (fst actions) in 
	let _ = List.iter (print_side_effects log env " ") (snd actions) in 
	let _ = Printf.fprintf log "***\n"  in 
        () 
    in 
      ()

  let print_refined_init log env refined_init = ()
  
  let gen f1 f2 f3 f4 step = 
    match step
    with 
      | Event a -> f1 a 
      | Init a -> f2 a
      | Obs a -> f3 a 
      | Dummy  -> f4 ()

  let genbis f1 f2 f3  = 
    gen (fun a -> Event (f1 a)) (fun a -> Init (f2 a)) (fun a -> Obs (f3 a))     (fun a -> Dummy)
  
  let print_refined_step log env = 
    gen (print_refined_event log env) (print_refined_init log env) (print_refined_obs log env) (fun _  -> ())

  let tests_of_refined_step =
    gen tests_of_refined_event tests_of_refined_init tests_of_refined_obs 
(fun _ -> [])

  let is_obs_of_refined_step x = 
    match x 
    with 
      | Obs _ -> true
      | _ -> false
      
  let refine_step env (x:step) = 
    genbis (refine_event env) (refine_init env) (refine_obs env) x
  
  let step_of_refined_step = 
    genbis event_of_refined_event init_of_refined_init obs_of_refined_obs 

  let actions_of_refined_step = 
    gen actions_of_refined_event actions_of_refined_init actions_of_refined_obs (fun _ -> [],[])

  let import_event x = x 
  let import_env x = x
  let store_event (event:event) (step_list:step list) = 
    (Event event)::step_list    
  let store_init init step_list = create_init init step_list  
  let store_obs (i,a,x) step_list = Obs(i,a,x)::step_list 

  let build_grid list env = 
    let grid = Causal.empty_grid () in 
    let grid,_ = 
      List.fold_left 
        (fun (grid,side_effect) (k,_) ->
          match k 
          with 
            | Event (a,_,_) -> 
              begin 
                let d = [] in 
                let side_effect =
                  List.fold_left
                    (fun set i -> Mods.Int2Set.add i set)
                    side_effect 
                    d 
                in 
                let obs_from_rule_app,r,counter = get_causal a in 
                let phi = embedding_of_event a in 
                let psi = fresh_map_of_event a in 
                let _ = Printf.fprintf stderr "EVENT\n" in 
                Causal.record ~decorate_with:obs_from_rule_app r side_effect (phi,psi) counter grid env,Mods.Int2Set.empty 
              end
            | Init b -> 
              begin 
                let _ = Printf.fprintf stderr "INIT\n" in 
                grid,side_effect
              end
            | Obs c  -> 
              begin 
                let _ = Printf.fprintf stderr "OBS\n" in 
                grid,side_effect
              end 
            | Dummy -> 
              let _ = Printf.fprintf stderr "DUMMY\n" in 
              let d = [] in 
              grid,
              List.fold_left 
                (fun side_effect x -> Mods.Int2Set.add x side_effect)
                side_effect d 
        ) 
        (grid,Mods.Int2Set.empty) list 
    in grid 
end:Cflow_signature)

