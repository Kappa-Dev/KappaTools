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

module P = StoryProfiling.StoryStats

    
module type Cflow_signature =
sig
  module H:Cflow_handler.Cflow_handler
 
  type agent_id = int 
  module AgentIdSet:SetMap.Set with type elt = agent_id

  type internal_state = int 

  type side_effect = Instantiation.concrete Instantiation.site list
  type refined_event = Causal.event_kind * Instantiation.concrete Instantiation.event
  type refined_obs =
      Causal.event_kind * Instantiation.concrete Instantiation.test list * unit Mods.simulation_info
  type refined_step

  val empty_side_effect: side_effect
  val dummy_refined_step: string -> refined_step
  val agent_name_of_binding_type: Instantiation.binding_type -> Instantiation.agent_name
  val site_name_of_binding_type: Instantiation.binding_type -> Instantiation.site_name
  val agent_id_of_agent:
    Instantiation.concrete -> int
  val agent_name_of_agent: Instantiation.concrete -> Instantiation.agent_name
  val agent_of_site: Instantiation.concrete Instantiation.site -> Instantiation.concrete
  val agent_id_of_site: Instantiation.concrete Instantiation.site -> int
  val agent_name_of_site: Instantiation.concrete Instantiation.site -> Instantiation.agent_name
  val site_name_of_site: Instantiation.concrete Instantiation.site -> Instantiation.site_name
  val build_subs_refined_step: int -> int -> refined_step
  val tests_of_refined_step: (refined_step, Instantiation.concrete Instantiation.test list) H.unary
  val actions_of_refined_step:
    (refined_step,
       (Instantiation.concrete Instantiation.action list *
	  (Instantiation.concrete Instantiation.site*Instantiation.concrete Instantiation.binding_state) list)) H.unary
  val is_obs_of_refined_step: refined_step -> bool
  val is_init_of_refined_step: refined_step -> bool
  val is_subs_of_refined_step: refined_step -> bool
  val is_event_of_refined_step: refined_step -> bool
  val simulation_info_of_refined_step:
    refined_step -> unit Mods.simulation_info option
  val print_side:
    Format.formatter -> H.handler -> string  ->
    (Instantiation.concrete Instantiation.site*Instantiation.concrete Instantiation.binding_state) -> unit

  val print_refined_step:
    ?handler:H.handler -> Format.formatter -> refined_step -> unit

  val store_event:
    P.log_info -> refined_event -> refined_step list -> P.log_info * refined_step list
  val store_obs :
    P.log_info -> refined_obs  -> refined_step list -> P.log_info * refined_step list

  val build_grid:
    (refined_step * Instantiation.concrete Instantiation.site list)  list -> bool ->
    H.handler -> Causal.grid
  val print_side_effect: Format.formatter -> side_effect -> unit
  val side_effect_of_list: Instantiation.concrete Instantiation.site list -> side_effect

  val get_kasim_side_effects: refined_step -> side_effect

  val level_of_event: (refined_step,(agent_id -> bool),Priority.level) H.binary
  val disambiguate: refined_step list -> refined_step list
  val clean_events: refined_step list -> refined_step list
  val agent_id_in_obs: (refined_step, AgentIdSet.t) H.unary 

  val fill_siphon: refined_step list -> refined_step list
  val split_init: refined_step list -> refined_step list 
  val agent_id_in_obs: (refined_step, AgentIdSet.t) H.unary
end



module Cflow_linker =
  (struct
    module H = Cflow_handler.Cflow_handler
    module P = StoryProfiling.StoryStats
    module PI = Instantiation

  type agent_id = int

  module AgIdSet = Mods.IntSet
  type side_effect = PI.concrete PI.site list

  module AgentIdMap = Mods.IntMap
  module AgentIdSet = Mods.IntSet
  module SiteMap = Mods.IntMap
  module SiteSet = Mods.IntSet
  type internal_state  = int 

  type refined_event =
      Causal.event_kind * PI.concrete PI.event
  type refined_obs =
      Causal.event_kind *
	PI.concrete PI.test list *
	  unit Mods.simulation_info
  type refined_step =
  | Subs of (agent_id * agent_id)
  | Event of refined_event
  | Init of PI.concrete PI.action list
  | Obs of refined_obs
  | Dummy  of string

  let build_subs_refined_step a b = Subs (a,b)
  let get_kasim_side_effects = function
    | Event ((_,(_,(_,_,a)))) -> a
    | Subs _ | Obs _ | Dummy _ | Init _ -> []

  let dummy_refined_step x = Dummy x
  let empty_side_effect = []

  let site_name_of_binding_type = snd
  let agent_name_of_binding_type = fst
  let agent_id_of_agent = fst

  let agent_name_of_agent = snd

  let agent_of_site = fst

  let agent_id_of_site x = agent_id_of_agent @@ agent_of_site x

  let agent_name_of_site x = agent_name_of_agent @@ agent_of_site x

  let site_name_of_site = snd

  let print_site ?env f ((ag_id,ag),s) =
    Format.fprintf f "%a_%i.%a"
		   (Environment.print_agent ?env) ag ag_id
		   (match env with
		    | Some env ->
		       Signature.print_site (Environment.signatures env) ag
		    | None -> Format.pp_print_int) s

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

  let print_side log hand prefix (s,binding_state) =
    let env = hand.H.env in
    Format.fprintf log "%s(%a,%a)\n" prefix (print_site ~env) s
		   (print_binding_state ~env) binding_state

  let tests_of_refined_obs _ _ info error (_,x,_) = error, info, x
  let tests_of_refined_event _ _ info error (_,(y,(_,_,_))) =  error, info, y
  let tests_of_refined_init _ _ info error _ =  error, info, []
  let tests_of_refined_subs _ _ info error _ _ = error, info, []
  let actions_of_refined_event _ _ info error (_,(_,(x,y,_))) = error, info, (x,y)
  let actions_of_refined_init _ _ info error y = error, info, (y,[])
  let actions_of_refined_obs _ _ info error _ = error, info, ([],[])
  let actions_of_refined_subs _ _ info error _ _ = error, info, ([],[])

  let print_side_effects ?handler =
    let env = Tools.option_map (fun x -> x.H.env) handler in
    Pp.list Pp.space
	    (fun f (site,state) ->
	     Format.fprintf
	       f "Side_effects(%a:%a)@,"
	       (print_site ?env) site (print_binding_state ?env) state)

  let print_refined_obs ?handler f (ev_kind,tests,_) =
    let sigs = match handler with
      | None -> None
      | Some handler -> Some (Environment.signatures handler.H.env) in
    Format.fprintf
      f "***@[<1>OBS %s:%a@]***"
      (Causal.label ?env:(Tools.option_map (fun x -> x.H.env) handler) ev_kind)
      (Pp.list Pp.space (PI.print_concrete_test ?sigs)) tests

  let print_refined_subs _f (_a,_b)  = ()

  let print_refined_init ?handler log actions =
    let sigs = match handler with
      | None -> None
      | Some handler -> Some (Environment.signatures handler.H.env) in
    Format.fprintf log "***@[<1>INIT:%a@]***"
		   (Pp.list Pp.space (PI.print_concrete_action ?sigs)) actions

  let print_refined_event ?handler log (ev_kind,(tests,(actions,side_sites,_))) =
    let sigs = match handler with
      | None -> None
      | Some handler -> Some (Environment.signatures handler.H.env) in
    let () = Format.fprintf log "@[***Refined event:***@,* Kappa_rule %s@,"
			    (Causal.label ?env:(Tools.option_map (fun x -> x.H.env) handler) ev_kind) in
    Format.fprintf log "Story encoding:@[<1>@,%a%a%a@]@,***@]"
		   (Pp.list ~trailing:Pp.space Pp.space (PI.print_concrete_test ?sigs))
		   tests
		   (Pp.list ~trailing:Pp.space Pp.space (PI.print_concrete_action ?sigs))
		   actions
		   (print_side_effects ?handler) side_sites

  let gen f0 f1 f2 f3 f4 (p:H.parameter) h l e step =
    match step with
    | Subs (a,b) -> f0 p h l e a b
    | Event (x,y) -> f1 p h l e (x,y)
    | Init a -> f2 p h l e a
    | Obs a -> f3 p h l e a
    | Dummy x  -> f4 p h l e x

  let print_refined_step ?handler f = function
    | Subs (a,b) -> print_refined_subs f (a,b)
    | Event (x,y) -> print_refined_event ?handler f (x,y)
    | Init a -> print_refined_init ?handler f a
    | Obs a -> print_refined_obs ?handler f a
    | Dummy _  -> ()

  let tests_of_refined_step =
    gen
      tests_of_refined_subs
      tests_of_refined_event
      tests_of_refined_init
      tests_of_refined_obs
      (fun _ _ l error _ -> error, l, ([]:Instantiation.concrete Instantiation.test list))

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

  let actions_of_refined_step =
    gen
      actions_of_refined_subs
      actions_of_refined_event
      actions_of_refined_init
      actions_of_refined_obs
      (fun _ _ l error _ -> error,l,([],[]))
  let store_event log_info (event:refined_event) (step_list:refined_step list) =
    match event with
    | Causal.INIT _,(_,(actions,_,_)) ->
       P.inc_n_kasim_events log_info,(Init actions)::step_list
    | Causal.OBS _,_ -> assert false
    | (Causal.RULE _ | Causal.PERT _ as k),x ->
       P.inc_n_kasim_events log_info,(Event (k,x))::step_list
  let store_obs log_info (i,x,c) step_list =
    P.inc_n_obs_events log_info,Obs(i,x,c)::step_list

  let creation_of_actions op actions =
    List.fold_left
      (fun l -> function
	     | PI.Create (x,_) -> op x :: l
	     | PI.Mod_internal _ | PI.Bind _ | PI.Bind_to _ | PI.Free _
	     | PI.Remove _ -> l) [] actions
  let creation_of_event = function
    | (Event (_,(_,(ac,_,_))) | Init ac) -> creation_of_actions fst ac
    | Obs _ | Dummy _ | Subs _ -> []

  let build_grid list bool handler =
    let env = handler.H.env in
    let empty_set = [] in
    let grid = Causal.empty_grid () in
    let grid,_,_,_ =
      List.fold_left
        (fun (grid,side_effect,counter,subs) (k,side) ->
	 let maybe_side_effect =
	   if bool then fun se -> se
	   else fun _ -> List.rev_append side_effect side in
	 let translate y = Mods.IntMap.find_default y y subs in
         match (k:refined_step) with
         | Event (id,event) ->
	    let (tests,(actions,side_effects,kappa_side)) =
	      PI.subst_map_agent_in_concrete_event translate event in
            let kasim_side_effect = maybe_side_effect kappa_side in
            Causal.record
	      (id,(tests,(actions,side_effects,kasim_side_effect)))
	      counter env grid,
            empty_set,counter+1,Mods.IntMap.empty
         | Obs (id,tests,info) ->
	    let tests' =
	      Tools.list_smart_map
		(PI.subst_map_agent_in_concrete_test translate) tests in
	    Causal.record_obs
	      (id,tests',info) side_effect counter grid,
	    maybe_side_effect empty_set,counter+1,Mods.IntMap.empty
         | Subs (a,b) ->
            grid, side_effect, counter, Mods.IntMap.add a b subs
	 | Init actions ->
	    let actions' =
	      Tools.list_smart_map
		(PI.subst_map_agent_in_concrete_action translate) actions in
            Causal.record_init (creation_of_actions snd actions',actions')
			       counter env grid,
	    side_effect,counter+1,Mods.IntMap.empty
         | Dummy _ ->
            grid, maybe_side_effect empty_set, counter, subs
        )
        (grid,empty_set,1,Mods.IntMap.empty) list
    in grid

  let clean_events =
    List.filter
      (function Event _ | Obs _ | Init _ -> true | Dummy _ | Subs _ -> false)

  let print_side_effect =
    Pp.list Pp.comma (fun f ((a,_),b) -> Format.fprintf f "(%i,%i)," a b)
  let side_effect_of_list l = l

  let level_of_event parameter handler log_info error e set =
    match H.get_priorities parameter with
    | None -> error,log_info,0
    | Some priorities ->
       match e with
       | Obs _ -> error,log_info,priorities.Priority.other_events
       | Event _ ->
          begin
            let error,log_info,actions =
	      actions_of_refined_step parameter handler log_info error e in
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
            error,log_info,priority
          end
       | (Dummy _ | Subs _ | Init _) ->
	  error,log_info,priorities.Priority.substitution

  let subs_agent_in_event mapping mapping' = function
    | Event (a,event) ->
       Event
	 (a,
	  PI.subst_map2_agent_in_concrete_event
	    (fun x -> AgentIdMap.find_default x x mapping)
	    (fun x -> AgentIdMap.find_default x x mapping')
	    event)
    | Obs (a,b,c) ->
       Obs(a,
	   Tools.list_smart_map
	     (PI.subst_map_agent_in_concrete_test
		(fun x -> AgentIdMap.find_default x x mapping)) b,
	   c)
    | Init b ->
       Init
	 (Tools.list_smart_map
	    (PI.subst_map_agent_in_concrete_action
	       (fun x -> AgentIdMap.find_default x x mapping')) b)
    | Dummy _ | Subs _ as event -> event

  let disambiguate event_list =
    let _,_,_,event_list_rev =
      List.fold_left
        (fun (max_id,used,mapping,event_list) event ->
         let max_id,used,mapping' =
           List.fold_left
             (fun (max_id,used,mapping) x ->
              if AgentIdSet.mem x used
              then
                (max_id+1,AgentIdSet.add (max_id+1) used,
		 AgentIdMap.add x (max_id+1) mapping)
              else (max x max_id,AgentIdSet.add x used,mapping))
             (max_id,used,mapping) (creation_of_event event) in
         let list = (subs_agent_in_event mapping mapping' event)::event_list in
         max_id,used,mapping',list)
        (0,AgentIdSet.empty,AgentIdMap.empty,[])
        event_list
    in List.rev event_list_rev

  type agent_info = 
    {
      initial_step: refined_step ;
      internal_states: internal_state SiteMap.t ;
      bound_sites: SiteSet.t ;
      sites_with_wrong_internal_state: SiteSet.t 
    }

  let convert_init remanent step_list action_list =
    let extract_agent id soup =
      List.partition
	(function
	  | (PI.Create ((id',_),_) | PI.Mod_internal (((id',_),_),_) |
	     PI.Free ((id',_),_) | PI.Bind_to (((id',_),_),_)) -> id = id'
	  | (PI.Bind _ | PI.Remove _) -> failwith "Problematic initial event")
	soup in
    let rec aux recur acc soup = function
      | [] -> (if soup <> [] then Init soup::acc else acc),recur
      | PI.Free _ :: t -> aux recur acc soup t
      | (PI.Bind _ | PI.Remove _ | PI.Bind_to _ | PI.Mod_internal _) :: t ->
	 aux recur acc soup t
      | PI.Create ((id,_),site_list) :: t ->
	 let this,soup' = extract_agent id soup in
	 let standalone =
	   List.for_all
	     (function
	       | (PI.Create _ | PI.Free _) -> true
	       | (PI.Mod_internal _ |PI.Bind_to _ | PI.Bind _ | PI.Remove _) -> false)
	     this in
	 let this = Init this in
	 if standalone then
	   let map =
	     List.fold_left
	       (fun map -> function
			| s, Some u -> SiteMap.add s u map
			| _, None -> map)
	       SiteMap.empty site_list in
	   let agent_info =
	     {
	       initial_step=this;
	       internal_states=map;
	       bound_sites=SiteSet.empty;
	       sites_with_wrong_internal_state=SiteSet.empty
	     }
	   in aux (AgentIdMap.add id agent_info recur) (this::acc) soup' t
	 else aux recur (this::acc) soup' t
    in aux remanent step_list action_list action_list

  let as_init agent_info = 
    SiteSet.is_empty agent_info.bound_sites 
      && SiteSet.is_empty agent_info.sites_with_wrong_internal_state 

  let mod_site site state (remanent,set) = 
    let agid = agent_id_of_site site in 
    let s_name = site_name_of_site site in 
    match AgentIdMap.find_option agid remanent with
    | None -> remanent,set
    | Some ag_info ->
       match SiteMap.find_option s_name ag_info.internal_states with
       | None -> remanent,set
       | Some state_ref ->
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
	
  let unbind_side (agid,s_name) (remanent,set) = 
    match AgentIdMap.find_option agid remanent with
    | None -> remanent,set
    | Some ag_info ->
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


  let unbind site rem  = 
    let agid = agent_id_of_site site in 
    let s_name = site_name_of_site site in 
     unbind_side (agid,s_name) rem 

  let bind site (remanent,set) = 
    let agid = agent_id_of_site site in 
    let s_name = site_name_of_site site in 
    match AgentIdMap.find_option agid remanent with
    | None -> remanent,set
    | Some ag_info ->
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

  let split_init refined_step_list =
    let remanent = AgentIdMap.empty in 
    fst (List.fold_left
      (fun (step_list,remanent) refined_step ->
       match refined_step
       with
       | Init init -> convert_init remanent step_list init
       | _ -> (refined_step::step_list,remanent))
      ([],remanent)
      (List.rev refined_step_list))

  let fill_siphon refined_step_list =
    let remanent = AgentIdMap.empty in 
    let a,_ =
      List.fold_left
	(fun (step_list,remanent) refined_step -> 
	 match refined_step with
	 | Init init -> convert_init remanent step_list init
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
		 match AgentIdMap.find_option id remanent with
		 | Some x -> x.initial_step::list
		 | None -> list)
		set
		(refined_step::step_list)),remanent)
	 | Subs _ | Obs _ | Dummy _ -> (refined_step::step_list,remanent))
	([],remanent)
	refined_step_list in
    List.rev a

  let agent_id_in_obs _parameter _handler info error = function
    | Subs _ | Event _ | Init _ | Dummy _ -> error,info,AgentIdSet.empty
    | Obs (_,tests,_) ->
       error, info, 
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
