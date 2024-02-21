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
  let error, y = g parameter handler error x in
  f parameter handler error y

module P = StoryProfiling.StoryStats

module type Cflow_signature = sig
  module H : Cflow_handler.Cflow_handler

  type agent_id = int

  module AgentIdSet : SetMap.Set with type elt = agent_id

  type internal_state = int
  type side_effect = Instantiation.concrete Instantiation.site list

  val empty_side_effect : side_effect

  val agent_name_of_binding_type :
    Instantiation.binding_type -> Instantiation.agent_name

  val site_name_of_binding_type :
    Instantiation.binding_type -> Instantiation.site_name

  val agent_id_of_agent : Instantiation.concrete -> int
  val agent_name_of_agent : Instantiation.concrete -> Instantiation.agent_name

  val agent_of_site :
    Instantiation.concrete Instantiation.site -> Instantiation.concrete

  val agent_id_of_site : Instantiation.concrete Instantiation.site -> int

  val agent_name_of_site :
    Instantiation.concrete Instantiation.site -> Instantiation.agent_name

  val site_name_of_site :
    Instantiation.concrete Instantiation.site -> Instantiation.site_name

  val build_grid :
    (Trace.step * Instantiation.concrete Instantiation.site list) list ->
    bool ->
    H.handler ->
    Causal.grid

  val print_side_effect : Loggers.t -> side_effect -> unit

  val side_effect_of_list :
    Instantiation.concrete Instantiation.site list -> side_effect

  val get_id_of_refined_step : Trace.step -> int option
  val get_time_of_refined_step : Trace.step -> float option

  val level_of_event :
    Priority.priorities option ->
    (Trace.step, agent_id -> bool, Priority.level) H.binary

  val disambiguate : Trace.t -> Trace.t
  val clean_events : Trace.t -> Trace.t
  val fill_siphon : Trace.t -> Trace.t
  val split_init : Trace.t -> Trace.t
  val agent_id_in_obs : (Trace.step, AgentIdSet.t) H.unary
end

module Cflow_linker : Cflow_signature = struct
  module H = Cflow_handler.Cflow_handler
  module PI = Instantiation

  type agent_id = int
  type side_effect = PI.concrete PI.site list

  module AgentIdMap = Mods.IntMap
  module AgentIdSet = Mods.IntSet
  module SiteMap = Mods.IntMap
  module SiteSet = Mods.IntSet

  type internal_state = int

  let empty_side_effect = []
  let site_name_of_binding_type = snd
  let agent_name_of_binding_type = fst
  let agent_id_of_agent = fst
  let agent_name_of_agent = snd
  let agent_of_site = fst
  let agent_id_of_site x = agent_id_of_agent @@ agent_of_site x
  let agent_name_of_site x = agent_name_of_agent @@ agent_of_site x
  let site_name_of_site = snd

  let get_gen_of_refined_step f x =
    match Trace.simulation_info_of_step x with
    | None -> None
    | Some a -> Some (f a)

  let get_time_of_refined_step x =
    get_gen_of_refined_step (fun x -> x.Trace.Simulation_info.story_time) x

  let get_id_of_refined_step x =
    get_gen_of_refined_step (fun x -> x.Trace.Simulation_info.story_event) x

  let build_grid list bool handler =
    let env = handler.H.env in
    let empty_set = [] in
    let grid = Causal.empty_grid () in
    let grid, _, _, _ =
      List.fold_left
        (fun (grid, side_effect, counter, subs) (k, side) ->
          let maybe_side_effect =
            if bool then
              fun se ->
            se
            else
              fun _ ->
            List.rev_append side_effect side
          in
          let translate y = Mods.IntMap.find_default y y subs in
          match k with
          | Trace.Rule (id, event, info) ->
            let event' = PI.subst_map_agent_in_concrete_event translate event in
            let side_effects_dst =
              maybe_side_effect event'.Instantiation.side_effects_dst
            in
            ( Causal.record
                ( Trace.RULE id,
                  {
                    Instantiation.tests = event'.Instantiation.tests;
                    Instantiation.actions = event'.Instantiation.actions;
                    Instantiation.side_effects_src =
                      event'.Instantiation.side_effects_src;
                    Instantiation.side_effects_dst;
                    Instantiation.connectivity_tests =
                      event'.Instantiation.connectivity_tests;
                  },
                  info )
                counter env grid,
              empty_set,
              counter + 1,
              Mods.IntMap.empty )
          | Trace.Pert (id, event, info) ->
            let event' = PI.subst_map_agent_in_concrete_event translate event in
            let side_effects_dst =
              maybe_side_effect event'.Instantiation.side_effects_dst
            in
            ( Causal.record
                ( Trace.PERT id,
                  {
                    Instantiation.tests = event'.Instantiation.tests;
                    Instantiation.actions = event'.Instantiation.actions;
                    Instantiation.side_effects_src =
                      event'.Instantiation.side_effects_src;
                    Instantiation.side_effects_dst;
                    Instantiation.connectivity_tests =
                      event'.Instantiation.connectivity_tests;
                  },
                  info )
                counter env grid,
              empty_set,
              counter + 1,
              Mods.IntMap.empty )
          | Trace.Obs (id, tests, info) ->
            let tests' =
              List_util.smart_map
                (List_util.smart_map
                   (PI.subst_map_agent_in_concrete_test translate))
                tests
            in
            ( Causal.record_obs (id, tests', info) side_effect counter grid,
              maybe_side_effect empty_set,
              counter + 1,
              Mods.IntMap.empty )
          | Trace.Subs (a, b) ->
            grid, side_effect, counter, Mods.IntMap.add a b subs
          | Trace.Init actions ->
            let actions' =
              List_util.smart_map
                (PI.subst_map_agent_in_concrete_action translate)
                actions
            in
            ( Causal.record_init
                (Trace.creation_of_actions snd actions', actions')
                counter env grid,
              side_effect,
              counter + 1,
              Mods.IntMap.empty )
          | Trace.Dummy _ -> grid, maybe_side_effect empty_set, counter, subs)
        (grid, empty_set, 1, Mods.IntMap.empty)
        list
    in
    grid

  let clean_events =
    List.filter (function
      | Trace.Rule _ | Trace.Pert _ | Trace.Obs _ | Trace.Init _ -> true
      | Trace.Dummy _ | Trace.Subs _ -> false)

  let print_side_effect log =
    List.iter (fun ((a, _), b) -> Loggers.fprintf log "(%i,%i)," a b)

  let side_effect_of_list l = l

  let level_of_event priority_opt parameter _handler log_info error e set =
    match priority_opt, H.get_priorities parameter with
    | None, None -> error, log_info, Priority.highest
    | Some priorities, _ | None, Some priorities ->
      (match e with
      | Trace.Obs _ -> error, log_info, priorities.Priority.other_events
      | Trace.Rule _ | Trace.Pert _ ->
        let actions = Trace.actions_of_step e in
        let priority =
          List.fold_left
            (fun priority -> function
              | PI.Create (ag, _) ->
                let ag_id = agent_id_of_agent ag in
                if set ag_id then
                  priority
                else
                  Priority.min_level priority priorities.Priority.creation
              | PI.Remove _ ->
                Priority.min_level priority priorities.Priority.removal
              | PI.Mod_internal _ -> priority
              | PI.Free _ ->
                Priority.min_level priority priorities.Priority.unbinding
              | PI.Bind (_, _) | PI.Bind_to (_, _) -> priority)
            priorities.Priority.other_events (fst actions)
        in
        error, log_info, priority
      | Trace.Dummy _ | Trace.Subs _ | Trace.Init _ ->
        error, log_info, priorities.Priority.substitution)

  let subs_agent_in_event mapping mapping' = function
    (* mapping -> before the event, including agents to be removed *)
    (* mapping' -> after the event, including agents to be created *)
    (* This is useful when one agent is removed, and one is created with the same id in a single event *)
    | Trace.Rule (a, event, info) ->
      Trace.Rule
        ( a,
          PI.subst_map2_agent_in_concrete_event
            (fun x -> AgentIdMap.find_default x x mapping)
            (fun x -> AgentIdMap.find_default x x mapping')
            event,
          info )
    | Trace.Pert (a, event, info) ->
      Trace.Pert
        ( a,
          PI.subst_map2_agent_in_concrete_event
            (fun x -> AgentIdMap.find_default x x mapping)
            (fun x -> AgentIdMap.find_default x x mapping')
            event,
          info )
    | Trace.Obs (a, b, c) ->
      Trace.Obs
        ( a,
          List_util.smart_map
            (List_util.smart_map
               (PI.subst_map_agent_in_concrete_test (fun x ->
                    AgentIdMap.find_default x x mapping)))
            b,
          c )
    | Trace.Init b ->
      Trace.Init
        (List_util.smart_map
           (PI.subst_map_agent_in_concrete_action (fun x ->
                AgentIdMap.find_default x x mapping'))
           b)
    | (Trace.Dummy _ | Trace.Subs _) as event -> event

  let disambiguate event_list =
    let _, _, _, event_list_rev =
      List.fold_left
        (fun (max_id, used, mapping, event_list) event ->
          let max_id, used, mapping' =
            List.fold_left
              (fun (max_id, used, mapping) x ->
                if AgentIdSet.mem x used then
                  ( max_id + 1,
                    AgentIdSet.add (max_id + 1) used,
                    AgentIdMap.add x (max_id + 1) mapping )
                else
                  max x max_id, AgentIdSet.add x used, mapping)
              (max_id, used, mapping)
              (Trace.creation_of_step event)
          in
          (* mapping can be safely applied to all agents except the newly created ones *)
          (* mapping' can be safely applied to all agents except the ones that have been just removes *)
          let list = subs_agent_in_event mapping mapping' event :: event_list in
          max_id, used, mapping', list)
        (0, AgentIdSet.empty, AgentIdMap.empty, [])
        event_list
    in
    List.rev event_list_rev

  type agent_info = {
    initial_step: Trace.step;
    internal_states: internal_state SiteMap.t;
    bound_sites: SiteSet.t;
    sites_with_wrong_internal_state: SiteSet.t;
  }

  let convert_init remanent step_list action_list =
    let extract_agent id soup =
      List.partition
        (function
          | PI.Create ((id', _), _)
          | PI.Mod_internal (((id', _), _), _)
          | PI.Free ((id', _), _)
          | PI.Bind_to (((id', _), _), _) ->
            id = id'
          | PI.Bind _ | PI.Remove _ -> failwith "Problematic initial event")
        soup
    in
    let rec aux recur acc soup = function
      | [] ->
        ( (if soup <> [] then
             Trace.Init soup :: acc
           else
             acc),
          recur )
      | PI.Free _ :: t -> aux recur acc soup t
      | (PI.Bind _ | PI.Remove _ | PI.Bind_to _ | PI.Mod_internal _) :: t ->
        aux recur acc soup t
      | PI.Create ((id, _), site_list) :: t ->
        let this, soup' = extract_agent id soup in
        let standalone =
          List.for_all
            (function
              | PI.Create _ | PI.Free _ | PI.Mod_internal _ -> true
              | PI.Bind_to _ | PI.Bind _ | PI.Remove _ -> false)
            this
        in
        let this = Trace.Init this in
        if standalone then (
          let map =
            List.fold_left
              (fun map -> function
                | s, Some u -> SiteMap.add s u map
                | _, None -> map)
              SiteMap.empty site_list
          in
          let agent_info =
            {
              initial_step = this;
              internal_states = map;
              bound_sites = SiteSet.empty;
              sites_with_wrong_internal_state = SiteSet.empty;
            }
          in
          aux (AgentIdMap.add id agent_info recur) (this :: acc) soup' t
        ) else
          aux recur (this :: acc) soup' t
    in
    aux remanent step_list action_list action_list

  let as_init restriction_map agid agent_info =
    let restriction =
      AgentIdMap.find_default SiteSet.empty agid restriction_map
    in
    SiteSet.is_empty (SiteSet.inter agent_info.bound_sites restriction)
    && SiteSet.is_empty
         (SiteSet.inter agent_info.sites_with_wrong_internal_state restriction)

  let mod_site restriction_map site state (remanent, set) =
    let agid = agent_id_of_site site in
    let s_name = site_name_of_site site in
    match AgentIdMap.find_option agid remanent with
    | None -> remanent, set
    | Some ag_info ->
      (match SiteMap.find_option s_name ag_info.internal_states with
      | None -> remanent, set
      | Some state_ref ->
        if state_ref = state (* Back to the original internal state*) then
          if SiteSet.mem s_name ag_info.sites_with_wrong_internal_state then (
            let ag_info =
              {
                ag_info with
                sites_with_wrong_internal_state =
                  SiteSet.remove s_name ag_info.sites_with_wrong_internal_state;
              }
            in
            let remanent = AgentIdMap.add agid ag_info remanent in
            if as_init restriction_map agid ag_info then
              remanent, AgentIdSet.add agid set
            else
              remanent, set
          ) else
            remanent, set
        else if
          (* No longer the default state *)
          SiteSet.mem s_name ag_info.sites_with_wrong_internal_state
        then
          remanent, set
        else (
          let ag_info =
            {
              ag_info with
              sites_with_wrong_internal_state =
                SiteSet.add s_name ag_info.sites_with_wrong_internal_state;
            }
          in
          let remanent = AgentIdMap.add agid ag_info remanent in
          if as_init restriction_map agid ag_info then
            remanent, set
          else
            remanent, AgentIdSet.remove agid set
        ))

  let unbind_side restriction_map (agid, s_name) (remanent, set) =
    match AgentIdMap.find_option agid remanent with
    | None -> remanent, set
    | Some ag_info ->
      if SiteSet.mem s_name ag_info.bound_sites then (
        let ag_info =
          {
            ag_info with
            bound_sites = SiteSet.remove s_name ag_info.bound_sites;
          }
        in
        let remanent = AgentIdMap.add agid ag_info remanent in
        if as_init restriction_map agid ag_info then
          remanent, AgentIdSet.add agid set
        else
          remanent, set
      ) else
        remanent, set

  let unbind restriction_map site rem =
    let agid = agent_id_of_site site in
    let s_name = site_name_of_site site in
    unbind_side restriction_map (agid, s_name) rem

  let bind restriction_map site (remanent, set) =
    let agid = agent_id_of_site site in
    let s_name = site_name_of_site site in
    match AgentIdMap.find_option agid remanent with
    | None -> remanent, set
    | Some ag_info ->
      if SiteSet.mem s_name ag_info.bound_sites then
        remanent, set
      else (
        let ag_info =
          { ag_info with bound_sites = SiteSet.add s_name ag_info.bound_sites }
        in
        let remanent = AgentIdMap.add agid ag_info remanent in
        if as_init restriction_map agid ag_info then
          remanent, set
        else
          remanent, AgentIdSet.remove agid set
      )

  let split_init refined_step_list =
    let remanent = AgentIdMap.empty in
    fst
      (List.fold_left
         (fun (step_list, remanent) refined_step ->
           match refined_step with
           | Trace.Init init -> convert_init remanent step_list init
           | Trace.Subs _ | Trace.Obs _ | Trace.Dummy _ | Trace.Rule _
           | Trace.Pert _ ->
             refined_step :: step_list, remanent)
         ([], remanent)
         (List.rev refined_step_list))

  let add_in_scope site scope =
    let agid = agent_id_of_site site in
    let s_name = site_name_of_site site in
    let old_set = AgentIdMap.find_default SiteSet.empty agid scope in
    let new_set = SiteSet.add s_name old_set in
    if old_set == new_set then
      scope
    else
      AgentIdMap.add agid new_set scope

  let deal_with_tests tests scope =
    List.fold_left
      (List.fold_left (fun scope x ->
           match x with
           | PI.Is_Here _ -> scope
           | PI.Is_Bound site
           | PI.Is_Free site
           | PI.Has_Binding_type (site, _)
           | PI.Has_Internal (site, _) ->
             add_in_scope site scope
           | PI.Is_Bound_to (site1, site2) ->
             add_in_scope site1 (add_in_scope site2 scope)))
      scope tests

  let fill_siphon refined_step_list =
    let rev_trace = List.rev refined_step_list in
    let scope = AgentIdMap.empty in
    let refined_step_with_scope_list, _ =
      List.fold_left
        (fun (step_list, scope) refined_step ->
          match refined_step with
          | Trace.Init _ -> (refined_step, scope) :: step_list, scope
          | Trace.Rule (_, event, _) | Trace.Pert (_, event, _) ->
            let scope' = deal_with_tests event.Instantiation.tests scope in
            (refined_step, scope) :: step_list, scope'
          | Trace.Obs (_, tests, _) ->
            let scope' = deal_with_tests tests scope in
            (refined_step, scope) :: step_list, scope'
          | Trace.Subs _ | Trace.Dummy _ -> assert false)
        ([], scope) rev_trace
    in
    let remanent = AgentIdMap.empty in
    let a, _ =
      List.fold_left
        (fun (step_list, remanent) refined_step ->
          match refined_step with
          | Trace.Init init, _ -> convert_init remanent step_list init
          | Trace.Rule (_, event, _), scope | Trace.Pert (_, event, _), scope ->
            let remanent, set =
              List.fold_left
                (fun recur -> function
                  | PI.Create _ -> recur
                  | PI.Mod_internal (site, state) ->
                    mod_site scope site state recur
                  | PI.Bind (site1, site2) | PI.Bind_to (site1, site2) ->
                    bind scope site1 (bind scope site2 recur)
                  | PI.Free site -> unbind scope site recur
                  | PI.Remove _ -> recur)
                (remanent, AgentIdSet.empty)
                event.Instantiation.actions
            in
            let remanent, set =
              List.fold_right (unbind scope)
                event.Instantiation.side_effects_dst (remanent, set)
            in
            ( AgentIdSet.fold
                (fun id list ->
                  match AgentIdMap.find_option id remanent with
                  | Some x -> x.initial_step :: list
                  | None -> list)
                set
                (fst refined_step :: step_list),
              remanent )
          | Trace.Obs _, _ -> fst refined_step :: step_list, remanent
          | Trace.Subs _, _ | Trace.Dummy _, _ -> assert false)
        ([], remanent) refined_step_with_scope_list
    in
    List.rev a

  let agent_id_in_obs _parameter _handler info error = function
    | Trace.Subs _ | Trace.Rule _ | Trace.Pert _ | Trace.Init _ | Trace.Dummy _
      ->
      error, info, AgentIdSet.empty
    | Trace.Obs (_, tests, _) ->
      ( error,
        info,
        List.fold_left
          (List.fold_left (fun l x ->
               match x with
               | PI.Is_Here x -> AgentIdSet.add (agent_id_of_agent x) l
               | PI.Is_Bound _ | PI.Is_Free _ | PI.Has_Binding_type _
               | PI.Is_Bound_to _ | PI.Has_Internal _ ->
                 l))
          AgentIdSet.empty tests )
end
