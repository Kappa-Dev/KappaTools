(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type state = {
  graph: Edges.t;
  time: float;
  event: int;
  connected_components: Agent.SetMap.Set.t Mods.IntMap.t option;
}

type summary = { unary_distances: (int * int) option }

let init_state ~with_connected_components =
  {
    graph = Edges.empty ~with_connected_components;
    time = 0.;
    event = 0;
    connected_components =
      (if with_connected_components then
         Some Mods.IntMap.empty
       else
         None);
  }

let cc_of_agent ag e work =
  let rec fold_arity_list f x arity acc =
    if x = arity then
      acc
    else
      fold_arity_list f (succ x) arity (f acc x)
  in

  let add_agent a e (work, morphism, todos) =
    let aid = Agent.id a in
    let atype = Agent.sort a in
    let arity = Edges.get_sites aid e in
    let w_agent, work' = Pattern.new_node work atype in
    let todos' = fold_arity_list (fun acc x -> (aid, x) :: acc) 0 arity todos in
    let work'' =
      fold_arity_list
        (fun w x ->
          try
            let internal = Edges.get_internal aid x e in
            Pattern.new_internal_state w (w_agent, x) internal
          with Failure _ -> w)
        0 arity work'
    in
    w_agent, work'', (aid, w_agent) :: morphism, todos'
  in

  let add_links (work, morphism, todos) (aid, x) e =
    let _, w_agent = List.find (fun (id, _) -> id = aid) morphism in
    let not_agent (id, s) = not (id = aid && x = s) in
    match Edges.link_destination aid x e with
    | None ->
      let work' = Pattern.new_free work (w_agent, x) in
      let todos' = List.filter not_agent todos in
      work', morphism, todos'
    | Some (b, y) ->
      let bid = Agent.id b in
      let not_agents (id, s) =
        not_agent (id, s) && not (id = Agent.id b && s = y)
      in
      (try
         let _, wb_agent = List.find (fun (id, _) -> id = bid) morphism in
         let work' = Pattern.new_link work (w_agent, x) (wb_agent, y) in
         let todos' = List.filter not_agents todos in
         work', morphism, todos'
       with Not_found ->
         let wb_agent, work', morphism', todos' =
           add_agent b e (work, morphism, todos)
         in
         let work'' = Pattern.new_link work' (w_agent, x) (wb_agent, y) in
         let todos'' = List.filter not_agents todos' in
         work'', morphism', todos'')
  in

  let rec working_todo (work, morphism, todo) =
    match todo with
    | [] -> morphism, work
    | port :: _ ->
      let work', morphism', todo' = add_links (work, morphism, todo) port e in
      working_todo (work', morphism', todo')
  in

  let _, w, m, t = add_agent ag e (work, [], []) in
  working_todo (w, m, t)

let cc_of_state ~debugMode s env =
  let cc_of_root agent e' =
    let work = Pattern.begin_new e' in
    let morphism, work' = cc_of_agent agent s.graph work in
    let en, _, c, i = Pattern.finish_new ~debugMode work' in
    en, List.map (fun (cid, (aid, _)) -> cid, aid) morphism, c, i
  in
  match s.connected_components with
  | Some cc_maps ->
    Mods.IntMap.fold
      (fun root cc_map (e, acc) ->
        Agent.SetMap.Set.fold
          (fun agent (e', acc') ->
            if Agent.id agent = root then (
              let en, r, c, i = cc_of_root agent e' in
              en, (r, c, i) :: acc'
            ) else
              e', acc')
          cc_map (e, acc))
      cc_maps (env, [])
  | None -> env, []

let break_apart_cc graph ccs = function
  | None -> ccs
  | Some (origin_cc, new_cc) ->
    let set = Mods.IntMap.find_default Agent.SetMap.Set.empty origin_cc ccs in
    if Agent.SetMap.Set.is_empty set then
      ccs
    else (
      let nset, oset' =
        Agent.SetMap.Set.partition
          (fun (x, _) -> Edges.get_connected_component x graph = Some new_cc)
          set
      in
      Mods.IntMap.add new_cc nset (Mods.IntMap.add origin_cc oset' ccs)
    )

let merge_cc ccs = function
  | None -> ccs
  | Some (cc1, cc2) ->
    let set1 = Mods.IntMap.find_default Agent.SetMap.Set.empty cc1 ccs in
    (match Mods.IntMap.pop cc2 ccs with
    | None, _ -> ccs
    | Some set2, ccs' ->
      Mods.IntMap.add cc1 (Agent.SetMap.Set.union set1 set2) ccs')

let do_negative_part ((a, _), s) (graph, ccs) =
  match Edges.link_destination a s graph with
  | None -> Edges.remove_free a s graph, ccs
  | Some ((a', _), s') ->
    let graph', cc_change = Edges.remove_link a s a' s' graph in
    ( graph',
      (match ccs with
      | None -> None
      | Some ccs -> Some (break_apart_cc graph' ccs cc_change)) )

let do_action sigs ((graph, ccs) as pack) = function
  | Instantiation.Create (((id, ty) as ag), _graphs) ->
    ( snd @@ Edges.add_agent ~id sigs ty graph,
      Option_util.map (Mods.IntMap.add id (Agent.SetMap.Set.singleton ag)) ccs )
  | Instantiation.Mod_internal (((a, _), s), i) ->
    Edges.add_internal a s i graph, ccs
  | Instantiation.Bind (((a1, s1) as x1), ((a2, s2) as x2))
  | Instantiation.Bind_to (((a1, s1) as x1), ((a2, s2) as x2)) ->
    let graph', ccs' = do_negative_part x2 (do_negative_part x1 pack) in
    let graph'', cc_change = Edges.add_link a1 s1 a2 s2 graph' in
    ( graph'',
      (match ccs' with
      | None -> None
      | Some ccs' -> Some (merge_cc ccs' cc_change)) )
  | Instantiation.Free (((a, _), s) as x) ->
    let graph', ccs' = do_negative_part x pack in
    Edges.add_free a s graph', ccs'
  | Instantiation.Remove ((id, ty) as a) ->
    let graph', ccs' =
      Tools.recti
        (fun st s -> do_negative_part (a, s) st)
        pack (Signature.arity sigs ty)
    in
    (match ccs' with
    | None -> Edges.remove_agent id graph', None
    | Some ccs' ->
      (match Mods.IntMap.pop id ccs' with
      | None, _ -> assert false
      | Some x, ccs'' ->
        let () = assert (Agent.SetMap.Set.is_singleton x) in
        Edges.remove_agent id graph', Some ccs''))

let involved_agents l =
  List_util.map_option
    (function
      | Instantiation.Is_Here a -> Some a
      | Instantiation.Is_Free _ | Instantiation.Has_Internal _
      | Instantiation.Is_Bound _ | Instantiation.Is_Bound_to _
      | Instantiation.Has_Binding_type _ ->
        None)
    l

let store_distances r graph = function
  | [] | [ _ ] | _ :: _ :: _ :: _ -> None
  | [ cc1; cc2 ] ->
    let cc1_ags = involved_agents cc1 in
    let cc2_ags = involved_agents cc2 in
    (match Edges.are_connected graph cc1_ags cc2_ags with
    | None -> None
    | Some path -> Some (r, List.length path))

let test_pass_on graph = function
  | Instantiation.Is_Here ag -> Edges.is_agent ag graph
  | Instantiation.Has_Internal ((ag, s), st) ->
    Edges.is_agent ag graph && Edges.is_internal st (Agent.id ag) s graph
  | Instantiation.Is_Free (ag, s) ->
    Edges.is_agent ag graph && Edges.is_free (Agent.id ag) s graph
  | Instantiation.Is_Bound (ag, s) ->
    Edges.is_agent ag graph && not (Edges.is_free (Agent.id ag) s graph)
  | Instantiation.Is_Bound_to ((ag, s), (ag', s')) ->
    Edges.is_agent ag graph && Edges.is_agent ag' graph
    && Edges.link_exists (Agent.id ag) s (Agent.id ag') s' graph
  | Instantiation.Has_Binding_type ((ag, s), (ag_ty, s')) ->
    Edges.is_agent ag graph
    &&
    (match Edges.link_destination (Agent.id ag) s graph with
    | None -> false
    | Some ((_, dst_ag_ty), dst_s) -> dst_ag_ty = ag_ty && dst_s = s')

let tests_pass_on graph tests =
  List.for_all (test_pass_on graph) (List.concat tests)

let is_step_triggerable_on_edges graph = function
  | Trace.Subs _ | Trace.Init _ | Trace.Pert _ | Trace.Dummy _ -> true
  | Trace.Rule (_r, event, _info) ->
    tests_pass_on graph event.Instantiation.tests
  | Trace.Obs (_, tests, _) -> tests_pass_on graph tests

let is_step_triggerable state = is_step_triggerable_on_edges state.graph

(* There is a subtelty when executing a sequence of actions. Indeed,
   whenever a rule both creates and removes agents, there is currently
   no guarantee that the creation actions are placed before the removal
   actions in [event.Instantiation.actions]. This can be an issue in a
   case where an event performs the following two actions for example:
   ["create agent with id 8", "remove agent with id 8"]. In this case,
   agent id 8 is not available when the creation action is performed and
   so the [Edges] module throws an exception.

   As a temporary fix, we make sure that all deletion actions are
   executed first. This implicitly assumes that a step deleting an agent
   cannot perform any other action involving this agent.

   TODO: Shouldn't we rather ensure that actions are properly sorted in
   the trace file in the first place? *)
let do_actions sigs st actions =
  let is_removal =
    let open Instantiation in
    function
    | Remove _ -> true
    | Create _ | Mod_internal _ | Bind _ | Bind_to _ | Free _ -> false
  in
  let removals, others = List.partition is_removal actions in
  let do_in_order actions st = List.fold_left (do_action sigs) st actions in
  st |> do_in_order removals |> do_in_order others

let do_step sigs state = function
  | Trace.Subs _ -> state, { unary_distances = None }
  | Trace.Rule (kind, event, info) ->
    let unary_distances =
      if state.connected_components = None then
        None
      else
        store_distances kind state.graph event.Instantiation.tests
    in
    let pregraph, connected_components =
      do_actions sigs
        (state.graph, state.connected_components)
        event.Instantiation.actions
    in
    let graph =
      List.fold_left
        (fun graph ((id, _), s) -> Edges.add_free id s graph)
        pregraph event.Instantiation.side_effects_dst
    in
    ( {
        graph;
        connected_components;
        time = info.Trace.Simulation_info.story_time;
        event = info.Trace.Simulation_info.story_event;
      },
      { unary_distances } )
  | Trace.Pert (_, event, info) ->
    let pregraph, connected_components =
      do_actions sigs
        (state.graph, state.connected_components)
        event.Instantiation.actions
    in
    let graph =
      List.fold_left
        (fun graph ((id, _), s) -> Edges.add_free id s graph)
        pregraph event.Instantiation.side_effects_dst
    in
    ( {
        graph;
        connected_components;
        time = info.Trace.Simulation_info.story_time;
        event = info.Trace.Simulation_info.story_event;
      },
      { unary_distances = None } )
  | Trace.Init actions ->
    let graph, connected_components =
      do_actions sigs (state.graph, state.connected_components) actions
    in
    ( { graph; connected_components; time = state.time; event = state.event },
      { unary_distances = None } )
  | Trace.Obs (_, _, info) ->
    ( {
        graph = state.graph;
        time = info.Trace.Simulation_info.story_time;
        event = info.Trace.Simulation_info.story_event;
        connected_components = state.connected_components;
      },
      { unary_distances = None } )
  | Trace.Dummy _ -> state, { unary_distances = None }
