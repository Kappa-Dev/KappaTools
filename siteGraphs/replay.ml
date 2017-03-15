(* This file is part of ReConKa
   Copyright 2017 Harvard Medical School

   ReConKa is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

type state = {
  graph : Edges.t;
  time : float;
  event : int;
  connected_components : Mods.IntSet.t Mods.IntMap.t;
}

let init_state = {
  graph = Edges.empty ~with_connected_components:true;
  time = 0.;
  event = 0;
  connected_components = Mods.IntMap.empty;
}

let break_apart_cc graph ccs = function
  | None -> ccs
  | Some (origin_cc,new_cc) ->
    let set = Mods.IntMap.find_default Mods.IntSet.empty origin_cc ccs in
    if Mods.IntSet.is_empty set then ccs
    else
      let nset,oset' =
        Mods.IntSet.partition
          (fun x -> Edges.get_connected_component x graph = Some new_cc)
          set in
      Mods.IntMap.add new_cc nset (Mods.IntMap.add origin_cc oset' ccs)

let merge_cc ccs = function
  | None -> ccs
  | Some (cc1,cc2) ->
    let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc1 ccs in
    match Mods.IntMap.pop cc2 ccs with
    | None, _ -> ccs
    | Some set2,ccs' -> Mods.IntMap.add cc1 (Mods.IntSet.union set1 set2) ccs'

let do_negative_part ((a,_),s) (graph,ccs) =
  match Edges.link_destination a s graph with
  | None -> (Edges.remove_free a s graph,ccs)
  | Some ((a',_),s') ->
    let graph',cc_change = Edges.remove_link a s a' s' graph in
    (graph',break_apart_cc graph' ccs cc_change)

let do_action sigs (graph,ccs as pack) = function
  | Instantiation.Create ((id,ty),_graphs) ->
    (snd @@ Edges.add_agent ~id sigs ty graph,
     Mods.IntMap.add id (Mods.IntSet.singleton id) ccs)
  | Instantiation.Mod_internal (((a,_),s),i) ->
    (Edges.add_internal a s i graph,ccs)
  | Instantiation.Bind ((a1,s1 as x1),(a2,s2 as x2))
  | Instantiation.Bind_to ((a1,s1 as x1),(a2,s2 as x2)) ->
    let graph',ccs' = do_negative_part x2 (do_negative_part x1 pack) in
    let graph'',cc_change = Edges.add_link a1 s1 a2 s2 graph' in
    (graph'',merge_cc ccs' cc_change)
  | Instantiation.Free ((a,_),s as x) ->
    let graph',ccs' =  do_negative_part x pack in
    (Edges.add_free a s graph',ccs')
  | Instantiation.Remove (id,ty as a) ->
    let graph',ccs' =
      Tools.recti (fun st s -> do_negative_part (a,s) st)
        pack (Signature.arity sigs ty) in
    match Mods.IntMap.pop id ccs' with
    | None,_ -> assert false
    | Some x,ccs'' ->
      let () = assert (Mods.IntSet.is_singleton x) in
      (Edges.remove_agent id graph',ccs'')

let involved_agents l =
  List_util.map_option
    (function
      | Instantiation.Is_Here a -> Some a
      | Instantiation.Is_Free _ | Instantiation.Has_Internal _
      | Instantiation.Is_Bound _ | Instantiation.Is_Bound_to _
      | Instantiation.Has_Binding_type _ -> None) l

let store_distances r graph = function
  | [] | [ _ ] | _ :: _ :: _ :: _ -> None
  | [ cc1; cc2 ] ->
    let cc1_ags = involved_agents cc1 in
    let cc2_ags = involved_agents cc2 in
    match Edges.are_connected graph cc1_ags cc2_ags with
    | None -> None
    | Some path -> Some (r,List.length path)

let do_step sigs state = function
  | Trace.Subs _ -> state,None
  | Trace.Rule (kind,event,info) ->
    let dist = store_distances kind state.graph event.Instantiation.tests in
    let pregraph,connected_components =
        List.fold_left
           (do_action sigs) (state.graph,state.connected_components)
           event.Instantiation.actions in
    let graph =
      List.fold_left
        (fun graph ((id,_),s) -> Edges.add_free id s graph)
        pregraph event.Instantiation.side_effects_dst in
    {
      graph; connected_components;
      time = info.Trace.Simulation_info.story_time;
      event = info.Trace.Simulation_info.story_event;
    },dist
  | Trace.Pert (_,event,info) ->
    let pregraph,connected_components =
        List.fold_left
           (do_action sigs) (state.graph,state.connected_components)
           event.Instantiation.actions in
    let graph =
      List.fold_left
        (fun graph ((id,_),s) -> Edges.add_free id s graph)
        pregraph event.Instantiation.side_effects_dst in
    {
      graph; connected_components;
      time = info.Trace.Simulation_info.story_time;
      event = info.Trace.Simulation_info.story_event;
    },None
  | Trace.Init actions ->
    let graph,connected_components =
      List.fold_left
        (do_action sigs) (state.graph, state.connected_components) actions in
    { graph; connected_components; time = state.time; event = state.event; },
    None
  | Trace.Obs (_,_,info) ->
    {
      graph = state.graph;
      time = info.Trace.Simulation_info.story_time;
      event = info.Trace.Simulation_info.story_event;
      connected_components = state.connected_components;
    },None
  | Trace.Dummy _ -> state,None
