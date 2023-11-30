(**
  * po_cut.ml
  *
  * Cut concurrent events: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS
  *
  * Creation: 16/04/2012
  * Last modification: 02/08/2013
  * *
  * Some parameter references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Po_cut = sig
  module K : Kappa_instantiation.Cflow_signature

  val cut : (Trace.t, Trace.t * int) K.H.unary

  type on_the_fly_state

  val init_cut : on_the_fly_state
  val cut_step : on_the_fly_state -> Trace.step -> on_the_fly_state
  val finalize_cut : on_the_fly_state -> Trace.step list * int

  val cut_rev_trace :
    Trace.step list (*reverse order*) ->
    Trace.step list (* correct order *) * int
end

module Po_cut : Po_cut = struct
  module K = Kappa_instantiation.Cflow_linker

  type predicate_info =
    | Here of K.agent_id
    | Bound_site of K.agent_id * Instantiation.site_name
    | Internal_state of K.agent_id * Instantiation.site_name

  module PSM = SetMap.Make (struct
    type t = predicate_info

    let compare = compare
    let print _ _ = ()
  end)

  module PS = PSM.Set

  let created_predicates_of_action action =
    match action with
    | Instantiation.Create (ag, interface) ->
      let ag_id = K.agent_id_of_agent ag in
      List.fold_left
        (fun list (s_id, opt) ->
          let list = Bound_site (ag_id, s_id) :: list in
          match opt with
          | None -> list
          | Some _ -> Internal_state (ag_id, s_id) :: list)
        [ Here ag_id ] interface
    | Instantiation.Bind _ | Instantiation.Bind_to _ | Instantiation.Remove _
    | Instantiation.Free _ | Instantiation.Mod_internal _ ->
      []

  let predicates_of_action action =
    match action with
    | Instantiation.Create (ag, interface) ->
      let ag_id = K.agent_id_of_agent ag in
      List.fold_left
        (fun list (s_id, opt) ->
          let list = Bound_site (ag_id, s_id) :: list in
          match opt with
          | None -> list
          | Some _ -> Internal_state (ag_id, s_id) :: list)
        [ Here ag_id ] interface
    | Instantiation.Mod_internal (site, _) ->
      [ Internal_state (K.agent_id_of_site site, K.site_name_of_site site) ]
    | Instantiation.Bind_to (s1, s2) | Instantiation.Bind (s1, s2) ->
      [
        Bound_site (K.agent_id_of_site s1, K.site_name_of_site s1);
        Bound_site (K.agent_id_of_site s2, K.site_name_of_site s2);
      ]
    | Instantiation.Free s ->
      [ Bound_site (K.agent_id_of_site s, K.site_name_of_site s) ]
    | Instantiation.Remove _ -> []

  let predicates_of_test test =
    match test with
    | Instantiation.Is_Here agent -> [ Here (K.agent_id_of_agent agent) ]
    | Instantiation.Has_Internal (site, _) ->
      [ Internal_state (K.agent_id_of_site site, K.site_name_of_site site) ]
    | Instantiation.Is_Free s
    | Instantiation.Is_Bound s
    | Instantiation.Has_Binding_type (s, _) ->
      [ Bound_site (K.agent_id_of_site s, K.site_name_of_site s) ]
    | Instantiation.Is_Bound_to (s1, s2) ->
      [
        Bound_site (K.agent_id_of_site s1, K.site_name_of_site s1);
        Bound_site (K.agent_id_of_site s2, K.site_name_of_site s2);
      ]

  let predicates_of_side_effects sides =
    List.map (fun ((ag_id, _), s_id) -> Bound_site (ag_id, s_id)) sides

  type on_the_fly_state = PS.t * Trace.step list * int

  let init_cut = PS.empty, [], 0
  let finalize_cut (_a, b, c) = b, c

  let cut_step (seen, kept, n_cut) event =
    let rec keep l =
      match l with
      | [] -> false
      | t0 :: q0 ->
        let rec aux1 l =
          match l with
          | [] -> keep q0
          | t1 :: q1 ->
            if PS.mem t1 seen then
              true
            else
              aux1 q1
        in
        aux1 (predicates_of_action t0)
    in
    let rec keep2 l =
      match l with
      | [] -> false
      | t :: q ->
        if PS.mem t seen then
          true
        else
          keep2 q
    in
    let action_list, _ = Trace.actions_of_step event in
    let seen =
      List.fold_left
        (fun seen action ->
          List.fold_left
            (fun seen elt -> PS.remove elt seen)
            seen
            (created_predicates_of_action action))
        seen action_list
    in
    let actions, _ = Trace.actions_of_step event in
    if
      Trace.step_is_obs event || keep actions
      || keep2 (predicates_of_side_effects (Trace.side_effects_of_step event))
    then (
      let kept = event :: kept in
      let tests = Trace.tests_of_step event in
      let tests' =
        predicates_of_side_effects (Trace.side_effects_of_step event)
      in
      let seen =
        List.fold_left
          (fun seen test ->
            List.fold_left
              (fun seen predicate_info -> PS.add predicate_info seen)
              seen (predicates_of_test test))
          seen tests
      in
      let seen =
        List.fold_left
          (fun seen predicate_info -> PS.add predicate_info seen)
          seen tests'
      in
      seen, kept, n_cut
    ) else
      seen, kept, n_cut + 1

  let cut_rev_trace rev_event_list =
    let _, event_list, n = List.fold_left cut_step init_cut rev_event_list in
    event_list, n

  let cut _parameter _handler info error event_list =
    let trace = cut_rev_trace (List.rev event_list) in
    error, info, trace
end
