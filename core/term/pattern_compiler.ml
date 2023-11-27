(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let link_occurence_failure key pos =
  raise
    (ExceptionDefn.Internal_Error
       ( "Bug: Link " ^ string_of_int key
         ^ " is problematic! LKappa is either broken"
         ^ " or unused! Please report.",
         pos ))

let ports_from_contact_map contact_map ty_id p_id =
  snd contact_map.(ty_id).(p_id)

let find_implicit_infos contact_map ags =
  let new_switch = function
    | LKappa.Maintained -> LKappa.Maintained
    | LKappa.Freed | LKappa.Linked _ | LKappa.Erased -> LKappa.Freed
  in
  let rec aux_one free_id previous current todos ag_tail ag ports i =
    let ty_id = ag.LKappa.ra_type in
    if i = Array.length ports then (
      let current' =
        {
          LKappa.ra_type = ag.LKappa.ra_type;
          LKappa.ra_ports = ports;
          LKappa.ra_ints = ag.LKappa.ra_ints;
          LKappa.ra_erased = ag.LKappa.ra_erased;
          LKappa.ra_syntax = ag.LKappa.ra_syntax;
        }
        :: current
      in
      aux_ags free_id previous current' todos ag_tail
    ) else (
      match ports.(i) with
      | (LKappa.LNK_TYPE (p, a), _), s ->
        let or_ty = i, ty_id in
        let () =
          ports.(i) <-
            Loc.annot_with_dummy (LKappa.LNK_VALUE (free_id, (p, a))), s
        in
        aux_one (succ free_id) previous current
          ((free_id, (p, a), or_ty, new_switch s) :: todos)
          ag_tail ag ports (succ i)
      | (LKappa.LNK_SOME, _), s ->
        let or_ty = i, ty_id in
        Mods.Int2Set.fold
          (fun (a, p) prev' ->
            let ports' = Array.copy ports in
            let () =
              ports'.(i) <-
                Loc.annot_with_dummy (LKappa.LNK_VALUE (free_id, (p, a))), s
            in
            let todos' = (free_id, (p, a), or_ty, new_switch s) :: todos in
            aux_one (succ free_id) prev' current todos' ag_tail ag ports'
              (succ i))
          (ports_from_contact_map contact_map ty_id i)
          previous
      | (LKappa.LNK_VALUE _, _), _ ->
        aux_one free_id previous current todos ag_tail ag ports (succ i)
      | (LKappa.LNK_FREE, pos), ((LKappa.Maintained | LKappa.Erased) as s) ->
        let () =
          (* Do not make test if being free is the only possibility *)
          if Mods.Int2Set.is_empty (ports_from_contact_map contact_map ty_id i)
          then
            ports.(i) <- (LKappa.LNK_ANY, pos), s
          else
            ()
        in
        aux_one free_id previous current todos ag_tail ag ports (succ i)
      | (LKappa.LNK_FREE, _), LKappa.Freed ->
        failwith "A free site cannot be freed"
      | (LKappa.LNK_FREE, _), LKappa.Linked _ ->
        aux_one free_id previous current todos ag_tail ag ports (succ i)
      | ((LKappa.LNK_ANY | LKappa.ANY_FREE), _), LKappa.Maintained ->
        aux_one free_id previous current todos ag_tail ag ports (succ i)
      | ( ((LKappa.LNK_ANY | LKappa.ANY_FREE), pos),
          ((LKappa.Erased | LKappa.Linked _ | LKappa.Freed) as s) ) ->
        if
          Mods.Int2Set.is_empty (ports_from_contact_map contact_map ty_id i)
          && s = LKappa.Freed
        then (
          (* Do not make test is being free is the only possibility *)
          let () = ports.(i) <- (LKappa.LNK_ANY, pos), LKappa.Maintained in
          aux_one free_id previous current todos ag_tail ag ports (succ i)
        ) else
          aux_one free_id previous current todos ag_tail ag ports (succ i)
    )
  and aux_ags free_id previous current todos = function
    | [] -> (List.rev current, todos) :: previous
    | ag :: ag_tail ->
      aux_one free_id previous current todos ag_tail ag ag.LKappa.ra_ports 0
  in
  aux_ags (succ (LKappa.max_link_id ags)) [] [] [] ags

let complete_with_candidate outs prevs ag ag_tail id todo p_id dst_info p_switch
    =
  Tools.array_fold_lefti
    (fun i acc port ->
      if i <> p_id then
        acc
      else (
        match port with
        | ((LKappa.LNK_ANY | LKappa.ANY_FREE), _), s ->
          if s = LKappa.Maintained then (
            let ports' = Array.copy ag.LKappa.ra_ports in
            let () =
              ports'.(i) <-
                Loc.annot_with_dummy (LKappa.LNK_VALUE (id, dst_info)), p_switch
            in
            ( List.rev_append prevs
                ({
                   LKappa.ra_type = ag.LKappa.ra_type;
                   LKappa.ra_ports = ports';
                   LKappa.ra_ints = ag.LKappa.ra_ints;
                   LKappa.ra_erased = ag.LKappa.ra_erased;
                   LKappa.ra_syntax = ag.LKappa.ra_syntax;
                 }
                :: ag_tail),
              todo )
            :: acc
          ) else if s = LKappa.Erased && p_switch = LKappa.Freed then (
            let ports' = Array.copy ag.LKappa.ra_ports in
            let () =
              ports'.(i) <-
                Loc.annot_with_dummy (LKappa.LNK_VALUE (id, dst_info)), s
            in
            ( List.rev_append prevs
                ({
                   LKappa.ra_type = ag.LKappa.ra_type;
                   LKappa.ra_ports = ports';
                   LKappa.ra_ints = ag.LKappa.ra_ints;
                   LKappa.ra_erased = ag.LKappa.ra_erased;
                   LKappa.ra_syntax = ag.LKappa.ra_syntax;
                 }
                :: ag_tail),
              todo )
            :: acc
          ) else
            acc
        | (LKappa.LNK_VALUE (k, x), _), s ->
          if x = dst_info then (
            match
              List.partition
                (fun (j, _, (p', a'), sw') ->
                  j = k && i = p' && a' = ag.LKappa.ra_type && sw' = p_switch)
                todo
            with
            | [ _ ], todo' ->
              let ports' = Array.copy ag.LKappa.ra_ports in
              let () =
                ports'.(i) <- Loc.annot_with_dummy (LKappa.LNK_VALUE (id, x)), s
              in
              ( List.rev_append prevs
                  ({
                     LKappa.ra_type = ag.LKappa.ra_type;
                     LKappa.ra_ports = ports';
                     LKappa.ra_ints = ag.LKappa.ra_ints;
                     LKappa.ra_erased = ag.LKappa.ra_erased;
                     LKappa.ra_syntax = ag.LKappa.ra_syntax;
                   }
                  :: ag_tail),
                todo' )
              :: acc
            | [], _ -> acc
            | _ :: _ :: _, _ -> assert false
          ) else
            acc
        | ((LKappa.LNK_TYPE _ | LKappa.LNK_FREE | LKappa.LNK_SOME), _), _ -> acc
      ))
    outs ag.LKappa.ra_ports

let new_agent_with_one_link sigs ty_id port link dst_info switch =
  let arity = Signature.arity sigs ty_id in
  let ports =
    Array.make arity (Loc.annot_with_dummy LKappa.LNK_ANY, LKappa.Maintained)
  in
  let internals = Array.make arity LKappa.I_ANY in
  let () =
    ports.(port) <-
      Loc.annot_with_dummy (LKappa.LNK_VALUE (link, dst_info)), switch
  in
  {
    LKappa.ra_type = ty_id;
    LKappa.ra_ports = ports;
    LKappa.ra_ints = internals;
    LKappa.ra_erased = false;
    LKappa.ra_syntax = None;
  }

let rec add_one_implicit_info sigs id (((port, ty_id), dst_info, s) as info) acc
    out todo = function
  | [] ->
    ( List.rev_append acc
        [ new_agent_with_one_link sigs ty_id port id dst_info s ],
      todo )
    :: out
  | ag :: ag_tail ->
    let out_tail =
      add_one_implicit_info sigs id info (ag :: acc) out todo ag_tail
    in
    if ty_id = ag.LKappa.ra_type then
      complete_with_candidate out_tail acc ag ag_tail id todo port dst_info s
    else
      out_tail

let add_implicit_infos sigs l =
  let rec aux acc = function
    | [] -> acc
    | (m, []) :: t -> aux (m :: acc) t
    | (m, (id, info, dst_info, s) :: todo') :: t ->
      aux acc (add_one_implicit_info sigs id (info, dst_info, s) [] t todo' m)
  in
  aux [] l

let is_linked_on_port me i id = function
  | (LKappa.LNK_VALUE (j, _), _), _ when i = j -> id <> me
  | ( ( ( LKappa.LNK_VALUE _ | LKappa.LNK_FREE | LKappa.LNK_TYPE _
        | LKappa.LNK_ANY | LKappa.LNK_SOME | LKappa.ANY_FREE ),
        _ ),
      _ ) ->
    false

let is_linked_on i ag =
  Tools.array_filter (is_linked_on_port (-1) i) ag.LKappa.ra_ports <> []

let define_full_transformation ((removed, added) as transf) links_transf place
    site (cand, _) switch =
  match switch with
  | LKappa.Freed ->
    ( (cand :: removed, Primitives.Transformation.Freed (place, site) :: added),
      links_transf )
  | LKappa.Maintained -> transf, links_transf
  | LKappa.Erased -> (cand :: removed, added), links_transf
  | LKappa.Linked i ->
    (match Mods.IntMap.find_option i links_transf with
    | None ->
      let links_transf' = Mods.IntMap.add i (place, site) links_transf in
      (cand :: removed, added), links_transf'
    | Some dst' ->
      let links_transf' = Mods.IntMap.remove i links_transf in
      ( ( cand :: removed,
          Primitives.Transformation.Linked ((place, site), dst') :: added ),
        links_transf' ))

let define_positive_transformation ((removed, added) as transf) links_transf
    place site switch =
  match switch with
  | LKappa.Freed ->
    ( (removed, Primitives.Transformation.Freed (place, site) :: added),
      links_transf )
  | LKappa.Erased -> transf, links_transf
  | LKappa.Maintained -> transf, links_transf
  | LKappa.Linked i ->
    (match Mods.IntMap.find_option i links_transf with
    | None ->
      let links_transf' = Mods.IntMap.add i (place, site) links_transf in
      transf, links_transf'
    | Some dst' ->
      let links_transf' = Mods.IntMap.remove i links_transf in
      ( ( removed,
          Primitives.Transformation.Linked ((place, site), dst') :: added ),
        links_transf' ))

let add_instantiation_free actions pl s = function
  | LKappa.Freed -> Instantiation.Free (pl, s) :: actions
  | LKappa.Linked _ | LKappa.Maintained | LKappa.Erased -> actions

let add_side_site side_sites bt pl s = function
  | LKappa.Freed | LKappa.Linked _ | LKappa.Erased ->
    ((pl, s), bt) :: side_sites
  | LKappa.Maintained -> side_sites

let add_freed_side_effect side_effects pl s = function
  | (LKappa.LNK_VALUE _, _), LKappa.Freed -> (pl, s) :: side_effects
  | ( (LKappa.LNK_VALUE _, _),
      (LKappa.Maintained | LKappa.Erased | LKappa.Linked _) )
  | ( ( ( LKappa.LNK_FREE | LKappa.LNK_ANY | LKappa.LNK_SOME | LKappa.LNK_TYPE _
        | LKappa.ANY_FREE ),
        _ ),
      _ ) ->
    side_effects

let add_extra_side_effects side_effects place refined =
  let rec aux side_effects site_id =
    if site_id < 0 then
      side_effects
    else
      aux
        (add_freed_side_effect side_effects place site_id refined.(site_id))
        (pred site_id)
  in
  aux side_effects (pred (Array.length refined))

(* Deals with tests, erasure internal state change and release (but
   not binding)*)
let make_instantiation place links event ref_ports is_erased = function
  | None ->
    {
      Instantiation.tests = event.Instantiation.tests;
      Instantiation.actions = event.Instantiation.actions;
      Instantiation.side_effects_src = event.Instantiation.side_effects_src;
      Instantiation.side_effects_dst =
        add_extra_side_effects event.Instantiation.side_effects_dst place
          ref_ports;
      Instantiation.connectivity_tests = event.Instantiation.connectivity_tests;
    }
  | Some (ports, ints) ->
    (match event.Instantiation.tests with
    | [] -> assert false
    | this_cc_tests :: o_cc_tests ->
      let rec aux site_id tests actions side_effects_src side_effects_dst links
          =
        if site_id >= Array.length ports then
          {
            Instantiation.tests = tests :: o_cc_tests;
            Instantiation.actions;
            Instantiation.side_effects_src;
            Instantiation.side_effects_dst;
            Instantiation.connectivity_tests =
              event.Instantiation.connectivity_tests;
          }
        else (
          let tests', actions' =
            match ints.(site_id) with
            | LKappa.I_ANY | LKappa.I_ANY_ERASED -> tests, actions
            | LKappa.I_ANY_CHANGED j ->
              tests, Instantiation.Mod_internal ((place, site_id), j) :: actions
            | LKappa.I_VAL_CHANGED (i, j) ->
              ( Instantiation.Has_Internal ((place, site_id), i) :: tests,
                if i <> j then
                  Instantiation.Mod_internal ((place, site_id), j) :: actions
                else
                  actions )
            | LKappa.I_VAL_ERASED i ->
              Instantiation.Has_Internal ((place, site_id), i) :: tests, actions
          in
          let tests'', actions'', side_sites', side_effects', links' =
            match ports.(site_id) with
            | ((LKappa.LNK_ANY | LKappa.ANY_FREE), _), s ->
              let side_effects' =
                match s with
                | LKappa.Maintained ->
                  add_freed_side_effect side_effects_dst place site_id
                    ref_ports.(site_id)
                | LKappa.Erased | LKappa.Linked _ | LKappa.Freed ->
                  side_effects_dst
              in
              ( tests',
                add_instantiation_free actions' place site_id s,
                add_side_site side_effects_src Instantiation.ANY place site_id s,
                side_effects',
                links )
            | (LKappa.LNK_FREE, _), s ->
              ( Instantiation.Is_Free (place, site_id) :: tests',
                add_instantiation_free actions' place site_id s,
                side_effects_src,
                side_effects_dst,
                links )
            | (LKappa.LNK_SOME, _), s ->
              ( Instantiation.Is_Bound (place, site_id) :: tests',
                add_instantiation_free actions' place site_id s,
                add_side_site side_effects_src Instantiation.BOUND place site_id
                  s,
                side_effects_dst,
                links )
            | (LKappa.LNK_TYPE (b, a), _), s ->
              ( Instantiation.Has_Binding_type ((place, site_id), (a, b))
                :: tests',
                add_instantiation_free actions' place site_id s,
                add_side_site side_effects_src
                  (Instantiation.BOUND_TYPE (a, b))
                  place site_id s,
                side_effects_dst,
                links )
            | (LKappa.LNK_VALUE (i, _), _), s ->
              (match Mods.IntMap.find_option i links with
              | Some x ->
                ( x :: tests',
                  add_instantiation_free actions' place site_id s,
                  side_effects_src,
                  side_effects_dst,
                  Mods.IntMap.remove i links )
              | None ->
                ( tests',
                  add_instantiation_free actions' place site_id s,
                  side_effects_src,
                  side_effects_dst,
                  links ))
          in
          aux (succ site_id) tests'' actions'' side_sites' side_effects' links'
        )
      in
      aux 0
        (Instantiation.Is_Here place :: this_cc_tests)
        (if is_erased then
           Instantiation.Remove place :: event.Instantiation.actions
         else
           event.Instantiation.actions)
        event.Instantiation.side_effects_src
        event.Instantiation.side_effects_dst links)

let rec add_agents_in_cc sigs id wk registered_links
    ((removed, added) as transf) links_transf instantiations remains = function
  | [] ->
    (match Mods.IntMap.root registered_links with
    | None -> wk, transf, links_transf, instantiations, remains
    | Some (key, _) -> link_occurence_failure key Loc.dummy)
  | ag :: ag_l ->
    let node, wk = Pattern.new_node wk ag.LKappa.ra_type in
    let place = Matching.Agent.Existing (node, id) in
    let transf' =
      if ag.LKappa.ra_erased then
        Primitives.Transformation.Agent place :: removed, added
      else
        transf
    in
    let rec handle_ports wk r_l c_l (removed, added) l_t re acc site_id =
      if site_id = Array.length ag.LKappa.ra_ports then (
        let instantiations' =
          make_instantiation place c_l instantiations ag.LKappa.ra_ports
            ag.LKappa.ra_erased ag.LKappa.ra_syntax
        in
        add_agents_in_cc sigs id wk r_l (removed, added) l_t instantiations' re
          acc
      ) else (
        let transf, wk' =
          match ag.LKappa.ra_ints.(site_id) with
          | LKappa.I_ANY -> (removed, added), wk
          | LKappa.I_ANY_ERASED ->
            ( ( Primitives.Transformation.NegativeInternalized (place, site_id)
                :: removed,
                added ),
              wk )
          | LKappa.I_ANY_CHANGED j ->
            ( ( Primitives.Transformation.NegativeInternalized (place, site_id)
                :: removed,
                Primitives.Transformation.PositiveInternalized
                  (place, site_id, j)
                :: added ),
              wk )
          | LKappa.I_VAL_CHANGED (i, j) ->
            ( (if i = j then
                 removed, added
               else
                 ( Primitives.Transformation.NegativeInternalized
                     (place, site_id)
                   :: removed,
                   Primitives.Transformation.PositiveInternalized
                     (place, site_id, j)
                   :: added )),
              Pattern.new_internal_state wk (node, site_id) i )
          | LKappa.I_VAL_ERASED i ->
            ( ( Primitives.Transformation.NegativeInternalized (place, site_id)
                :: removed,
                added ),
              Pattern.new_internal_state wk (node, site_id) i )
        in
        match ag.LKappa.ra_ports.(site_id) with
        | ((LKappa.LNK_ANY | LKappa.ANY_FREE), pos), s ->
          let transf', l_t' =
            define_full_transformation transf l_t place site_id
              ( Primitives.Transformation.NegativeWhatEver (place, site_id),
                Some pos )
              s
          in
          handle_ports wk' r_l c_l transf' l_t' re acc (succ site_id)
        | (LKappa.LNK_FREE, _), s ->
          let wk'' = Pattern.new_free wk' (node, site_id) in
          let transf', l_t' =
            define_full_transformation transf l_t place site_id
              (Primitives.Transformation.Freed (place, site_id), None)
              s
          in
          handle_ports wk'' r_l c_l transf' l_t' re acc (succ site_id)
        | ((LKappa.LNK_SOME | LKappa.LNK_TYPE _), _), _ ->
          raise
            (ExceptionDefn.Internal_Error
               (Loc.annot_with_dummy
                  "Try to create the connected components of an ambiguous \
                   mixture."))
        | (LKappa.LNK_VALUE (i, _), pos), s ->
          (match Mods.IntMap.find_option i r_l with
          | Some ((node', site') as dst) ->
            let dst_place = Matching.Agent.Existing (node', id), site' in
            let wk'' = Pattern.new_link wk' (node, site_id) dst in
            let c_l' =
              Mods.IntMap.add i
                (Instantiation.Is_Bound_to ((place, site_id), dst_place))
                c_l
            in
            let transf', l_t' =
              define_full_transformation transf l_t place site_id
                ( Primitives.Transformation.Linked ((place, site_id), dst_place),
                  Some pos )
                s
            in
            handle_ports wk'' (Mods.IntMap.remove i r_l) c_l' transf' l_t' re
              acc (succ site_id)
          | None ->
            (match
               Tools.array_filter
                 (is_linked_on_port site_id i)
                 ag.LKappa.ra_ports
             with
            | [ site_id' ]
            (* link between 2 sites of 1 agent *)
              when List.for_all (fun x -> not (is_linked_on i x)) acc
                   && List.for_all (fun x -> not (is_linked_on i x)) re ->
              let wk'', (transf', l_t') =
                if site_id' > site_id then
                  ( Pattern.new_link wk' (node, site_id) (node, site_id'),
                    define_full_transformation transf l_t place site_id
                      ( Primitives.Transformation.Linked
                          ((place, site_id), (place, site_id')),
                        Some pos )
                      s )
                else
                  wk', define_positive_transformation transf l_t place site_id s
              in
              let c_l' =
                Mods.IntMap.add i
                  (Instantiation.Is_Bound_to
                     ((place, site_id), (place, site_id')))
                  c_l
              in
              handle_ports wk'' r_l c_l' transf' l_t' re acc (succ site_id)
            | _ :: _ -> link_occurence_failure i pos
            | [] ->
              (* link between 2 agents *)
              let r_l' = Mods.IntMap.add i (node, site_id) r_l in
              let transf', l_t' =
                define_positive_transformation transf l_t place site_id s
              in
              (match List.partition (is_linked_on i) re with
              | [], re' ->
                if List_util.exists_uniq (is_linked_on i) acc then
                  handle_ports wk' r_l' c_l transf' l_t' re' acc (succ site_id)
                else
                  link_occurence_failure i pos
              | [ n ], re'
                when List.for_all (fun x -> not (is_linked_on i x)) acc ->
                handle_ports wk' r_l' c_l transf' l_t' re' (n :: acc)
                  (succ site_id)
              | _, _ -> link_occurence_failure i pos)))
      )
    in
    handle_ports wk registered_links Mods.IntMap.empty transf' links_transf
      remains ag_l 0

let rec complete_with_creation sigs (removed, added) links_transf create_actions
    actions fresh = function
  | [] ->
    (match Mods.IntMap.root links_transf with
    | None ->
      List.rev_append actions create_actions, (List.rev removed, List.rev added)
    | Some (i, _) -> link_occurence_failure i Loc.dummy)
  | ag :: ag_l ->
    let place = Matching.Agent.Fresh (ag.Raw_mixture.a_type, fresh) in
    let rec handle_ports added l_t actions intf site_id =
      if site_id = Array.length ag.Raw_mixture.a_ports then (
        let create_actions' =
          Instantiation.Create (place, List.rev intf) :: create_actions
        in
        complete_with_creation sigs (removed, added) l_t create_actions' actions
          (succ fresh) ag_l
      ) else (
        let added', actions', point =
          match ag.Raw_mixture.a_ints.(site_id) with
          | None -> added, actions, (site_id, None)
          | Some i ->
            ( Primitives.Transformation.PositiveInternalized (place, site_id, i)
              :: added,
              Instantiation.Mod_internal ((place, site_id), i) :: actions,
              (site_id, Some i) )
        in
        let added'', actions'', l_t' =
          match ag.Raw_mixture.a_ports.(site_id) with
          | Raw_mixture.FREE ->
            ( Primitives.Transformation.Freed (place, site_id) :: added',
              Instantiation.Free (place, site_id) :: actions',
              l_t )
          | Raw_mixture.VAL i ->
            (match Mods.IntMap.pop i l_t with
            | Some dst, l_t' ->
              ( Primitives.Transformation.Linked ((place, site_id), dst)
                :: added',
                Instantiation.Bind_to ((place, site_id), dst)
                :: Instantiation.Bind_to (dst, (place, site_id))
                :: actions',
                l_t' )
            | None, l_t ->
              let l_t' = Mods.IntMap.add i (place, site_id) l_t in
              added', actions', l_t')
        in
        handle_ports added'' l_t' actions'' (point :: intf) (succ site_id)
      )
    in
    handle_ports
      (Primitives.Transformation.Agent place :: added)
      links_transf actions [] 0

let incr_origin = function
  | (Operator.ALG _ | Operator.MODIF _) as x -> x
  | Operator.RULE i -> Operator.RULE (succ i)

let connected_components_of_mixture ~debug_mode created mix (env, origin) =
  let sigs = Pattern.PreEnv.sigs env in
  let rec aux env transformations instantiations links_transf acc id = function
    | [] ->
      let removed, added = transformations in
      let actions' =
        List.fold_left
          (fun acs -> function
            | Primitives.Transformation.Linked (((ax, _) as x), ((ay, _) as y))
              when Matching.Agent.is_fresh ax || Matching.Agent.is_fresh ay ->
              Instantiation.Bind_to (x, y) :: acs
            | Primitives.Transformation.Linked (x, y) ->
              Instantiation.Bind (x, y) :: acs
            | Primitives.Transformation.Freed _
            | Primitives.Transformation.NegativeWhatEver _
            | Primitives.Transformation.PositiveInternalized _
            | Primitives.Transformation.NegativeInternalized _
            | Primitives.Transformation.Agent _ ->
              acs)
          instantiations.Instantiation.actions added
      in
      let transformations' = List.rev removed, List.rev added in
      let actions'', transformations'' =
        complete_with_creation sigs transformations' links_transf [] actions' 0
          created
      in
      ( ( origin,
          Tools.array_rev_of_list acc,
          { instantiations with Instantiation.actions = actions'' },
          transformations'' ),
        (env, Option_util.map incr_origin origin) )
    | h :: t ->
      let wk = Pattern.begin_new env in
      let instantiations' =
        {
          Instantiation.tests = [] :: instantiations.Instantiation.tests;
          Instantiation.actions = instantiations.Instantiation.actions;
          Instantiation.side_effects_src =
            instantiations.Instantiation.side_effects_src;
          Instantiation.side_effects_dst =
            instantiations.Instantiation.side_effects_dst;
          Instantiation.connectivity_tests =
            instantiations.Instantiation.connectivity_tests;
        }
      in
      let wk_out, (removed, added), l_t, event, remains =
        add_agents_in_cc sigs id wk Mods.IntMap.empty transformations
          links_transf instantiations' t [ h ]
      in
      let env', inj, cc, cc_id =
        Pattern.finish_new ~debug_mode ?origin wk_out
      in
      let added' =
        List_util.smart_map
          (Primitives.Transformation.rename ~debug_mode id inj)
          added
      in
      let removed' =
        List_util.smart_map
          (Primitives.Transformation.rename ~debug_mode id inj)
          removed
      in
      let event' =
        Instantiation.rename_abstract_event ~debug_mode id inj event
      in
      let l_t' =
        Mods.IntMap.map
          (fun ((p, s) as x) ->
            let p' = Matching.Agent.rename ~debug_mode id inj p in
            if p == p' then
              x
            else
              p', s)
          l_t
      in
      aux env' (removed', added') event' l_t' ((cc_id, cc) :: acc) (succ id)
        remains
  in
  aux env ([], []) Instantiation.empty_event Mods.IntMap.empty [] 0 mix

let rule_mixtures_of_ambiguous_rule contact_map sigs precomp_mixs =
  add_implicit_infos sigs
    (find_implicit_infos contact_map
       (List.rev (List.rev_map LKappa.copy_rule_agent precomp_mixs)))

let connected_components_sum_of_ambiguous_rule ~debug_mode ~compile_mode_on
    contact_map env ?origin precomp_mixs created =
  let noCounters = debug_mode in
  let sigs = Pattern.PreEnv.sigs env in
  let all_mixs =
    rule_mixtures_of_ambiguous_rule contact_map sigs precomp_mixs
  in
  let () =
    if compile_mode_on then
      Format.eprintf "@[<v>_____(%i)@,%a@]@." (List.length all_mixs)
        (Pp.list Pp.cut (fun f x ->
             Format.fprintf f "@[%a%a@]"
               (LKappa.print_rule_mixture ~noCounters sigs ~ltypes:true created)
               x
               (Raw_mixture.print ~noCounters ~created:true
                  ~initial_comma:(x <> []) ~sigs)
               (List.rev created)))
        all_mixs
  in
  List_util.fold_right_map
    (connected_components_of_mixture ~debug_mode created)
    all_mixs (env, origin)

let connected_components_sum_of_ambiguous_mixture ~debug_mode ~compile_mode_on
    contact_map env ?origin mix =
  let rules, (cc_env, _) =
    connected_components_sum_of_ambiguous_rule ~debug_mode ~compile_mode_on
      contact_map env ?origin mix []
  in
  ( cc_env,
    List.rev_map
      (function
        | _, l, event, ([], []) -> l, event.Instantiation.tests
        | _ -> assert false)
      rules )

let aux_lkappa_of_pattern free_id p =
  Pattern.fold_by_type
    (fun ~pos ~agent_type intf (acc, lnk_pack) ->
      let ra_ports =
        Array.make (Array.length intf)
          (Loc.annot_with_dummy LKappa.LNK_ANY, LKappa.Maintained)
      in
      let ra_ints = Array.make (Array.length intf) LKappa.I_ANY in
      let out =
        {
          LKappa.ra_type = agent_type;
          LKappa.ra_erased = false;
          LKappa.ra_syntax = None;
          LKappa.ra_ports;
          LKappa.ra_ints;
        }
      in
      let acc' = Mods.IntMap.add pos out acc in
      let lnk_pack' =
        Tools.array_fold_lefti
          (fun site ((free_id, known_src) as pack) (link, int) ->
            let () =
              if int <> -1 then ra_ints.(site) <- LKappa.I_VAL_CHANGED (int, int)
            in
            match link with
            | Pattern.UnSpec -> pack
            | Pattern.Free ->
              let () =
                ra_ports.(site) <-
                  Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Maintained
              in
              pack
            | Pattern.Link (dst_a, dst_s) ->
              let src_info = site, agent_type in
              (match Mods.Int2Map.find_option (dst_a, dst_s) known_src with
              | Some (id, dst_info) ->
                let () =
                  ra_ports.(site) <-
                    ( Loc.annot_with_dummy (LKappa.LNK_VALUE (id, dst_info)),
                      LKappa.Maintained )
                in
                let () =
                  (Mods.IntMap.find_default out dst_a acc').LKappa.ra_ports.(dst_s) <-
                    ( Loc.annot_with_dummy (LKappa.LNK_VALUE (id, src_info)),
                      LKappa.Maintained )
                in
                pack
              | None ->
                ( succ free_id,
                  Mods.Int2Map.add (pos, site) (free_id, src_info) known_src )))
          lnk_pack intf
      in
      acc', lnk_pack')
    p
    (Mods.IntMap.empty, (free_id, Mods.Int2Map.empty))

let register_positive_transformations sigs mixs free_id transfs =
  List.fold_left
    (fun ((fid, fr) as pack) -> function
      | Primitives.Transformation.NegativeWhatEver _
      | Primitives.Transformation.NegativeInternalized _
      | Primitives.Transformation.Agent (Matching.Agent.Existing _) ->
        assert false
      | Primitives.Transformation.Agent (Matching.Agent.Fresh (a_type, id)) ->
        let si = Signature.arity sigs a_type in
        let n =
          {
            Raw_mixture.a_type;
            Raw_mixture.a_ports = Array.make si Raw_mixture.FREE;
            Raw_mixture.a_ints = Array.make si None;
          }
        in
        fid, Mods.IntMap.add id n fr
      | Primitives.Transformation.PositiveInternalized
          (Matching.Agent.Existing ((id, _), cc_id), s, i) ->
        let () =
          match Mods.IntMap.find_option id mixs.(cc_id) with
          | None -> assert false
          | Some a ->
            (match a.LKappa.ra_ints.(s) with
            | LKappa.I_ANY_CHANGED _ | LKappa.I_ANY_ERASED
            | LKappa.I_VAL_ERASED _ ->
              assert false
            | LKappa.I_VAL_CHANGED (j, k) ->
              let () = assert (j = k) in
              a.LKappa.ra_ints.(s) <- LKappa.I_VAL_CHANGED (j, i)
            | LKappa.I_ANY -> a.LKappa.ra_ints.(s) <- LKappa.I_ANY_CHANGED i)
        in
        pack
      | Primitives.Transformation.PositiveInternalized
          (Matching.Agent.Fresh (_, id), s, i) ->
        let () =
          match Mods.IntMap.find_option id fr with
          | Some a -> a.Raw_mixture.a_ints.(s) <- Some i
          | None -> ()
        in
        pack
      | Primitives.Transformation.Freed (Matching.Agent.Fresh _, _) -> fid, fr
      | Primitives.Transformation.Freed
          (Matching.Agent.Existing ((id, _), cc_id), s) ->
        let () =
          match Mods.IntMap.find_option id mixs.(cc_id) with
          | Some a ->
            let test, edit = a.LKappa.ra_ports.(s) in
            let () = assert (edit = LKappa.Maintained) in
            a.LKappa.ra_ports.(s) <- test, LKappa.Freed
          | None -> assert false
        in
        pack
      | Primitives.Transformation.Linked
          ( (Matching.Agent.Existing ((id1, _), cc_id1), s1),
            (Matching.Agent.Existing ((id2, _), cc_id2), s2) ) ->
        let () =
          match Mods.IntMap.find_option id1 mixs.(cc_id1) with
          | Some a ->
            let test, edit = a.LKappa.ra_ports.(s1) in
            let () = assert (edit = LKappa.Maintained) in
            a.LKappa.ra_ports.(s1) <- test, LKappa.Linked fid
          | None -> assert false
        in
        let () =
          match Mods.IntMap.find_option id2 mixs.(cc_id2) with
          | Some a ->
            let test, edit = a.LKappa.ra_ports.(s2) in
            let () = assert (edit = LKappa.Maintained) in
            a.LKappa.ra_ports.(s2) <- test, LKappa.Linked fid
          | None -> assert false
        in
        succ fid, fr
      | Primitives.Transformation.Linked
          ( (Matching.Agent.Fresh (_, id), s1),
            (Matching.Agent.Existing ((eid, _), cc_id), s2) )
      | Primitives.Transformation.Linked
          ( (Matching.Agent.Existing ((eid, _), cc_id), s2),
            (Matching.Agent.Fresh (_, id), s1) ) ->
        let () =
          match Mods.IntMap.find_option id fr with
          | Some a -> a.Raw_mixture.a_ports.(s1) <- Raw_mixture.VAL fid
          | None -> assert false
        in
        let () =
          match Mods.IntMap.find_option eid mixs.(cc_id) with
          | Some a ->
            let test, edit = a.LKappa.ra_ports.(s2) in
            let () = assert (edit = LKappa.Maintained) in
            a.LKappa.ra_ports.(s2) <- test, LKappa.Linked fid
          | None -> assert false
        in
        succ fid, fr
      | Primitives.Transformation.Linked
          ( (Matching.Agent.Fresh (_, id1), s1),
            (Matching.Agent.Fresh (_, id2), s2) ) ->
        let () =
          match Mods.IntMap.find_option id1 fr with
          | Some a -> a.Raw_mixture.a_ports.(s1) <- Raw_mixture.VAL fid
          | None -> assert false
        in
        let () =
          match Mods.IntMap.find_option id2 fr with
          | Some a -> a.Raw_mixture.a_ports.(s2) <- Raw_mixture.VAL fid
          | None -> assert false
        in
        succ fid, fr)
    (free_id, Mods.IntMap.empty)
    transfs
  |> snd

let add_negative_transformations sigs mixs transfs =
  List.iter
    (function
      | Primitives.Transformation.Agent (Matching.Agent.Fresh _)
      | Primitives.Transformation.NegativeInternalized
          (Matching.Agent.Fresh _, _)
      | Primitives.Transformation.PositiveInternalized _
      | Primitives.Transformation.Linked ((Matching.Agent.Fresh _, _), _)
      | Primitives.Transformation.Linked (_, (Matching.Agent.Fresh _, _))
      | Primitives.Transformation.Freed (Matching.Agent.Fresh _, _) ->
        assert false
      | Primitives.Transformation.NegativeWhatEver _
      | Primitives.Transformation.NegativeInternalized
          (Matching.Agent.Existing _, _)
      | Primitives.Transformation.Linked
          ((Matching.Agent.Existing _, _), (Matching.Agent.Existing _, _))
      | Primitives.Transformation.Freed (Matching.Agent.Existing _, _) ->
        ()
      | Primitives.Transformation.Agent
          (Matching.Agent.Existing ((id, _), cc_id)) ->
        let ag =
          match Mods.IntMap.find_option id mixs.(cc_id) with
          | None -> assert false
          | Some a -> a
        in
        mixs.(cc_id) <-
          Mods.IntMap.add id (LKappa.agent_to_erased sigs ag) mixs.(cc_id))
    transfs

let lkappa_of_elementary_rule sigs domain r =
  let nb_cc = Array.length r.Primitives.connected_components in
  let mixs = Array.make nb_cc Mods.IntMap.empty in
  let free_id =
    Tools.array_fold_lefti
      (fun cc_id free_id cc ->
        let out, (free_id', _) =
          aux_lkappa_of_pattern free_id
            (Pattern.Env.content (Pattern.Env.get domain cc))
        in
        let () = mixs.(cc_id) <- out in
        free_id')
      1 r.Primitives.connected_components
  in
  let news =
    register_positive_transformations sigs mixs free_id r.Primitives.inserted
  in
  let () = add_negative_transformations sigs mixs r.Primitives.removed in
  let r_mix =
    Array.fold_left
      (fun a b -> Mods.IntMap.fold (fun _ x acc -> x :: acc) b a)
      [] mixs
    |> List.rev
  in
  let r_created =
    Mods.IntMap.fold (fun _ x acc -> x :: acc) news [] |> List.rev
  in
  r_mix, r_created
(*{
  LKappa.r_mix; LKappa.r_created; LKappa.r_edit_style = true;
  LKappa.r_rate = r.Primitives.rate;
  LKappa.r_un_rate = r.Primitives.unary_rate;
  LKappa.r_delta_tokens = r.Primitives.delta_tokens;
  }*)
