(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let link_occurence_failure key pos =
  raise (ExceptionDefn.Internal_Error
           ("Bug: Link "^string_of_int key^
            " is problematic! LKappa is either broken"^
            " or unused! Please report.",pos))

let ports_from_contact_map contact_map ty_id p_id =
  snd contact_map.(ty_id).(p_id)

let find_implicit_infos contact_map ags =
  let max_s m = function
    | LKappa.Linked (i,_) -> max i m
    | LKappa.Freed | LKappa.Maintained | LKappa.Erased -> m in
  let new_switch = function
    | LKappa.Maintained -> LKappa.Maintained
    | LKappa.Freed | LKappa.Linked _ | LKappa.Erased -> LKappa.Freed in
  let rec aux_one ag_tail ty_id max_id ports i =
    let or_ty = (i,ty_id) in
    if i = Array.length ports
    then List.map (fun (f,a,c) -> (f,ports,a,c)) (aux_ags max_id ag_tail)
    else
      match ports.(i) with
      | (Ast.LNK_TYPE (p,a),_),s ->
        List.map
          (fun (free_id,ports,ags,cor) ->
             let () =
               ports.(i) <-
                 (Locality.dummy_annot (Ast.LNK_VALUE (free_id,(p,a))),s) in
             (succ free_id, ports, ags,
              (free_id,(p,a),or_ty,new_switch s)::cor))
          (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
      | (Ast.LNK_SOME,_), s ->
        List_util.map_flatten
          (fun (free_id,ports,ags,cor) ->
             List.map
               (fun (a,p) ->
                  let ports' = Array.copy ports in
                  let () =
                    ports'.(i) <-
                      (Locality.dummy_annot
                         (Ast.LNK_VALUE (free_id,(p,a))),s) in
                  (succ free_id, ports', ags,
                   (free_id,(p,a),or_ty,new_switch s)::cor))
               (ports_from_contact_map contact_map ty_id i))
          (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
      | (Ast.LNK_VALUE (j,_),_),s ->
        aux_one ag_tail ty_id (max_s (max j max_id) s) ports (succ i)
      | (Ast.LNK_FREE, pos), LKappa.Maintained ->
        let () = (* Do not make test if being free is the only possibility *)
          match ports_from_contact_map contact_map ty_id i with
          | [] -> ports.(i) <- (Ast.LNK_ANY,pos), LKappa.Maintained
          | _ :: _ -> () in
        aux_one ag_tail ty_id max_id ports (succ i)
      | (Ast.LNK_FREE, _), (LKappa.Erased | LKappa.Linked _ | LKappa.Freed as s) ->
        aux_one ag_tail ty_id (max_s max_id s) ports (succ i)
      | ((Ast.LNK_ANY|Ast.ANY_FREE),_), LKappa.Maintained ->
        aux_one ag_tail ty_id max_id ports (succ i)
      | ((Ast.LNK_ANY|Ast.ANY_FREE),pos),
        (LKappa.Erased | LKappa.Linked _ | LKappa.Freed as s) ->
        match ports_from_contact_map contact_map ty_id i with
        | [] when s = LKappa.Freed ->
          (* Do not make test is being free is the only possibility *)
          let () = ports.(i) <- (Ast.LNK_ANY,pos), LKappa.Maintained in
          aux_one ag_tail ty_id max_id ports (succ i)
        | _pfcm ->
          (*List_util.map_flatten
            (fun (free_id,ports,ags,cor) ->
            let () = ports.(i) <-
            (Locality.dummy_annot Ast.FREE,
            if s = LKappa.Freed then LKappa.Maintained else s) in
            (free_id, ports, ags, cor) ::
            List.map
            (fun (a,p) ->
            let ports' = Array.copy ports in
            let () =
            ports'.(i) <- (Locality.dummy_annot
            (Ast.LNK_VALUE (free_id,(p,a))),s) in
            (succ free_id, ports', ags,
            (free_id,(p,a),or_ty,new_switch s)::cor))
            pfcm)*)
          (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
  and aux_ags max_id = function
    | [] -> [succ max_id,[],[]]
    | ag :: ag_tail ->
      List.map
        (fun (free_id,ports,ags,cor) ->
           (free_id,
            {LKappa.ra_type = ag.LKappa.ra_type; LKappa.ra_ports = ports;
             LKappa.ra_ints = ag.LKappa.ra_ints;
             LKappa.ra_erased = ag.LKappa.ra_erased;
             LKappa.ra_syntax = ag.LKappa.ra_syntax}::ags,
            cor)
        )
        (aux_one ag_tail ag.LKappa.ra_type max_id ag.LKappa.ra_ports 0)
  in List.rev @@ List.rev_map (fun (_,mix,todo) -> (mix,todo)) (aux_ags 0 ags)

let complete_with_candidate ag id todo p_id dst_info p_switch =
  Tools.array_fold_lefti
    (fun i acc port ->
       if i <> p_id then acc else
         match port with
         | ((Ast.LNK_ANY|Ast.ANY_FREE),_), s ->
           if s = LKappa.Maintained then
             let ports' = Array.copy ag.LKappa.ra_ports in
             let () =
               ports'.(i) <-
                 (Locality.dummy_annot
                    (Ast.LNK_VALUE (id,dst_info)),p_switch) in
             ({ LKappa.ra_type = ag.LKappa.ra_type; LKappa.ra_ports = ports';
                LKappa.ra_ints = ag.LKappa.ra_ints;
                LKappa.ra_erased = ag.LKappa.ra_erased;
                LKappa.ra_syntax = ag.LKappa.ra_syntax;}, todo)
             :: acc
           else if s = LKappa.Erased && p_switch = LKappa.Freed then
             let ports' = Array.copy ag.LKappa.ra_ports in
             let () =
               ports'.(i) <-
                 (Locality.dummy_annot (Ast.LNK_VALUE (id,dst_info)),s) in
             ({ LKappa.ra_type = ag.LKappa.ra_type; LKappa.ra_ports = ports';
                LKappa.ra_ints = ag.LKappa.ra_ints;
                LKappa.ra_erased = ag.LKappa.ra_erased;
                LKappa.ra_syntax = ag.LKappa.ra_syntax;}, todo)
             :: acc
           else acc
         | (Ast.LNK_VALUE (k,x),_),s ->
           begin
             match
               List.partition
                 (fun (j,_,(p',a'),sw') -> j=k && i=p' && a'= ag.LKappa.ra_type
                                           && sw' = p_switch) todo with
             | [ _ ], todo' ->
               let ports' = Array.copy ag.LKappa.ra_ports in
               let () = assert (x = dst_info) in
               let () = ports'.(i) <-
                   (Locality.dummy_annot (Ast.LNK_VALUE (id,x)),s) in
               ({ LKappa.ra_type = ag.LKappa.ra_type; LKappa.ra_ports = ports';
                  LKappa.ra_ints = ag.LKappa.ra_ints;
                  LKappa.ra_erased = ag.LKappa.ra_erased;
                  LKappa.ra_syntax = ag.LKappa.ra_syntax;},
                todo') :: acc
             |[], _ -> acc
             | _ :: _ :: _, _ -> assert false
           end
         | ((Ast.LNK_TYPE _ | Ast.LNK_FREE | Ast.LNK_SOME),_), _ -> acc)
    [] ag.LKappa.ra_ports

let new_agent_with_one_link sigs ty_id port link dst_info switch =
  let arity = Signature.arity sigs ty_id in
  let ports =
    Array.make arity (Locality.dummy_annot Ast.LNK_ANY, LKappa.Maintained) in
  let internals = Array.make arity LKappa.I_ANY in
  let () = ports.(port) <-
      (Locality.dummy_annot (Ast.LNK_VALUE (link,dst_info)),switch) in
  { LKappa.ra_type = ty_id; LKappa.ra_ports = ports; LKappa.ra_ints = internals;
    LKappa.ra_erased = false; LKappa.ra_syntax = None;}

let rec add_one_implicit_info sigs id ((port,ty_id),dst_info,s as info) todo =
  function
  | [] -> [[new_agent_with_one_link sigs ty_id port id dst_info s],todo]
  | ag :: ag_tail ->
    let out_tail = add_one_implicit_info sigs id info todo ag_tail in
    let extra_ags =
      if ty_id = ag.LKappa.ra_type then
        (List.map
           (fun (ag',todo') -> ag'::ag_tail,todo')
           (complete_with_candidate ag id todo port dst_info s))
      else [] in
    List.fold_left (fun l (x,todo') -> ((ag::x,todo')::l)) extra_ags out_tail

let add_implicit_infos sigs l =
  let rec aux acc = function
    | [] -> acc
    | (m,[]) :: t -> aux (m::acc) t
    | (m,((id,info,dst_info,s) :: todo')) :: t ->
      aux acc
        (List.rev_append
           (add_one_implicit_info sigs id (info,dst_info,s) todo' m) t)
  in aux [] l

let is_linked_on_port me i id = function
  | (Ast.LNK_VALUE (j,_),_),_ when i = j -> id <> me
  | ((Ast.LNK_VALUE _ | Ast.LNK_FREE | Ast.LNK_TYPE _ |
      Ast.LNK_ANY | Ast.LNK_SOME | Ast.ANY_FREE),_),_ -> false

let is_linked_on i ag =
  Tools.array_filter (is_linked_on_port (-1) i) ag.LKappa.ra_ports <> []

let define_full_transformation
    sigs (removed,added as transf) links_transf place site (cand,cand_pos) switch =
  match switch with
  | LKappa.Freed ->
    ((cand::removed, (Primitives.Transformation.Freed(place,site)::added)),
     links_transf)
  | LKappa.Maintained ->
    (transf,links_transf)
  | LKappa.Erased ->
    ((cand::removed,added),links_transf)
  | LKappa.Linked (i,pos) ->
    match Mods.IntMap.find_option i links_transf with
    | None ->
      let links_transf' =
        Mods.IntMap.add
          i ((place,site),Option_util.map (fun _ -> pos) cand_pos)
          links_transf in
      ((cand::removed,added),links_transf')
    | Some ((place',site' as dst'),risk) ->
      let links_transf' = Mods.IntMap.remove i links_transf in
      ((cand::removed,
        Primitives.Transformation.Linked((place,site),dst')::added),
       links_transf')

let define_positive_transformation
    sigs (removed,added as transf) links_transf place site switch =
  match switch with
  | LKappa.Freed ->
    ((removed,
      Primitives.Transformation.Freed (place,site)::added),links_transf)
  | LKappa.Erased -> (transf,links_transf)
  | LKappa.Maintained -> (transf,links_transf)
  | LKappa.Linked (i,pos) ->
    match Mods.IntMap.find_option i links_transf with
    | None ->
      let links_transf' =
        Mods.IntMap.add i ((place,site),Some pos) links_transf in
      (transf,links_transf')
    | Some (dst',_) ->
      let links_transf' = Mods.IntMap.remove i links_transf in
      ((removed,
        Primitives.Transformation.Linked((place,site),dst')::added),
       links_transf')

let add_instantiation_free actions pl s = function
  | LKappa.Freed -> Instantiation.Free (pl,s) :: actions
  | (LKappa.Linked _ | LKappa.Maintained | LKappa.Erased) -> actions
let add_side_site side_sites bt pl s = function
  | (LKappa.Freed | LKappa.Linked _ | LKappa.Erased) -> ((pl,s),bt)::side_sites
  | LKappa.Maintained -> side_sites
let add_freed_side_effect side_effects pl s = function
  | (Ast.LNK_VALUE _,_),LKappa.Freed -> (pl,s)::side_effects
  | (Ast.LNK_VALUE _,_),(LKappa.Maintained | LKappa.Erased | LKappa.Linked _)
  | ((Ast.LNK_FREE | Ast.LNK_ANY | Ast.LNK_SOME |
      Ast.LNK_TYPE _ | Ast.ANY_FREE),_),_ ->
    side_effects
let add_extra_side_effects side_effects place refined =
  let rec aux side_effects site_id =
    if site_id < 0 then side_effects
    else
      aux
        (add_freed_side_effect side_effects place site_id refined.(site_id))
        (pred site_id) in
  aux side_effects (pred (Array.length refined))

(* Deals with tests, erasure internal state change and release (but
   not binding)*)
let make_instantiation place links event ref_ports is_erased =
  function
  | None -> {
      Instantiation.tests = event.Instantiation.tests;
      Instantiation.actions = event.Instantiation.actions;
      Instantiation.side_effects_src = event.Instantiation.side_effects_src;
      Instantiation.side_effects_dst = add_extra_side_effects
          event.Instantiation.side_effects_dst place ref_ports;
      Instantiation.connectivity_tests = event.Instantiation.connectivity_tests;
    }
  | Some (ports, ints) ->
    match event.Instantiation.tests with
    | [] -> assert false
    | this_cc_tests :: o_cc_tests ->
      let rec aux site_id tests actions side_effects_src side_effects_dst links=
        if site_id >= Array.length ports
        then { Instantiation.tests = tests :: o_cc_tests;
               Instantiation.actions;
               Instantiation.side_effects_src; Instantiation.side_effects_dst;
               Instantiation.connectivity_tests =
                 event.Instantiation.connectivity_tests; }
        else
          let tests',actions' =
            match ints.(site_id) with
            | (LKappa.I_ANY | LKappa.I_ANY_ERASED) -> tests,actions
            | LKappa.I_ANY_CHANGED j ->
              tests,
              Instantiation.Mod_internal ((place,site_id),j) :: actions
            | LKappa.I_VAL_CHANGED (i,j) ->
              Instantiation.Has_Internal ((place,site_id),i) :: tests,
              if i <> j then
                Instantiation.Mod_internal ((place,site_id),j) :: actions
              else actions
            | LKappa.I_VAL_ERASED i ->
              Instantiation.Has_Internal ((place,site_id),i) :: tests,
              actions in
          let tests'',actions'',side_sites',side_effects',links' =
            match ports.(site_id) with
            | ((Ast.LNK_ANY|Ast.ANY_FREE),_), s ->
              let side_effects' =
                match s with
                | LKappa.Maintained ->
                  add_freed_side_effect
                    side_effects_dst place site_id ref_ports.(site_id)
                | LKappa.Erased | LKappa.Linked _ | LKappa.Freed ->
                  side_effects_dst in
              tests', add_instantiation_free actions' place site_id s,
              add_side_site side_effects_src Instantiation.ANY
                place site_id s,
              side_effects',
              links
            | (Ast.LNK_FREE,_), s ->
              (Instantiation.Is_Free (place,site_id) :: tests'),
              add_instantiation_free actions' place site_id s,side_effects_src,
              side_effects_dst, links
            | (Ast.LNK_SOME,_), s ->
              Instantiation.Is_Bound (place,site_id) :: tests',
              add_instantiation_free actions' place site_id s,
              add_side_site side_effects_src Instantiation.BOUND
                place site_id s,
              side_effects_dst, links
            | (Ast.LNK_TYPE (b,a),_),s ->
              Instantiation.Has_Binding_type ((place,site_id),(a,b))
              :: tests',
              add_instantiation_free actions' place site_id s,
              add_side_site
                side_effects_src (Instantiation.BOUND_TYPE (a,b))
                place site_id s,
              side_effects_dst, links
            | (Ast.LNK_VALUE (i,_),_),s ->
              match Mods.IntMap.find_option i links with
              | Some x -> x :: tests',
                          add_instantiation_free actions' place site_id s,
                          side_effects_src, side_effects_dst,
                          Mods.IntMap.remove i links
              | None ->
                tests', add_instantiation_free actions' place site_id s,
                side_effects_src, side_effects_dst, links in
          aux (succ site_id) tests''
            actions'' side_sites' side_effects' links' in
      aux 0 (Instantiation.Is_Here place :: this_cc_tests)
        (if is_erased
         then Instantiation.Remove place :: event.Instantiation.actions
         else event.Instantiation.actions) event.Instantiation.side_effects_src
        event.Instantiation.side_effects_dst links

let rec add_agents_in_cc sigs id wk registered_links (removed,added as transf)
    links_transf instantiations remains =
  function
  | [] ->
    begin match Mods.IntMap.root registered_links with
      | None -> (wk,transf,links_transf,instantiations,remains)
      | Some (key,_) -> link_occurence_failure key Locality.dummy
    end
  | ag :: ag_l ->
    let (node,wk) = Pattern.new_node wk ag.LKappa.ra_type in
    let place = Matching.Agent.Existing (node,id) in
    let transf' =
      if ag.LKappa.ra_erased
      then Primitives.Transformation.Agent place::removed,added
      else transf in
    let rec handle_ports wk r_l c_l (removed,added) l_t re acc site_id =
      if site_id = Array.length ag.LKappa.ra_ports
      then
        let instantiations' =
          make_instantiation
            place c_l instantiations ag.LKappa.ra_ports ag.LKappa.ra_erased ag.LKappa.ra_syntax in
        add_agents_in_cc
          sigs id wk r_l (removed,added) l_t instantiations' re acc
      else
        let transf,wk' = match ag.LKappa.ra_ints.(site_id) with
          | LKappa.I_ANY -> (removed,added),wk
          | LKappa.I_ANY_ERASED ->
            (Primitives.Transformation.NegativeInternalized (place,site_id)::removed,added),
            wk
          | LKappa.I_ANY_CHANGED j ->
            (Primitives.Transformation.NegativeInternalized (place,site_id)::removed,
             Primitives.Transformation.PositiveInternalized (place,site_id,j)::added),
            wk
          | LKappa.I_VAL_CHANGED (i,j) ->
            (if i = j then (removed,added)
             else
               Primitives.Transformation.NegativeInternalized (place,site_id)::removed,
               Primitives.Transformation.PositiveInternalized (place,site_id,j)::added),
            Pattern.new_internal_state wk (node,site_id) i
          | LKappa.I_VAL_ERASED i ->
            (Primitives.Transformation.NegativeInternalized (place,site_id)::removed,added),
            Pattern.new_internal_state wk (node,site_id) i
        in
        match ag.LKappa.ra_ports.(site_id) with
        | ((Ast.LNK_ANY|Ast.ANY_FREE),pos), s ->
          let transf',l_t' =
            define_full_transformation
              sigs transf l_t place site_id
              (Primitives.Transformation.NegativeWhatEver
                 (place,site_id),Some pos) s in
          handle_ports wk' r_l c_l transf' l_t' re acc (succ site_id)
        | (Ast.LNK_FREE,_), s ->
          let wk'' = Pattern.new_free wk' (node,site_id) in
          let transf',l_t' =
            define_full_transformation
              sigs transf l_t place site_id
              (Primitives.Transformation.Freed (place,site_id),None) s in
          handle_ports
            wk'' r_l c_l transf' l_t' re acc (succ site_id)
        | ((Ast.LNK_SOME | Ast.LNK_TYPE _),_),_ ->
          raise (ExceptionDefn.Internal_Error
                   (Locality.dummy_annot
                      "Try to create the connected components of an ambiguous mixture."))
        | (Ast.LNK_VALUE (i,_),pos),s ->
          match Mods.IntMap.find_option i r_l with
          | Some (node',site' as dst) ->
            let dst_place = Matching.Agent.Existing (node',id),site' in
            let wk'' = Pattern.new_link wk' (node,site_id) dst in
            let c_l' =
              Mods.IntMap.add
                i (Instantiation.Is_Bound_to ((place,site_id),dst_place))
                c_l in
            let transf',l_t' =
              define_full_transformation
                sigs transf l_t place site_id
                (Primitives.Transformation.Linked
                   ((place,site_id),(dst_place)),Some pos) s in
            handle_ports wk'' (Mods.IntMap.remove i r_l) c_l' transf'
              l_t' re acc (succ site_id)
          | None ->
            match Tools.array_filter
                    (is_linked_on_port site_id i) ag.LKappa.ra_ports with
            | [site_id'] (* link between 2 sites of 1 agent *)
              when List.for_all (fun x -> not(is_linked_on i x)) acc &&
                   List.for_all (fun x -> not(is_linked_on i x)) re ->
              let wk'' =
                if site_id' > site_id then
                  Pattern.new_link
                    wk' (node,site_id) (node,site_id')
                else wk' in
              let transf',l_t' =
                define_full_transformation
                  sigs transf l_t place site_id
                  (Primitives.Transformation.Linked
                     ((place,site_id),(place,site_id')),Some pos) s in
              let transf'',l_t'' =
                define_full_transformation
                  sigs transf' l_t' place site_id'
                  (Primitives.Transformation.Linked
                     ((place,site_id'),(place,site_id)),Some pos) s in
              let c_l' =
                Mods.IntMap.add
                  i (Instantiation.Is_Bound_to
                       ((place,site_id),(place,site_id'))) c_l in
              handle_ports
                wk'' r_l c_l' transf'' l_t'' re acc (succ site_id)
            | _ :: _ ->
              link_occurence_failure i pos
            | [] -> (* link between 2 agents *)
              let r_l' = Mods.IntMap.add i (node,site_id) r_l in
              let transf',l_t' =
                define_positive_transformation
                  sigs transf l_t place site_id s in
              match List.partition (is_linked_on i) re with
              | [], re' ->
                if List_util.exists_uniq (is_linked_on i) acc then
                  handle_ports
                    wk' r_l' c_l transf' l_t' re' acc (succ site_id)
                else
                  link_occurence_failure i pos
              | [n], re' when List.for_all
                    (fun x -> not(is_linked_on i x)) acc ->
                handle_ports
                  wk' r_l' c_l transf' l_t' re' (n::acc) (succ site_id)
              | _, _ -> link_occurence_failure i pos in
    handle_ports wk registered_links Mods.IntMap.empty
      transf' links_transf remains ag_l 0

let rec complete_with_creation
    sigs (removed,added) links_transf create_actions actions fresh =
  function
  | [] ->
    begin match Mods.IntMap.root links_transf with
      | None -> List.rev_append actions create_actions,
                (List.rev removed, List.rev added)
      | Some (i,_) -> link_occurence_failure i Locality.dummy
    end
  | ag :: ag_l ->
    let place = Matching.Agent.Fresh (ag.Raw_mixture.a_type,fresh) in
    let rec handle_ports added l_t actions intf site_id =
      if site_id = Array.length ag.Raw_mixture.a_ports then
        let create_actions' =
          Instantiation.Create (place,List.rev intf)
          :: create_actions in
        complete_with_creation
          sigs (removed,added) l_t create_actions' actions (succ fresh) ag_l
      else
        let added',actions',point =
          match ag.Raw_mixture.a_ints.(site_id) with
          | None -> added,actions,(site_id,None)
          | Some i ->
            Primitives.Transformation.PositiveInternalized (place,site_id,i)::added,
            Instantiation.Mod_internal((place,site_id),i)::actions,
            (site_id,Some i) in
        let added'',actions'',l_t' =
          match ag.Raw_mixture.a_ports.(site_id) with
          | Raw_mixture.FREE ->
            Primitives.Transformation.Freed (place,site_id)::added',
            (Instantiation.Free (place,site_id) :: actions'),
            l_t
          | Raw_mixture.VAL i ->
            match Mods.IntMap.pop i l_t with
            | Some ((place',site' as dst),risk),l_t' ->
              Primitives.Transformation.Linked((place,site_id),dst)::added',
              (Instantiation.Bind_to((place,site_id),dst)
               ::(Instantiation.Bind_to((dst,(place,site_id))))::actions'),
              l_t'
            | None,l_t ->
              let l_t' = Mods.IntMap.add i ((place,site_id),None) l_t in
              (added',actions',l_t') in
        handle_ports added'' l_t' actions'' (point::intf) (succ site_id) in
    handle_ports
      (Primitives.Transformation.Agent place::added) links_transf actions [] 0

let incr_origin = function
  | ( Operator.ALG _ | Operator.PERT _  as x) -> x
  | Operator.RULE i -> Operator.RULE (succ i)

let connected_components_of_mixture created mix (env,origin) =
  let sigs = Pattern.PreEnv.sigs env in
  let rec aux env transformations instantiations links_transf acc id = function
    | [] ->
      let removed,added = transformations in
      let actions' =
        List.fold_left
          (fun acs -> function
             | Primitives.Transformation.Linked ((ax, _ as x),(ay, _ as y))
               when Matching.Agent.is_fresh ax ||
                    Matching.Agent.is_fresh ay ->
               Instantiation.Bind_to (x,y) :: acs
             | Primitives.Transformation.Linked (x,y) ->
               Instantiation.Bind (x,y) :: acs
             | (Primitives.Transformation.Freed _ |
                Primitives.Transformation.NegativeWhatEver _ |
                Primitives.Transformation.PositiveInternalized _ |
                Primitives.Transformation.NegativeInternalized _ |
                Primitives.Transformation.Agent _) -> acs)
          instantiations.Instantiation.actions added in
      let transformations' = (List.rev removed, List.rev added) in
      let actions'',transformations'' =
        complete_with_creation
          sigs transformations' links_transf [] actions' 0 created in
      ((origin,Tools.array_rev_of_list acc,
        { instantiations with Instantiation.actions = actions'' },
        transformations''),
       (env,Option_util.map incr_origin origin))
    | h :: t ->
      let wk = Pattern.begin_new env in
      let instantiations' = {
        Instantiation.tests = [] :: instantiations.Instantiation.tests;
        Instantiation.actions = instantiations.Instantiation.actions;
        Instantiation.side_effects_src =
          instantiations.Instantiation.side_effects_src;
        Instantiation.side_effects_dst =
          instantiations.Instantiation.side_effects_dst;
        Instantiation.connectivity_tests =
          instantiations.Instantiation.connectivity_tests } in
      let (wk_out,(removed,added),l_t,event, remains) =
        add_agents_in_cc
          sigs id wk Mods.IntMap.empty transformations
          links_transf instantiations' t [h] in
      let (env',inj, cc, cc_id) =
        Pattern.finish_new ?origin wk_out in
      let added' =
        List_util.smart_map
          (Primitives.Transformation.rename id inj) added in
      let removed' =
        List_util.smart_map
          (Primitives.Transformation.rename id inj) removed in
      let event' =
        Instantiation.rename_abstract_event id inj event in
      let l_t' = Mods.IntMap.map
          (fun (((p,s),b) as x) ->
             let p' = Matching.Agent.rename id inj p in
             if p == p' then x else ((p',s),b)) l_t in
      aux env' (removed',added') event' l_t' ((cc_id,cc)::acc) (succ id) remains
  in aux env ([],[]) Instantiation.empty_event Mods.IntMap.empty [] 0 mix

let rule_mixtures_of_ambiguous_rule contact_map sigs precomp_mixs =
  add_implicit_infos
    sigs (find_implicit_infos
            contact_map
            (List.map LKappa.copy_rule_agent precomp_mixs))

let connected_components_sum_of_ambiguous_rule
   ~compileModeOn contact_map env ?origin precomp_mixs created =
  let sigs = Pattern.PreEnv.sigs env in
  let all_mixs =
    rule_mixtures_of_ambiguous_rule contact_map sigs precomp_mixs in
  let () =
    if compileModeOn then
      Format.eprintf "@[<v>_____(%i)@,%a@]@."
        (List.length all_mixs)
        (Pp.list
           Pp.cut
           (fun f x ->
              Format.fprintf
                f "@[%a%t%a@]"
                (LKappa.print_rule_mixture sigs ~ltypes:true created) x
                (if x <> [] && created <> [] then Pp.comma else Pp.empty)
                (Raw_mixture.print
                   ~explicit_free:true ~compact:false ~created:true ~sigs)
                (List.rev created)))
        all_mixs in
  List_util.fold_right_map (connected_components_of_mixture created)
    all_mixs (env,origin)

let connected_components_sum_of_ambiguous_mixture
    ~compileModeOn contact_map env ?origin mix =
  let rules,(cc_env,_) =
    connected_components_sum_of_ambiguous_rule
      ~compileModeOn contact_map env ?origin mix [] in
  (cc_env, List.map
     (function _, l, event, ([],[]) -> l, event.Instantiation.tests
             | _ -> assert false) rules)

let patterns_of_mixture contact_map sigs pre_env e =
  let snap = Edges.build_snapshot sigs e in
  let pre_env', acc =
    List.fold_left
      (fun (cc_cache,acc) (i,m) ->
         match connected_components_sum_of_ambiguous_mixture
                 ~compileModeOn:false contact_map
                 cc_cache (LKappa_compiler.of_user_graph sigs m) with
         | cc_cache',[[|_,x|],_] ->
           cc_cache',Tools.recti (fun a _ -> x::a) acc i
         | _ -> assert false)
      (pre_env,[]) snap
  in
   (pre_env', acc)
