(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* TODO originally_from term/lKappa.ml, see if it makes sense here *)
let raise_if_modification_agent (pos : Loc.t) = function
  | Ast.NoMod -> ()
  | Ast.Erase | Ast.Create ->
    raise
      (ExceptionDefn.Malformed_Decl ("A modification is forbidden here.", pos))

let build_l_type sigs pos dst_ty dst_p switch =
  let ty_id = Signature.num_of_agent dst_ty sigs in
  let p_id = Signature.id_of_site dst_ty dst_p sigs in
  (LKappa.LNK_TYPE (p_id, ty_id), pos), switch

let add_link_contact_map ?contact_map sty sp dty dp =
  match contact_map with
  | None -> ()
  | Some contact_map ->
    let si, sl = contact_map.(sty).(sp) in
    let di, dl = contact_map.(dty).(dp) in
    let () = contact_map.(sty).(sp) <- si, Mods.Int2Set.add (dty, dp) sl in
    contact_map.(dty).(dp) <- di, Mods.Int2Set.add (sty, sp) dl

let rule_induces_link_permutation ~warning ~pos ?dst_ty sigs sort site =
  let warning_for_counters =
    if Signature.is_counter_agent sigs sort then
      true
    else (
      match dst_ty with
      | None -> false
      | Some s -> Signature.is_counter_agent sigs s
    )
  in

  if not warning_for_counters then
    warning ~pos (fun f ->
        Format.fprintf f
          "rule induces a link permutation on site '%a' of agent '%a'"
          (Signature.print_site sigs sort)
          site
          (Signature.print_agent sigs)
          sort)

let site_should_made_be_free i sigs ag_ty p_id pos =
  LKappa.raise_link_should_be_removed i
    (let () =
       Format.fprintf Format.str_formatter "%a"
         (Signature.print_agent sigs)
         ag_ty
     in
     Format.flush_str_formatter ())
    ( (let () =
         Format.fprintf Format.str_formatter "%a"
           (Signature.print_site sigs ag_ty)
           p_id
       in
       Format.flush_str_formatter ()),
      pos )

let build_link ?warn_on_swap sigs ?contact_map pos i ag_ty p_id switch
    (links_one, links_two) =
  if Mods.IntMap.mem i links_two then
    raise
      (ExceptionDefn.Malformed_Decl
         ( "This is the third occurence of link '" ^ string_of_int i
           ^ "' in the same mixture.",
           pos ))
  else (
    match Mods.IntMap.pop i links_one with
    | None, one' ->
      let new_link =
        match switch with
        | LKappa.Linked j -> Some j
        | LKappa.Freed | LKappa.Erased | LKappa.Maintained -> None
      in
      ( ((LKappa.LNK_VALUE (i, (-1, -1)), pos), switch),
        (Mods.IntMap.add i (ag_ty, p_id, new_link, pos, switch) one', links_two)
      )
    | Some (dst_ty, dst_p, dst_id, pos', switch'), one' ->
      if Signature.allowed_link ag_ty p_id dst_ty dst_p sigs then (
        let () = add_link_contact_map ?contact_map ag_ty p_id dst_ty dst_p in
        let maintained =
          match switch with
          | LKappa.Linked j ->
            let link_swap = Some j <> dst_id in
            let () =
              if link_swap then (
                match warn_on_swap with
                | None -> ()
                | Some warning ->
                  rule_induces_link_permutation ~warning ~pos ~dst_ty sigs ag_ty
                    p_id
              )
            in
            not link_swap
          | LKappa.Freed | LKappa.Erased | LKappa.Maintained -> false
        in
        let _check_compatibilty =
          match switch, switch' with
          | LKappa.Maintained, LKappa.Maintained -> ()
          | LKappa.Maintained, (LKappa.Freed | LKappa.Erased | LKappa.Linked _)
            ->
            site_should_made_be_free i sigs ag_ty p_id pos
          | (LKappa.Freed | LKappa.Erased | LKappa.Linked _), LKappa.Maintained
            ->
            site_should_made_be_free i sigs dst_ty dst_p pos'
          | ( (LKappa.Freed | LKappa.Erased | LKappa.Linked _),
              (LKappa.Freed | LKappa.Erased | LKappa.Linked _) ) ->
            ()
        in
        ( ( (LKappa.LNK_VALUE (i, (dst_p, dst_ty)), pos),
            if maintained then
              LKappa.Maintained
            else
              switch ),
          (one', Mods.IntMap.add i (ag_ty, p_id, maintained) links_two) )
      ) else
        raise
          (ExceptionDefn.Malformed_Decl
             ( Format.asprintf
                 "Forbidden link to a %a.%a from signature declaration"
                 (Signature.print_site sigs dst_ty)
                 dst_p
                 (Signature.print_agent sigs)
                 dst_ty,
               pos ))
  )

let annotate_dropped_agent ~warning ~syntax_version ~r_edit_style sigs
    links_annot ((agent_name, _) as ag_ty) intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Loc.annot_with_dummy LKappa.LNK_ANY, LKappa.Erased)
  in
  let internals =
    Array.init arity (fun i ->
        match Signature.default_internal_state ag_id i sigs with
        | None -> LKappa.I_ANY
        | Some _ -> LKappa.I_ANY_ERASED)
  in
  let lannot, _ =
    List.fold_left
      (fun (lannot, pset) p ->
        let ((_, p_pos) as port_name) = p.Ast.port_name in
        let p_id = Signature.num_of_site ~agent_name port_name sign in
        let () =
          match Signature.counter_of_site_id p_id sign with
          | Some _ -> LKappa.raise_counter_misused agent_name p.Ast.port_name
          | None -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.raise_several_occurence_of_site agent_name p.Ast.port_name
        in
        let () =
          match p.Ast.port_link_mod, p.Ast.port_link with
          | None, _ -> ()
          | Some None, [ (LKappa.LNK_VALUE (_, ()), _) ]
          (* [i/.] is allowed in degraded agent.
             It will be checked later that the other site with link id i is also freed in the rule *)
          (* Please note that a rule written as A(x[1])-,B(x[1/.])- is allowed *)
            ->
            ()
          | ( Some (None | Some _),
              ( []
              | [ (LKappa.LNK_VALUE (_, ()), _) ]
              | [ (LKappa.ANY_FREE, _) ]
              | [ (LKappa.LNK_FREE, _) ]
              | [ (LKappa.LNK_ANY, _) ]
              | [ (LKappa.LNK_SOME, _) ]
              | [ (LKappa.LNK_TYPE (_, _), _) ]
              | _ :: _ :: _ ) ) ->
            LKappa.raise_if_modification p_pos p.Ast.port_link_mod
        in
        let () = LKappa.raise_if_modification p_pos p.Ast.port_int_mod in

        let () =
          match p.Ast.port_int with
          | [] | [ (None, _) ] -> ()
          | [ (Some va, pos) ] ->
            internals.(p_id) <-
              LKappa.I_VAL_ERASED
                (Signature.num_of_internal_state p_id (va, pos) sign)
          | _ :: (_, pos) :: _ -> LKappa.raise_several_internal_states pos
        in
        match p.Ast.port_link with
        | [ (LKappa.LNK_ANY, pos) ] ->
          let () = ports.(p_id) <- (LKappa.ANY_FREE, pos), LKappa.Erased in
          lannot, pset'
        | [ (LKappa.LNK_SOME, pos_link) ] ->
          let na, pos = p.Ast.port_name in
          let () =
            warning ~pos (fun f ->
                Format.fprintf f
                  "breaking a semi-link on site '%s' will induce a side effect"
                  na)
          in
          let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_link), LKappa.Erased in
          lannot, pset'
        | [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_link) ] ->
          let na, pos = p.Ast.port_name in
          let () =
            warning ~pos (fun f ->
                Format.fprintf f
                  "breaking a semi-link on site '%s' will induce a side effect"
                  na)
          in
          let () =
            ports.(p_id) <-
              build_l_type sigs pos_link dst_ty dst_p LKappa.Erased
          in
          lannot, pset'
        | ([ (LKappa.ANY_FREE, _) ] | []) when syntax_version = Ast.V3 ->
          let () =
            ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Erased
          in
          lannot, pset'
        | [ (LKappa.ANY_FREE, _) ] | [] ->
          let () =
            ports.(p_id) <- Loc.annot_with_dummy LKappa.ANY_FREE, LKappa.Erased
          in
          lannot, pset'
        | [ (LKappa.LNK_FREE, _) ] ->
          let () =
            ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Erased
          in
          lannot, pset'
        | [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
          let va, lannot' =
            let warn_on_swap =
              if r_edit_style then
                None
              else
                Some warning
            in
            build_link ?warn_on_swap sigs pos i ag_id p_id LKappa.Erased lannot
          in
          let () = ports.(p_id) <- va in
          lannot', pset'
        | _ :: (_, pos) :: _ ->
          raise
            (ExceptionDefn.Malformed_Decl
               ("Several link state for a single site", pos)))
      (links_annot, Mods.IntSet.empty)
      intf
  in
  let ra =
    {
      LKappa.ra_type = ag_id;
      ra_ports = ports;
      ra_ints = internals;
      ra_erased = true;
      ra_syntax = Some (Array.copy ports, Array.copy internals);
    }
  in
  ( Counters_compiler.annotate_dropped_counters sign counts ra arity agent_name
      None,
    lannot )

let annotate_created_agent ~warning ~syntax_version ~r_edit_style sigs
    ?contact_map rannot ((agent_name, _) as ag_ty) intf =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity Raw_mixture.FREE in
  let internals =
    Array.init arity (fun i -> Signature.default_internal_state ag_id i sigs)
  in
  let _, rannot =
    List.fold_left
      (fun (pset, rannot) p ->
        let ((_, p_pos) as port_name) = p.Ast.port_name in
        let p_id = Signature.num_of_site ~agent_name port_name sign in
        let () =
          match Signature.counter_of_site_id p_id sign with
          | Some _ -> LKappa.raise_counter_misused agent_name p.Ast.port_name
          | None -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.raise_several_occurence_of_site agent_name p.Ast.port_name
        in
        let () = LKappa.raise_if_modification p_pos p.Ast.port_link_mod in
        let () = LKappa.raise_if_modification p_pos p.Ast.port_int_mod in
        let () =
          match p.Ast.port_int with
          | [] -> ()
          | [ (None, _) ] ->
            LKappa.raise_not_enough_specified ~status:"internal" ~side:"left"
              agent_name port_name
          | [ (Some va, pos) ] ->
            internals.(p_id) <-
              Some (Signature.num_of_internal_state p_id (va, pos) sign)
          | _ :: (_, pos) :: _ -> LKappa.raise_several_internal_states pos
        in
        match p.Ast.port_link with
        | [ (LKappa.LNK_ANY, _) ]
        | [ (LKappa.LNK_SOME, _) ]
        | [ (LKappa.LNK_TYPE _, _) ]
        | _ :: _ :: _ ->
          LKappa.raise_not_enough_specified ~status:"linking" ~side:"left"
            agent_name port_name
        | [ (LKappa.ANY_FREE, _) ] when syntax_version = Ast.V4 ->
          LKappa.raise_not_enough_specified ~status:"linking" ~side:"left"
            agent_name port_name
        | [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
          let () = ports.(p_id) <- Raw_mixture.VAL i in
          let _, rannot' =
            let warn_on_swap =
              if r_edit_style then
                None
              else
                Some warning
            in
            build_link ?warn_on_swap sigs ?contact_map pos i ag_id p_id
              LKappa.Freed rannot
          in
          pset', rannot'
        | [ ((LKappa.ANY_FREE | LKappa.LNK_FREE), _) ] | [] -> pset', rannot)
      (Mods.IntSet.empty, rannot)
      intf
  in
  ( rannot,
    {
      Raw_mixture.a_type = ag_id;
      Raw_mixture.a_ports = ports;
      Raw_mixture.a_ints = internals;
    } )

let translate_modification ~warning sigs ?contact_map ag_id p_id ?warn
    ((lhs_links, rhs_links) as links_annot) = function
  | None -> LKappa.Maintained, links_annot
  | Some x ->
    let () =
      match warn with
      | None -> ()
      | Some (na, pos) ->
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
    in
    (match x with
    | None -> LKappa.Freed, links_annot
    | Some (j, pos_j) ->
      let _, rhs_links' =
        build_link ?warn_on_swap:None sigs ?contact_map pos_j j ag_id p_id
          LKappa.Freed rhs_links
      in
      LKappa.Linked j, (lhs_links, rhs_links'))

let annotate_edit_agent ~warning ~syntax_version ~is_rule sigs ?contact_map
    ((agent_name, _) as ag_ty) links_annot intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Loc.annot_with_dummy LKappa.LNK_ANY, LKappa.Maintained)
  in
  let internals = Array.make arity LKappa.I_ANY in
  let scan_port (links_annot, pset) p =
    let port_name, _ = p.Ast.port_name in
    let p_id = Signature.num_of_site ~agent_name p.Ast.port_name sign in
    let () =
      match Signature.counter_of_site_id p_id sign with
      | Some _ -> LKappa.raise_counter_misused agent_name p.Ast.port_name
      | None -> ()
    in
    let pset' = Mods.IntSet.add p_id pset in
    let () =
      if pset == pset' then
        LKappa.raise_several_occurence_of_site agent_name p.Ast.port_name
    in
    let links_annot' =
      match p.Ast.port_link with
      | [ ((LKappa.LNK_SOME, pos) as x) ] ->
        let modif, links_annot' =
          translate_modification ~warning ~warn:(port_name, pos) sigs
            ?contact_map ag_id p_id links_annot p.Ast.port_link_mod
        in
        let () = ports.(p_id) <- x, modif in
        links_annot'
      | [ (LKappa.LNK_ANY, pos) ] ->
        let modif, links_annot' =
          translate_modification ~warning ~warn:(port_name, pos) sigs
            ?contact_map ag_id p_id links_annot p.Ast.port_link_mod
        in
        let () = ports.(p_id) <- (LKappa.ANY_FREE, pos), modif in
        links_annot'
      | ([] | [ (LKappa.ANY_FREE, _) ]) when syntax_version = Ast.V3 ->
        let modif, links_annot' =
          translate_modification ~warning ?warn:None sigs ?contact_map ag_id
            p_id links_annot p.Ast.port_link_mod
        in
        let () = ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, modif in
        links_annot'
      | [] when p.Ast.port_link_mod = None -> links_annot
      | [ (LKappa.ANY_FREE, _) ] | [] ->
        LKappa.raise_not_enough_specified ~status:"linking" ~side:"left"
          agent_name p.Ast.port_name
      | [ (LKappa.LNK_FREE, _) ] ->
        let modif, links_annot' =
          translate_modification ~warning ?warn:None sigs ?contact_map ag_id
            p_id links_annot p.Ast.port_link_mod
        in
        let () = ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, modif in
        links_annot'
      | [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos) ] ->
        let modif, links_annot' =
          translate_modification ~warning ~warn:(port_name, pos) sigs
            ?contact_map ag_id p_id links_annot p.Ast.port_link_mod
        in
        let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p modif in
        links_annot'
      | [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
        let modif, (lhs_links, rhs_links) =
          translate_modification ~warning ?warn:None sigs ?contact_map ag_id
            p_id links_annot p.Ast.port_link_mod
        in
        let va, lhs_links' =
          build_link sigs
            ?contact_map:
              (if is_rule then
                 None
               else
                 contact_map)
            ?warn_on_swap:None pos i ag_id p_id modif lhs_links
        in
        let () = ports.(p_id) <- va in
        lhs_links', rhs_links
      | _ :: (_, pos) :: _ ->
        raise
          (ExceptionDefn.Malformed_Decl
             ("Several link state for a single site", pos))
    in
    let () =
      match p.Ast.port_int, p.Ast.port_int_mod with
      | ([ (None, _) ] | []), None -> ()
      | [ (Some va, pos) ], Some va' ->
        internals.(p_id) <-
          LKappa.I_VAL_CHANGED
            ( Signature.num_of_internal_state p_id (va, pos) sign,
              Signature.num_of_internal_state p_id va' sign )
      | [], Some ((_, pos) as va) ->
        let () =
          if syntax_version = Ast.V3 then
            warning ~pos (fun f ->
                Format.fprintf f
                  "internal state of site '%s' of agent '%s' is modified \
                   although it is left unpecified in the left hand side"
                  port_name agent_name)
          else
            raise
              (ExceptionDefn.Malformed_Decl
                 ("Modified internal state must appear on the left", pos))
        in
        internals.(p_id) <-
          LKappa.I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
      | [ (None, _) ], Some va ->
        internals.(p_id) <-
          LKappa.I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
      | [ (Some va, pos) ], None ->
        let i_id = Signature.num_of_internal_state p_id (va, pos) sign in
        internals.(p_id) <- LKappa.I_VAL_CHANGED (i_id, i_id)
      | _ :: (_, pos) :: _, _ -> LKappa.raise_several_internal_states pos
    in
    links_annot', pset'
  in
  let annoted', _ =
    List.fold_left scan_port (links_annot, Mods.IntSet.empty) intf
  in
  let ra =
    {
      LKappa.ra_type = ag_id;
      ra_ports = ports;
      ra_ints = internals;
      ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);
    }
  in
  ( Counters_compiler.annotate_edit_counters sigs ag_ty counts ra
      (add_link_contact_map ?contact_map),
    annoted' )

let annotate_agent_with_diff ~warning ~syntax_version sigs ?contact_map
    ((agent_name, _) as ag_ty) links_annot lp rp lc rc =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Loc.annot_with_dummy LKappa.LNK_ANY, LKappa.Maintained)
  in
  let internals = Array.make arity LKappa.I_ANY in
  let register_port_modif p_id lnk1 p' ((lhs_links, rhs_links) as links_annot) =
    let () =
      LKappa.raise_if_modification
        (Loc.get_annot p'.Ast.port_name)
        p'.Ast.port_link_mod
    in
    match lnk1, p'.Ast.port_link with
    | [ (LKappa.LNK_ANY, pos) ], [ (LKappa.LNK_ANY, _) ] ->
      let () = ports.(p_id) <- (LKappa.ANY_FREE, pos), LKappa.Maintained in
      links_annot
    | [ (LKappa.LNK_SOME, pos) ], [ (LKappa.LNK_SOME, _) ] ->
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos), LKappa.Maintained in
      links_annot
    | ( [
          ( LKappa.LNK_TYPE (((dst_p'', _) as dst_p), ((dst_ty'', _) as dst_ty)),
            pos );
        ],
        [ (LKappa.LNK_TYPE ((dst_p', _), (dst_ty', _)), _) ] )
      when dst_p'' = dst_p' && dst_ty'' = dst_ty' ->
      let () =
        ports.(p_id) <- build_l_type sigs pos dst_ty dst_p LKappa.Maintained
      in
      links_annot
    | ( _,
        ( [ (LKappa.LNK_ANY, _) ]
        | [ (LKappa.LNK_SOME, _) ]
        | [ (LKappa.LNK_TYPE _, _) ] ) ) ->
      LKappa.raise_not_enough_specified ~status:"linking" ~side:"right"
        agent_name p'.Ast.port_name
    | [ (LKappa.LNK_ANY, pos) ], [] when syntax_version = Ast.V3 ->
      let () = ports.(p_id) <- (LKappa.LNK_ANY, pos), LKappa.Freed in
      links_annot
    | [ (LKappa.LNK_ANY, pos) ], [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ->
      let () = ports.(p_id) <- (LKappa.LNK_ANY, pos), LKappa.Freed in
      links_annot
    | ( [ (LKappa.LNK_SOME, pos_link) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_link), LKappa.Freed in
      links_annot
    | ( [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_link) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () =
        ports.(p_id) <- build_l_type sigs pos_link dst_ty dst_p LKappa.Freed
      in
      links_annot
    | [ (LKappa.LNK_SOME, pos_link) ], [] when syntax_version = Ast.V3 ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_link), LKappa.Freed in
      links_annot
    | [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_link) ], []
      when syntax_version = Ast.V3 ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () =
        ports.(p_id) <- build_l_type sigs pos_link dst_ty dst_p LKappa.Freed
      in
      links_annot
    | ( ([ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] | []),
        ([ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] | []) )
      when syntax_version = Ast.V3 ->
      let () =
        ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Maintained
      in
      links_annot
    | ( [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let () =
        ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Maintained
      in
      links_annot
    | [], [] ->
      let () =
        ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_ANY, LKappa.Maintained
      in
      links_annot
    | ( [ (LKappa.LNK_VALUE (i, ()), pos) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let va, lhs_links' =
        build_link sigs ~warn_on_swap:warning pos i ag_id p_id LKappa.Freed
          lhs_links
      in
      let () = ports.(p_id) <- va in
      lhs_links', rhs_links
    | [ (LKappa.LNK_VALUE (i, ()), pos) ], [] when syntax_version = Ast.V3 ->
      let va, lhs_links' =
        build_link sigs ~warn_on_swap:warning pos i ag_id p_id LKappa.Freed
          lhs_links
      in
      let () = ports.(p_id) <- va in
      lhs_links', rhs_links
    | [ (LKappa.LNK_ANY, pos_link) ], [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
      let () = ports.(p_id) <- (LKappa.LNK_ANY, pos_link), LKappa.Linked i in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | [ (LKappa.LNK_SOME, pos_link) ], [ (LKappa.LNK_VALUE (i, ()), pos') ] ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_link), LKappa.Linked i in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos' i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | ( [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_link) ],
        [ (LKappa.LNK_VALUE (i, ()), pos') ] ) ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () =
        ports.(p_id) <-
          build_l_type sigs pos_link dst_ty dst_p (LKappa.Linked i)
      in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos' i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | ( [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ],
        [ (LKappa.LNK_VALUE (i, ()), pos) ] ) ->
      let () =
        ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Linked i
      in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | [], [ (LKappa.LNK_VALUE (i, ()), pos) ] when syntax_version = Ast.V3 ->
      let () =
        ports.(p_id) <- Loc.annot_with_dummy LKappa.LNK_FREE, LKappa.Linked i
      in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | ( [ (LKappa.LNK_VALUE (i, ()), pos_i) ],
        [ (LKappa.LNK_VALUE (j, ()), pos_j) ] ) ->
      let va, lhs_links' =
        build_link sigs ~warn_on_swap:warning pos_i i ag_id p_id
          (LKappa.Linked j) lhs_links
      in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos_j j ag_id p_id
          LKappa.Freed rhs_links
      in
      let () = ports.(p_id) <- va in
      lhs_links', rhs_links'
    | ( [
          ( ( LKappa.LNK_VALUE (_, ())
            | LKappa.LNK_FREE | LKappa.ANY_FREE
            | LKappa.LNK_TYPE (_, _)
            | LKappa.LNK_SOME | LKappa.LNK_ANY ),
            _ );
        ],
        [] ) ->
      LKappa.raise_not_enough_specified ~status:"linking" ~side:"right"
        agent_name p'.Ast.port_name
    | [], [ ((LKappa.ANY_FREE | LKappa.LNK_FREE | LKappa.LNK_VALUE (_, _)), _) ]
      ->
      LKappa.raise_not_enough_specified ~status:"linking" ~side:"left"
        agent_name p'.Ast.port_name
    | _ :: (_, pos) :: _, _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Several link state for a single site", pos))
    | _, _ :: (_, pos) :: _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Several link state for a single site", pos))
  in
  let register_internal_modif p_id int1 p' =
    let () =
      LKappa.raise_if_modification
        (Loc.get_annot p'.Ast.port_name)
        p'.Ast.port_int_mod
    in
    match int1, p'.Ast.port_int with
    | [], [] | [ (None, _) ], [ (None, _) ] -> ()
    | [ (Some va, pos) ], [ (Some va', pos') ] ->
      internals.(p_id) <-
        LKappa.I_VAL_CHANGED
          ( Signature.num_of_internal_state p_id (va, pos) sign,
            Signature.num_of_internal_state p_id (va', pos') sign )
    | [], [ (Some va, vapos) ] when syntax_version = Ast.V3 ->
      let na, pos = p'.Ast.port_name in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "internal state of site '%s' of agent '%s' is modified although \
               it is left unpecified in the left hand side"
              na agent_name)
      in
      internals.(p_id) <-
        LKappa.I_ANY_CHANGED
          (Signature.num_of_internal_state p_id (va, vapos) sign)
    | [ (None, _) ], [ (Some va, vapos) ] ->
      internals.(p_id) <-
        LKappa.I_ANY_CHANGED
          (Signature.num_of_internal_state p_id (va, vapos) sign)
    | [], [ _ ] ->
      LKappa.raise_not_enough_specified ~status:"internal" ~side:"left"
        agent_name p'.Ast.port_name
    | [ _ ], ([ (None, _) ] | []) ->
      LKappa.raise_not_enough_specified ~status:"internal" ~side:"right"
        agent_name p'.Ast.port_name
    | _ :: (_, pos) :: _, _ | _, _ :: (_, pos) :: _ ->
      LKappa.raise_several_internal_states pos
  in
  let find_in_r (na, pos) rp =
    let p', r =
      List.partition (fun p -> String.compare (Loc.v p.Ast.port_name) na = 0) rp
    in
    match p' with
    | [ p' ] -> p', r
    | [] ->
      LKappa.raise_not_enough_specified ~status:"linking" ~side:"right"
        agent_name (na, pos)
    | _ :: _ -> LKappa.raise_several_occurence_of_site agent_name (na, pos)
  in
  let rp_r, annoted, _ =
    List.fold_left
      (fun (rp, annoted, pset) p ->
        let ((_, p_pos) as port_name) = p.Ast.port_name in
        let p_id = Signature.num_of_site ~agent_name port_name sign in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.raise_several_occurence_of_site agent_name p.Ast.port_name
        in
        let () = LKappa.raise_if_modification p_pos p.Ast.port_link_mod in
        let () = LKappa.raise_if_modification p_pos p.Ast.port_int_mod in

        let p', rp' = find_in_r port_name rp in
        let annoted' = register_port_modif p_id p.Ast.port_link p' annoted in
        let () = register_internal_modif p_id p.Ast.port_int p' in
        rp', annoted', pset')
      (rp, links_annot, Mods.IntSet.empty)
      lp
  in
  let annoted' =
    List.fold_left
      (fun annoted p ->
        let port_name = p.Ast.port_name in
        let p_id = Signature.num_of_site ~agent_name port_name sign in
        let () = register_internal_modif p_id [] p in
        register_port_modif p_id
          [ Loc.annot_with_dummy LKappa.LNK_ANY ]
          p annoted)
      annoted rp_r
  in

  let ra =
    {
      LKappa.ra_type = ag_id;
      ra_ports = ports;
      ra_ints = internals;
      ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);
    }
  in
  ( Counters_compiler.annotate_counters_with_diff sigs ag_ty lc rc ra
      (add_link_contact_map ?contact_map),
    annoted' )

let refer_links_annot ?warning sigs links_annot mix =
  List.iter
    (fun (ra_ : LKappa.rule_agent Counters_compiler.with_agent_counters) ->
      let ra = ra_.agent in
      Array.iteri
        (fun i -> function
          | (LKappa.LNK_VALUE (j, (-1, -1)), pos), mods ->
            (match Mods.IntMap.find_option j links_annot with
            | None -> ()
            | Some (dst_ty, dst_p, maintained) ->
              let mods' =
                if maintained then
                  LKappa.Maintained
                else
                  mods
              in
              let () =
                match mods' with
                | LKappa.Erased | LKappa.Freed | LKappa.Maintained -> ()
                | LKappa.Linked _ ->
                  (match warning with
                  | None -> ()
                  | Some warning ->
                    rule_induces_link_permutation ~warning ~pos ~dst_ty sigs
                      ra.LKappa.ra_type i)
              in
              ra.LKappa.ra_ports.(i) <-
                (LKappa.LNK_VALUE (j, (dst_p, dst_ty)), pos), mods')
          | ( ( ( LKappa.LNK_VALUE _ | LKappa.LNK_ANY | LKappa.LNK_SOME
                | LKappa.LNK_TYPE _ | LKappa.LNK_FREE | LKappa.ANY_FREE ),
                _ ),
              _ ) ->
            ())
        ra.LKappa.ra_ports)
    mix

let separate_sites ls =
  let a, b =
    List.fold_left
      (fun (ps, cs) -> function
        | Ast.Port p -> p :: ps, cs
        | Ast.Counter c -> ps, c :: cs)
      ([], []) ls
  in
  List.rev a, b

let final_rule_sanity ?warning sigs
    ((lhs_links_one, lhs_links_two), (rhs_links_one, _)) mix =
  let () =
    match Mods.IntMap.root lhs_links_one with
    | None -> ()
    | Some (i, (_, _, _, pos, _)) -> LKappa.raise_link_only_one_occurence i pos
  in
  let () = refer_links_annot ?warning sigs lhs_links_two mix in
  match Mods.IntMap.root rhs_links_one with
  | None -> ()
  | Some (i, (_, _, _, pos, _)) -> LKappa.raise_link_only_one_occurence i pos

(*
Is responsible for the check that:
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice
*)
let annotate_lhs_with_diff_v3 ~warning sigs ?contact_map lhs rhs =
  let syntax_version = Ast.V3 in
  let rec aux links_annot acc lhs rhs =
    match lhs, rhs with
    | Ast.Absent pos :: _, _ | (Ast.Present _ :: _ | []), Ast.Absent pos :: _ ->
      raise
        (ExceptionDefn.Malformed_Decl ("Absent agent are KaSim > 3 syntax", pos))
    | ( Ast.Present (((lagent_name, lpos) as ag_ty), lag_s, lmod) :: lt,
        Ast.Present ((ragent_name, rpos), rag_s, rmod) :: rt )
      when String.compare lagent_name ragent_name = 0
           && Ast.no_more_site_on_right true lag_s rag_s ->
      raise_if_modification_agent lpos lmod;
      raise_if_modification_agent rpos rmod;
      let lag_p, lag_c = separate_sites lag_s in
      let rag_p, rag_c = separate_sites rag_s in
      let ra, links_annot' =
        annotate_agent_with_diff ~warning ~syntax_version sigs ?contact_map
          ag_ty links_annot lag_p rag_p lag_c rag_c
      in
      aux links_annot' (ra :: acc) lt rt
    | ((Ast.Present _ :: _ | []) as erased), added ->
      let () =
        if added <> [] then
          List.iter
            (function
              | Ast.Absent _ -> ()
              | Ast.Present ((lag, pos), lag_p, _) ->
                if
                  List.exists
                    (function
                      | Ast.Absent _ -> false
                      | Ast.Present ((rag, _), rag_p, _) ->
                        String.compare lag rag = 0
                        && Ast.no_more_site_on_right false lag_p rag_p)
                    added
                then
                  warning ~pos (fun f ->
                      Format.fprintf f
                        "Rule induced deletion AND creation of the agent %s" lag))
            erased
      in
      let mix, llinks =
        List.fold_left
          (fun (acc, lannot) -> function
            | Ast.Absent pos ->
              raise
                (ExceptionDefn.Malformed_Decl
                   ("Absent agent are KaSim > 3 syntax", pos))
            | Ast.Present (((_, pos) as na), sites, modif) ->
              raise_if_modification_agent pos modif;
              let intf, counts = separate_sites sites in
              let ra, lannot' =
                annotate_dropped_agent ~warning ~syntax_version
                  ~r_edit_style:false sigs lannot na intf counts
              in
              ra :: acc, lannot')
          (acc, fst links_annot)
          erased
      in
      let cmix, rlinks =
        List.fold_left
          (fun (acc, rannot) -> function
            | Ast.Absent pos ->
              raise
                (ExceptionDefn.Malformed_Decl
                   ("Absent agent are KaSim > 3 syntax", pos))
            | Ast.Present (((_, pos) as na), sites, modif) ->
              raise_if_modification_agent pos modif;
              let intf, counts = separate_sites sites in
              let rannot', x' =
                annotate_created_agent ~warning ~syntax_version
                  ~r_edit_style:false sigs ?contact_map rannot na intf
              in
              let x'' =
                Counters_compiler.annotate_created_counters sigs na counts
                  (add_link_contact_map ?contact_map)
                  x'
              in
              x'' :: acc, rannot')
          ([], snd links_annot)
          added
      in
      let () = final_rule_sanity ~warning sigs (llinks, rlinks) mix in
      List.rev mix, List.rev cmix
  in
  aux
    ( (Mods.IntMap.empty, Mods.IntMap.empty),
      (Mods.IntMap.empty, Mods.IntMap.empty) )
    [] (List.flatten lhs) (List.flatten rhs)

let annotate_lhs_with_diff_v4 ~warning sigs ?contact_map lhs rhs =
  let syntax_version = Ast.V4 in
  let rec aux links_annot mix cmix lhs rhs =
    match lhs, rhs with
    | [], [] -> links_annot, mix, cmix
    | Ast.Absent _ :: lt, Ast.Absent _ :: rt -> aux links_annot mix cmix lt rt
    | Ast.Present (((_, pos) as ty), sites, lmod) :: lt, Ast.Absent _ :: rt ->
      raise_if_modification_agent pos lmod;
      let intf, counts = separate_sites sites in
      let ra, lannot' =
        annotate_dropped_agent ~warning ~syntax_version ~r_edit_style:false sigs
          (fst links_annot) ty intf counts
      in
      aux (lannot', snd links_annot) (ra :: mix) cmix lt rt
    | Ast.Absent _ :: lt, Ast.Present (((_, pos) as ty), sites, rmod) :: rt ->
      raise_if_modification_agent pos rmod;
      let intf, counts = separate_sites sites in
      let rannot', x' =
        annotate_created_agent ~warning ~syntax_version ~r_edit_style:false sigs
          ?contact_map (snd links_annot) ty intf
      in
      let x'' =
        Counters_compiler.annotate_created_counters sigs ty counts
          (add_link_contact_map ?contact_map)
          x'
      in
      aux (fst links_annot, rannot') mix (x'' :: cmix) lt rt
    | ( Ast.Present (((lagent_name, lpos) as ag_ty), lag_s, lmod) :: lt,
        Ast.Present ((ragent_name, rpos), rag_s, rmod) :: rt ) ->
      if
        String.compare lagent_name ragent_name = 0
        && Ast.no_more_site_on_right true lag_s rag_s
      then (
        raise_if_modification_agent lpos lmod;
        raise_if_modification_agent rpos rmod;
        let lag_p, lag_c = separate_sites lag_s in
        let rag_p, rag_c = separate_sites rag_s in
        let ra, links_annot' =
          annotate_agent_with_diff ~warning ~syntax_version sigs ?contact_map
            ag_ty links_annot lag_p rag_p lag_c rag_c
        in
        aux links_annot' (ra :: mix) cmix lt rt
      ) else
        raise
          (ExceptionDefn.Malformed_Decl
             ("Left hand side/right hand side agent mismatch", rpos))
    | (Ast.Present ((_, pos), _, _) | Ast.Absent pos) :: _, []
    | [], (Ast.Present ((_, pos), _, _) | Ast.Absent pos) :: _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Left hand side/right hand side agent mismatch", pos))
  in
  let rec aux_line links_annot mix cmix lhs rhs =
    match lhs, rhs with
    | [], [] ->
      let () = final_rule_sanity ~warning sigs links_annot mix in
      List.rev mix, List.rev cmix
    | hl :: tl, hr :: tr ->
      let links_annot', mix', cmix' = aux links_annot mix cmix hl hr in
      aux_line links_annot' mix' cmix' tl tr
    | ((Ast.Present ((_, pos), _, _) | Ast.Absent pos) :: _) :: _, []
    | [], ((Ast.Present ((_, pos), _, _) | Ast.Absent pos) :: _) :: _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Left hand side/right hand side agent mismatch", pos))
    | [] :: _, [] | [], [] :: _ ->
      raise
        (ExceptionDefn.Internal_Error
           (Loc.annot_with_dummy "Invariant violation in annotate_lhs_with..."))
  in
  aux_line
    ( (Mods.IntMap.empty, Mods.IntMap.empty),
      (Mods.IntMap.empty, Mods.IntMap.empty) )
    [] [] lhs rhs

let annotate_lhs_with_diff ~warning ~syntax_version sigs ?contact_map lhs rhs =
  match syntax_version with
  | Ast.V3 -> annotate_lhs_with_diff_v3 ~warning sigs ?contact_map lhs rhs
  | Ast.V4 -> annotate_lhs_with_diff_v4 ~warning sigs ?contact_map lhs rhs

let annotate_edit_mixture ~warning ~syntax_version ~is_rule sigs ?contact_map
    (m : Ast.mixture) :
    Counters_compiler.rule_mixture_with_agent_counters
    * Counters_compiler.raw_mixture_with_agent_counters =
  (* mix is the mixture from initial state of the rule, cmix is the mixture after the rule was applied *)
  let links_annot, mix, cmix =
    List.fold_left
      (List.fold_left (fun (lannot, acc, news) -> function
         | Ast.Absent _ -> lannot, acc, news
         | Ast.Present (ty, sites, modif) ->
           let intf, counts = separate_sites sites in
           (match modif with
           | Ast.NoMod ->
             let a, lannot' =
               annotate_edit_agent ~warning ~syntax_version ~is_rule sigs
                 ?contact_map ty lannot intf counts
             in
             lannot', a :: acc, news
           | Ast.Create ->
             let rannot', x' =
               annotate_created_agent ~warning ~syntax_version
                 ~r_edit_style:true sigs ?contact_map (snd lannot) ty intf
             in
             let x'' =
               Counters_compiler.annotate_created_counters sigs ty counts
                 (add_link_contact_map ?contact_map)
                 x'
             in
             (fst lannot, rannot'), acc, x'' :: news
           | Ast.Erase ->
             let ra, lannot' =
               annotate_dropped_agent ~warning ~syntax_version
                 ~r_edit_style:true sigs (fst lannot) ty intf counts
             in
             (lannot', snd lannot), ra :: acc, news)))
      ( ( (Mods.IntMap.empty, Mods.IntMap.empty),
          (Mods.IntMap.empty, Mods.IntMap.empty) ),
        [],
        [] )
      m
  in
  final_rule_sanity ?warning:None sigs links_annot mix;
  List.rev mix, List.rev cmix

let annotate_created_mixture ~warning ~syntax_version sigs ?contact_map
    (m : Ast.mixture) :
    Raw_mixture.agent Counters_compiler.with_agent_counters list =
  let (rhs_links_one, _), cmix =
    List.fold_left
      (List.fold_left (fun (rannot, news) -> function
         | Ast.Absent pos ->
           raise
             (ExceptionDefn.Malformed_Decl
                ("Absent agent cannot occurs in created mixtures", pos))
         | Ast.Present (ty, sites, _modif) ->
           let intf, counts = separate_sites sites in
           let rannot', x' =
             annotate_created_agent ~warning ~syntax_version ~r_edit_style:true
               sigs ?contact_map rannot ty intf
           in
           let x'' =
             Counters_compiler.annotate_created_counters sigs ty counts
               (add_link_contact_map ?contact_map)
               x'
           in
           rannot', x'' :: news))
      ((Mods.IntMap.empty, Mods.IntMap.empty), [])
      m
  in
  let () =
    match Mods.IntMap.root rhs_links_one with
    | None -> ()
    | Some (i, (_, _, _, pos, _)) -> LKappa.raise_link_only_one_occurence i pos
  in
  List.rev cmix

let give_rule_label bidirectional (id, set) printer r = function
  | None -> (succ id, set), Format.asprintf "r%i: %a" id printer r
  | Some (lab, pos) ->
    let set' = Mods.StringSet.add lab set in
    if set == set' then
      raise
        (ExceptionDefn.Malformed_Decl
           ("A rule named '" ^ lab ^ "' already exists.", pos))
    else if bidirectional then (
      let set'' = Mods.StringSet.add (Ast.flip_label lab) set' in
      if set' == set'' then
        raise
          (ExceptionDefn.Malformed_Decl
             ("A rule named '" ^ Ast.flip_label lab ^ "' already exists.", pos))
      else
        (id, set''), lab
    ) else
      (id, set'), lab

let mixture_of_ast ~warning ~syntax_version sigs ?contact_map (pos : Loc.t)
    (mix : Ast.mixture) =
  match
    annotate_edit_mixture ~warning ~syntax_version ~is_rule:false sigs
      ?contact_map mix
  with
  | r, [] -> fst (Counters_compiler.compile_counter_in_rule sigs r [])
  | _, _ ->
    raise (ExceptionDefn.Internal_Error ("A mixture cannot create agents", pos))

let raw_mixture_of_ast ~warning ~syntax_version sigs ?contact_map
    (mix : Ast.mixture) =
  let b =
    annotate_created_mixture ~warning ~syntax_version sigs ?contact_map mix
  in
  snd (Counters_compiler.compile_counter_in_rule sigs [] b)

let convert_alg_var ?max_allowed_var algs lab pos =
  let i =
    match Mods.StringMap.find_option lab algs with
    | Some x -> x
    | None ->
      raise
        (ExceptionDefn.Malformed_Decl (lab ^ " is not a declared variable", pos))
  in
  let () =
    match max_allowed_var with
    | Some j when j < i ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Reference to not yet defined '" ^ lab ^ "' is forbidden.", pos))
    | None | Some _ -> ()
  in
  i

let convert_token_name tk_name tok pos =
  match Mods.StringMap.find_option tk_name tok with
  | Some x -> x
  | None ->
    raise
      (ExceptionDefn.Malformed_Decl (tk_name ^ " is not a declared token", pos))

let rec alg_expr_of_ast ~warning ~syntax_version sigs tok algs ?max_allowed_var
    (alg, pos) =
  ( (match alg with
    | Alg_expr.KAPPA_INSTANCE ast ->
      Alg_expr.KAPPA_INSTANCE
        (mixture_of_ast ~warning ~syntax_version sigs pos ast)
    | Alg_expr.ALG_VAR lab ->
      Alg_expr.ALG_VAR (convert_alg_var ?max_allowed_var algs lab pos)
    | Alg_expr.TOKEN_ID tk_name ->
      Alg_expr.TOKEN_ID (convert_token_name tk_name tok pos)
    | Alg_expr.DIFF_KAPPA_INSTANCE (expr, ast) ->
      Alg_expr.DIFF_KAPPA_INSTANCE
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var expr,
          mixture_of_ast ~warning ~syntax_version sigs pos ast )
    | Alg_expr.DIFF_TOKEN (expr, tk_name) ->
      Alg_expr.DIFF_TOKEN
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var expr,
          convert_token_name tk_name tok pos )
    | (Alg_expr.STATE_ALG_OP _ | Alg_expr.CONST _) as x -> x
    | Alg_expr.BIN_ALG_OP (op, a, b) ->
      Alg_expr.BIN_ALG_OP
        ( op,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var a,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var b )
    | Alg_expr.UN_ALG_OP (op, a) ->
      Alg_expr.UN_ALG_OP
        ( op,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var a )
    | Alg_expr.IF (cond, yes, no) ->
      Alg_expr.IF
        ( bool_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var cond,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var yes,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var no )),
    pos )

and bool_expr_of_ast ~warning ~syntax_version sigs tok algs ?max_allowed_var =
  function
  | ((Alg_expr.TRUE | Alg_expr.FALSE), _) as x -> x
  | Alg_expr.BIN_BOOL_OP (op, x, y), pos ->
    ( Alg_expr.BIN_BOOL_OP
        ( op,
          bool_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var x,
          bool_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var y ),
      pos )
  | Alg_expr.UN_BOOL_OP (op, x), pos ->
    ( Alg_expr.UN_BOOL_OP
        ( op,
          bool_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var x ),
      pos )
  | Alg_expr.COMPARE_OP (op, x, y), pos ->
    ( Alg_expr.COMPARE_OP
        ( op,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var x,
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var y ),
      pos )

let print_expr_of_ast ~warning ~syntax_version sigs tok algs = function
  | Primitives.Str_pexpr _ as x -> x
  | Primitives.Alg_pexpr x ->
    Primitives.Alg_pexpr
      (alg_expr_of_ast ~warning ~syntax_version sigs tok algs x)

type rule_inter_rep = {
  label_opt: (string * Loc.t) option;
  bidirectional: bool; (* TODO check *)
  mixture: LKappa.rule_agent Counters_compiler.with_agent_counters list;
  created_mix: Raw_mixture.agent Counters_compiler.with_agent_counters list;
  rm_token:
    (((Ast.mixture, string) Alg_expr.e * Loc.t) * (string * Loc.t)) list;
  add_token:
    (((Ast.mixture, string) Alg_expr.e * Loc.t) * (string * Loc.t)) list;
  k_def: (Ast.mixture, string) Alg_expr.e * Loc.t;
  k_un:
    (((Ast.mixture, string) Alg_expr.e * Loc.t)
    * ((Ast.mixture, string) Alg_expr.e * Loc.t) option)
    option;
  pos: Loc.t;
}
(** Intermediate representation for rule type *)

(** [assemble_rule] translates a rule_inter_rep into a LKappa.rule *)
let assemble_rule ~warning ~syntax_version (rule : rule_inter_rep)
    (sigs : Signature.s) (tok : int Mods.StringMap.t)
    (algs : int Mods.StringMap.t) : LKappa.rule =
  let (r_mix, r_created) : LKappa.rule_mixture * Raw_mixture.t =
    Counters_compiler.compile_counter_in_rule sigs rule.mixture rule.created_mix
  in
  let r_delta_tokens =
    List.rev_map
      (fun (al, (tk, pos)) ->
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            (Loc.annot_with_dummy (Alg_expr.UN_ALG_OP (Operator.UMINUS, al))),
          convert_token_name tk tok pos ))
      rule.rm_token
    |> List_util.rev_map_append
         (fun (al, (tk, pos)) ->
           ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs al,
             convert_token_name tk tok pos ))
         rule.add_token
    |> List.rev
  in
  let r_rate =
    alg_expr_of_ast ~warning ~syntax_version sigs tok algs rule.k_def
  in
  let r_un_rate =
    let r_dist d =
      alg_expr_of_ast ~warning ~syntax_version sigs tok algs
        ?max_allowed_var:None d
    in
    Option_util.map
      (fun (un_rate', dist) ->
        let un_rate'' =
          alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var:None un_rate'
        in
        match dist with
        | Some d -> un_rate'', Some (r_dist d)
        | None -> un_rate'', None)
      rule.k_un
  in
  {
    LKappa.r_mix;
    r_created;
    r_edit_style = rule.bidirectional;
    r_delta_tokens;
    r_rate;
    r_un_rate;
  }

let modif_expr_of_ast ~warning ~syntax_version sigs tok algs contact_map modif
    acc =
  match modif with
  | Ast.APPLY (nb, (ast_rule, pos)) ->
    let rule : rule_inter_rep =
      match ast_rule.Ast.rewrite with
      | Ast.Edit rule_content ->
        let mixture, created_mix =
          annotate_edit_mixture ~warning ~syntax_version:Ast.V4 ~is_rule:true
            sigs ~contact_map rule_content.mix
        in
        {
          label_opt = None;
          bidirectional = true;
          mixture;
          created_mix;
          rm_token = [];
          add_token = rule_content.delta_token;
          k_def = ast_rule.k_def;
          k_un = ast_rule.k_un;
          pos;
        }
      | Ast.Arrow rule_content ->
        let mixture, created_mix =
          annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
            rule_content.lhs rule_content.rhs
        in
        {
          label_opt = None;
          bidirectional = false;
          mixture;
          created_mix;
          rm_token = rule_content.rm_token;
          add_token = rule_content.add_token;
          k_def = ast_rule.k_def;
          k_un = ast_rule.k_un;
          pos;
        }
    in
    ( Ast.APPLY
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs nb,
          (assemble_rule ~warning ~syntax_version rule sigs tok algs, pos) ),
      acc )
  | Ast.UPDATE ((lab, pos), how) ->
    let i =
      Option_util.unsome_or_raise
        ~excep:
          (ExceptionDefn.Malformed_Decl
             ("Variable " ^ lab ^ " is not defined", pos))
        (Mods.StringMap.find_option lab algs)
    in
    ( Ast.UPDATE
        ((i, pos), alg_expr_of_ast ~warning ~syntax_version sigs tok algs how),
      i :: acc )
  | Ast.STOP p ->
    ( Ast.STOP
        (List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p),
      acc )
  | Ast.SNAPSHOT (raw, p) ->
    ( Ast.SNAPSHOT
        ( raw,
          List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p
        ),
      acc )
  | Ast.DIN (rel, p) ->
    ( Ast.DIN
        ( rel,
          List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p
        ),
      acc )
  | Ast.DINOFF p ->
    ( Ast.DINOFF
        (List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p),
      acc )
  | (Ast.PLOTENTRY | Ast.CFLOWLABEL (_, _)) as x -> x, acc
  | Ast.PRINT (p, p') ->
    ( Ast.PRINT
        ( List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p,
          List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p'
        ),
      acc )
  | Ast.CFLOWMIX (b, (m, pos)) ->
    ( Ast.CFLOWMIX (b, (mixture_of_ast ~warning ~syntax_version sigs pos m, pos)),
      acc )
  | Ast.SPECIES_OF (b, p, (m, pos)) ->
    ( Ast.SPECIES_OF
        ( b,
          List.map (print_expr_of_ast ~warning ~syntax_version sigs tok algs) p,
          (mixture_of_ast ~warning ~syntax_version sigs pos m, pos) ),
      acc )

let perturbation_of_ast ~warning ~syntax_version sigs tok algs contact_map
    ((alarm, pre, mods, post), pos) up_vars :
    (_, _, _, _) Ast.perturbation * int list =
  let mods', up_vars' =
    List_util.fold_right_map
      (modif_expr_of_ast ~warning ~syntax_version sigs tok algs contact_map)
      mods up_vars
  in
  let max_allowed_var = None in
  ( ( ( alarm,
        Option_util.map
          (bool_expr_of_ast ~warning ~syntax_version sigs tok algs
             ?max_allowed_var)
          pre,
        mods',
        Option_util.map
          (bool_expr_of_ast ~warning ~syntax_version sigs tok algs
             ?max_allowed_var)
          post ),
      pos ),
    up_vars' )

let init_of_ast ~warning ~syntax_version sigs tok contact_map = function
  | Ast.INIT_MIX (who, pos) ->
    Ast.INIT_MIX
      (raw_mixture_of_ast ~warning ~syntax_version sigs ~contact_map who, pos)
  | Ast.INIT_TOK lab ->
    Ast.INIT_TOK
      (List.map
         (fun (lab, pos) ->
           match Mods.StringMap.find_option lab tok with
           | Some x -> x, pos
           | None ->
             raise
               (ExceptionDefn.Malformed_Decl
                  (lab ^ " is not a declared token", pos)))
         lab)

let add_un_variable k_un acc rate_var =
  match k_un with
  | None -> acc, None
  | Some (k, dist) ->
    let acc_un, k' =
      if Alg_expr.has_mix (Loc.v k) then
        ( (Loc.annot_with_dummy rate_var, k) :: acc,
          Loc.annot_with_dummy (Alg_expr.ALG_VAR rate_var) )
      else
        acc, k
    in
    acc_un, Some (k', dist)

type acc_function_rules = {
  rule_names: int * Mods.StringSet.t;
  extra_vars:
    (string Loc.annoted * (Ast.mixture, string) Alg_expr.e Loc.annoted) list;
  cleaned_rules: rule_inter_rep list;
}

(** [name_and_purify] is called in a fold while compiling the rules from Ast.rules into rule_inter_rep *)
let name_and_purify_rule ~warning ~syntax_version sigs ~contact_map
    (acc : acc_function_rules)
    ((label_opt, (ast_rule, r_pos)) :
      string Loc.annoted option * Ast.rule Loc.annoted) : acc_function_rules =
  let rule_names', rule_label =
    give_rule_label ast_rule.bidirectional acc.rule_names Ast.print_ast_rule
      ast_rule label_opt
  in
  let acc', k_def =
    if Alg_expr.has_mix (Loc.v ast_rule.k_def) then (
      let rate_var = rule_label ^ "_rate" in
      ( (Loc.annot_with_dummy rate_var, ast_rule.k_def) :: acc.extra_vars,
        Loc.annot_with_dummy (Alg_expr.ALG_VAR rate_var) )
    ) else
      acc.extra_vars, ast_rule.Ast.k_def
  in
  let acc'', k_un =
    add_un_variable ast_rule.k_un acc' (rule_label ^ "_un_rate")
  in
  match ast_rule.rewrite with
  | Ast.Edit e ->
    if
      ast_rule.bidirectional || ast_rule.k_op <> None
      || ast_rule.k_op_un <> None
    then
      raise
        (ExceptionDefn.Malformed_Decl
           ("Rules in edit notation cannot be bidirectional", r_pos));
    let mixture, created_mix =
      annotate_edit_mixture ~warning ~syntax_version ~is_rule:true sigs
        ~contact_map e.Ast.mix
    in
    {
      rule_names = rule_names';
      extra_vars = acc'';
      cleaned_rules =
        {
          label_opt;
          bidirectional = true;
          mixture;
          created_mix;
          rm_token = [];
          add_token = e.Ast.delta_token;
          k_def;
          k_un;
          pos = r_pos;
        }
        :: acc.cleaned_rules;
    }
  | Ast.Arrow a ->
    let mixture, created_mix =
      annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
        a.Ast.lhs a.Ast.rhs
    in
    let rules' =
      {
        label_opt;
        bidirectional = false;
        mixture;
        created_mix;
        rm_token = a.Ast.rm_token;
        add_token = a.Ast.add_token;
        k_def;
        k_un;
        pos = r_pos;
      }
      :: acc.cleaned_rules
    in
    let acc''', rules'' =
      match ast_rule.bidirectional, ast_rule.k_op with
      | true, Some k when Alg_expr.has_mix (Loc.v k) ->
        let rate_var = Ast.flip_label rule_label ^ "_rate" in
        let rate_var_un = Ast.flip_label rule_label ^ "_un_rate" in
        let acc_un, k_op_un =
          add_un_variable ast_rule.k_op_un acc'' rate_var_un
        in
        let mixture, created_mix =
          annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
            a.Ast.rhs a.Ast.lhs
        in
        ( (Loc.annot_with_dummy rate_var, k) :: acc_un,
          {
            label_opt =
              Option_util.map (fun (l, p) -> Ast.flip_label l, p) label_opt;
            bidirectional = false;
            mixture;
            created_mix;
            rm_token = a.Ast.add_token;
            add_token = a.Ast.rm_token;
            k_def = Loc.annot_with_dummy (Alg_expr.ALG_VAR rate_var);
            k_un = k_op_un;
            pos = r_pos;
          }
          :: rules' )
      | true, Some rate ->
        let rate_var_un = Ast.flip_label rule_label ^ "_un_rate" in
        let acc_un, k_op_un =
          add_un_variable ast_rule.k_op_un acc'' rate_var_un
        in
        let mixture, created_mix =
          annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
            a.Ast.rhs a.Ast.lhs
        in
        ( acc_un,
          {
            label_opt =
              Option_util.map (fun (l, p) -> Ast.flip_label l, p) label_opt;
            bidirectional = false;
            mixture;
            created_mix;
            rm_token = a.Ast.add_token;
            add_token = a.Ast.rm_token;
            k_def = rate;
            k_un = k_op_un;
            pos = r_pos;
          }
          :: rules' )
      | false, None -> acc'', rules'
      | false, Some _ | true, None ->
        raise
          (ExceptionDefn.Malformed_Decl
             ( "Incompatible arrow and kinectic rate for inverse definition",
               r_pos ))
    in
    { rule_names = rule_names'; extra_vars = acc'''; cleaned_rules = rules'' }

type site_sig_with_links_as_lists =
  (string Loc.annoted * string Loc.annoted) list Signature.site_sig
(** Temporary type to store site signature with list links instead of array array links *)

(** [prepare_agent_sig ~sites evaluates to (site_sigs, counter_list) which describe data that can be used to create a Signature.t for a single agent*)
let prepare_agent_sig ~(sites : Ast.site list) :
    site_sig_with_links_as_lists NamedDecls.t * string Loc.annoted list =
  let ( (site_sigs_pre_nameddecls :
          (string Loc.annoted * site_sig_with_links_as_lists) list),
        (counter_names : string Loc.annoted list) ) =
    List.fold_right
      (fun site (acc_site_sigs, acc_counter_names) ->
        match site with
        (* TODO see if can remove Ast here *)
        | Ast.Port p ->
          ( ( p.port_name,
              {
                Signature.internal_state =
                  NamedDecls.create
                    (Tools.array_map_of_list
                       (function
                         | Some x, pos -> (x, pos), ()
                         | None, pos ->
                           raise
                             (ExceptionDefn.Malformed_Decl
                                ( "Forbidden internal state inside signature \
                                   definition",
                                  pos )))
                       p.port_int);
                links =
                  Some
                    (List.fold_left
                       (fun acc_links' -> function
                         | ( (LKappa.LNK_FREE | LKappa.ANY_FREE | LKappa.LNK_ANY),
                             _ ) ->
                           acc_links'
                         | (LKappa.LNK_SOME | LKappa.LNK_VALUE _), pos ->
                           raise
                             (ExceptionDefn.Malformed_Decl
                                ( "Forbidden link status inside signature \
                                   definition",
                                  pos ))
                         | LKappa.LNK_TYPE (a, b), _ -> (a, b) :: acc_links')
                       [] p.port_link);
                counters_info = None;
              } )
            :: acc_site_sigs,
            acc_counter_names )
        | Counter c ->
          (* Here, only CEQ tests are accepted *)
          (match c.counter_test with
          | None ->
            let n, pos = c.counter_name in
            raise
              (ExceptionDefn.Internal_Error
                 ("Counter " ^ n ^ " should have a test in signature", pos))
          | Some (test, pos) ->
            (match test with
            | CVAR _ ->
              raise
                (ExceptionDefn.Internal_Error
                   ("Counter should not have a var in signature", pos))
            | CGTE _ ->
              raise
                (ExceptionDefn.Internal_Error
                   ("Counter should not have >= in signature", pos))
            | CEQ j ->
              ( ( c.counter_name,
                  {
                    internal_state = NamedDecls.create [||];
                    (* Agent with counter can link to port [b] on counter agent [__counter_agent] *)
                    links =
                      Some
                        [
                          ( Loc.annot_with_dummy "b",
                            Loc.annot_with_dummy "__counter_agent" );
                        ];
                    counters_info = Some (j, Loc.v c.counter_delta);
                  } )
                :: acc_site_sigs,
                c.counter_name :: acc_counter_names ))))
      sites ([], [])
  in
  NamedDecls.create_from_list site_sigs_pre_nameddecls, counter_names

(** [make_counter_agent_site_sigs counters_per_agent] evaluates to
  (counter_agent_name, site_sigs_counter_agent) which describe the counter
      agent and its site signatures with possible links to other agents.
      [counter_info] associates each agent to a list of counter ports, one for each defined counter *)
let make_counter_agent_site_sigs
    (counters_per_agent : ((string * Loc.t) * (string * Loc.t) list) list) :
    (string * Loc.t)
    * ((string * Loc.t) * (string * Loc.t)) list Signature.site_sig NamedDecls.t
    =
  let counter_agent_name = "__counter_agent", Loc.dummy in
  let a_port_name = "a", Loc.dummy in
  (* after port *)
  let b_port_name = "b", Loc.dummy in

  (* before port *)

  (* Port [a] can link to port b of agent of type counter agent *)
  let a_port_sig =
    {
      Signature.internal_state = NamedDecls.create [||];
      links = Some [ b_port_name, counter_agent_name ];
      counters_info = None;
    }
  in
  (* Port [b] can link to port a of agent of type counter agent
   * OR for each agent [agent_name] with counters, to their ports
   * [agent_counter_port_name] *)
  let b_links =
    List.fold_right
      (fun (agent_name, counters_from_agent) acc ->
        List.map
          (fun agent_counter_port_name -> agent_counter_port_name, agent_name)
          counters_from_agent
        @ acc)
      counters_per_agent
      [ a_port_name, counter_agent_name ]
  in
  let b_port_sig =
    {
      Signature.internal_state = NamedDecls.create [||];
      links = Some b_links;
      counters_info = None;
    }
  in
  let site_sigs_counter_agent =
    NamedDecls.create [| a_port_name, a_port_sig; b_port_name, b_port_sig |]
  in
  counter_agent_name, site_sigs_counter_agent

let agent_sigs_of_agent_sigs_with_links_as_lists ~(build_contact_map : bool)
    (agent_sigs_pre : site_sig_with_links_as_lists NamedDecls.t NamedDecls.t) :
    Signature.t NamedDecls.t =
  let size_sigs = NamedDecls.size agent_sigs_pre in
  NamedDecls.mapi
    (fun ag_id ag_name ->
      NamedDecls.map (fun site_name_ag1 site_sig ->
          if not build_contact_map then
            { site_sig with Signature.links = None }
          else (
            (* Update links *)
            (* TODO improve comment above *)
            let site_links =
              Array.init (size_sigs - ag_id) (fun i ->
                  Array.make
                    (NamedDecls.size
                       (NamedDecls.elt_val agent_sigs_pre (i + ag_id)))
                    false)
            in
            List.iter
              (fun (((site_name_ag2, pos) as site_name), ((ag2_name, _) as ag)) ->
                let ag2_id = NamedDecls.elt_id ~kind:"ag" agent_sigs_pre ag in
                let site_id =
                  NamedDecls.elt_id ~kind:"site name"
                    (NamedDecls.elt_val agent_sigs_pre ag2_id)
                    site_name
                in
                if ag2_id >= ag_id then
                  site_links.(ag2_id - ag_id).(site_id) <- true;
                let should_raise_for_missing_link =
                  not
                    (List.exists
                       (fun ((x, _), (y, _)) ->
                         x = site_name_ag1 && y = ag_name)
                       ((NamedDecls.elt_val
                           (NamedDecls.elt_val agent_sigs_pre ag2_id)
                           site_id)
                          .links |> Option_util.unsome_or_raise))
                in
                if should_raise_for_missing_link then
                  raise
                    (ExceptionDefn.Malformed_Decl
                       ( Format.asprintf "No link to %s.%s from %s.%s."
                           site_name_ag1 ag_name site_name_ag2 ag2_name,
                         pos )))
              (Option_util.unsome_or_raise site_sig.links);
            { site_sig with Signature.links = Some site_links }
          )))
    agent_sigs_pre

let create_sigs (l : Ast.agent list) : Signature.s =
  (* Contact map should be built only if a specific link is described in the definition of signature *)
  let build_contact_map : bool =
    List.fold_left
      (fun acc0 -> function
        | Ast.Absent pos ->
          raise
            (ExceptionDefn.Malformed_Decl
               ("Absent agent are forbidden in signature", pos))
        | Ast.Present (_, sites, _) ->
          List.fold_left
            (fun acc1 site ->
              match site with
              | Ast.Counter _ -> acc1
              | Ast.Port p ->
                List.fold_left
                  (fun acc2 -> function
                    | (LKappa.LNK_FREE | LKappa.ANY_FREE | LKappa.LNK_ANY), _ ->
                      acc2
                    | (LKappa.LNK_SOME | LKappa.LNK_VALUE _), pos ->
                      raise
                        (ExceptionDefn.Malformed_Decl
                           ( "Forbidden link status inside a definition of \
                              signature",
                             pos ))
                    | LKappa.LNK_TYPE (_, _), _ -> true)
                  acc1 p.Ast.port_link)
            acc0 sites)
      false l
  in

  let ( (sigs_with_links_as_lists :
          (string Loc.annoted * site_sig_with_links_as_lists NamedDecls.t) list),
        (counters_per_agent :
          (string Loc.annoted * string Loc.annoted list) list) ) =
    List.fold_right
      (fun agent (acc_sigs, acc_counters_per_agent) ->
        match agent with
        | Ast.Absent _ -> acc_sigs, acc_counters_per_agent
        | Ast.Present (agent_name, sites, _) ->
          let site_sigs_nd, counters_agent = prepare_agent_sig ~sites in
          let counters' =
            if counters_agent = [] then
              acc_counters_per_agent
            else
              (agent_name, counters_agent) :: acc_counters_per_agent
          in
          (agent_name, site_sigs_nd) :: acc_sigs, counters')
      l ([], [])
  in

  let agent_sigs : Signature.t NamedDecls.t =
    (if counters_per_agent = [] then
       sigs_with_links_as_lists
     else
       make_counter_agent_site_sigs counters_per_agent
       :: sigs_with_links_as_lists)
    |> NamedDecls.create_from_list
    |> agent_sigs_of_agent_sigs_with_links_as_lists ~build_contact_map
  in

  (* TODO see agent_sigs namings *)
  Signature.create ~counters_per_agent agent_sigs

let init_of_ast ~warning ~syntax_version sigs contact_map tok algs inits =
  List.map
    (fun (expr, ini) ->
      ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs expr,
        init_of_ast ~warning ~syntax_version sigs tok contact_map ini ))
    inits

type ast_compiled_data = {
  agents_sig: Signature.s;
  contact_map: Contact_map.t;
  token_names: unit NamedDecls.t;
  alg_vars_finder: int Mods.StringMap.t;
  updated_alg_vars: int list;
  result:
    (Ast.agent, LKappa.rule_mixture, Raw_mixture.t, int, LKappa.rule) Ast.compil;
      (** Compiled data where identifiers are i Ast.compil where identifiers
     * are integers and not string, syntactic sugar on rules are expansed
     * (syntactic sugar on mixture are not) *)
}

let compil_of_ast ~warning ~debug_mode ~syntax_version ~var_overwrite ast_compil
    =
  let has_counters = Counters_compiler.has_counters ast_compil in
  let agent_sig_is_implicit =
    ast_compil.Ast.signatures = [] && ast_compil.Ast.tokens = []
  in
  (* Infer agent signatures if the signature is implicit *)
  let ast_compil =
    if agent_sig_is_implicit && has_counters then
      raise
        (ExceptionDefn.Malformed_Decl
           ("implicit signature is incompatible with counters", Loc.dummy))
    else if agent_sig_is_implicit then
      Ast.infer_agent_signatures ast_compil
    else
      ast_compil
  in
  (* Remove counter variable definition by splitting in several rules *)
  let ast_compil =
    if has_counters then
      Counters_compiler.split_counter_variables_into_separate_rules ~warning
        ~debug_mode ast_compil
    else
      ast_compil
  in

  let agents_sig : Signature.s = create_sigs ast_compil.Ast.signatures in
  (* Set an empty contact map *)
  let contact_map : (Mods.IntSet.t * Mods.Int2Set.t) array array =
    Array.init (Signature.size agents_sig) (fun i ->
        Array.init (Signature.arity agents_sig i) (fun s ->
            ( Tools.recti
                (fun a k -> Mods.IntSet.add k a)
                Mods.IntSet.empty
                (Signature.internal_states_number i s agents_sig),
              Mods.Int2Set.empty )))
  in
  let rule_names, extra_vars, cleaned_rules =
    let acc =
      List.fold_left
        (name_and_purify_rule ~warning ~syntax_version agents_sig ~contact_map)
        {
          rule_names = 0, Mods.StringSet.empty;
          extra_vars = [];
          cleaned_rules = [];
        }
        ast_compil.Ast.rules
    in
    snd acc.rule_names, acc.extra_vars, acc.cleaned_rules
  in
  let overwrite_vars (var_overwrite : (string * Nbr.t) list)
      (vars : (Ast.mixture, string) Ast.variable_def list) :
      (string * Nbr.t) list * (Ast.mixture, string) Ast.variable_def list =
    List.fold_left
      (fun (overwrite_vars_remaining, acc_vars) (((x, _), _) as var) ->
        let matchs, other_overwrite_vars =
          List.partition (fun (x', _) -> x = x') overwrite_vars_remaining
        in
        let acc_vars_with_x_rewritten_if_present =
          match matchs with
          | [] -> var :: acc_vars
          | [ (x, v) ] -> (Loc.annot_with_dummy x, Alg_expr.const v) :: acc_vars
          | (x, _) :: _ :: _ ->
            raise
              (ExceptionDefn.Malformed_Decl
                 ( "variable '" ^ x ^ "' is overwritten more than once",
                   Loc.dummy ))
        in

        other_overwrite_vars, acc_vars_with_x_rewritten_if_present)
      (var_overwrite, []) vars
    |> fun (var_overwrite_not_applied, rev_alg_vars) ->
    var_overwrite_not_applied, List.rev rev_alg_vars
  in
  let var_overwrite_not_applied, alg_vars_with_rewritten_vars =
    overwrite_vars var_overwrite (ast_compil.Ast.variables @ extra_vars)
  in
  let alg_vars_array =
    List_util.rev_map_append
      (fun (x, v) -> Loc.annot_with_dummy x, Alg_expr.const v)
      var_overwrite_not_applied alg_vars_with_rewritten_vars
    |> Array.of_list
  in
  let alg_vars_finder =
    alg_vars_array |> NamedDecls.create ~forbidden:rule_names |> fun nd ->
    nd.NamedDecls.finder
  in
  let token_names =
    ast_compil.Ast.tokens
    |> Tools.array_map_of_list (fun x -> x, ())
    |> NamedDecls.create
  in
  let tokens_finder = token_names.NamedDecls.finder in

  if has_counters then
    Counters_compiler.add_counter_to_contact_map agents_sig
      (add_link_contact_map ~contact_map);

  let pertubations_without_counters, updated_alg_vars =
    List_util.fold_right_map
      (perturbation_of_ast ~warning ~syntax_version agents_sig tokens_finder
         alg_vars_finder contact_map)
      ast_compil.Ast.perturbations []
  in
  let perturbations =
    if has_counters then
      Counters_compiler.counters_perturbations agents_sig
        [ ast_compil.Ast.signatures ]
      @ pertubations_without_counters
    else
      pertubations_without_counters
  in

  let rules =
    List.rev_map
      (fun (rule : rule_inter_rep) ->
        ( rule.label_opt,
          ( assemble_rule ~warning ~syntax_version rule agents_sig tokens_finder
              alg_vars_finder,
            rule.pos ) ))
      cleaned_rules
  in

  let variables =
    Tools.array_fold_righti
      (fun i (lab, expr) acc ->
        ( lab,
          alg_expr_of_ast ~warning ~syntax_version ~max_allowed_var:(pred i)
            agents_sig tokens_finder alg_vars_finder expr )
        :: acc)
      alg_vars_array []
  in
  let observables =
    List.rev_map
      (fun expr ->
        alg_expr_of_ast ~warning ~syntax_version agents_sig tokens_finder
          alg_vars_finder expr)
      (List.rev ast_compil.observables)
  in
  let init =
    init_of_ast ~warning ~syntax_version agents_sig contact_map tokens_finder
      alg_vars_finder ast_compil.init
  in

  {
    agents_sig;
    contact_map;
    token_names;
    alg_vars_finder;
    updated_alg_vars;
    result =
      {
        filenames = ast_compil.filenames;
        variables;
        rules;
        observables;
        init;
        perturbations;
        volumes = ast_compil.volumes;
        tokens = ast_compil.tokens;
        signatures = ast_compil.signatures;
        configurations = ast_compil.configurations;
      };
  }
