(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

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
  LKappa.link_should_be_removed i
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

let annotate_dropped_agent ~warning ~syntax_version ~r_editStyle sigs
    links_annot ((agent_name, _) as ag_ty) intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Erased)
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
        let ((_, p_pos) as p_na) = p.Ast.port_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let () =
          match Signature.counter_of_site p_id sign with
          | Some _ -> LKappa.counter_misused agent_name p.Ast.port_nme
          | None -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.several_occurence_of_site agent_name p.Ast.port_nme
        in
        let () =
          match p.Ast.port_lnk_mod, p.Ast.port_lnk with
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
            LKappa.forbid_modification p_pos p.Ast.port_lnk_mod
        in
        let () = LKappa.forbid_modification p_pos p.Ast.port_int_mod in

        let () =
          match p.Ast.port_int with
          | [] | [ (None, _) ] -> ()
          | [ (Some va, pos) ] ->
            internals.(p_id) <-
              LKappa.I_VAL_ERASED
                (Signature.num_of_internal_state p_id (va, pos) sign)
          | _ :: (_, pos) :: _ -> LKappa.several_internal_states pos
        in
        match p.Ast.port_lnk with
        | [ (LKappa.LNK_ANY, pos) ] ->
          let () = ports.(p_id) <- (LKappa.ANY_FREE, pos), LKappa.Erased in
          lannot, pset'
        | [ (LKappa.LNK_SOME, pos_lnk) ] ->
          let na, pos = p.Ast.port_nme in
          let () =
            warning ~pos (fun f ->
                Format.fprintf f
                  "breaking a semi-link on site '%s' will induce a side effect"
                  na)
          in
          let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_lnk), LKappa.Erased in
          lannot, pset'
        | [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_lnk) ] ->
          let na, pos = p.Ast.port_nme in
          let () =
            warning ~pos (fun f ->
                Format.fprintf f
                  "breaking a semi-link on site '%s' will induce a side effect"
                  na)
          in
          let () =
            ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p LKappa.Erased
          in
          lannot, pset'
        | ([ (LKappa.ANY_FREE, _) ] | []) when syntax_version = Ast.V3 ->
          let () =
            ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, LKappa.Erased
          in
          lannot, pset'
        | [ (LKappa.ANY_FREE, _) ] | [] ->
          let () =
            ports.(p_id) <- Locality.dummy_annot LKappa.ANY_FREE, LKappa.Erased
          in
          lannot, pset'
        | [ (LKappa.LNK_FREE, _) ] ->
          let () =
            ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, LKappa.Erased
          in
          lannot, pset'
        | [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
          let va, lannot' =
            let warn_on_swap =
              if r_editStyle then
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

let annotate_created_agent ~warning ~syntax_version ~r_editStyle sigs
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
        let ((_, p_pos) as p_na) = p.Ast.port_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let () =
          match Signature.counter_of_site p_id sign with
          | Some _ -> LKappa.counter_misused agent_name p.Ast.port_nme
          | None -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.several_occurence_of_site agent_name p.Ast.port_nme
        in
        let () = LKappa.forbid_modification p_pos p.Ast.port_lnk_mod in
        let () = LKappa.forbid_modification p_pos p.Ast.port_int_mod in
        let () =
          match p.Ast.port_int with
          | [] -> ()
          | [ (None, _) ] ->
            LKappa.not_enough_specified ~status:"internal" ~side:"left"
              agent_name p_na
          | [ (Some va, pos) ] ->
            internals.(p_id) <-
              Some (Signature.num_of_internal_state p_id (va, pos) sign)
          | _ :: (_, pos) :: _ -> LKappa.several_internal_states pos
        in
        match p.Ast.port_lnk with
        | [ (LKappa.LNK_ANY, _) ]
        | [ (LKappa.LNK_SOME, _) ]
        | [ (LKappa.LNK_TYPE _, _) ]
        | _ :: _ :: _ ->
          LKappa.not_enough_specified ~status:"linking" ~side:"left" agent_name
            p_na
        | [ (LKappa.ANY_FREE, _) ] when syntax_version = Ast.V4 ->
          LKappa.not_enough_specified ~status:"linking" ~side:"left" agent_name
            p_na
        | [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
          let () = ports.(p_id) <- Raw_mixture.VAL i in
          let _, rannot' =
            let warn_on_swap =
              if r_editStyle then
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
    Array.make arity (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained)
  in
  let internals = Array.make arity LKappa.I_ANY in
  let scan_port (links_annot, pset) p =
    let p_na, _ = p.Ast.port_nme in
    let p_id = Signature.num_of_site ~agent_name p.Ast.port_nme sign in
    let () =
      match Signature.counter_of_site p_id sign with
      | Some _ -> LKappa.counter_misused agent_name p.Ast.port_nme
      | None -> ()
    in
    let pset' = Mods.IntSet.add p_id pset in
    let () =
      if pset == pset' then
        LKappa.several_occurence_of_site agent_name p.Ast.port_nme
    in
    let links_annot' =
      match p.Ast.port_lnk with
      | [ ((LKappa.LNK_SOME, pos) as x) ] ->
        let modif, links_annot' =
          translate_modification ~warning ~warn:(p_na, pos) sigs ?contact_map
            ag_id p_id links_annot p.Ast.port_lnk_mod
        in
        let () = ports.(p_id) <- x, modif in
        links_annot'
      | [ (LKappa.LNK_ANY, pos) ] ->
        let modif, links_annot' =
          translate_modification ~warning ~warn:(p_na, pos) sigs ?contact_map
            ag_id p_id links_annot p.Ast.port_lnk_mod
        in
        let () = ports.(p_id) <- (LKappa.ANY_FREE, pos), modif in
        links_annot'
      | ([] | [ (LKappa.ANY_FREE, _) ]) when syntax_version = Ast.V3 ->
        let modif, links_annot' =
          translate_modification ~warning ?warn:None sigs ?contact_map ag_id
            p_id links_annot p.Ast.port_lnk_mod
        in
        let () = ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, modif in
        links_annot'
      | [] when p.Ast.port_lnk_mod = None -> links_annot
      | [ (LKappa.ANY_FREE, _) ] | [] ->
        LKappa.not_enough_specified ~status:"linking" ~side:"left" agent_name
          p.Ast.port_nme
      | [ (LKappa.LNK_FREE, _) ] ->
        let modif, links_annot' =
          translate_modification ~warning ?warn:None sigs ?contact_map ag_id
            p_id links_annot p.Ast.port_lnk_mod
        in
        let () = ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, modif in
        links_annot'
      | [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos) ] ->
        let modif, links_annot' =
          translate_modification ~warning ~warn:(p_na, pos) sigs ?contact_map
            ag_id p_id links_annot p.Ast.port_lnk_mod
        in
        let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p modif in
        links_annot'
      | [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
        let modif, (lhs_links, rhs_links) =
          translate_modification ~warning ?warn:None sigs ?contact_map ag_id
            p_id links_annot p.Ast.port_lnk_mod
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
                  p_na agent_name)
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
      | _ :: (_, pos) :: _, _ -> LKappa.several_internal_states pos
    in
    links_annot', pset'
  in
  let annot', _ =
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
    annot' )

let annotate_agent_with_diff ~warning ~syntax_version sigs ?contact_map
    ((agent_name, _) as ag_ty) links_annot lp rp lc rc =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained)
  in
  let internals = Array.make arity LKappa.I_ANY in
  let register_port_modif p_id lnk1 p' ((lhs_links, rhs_links) as links_annot) =
    let () =
      LKappa.forbid_modification (snd p'.Ast.port_nme) p'.Ast.port_lnk_mod
    in
    match lnk1, p'.Ast.port_lnk with
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
      LKappa.not_enough_specified ~status:"linking" ~side:"right" agent_name
        p'.Ast.port_nme
    | [ (LKappa.LNK_ANY, pos) ], [] when syntax_version = Ast.V3 ->
      let () = ports.(p_id) <- (LKappa.LNK_ANY, pos), LKappa.Freed in
      links_annot
    | [ (LKappa.LNK_ANY, pos) ], [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ->
      let () = ports.(p_id) <- (LKappa.LNK_ANY, pos), LKappa.Freed in
      links_annot
    | ( [ (LKappa.LNK_SOME, pos_lnk) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let na, pos = p'.Ast.port_nme in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_lnk), LKappa.Freed in
      links_annot
    | ( [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_lnk) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let na, pos = p'.Ast.port_nme in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () =
        ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p LKappa.Freed
      in
      links_annot
    | [ (LKappa.LNK_SOME, pos_lnk) ], [] when syntax_version = Ast.V3 ->
      let na, pos = p'.Ast.port_nme in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_lnk), LKappa.Freed in
      links_annot
    | [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_lnk) ], []
      when syntax_version = Ast.V3 ->
      let na, pos = p'.Ast.port_nme in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () =
        ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p LKappa.Freed
      in
      links_annot
    | ( ([ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] | []),
        ([ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] | []) )
      when syntax_version = Ast.V3 ->
      let () =
        ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, LKappa.Maintained
      in
      links_annot
    | ( [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ],
        [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ] ) ->
      let () =
        ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, LKappa.Maintained
      in
      links_annot
    | [], [] ->
      let () =
        ports.(p_id) <- Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained
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
    | [ (LKappa.LNK_ANY, pos_lnk) ], [ (LKappa.LNK_VALUE (i, ()), pos) ] ->
      let () = ports.(p_id) <- (LKappa.LNK_ANY, pos_lnk), LKappa.Linked i in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | [ (LKappa.LNK_SOME, pos_lnk) ], [ (LKappa.LNK_VALUE (i, ()), pos') ] ->
      let na, pos = p'.Ast.port_nme in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () = ports.(p_id) <- (LKappa.LNK_SOME, pos_lnk), LKappa.Linked i in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos' i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | ( [ (LKappa.LNK_TYPE (dst_p, dst_ty), pos_lnk) ],
        [ (LKappa.LNK_VALUE (i, ()), pos') ] ) ->
      let na, pos = p'.Ast.port_nme in
      let () =
        warning ~pos (fun f ->
            Format.fprintf f
              "breaking a semi-link on site '%s' will induce a side effect" na)
      in
      let () =
        ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p (LKappa.Linked i)
      in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos' i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | ( [ ((LKappa.LNK_FREE | LKappa.ANY_FREE), _) ],
        [ (LKappa.LNK_VALUE (i, ()), pos) ] ) ->
      let () =
        ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, LKappa.Linked i
      in
      let _, rhs_links' =
        build_link sigs ~warn_on_swap:warning ?contact_map pos i ag_id p_id
          LKappa.Freed rhs_links
      in
      lhs_links, rhs_links'
    | [], [ (LKappa.LNK_VALUE (i, ()), pos) ] when syntax_version = Ast.V3 ->
      let () =
        ports.(p_id) <- Locality.dummy_annot LKappa.LNK_FREE, LKappa.Linked i
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
      LKappa.not_enough_specified ~status:"linking" ~side:"right" agent_name
        p'.Ast.port_nme
    | [], [ ((LKappa.ANY_FREE | LKappa.LNK_FREE | LKappa.LNK_VALUE (_, _)), _) ]
      ->
      LKappa.not_enough_specified ~status:"linking" ~side:"left" agent_name
        p'.Ast.port_nme
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
      LKappa.forbid_modification (snd p'.Ast.port_nme) p'.Ast.port_int_mod
    in
    match int1, p'.Ast.port_int with
    | [], [] | [ (None, _) ], [ (None, _) ] -> ()
    | [ (Some va, pos) ], [ (Some va', pos') ] ->
      internals.(p_id) <-
        LKappa.I_VAL_CHANGED
          ( Signature.num_of_internal_state p_id (va, pos) sign,
            Signature.num_of_internal_state p_id (va', pos') sign )
    | [], [ (Some va, vapos) ] when syntax_version = Ast.V3 ->
      let na, pos = p'.Ast.port_nme in
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
      LKappa.not_enough_specified ~status:"internal" ~side:"left" agent_name
        p'.Ast.port_nme
    | [ _ ], ([ (None, _) ] | []) ->
      LKappa.not_enough_specified ~status:"internal" ~side:"right" agent_name
        p'.Ast.port_nme
    | _ :: (_, pos) :: _, _ | _, _ :: (_, pos) :: _ ->
      LKappa.several_internal_states pos
  in
  let find_in_r (na, pos) rp =
    let p', r =
      List.partition (fun p -> String.compare (fst p.Ast.port_nme) na = 0) rp
    in
    match p' with
    | [ p' ] -> p', r
    | [] ->
      LKappa.not_enough_specified ~status:"linking" ~side:"right" agent_name
        (na, pos)
    | _ :: _ -> LKappa.several_occurence_of_site agent_name (na, pos)
  in
  let rp_r, annot, _ =
    List.fold_left
      (fun (rp, annot, pset) p ->
        let ((_, p_pos) as p_na) = p.Ast.port_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.several_occurence_of_site agent_name p.Ast.port_nme
        in
        let () = LKappa.forbid_modification p_pos p.Ast.port_lnk_mod in
        let () = LKappa.forbid_modification p_pos p.Ast.port_int_mod in

        let p', rp' = find_in_r p_na rp in
        let annot' = register_port_modif p_id p.Ast.port_lnk p' annot in
        let () = register_internal_modif p_id p.Ast.port_int p' in
        rp', annot', pset')
      (rp, links_annot, Mods.IntSet.empty)
      lp
  in
  let annot' =
    List.fold_left
      (fun annot p ->
        let p_na = p.Ast.port_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let () = register_internal_modif p_id [] p in
        register_port_modif p_id [ Locality.dummy_annot LKappa.LNK_ANY ] p annot)
      annot rp_r
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
    annot' )

let refer_links_annot ?warning sigs links_annot mix =
  List.iter
    (fun r ->
      let ra = r.Counters_compiler.ra in
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
    | Some (i, (_, _, _, pos, _)) -> LKappa.link_only_one_occurence i pos
  in
  let () = refer_links_annot ?warning sigs lhs_links_two mix in
  match Mods.IntMap.root rhs_links_one with
  | None -> ()
  | Some (i, (_, _, _, pos, _)) -> LKappa.link_only_one_occurence i pos

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
    | ( Ast.Present (((lag_na, lpos) as ag_ty), lag_s, lmod) :: lt,
        Ast.Present ((rag_na, rpos), rag_s, rmod) :: rt )
      when String.compare lag_na rag_na = 0
           && Ast.no_more_site_on_right true lag_s rag_s ->
      let () = LKappa.forbid_modification lpos lmod in
      let () = LKappa.forbid_modification rpos rmod in
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
              let () = LKappa.forbid_modification pos modif in
              let intf, counts = separate_sites sites in
              let ra, lannot' =
                annotate_dropped_agent ~warning ~syntax_version
                  ~r_editStyle:false sigs lannot na intf counts
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
              let () = LKappa.forbid_modification pos modif in
              let intf, counts = separate_sites sites in
              let rannot', x' =
                annotate_created_agent ~warning ~syntax_version
                  ~r_editStyle:false sigs ?contact_map rannot na intf
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
      let () = LKappa.forbid_modification pos lmod in
      let intf, counts = separate_sites sites in
      let ra, lannot' =
        annotate_dropped_agent ~warning ~syntax_version ~r_editStyle:false sigs
          (fst links_annot) ty intf counts
      in
      aux (lannot', snd links_annot) (ra :: mix) cmix lt rt
    | Ast.Absent _ :: lt, Ast.Present (((_, pos) as ty), sites, rmod) :: rt ->
      let () = LKappa.forbid_modification pos rmod in
      let intf, counts = separate_sites sites in
      let rannot', x' =
        annotate_created_agent ~warning ~syntax_version ~r_editStyle:false sigs
          ?contact_map (snd links_annot) ty intf
      in
      let x'' =
        Counters_compiler.annotate_created_counters sigs ty counts
          (add_link_contact_map ?contact_map)
          x'
      in
      aux (fst links_annot, rannot') mix (x'' :: cmix) lt rt
    | ( Ast.Present (((lag_na, lpos) as ag_ty), lag_s, lmod) :: lt,
        Ast.Present ((rag_na, rpos), rag_s, rmod) :: rt ) ->
      if
        String.compare lag_na rag_na = 0
        && Ast.no_more_site_on_right true lag_s rag_s
      then (
        let () = LKappa.forbid_modification lpos lmod in
        let () = LKappa.forbid_modification rpos rmod in
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
           (Locality.dummy_annot "Invariant violation in annotate_lhs_with..."))
  in
  aux_line
    ( (Mods.IntMap.empty, Mods.IntMap.empty),
      (Mods.IntMap.empty, Mods.IntMap.empty) )
    [] [] lhs rhs

let annotate_lhs_with_diff ~warning ~syntax_version sigs ?contact_map lhs rhs =
  match syntax_version with
  | Ast.V3 -> annotate_lhs_with_diff_v3 ~warning sigs ?contact_map lhs rhs
  | Ast.V4 -> annotate_lhs_with_diff_v4 ~warning sigs ?contact_map lhs rhs

let annotate_edit_mixture ~warning ~syntax_version ~is_rule sigs ?contact_map m
    =
  let links_annot, mix, cmix =
    List.fold_left
      (List.fold_left (fun (lannot, acc, news) -> function
         | Ast.Absent _ -> lannot, acc, news
         | Ast.Present (ty, sites, modif) ->
           let intf, counts = separate_sites sites in
           (match modif with
           | None ->
             let a, lannot' =
               annotate_edit_agent ~warning ~syntax_version ~is_rule sigs
                 ?contact_map ty lannot intf counts
             in
             lannot', a :: acc, news
           | Some Ast.Create ->
             let rannot', x' =
               annotate_created_agent ~warning ~syntax_version ~r_editStyle:true
                 sigs ?contact_map (snd lannot) ty intf
             in
             let x'' =
               Counters_compiler.annotate_created_counters sigs ty counts
                 (add_link_contact_map ?contact_map)
                 x'
             in
             (fst lannot, rannot'), acc, x'' :: news
           | Some Ast.Erase ->
             let ra, lannot' =
               annotate_dropped_agent ~warning ~syntax_version ~r_editStyle:true
                 sigs (fst lannot) ty intf counts
             in
             (lannot', snd lannot), ra :: acc, news)))
      ( ( (Mods.IntMap.empty, Mods.IntMap.empty),
          (Mods.IntMap.empty, Mods.IntMap.empty) ),
        [],
        [] )
      m
  in
  let () = final_rule_sanity ?warning:None sigs links_annot mix in
  List.rev mix, List.rev cmix

let annotate_created_mixture ~warning ~syntax_version sigs ?contact_map m =
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
             annotate_created_agent ~warning ~syntax_version ~r_editStyle:true
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
    | Some (i, (_, _, _, pos, _)) -> LKappa.link_only_one_occurence i pos
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

let mixture_of_ast ~warning ~syntax_version sigs ?contact_map pos mix =
  match
    annotate_edit_mixture ~warning ~syntax_version ~is_rule:false sigs
      ?contact_map mix
  with
  | r, [] -> fst (Counters_compiler.remove_counter_rule sigs r [])
  | _, _ ->
    raise (ExceptionDefn.Internal_Error ("A mixture cannot create agents", pos))

let raw_mixture_of_ast ~warning ~syntax_version sigs ?contact_map mix =
  let b =
    annotate_created_mixture ~warning ~syntax_version sigs ?contact_map mix
  in
  snd (Counters_compiler.remove_counter_rule sigs [] b)

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

let convert_token_name tk_nme tok pos =
  match Mods.StringMap.find_option tk_nme tok with
  | Some x -> x
  | None ->
    raise
      (ExceptionDefn.Malformed_Decl (tk_nme ^ " is not a declared token", pos))

let rec alg_expr_of_ast ~warning ~syntax_version sigs tok algs ?max_allowed_var
    (alg, pos) =
  ( (match alg with
    | Alg_expr.KAPPA_INSTANCE ast ->
      Alg_expr.KAPPA_INSTANCE
        (mixture_of_ast ~warning ~syntax_version sigs pos ast)
    | Alg_expr.ALG_VAR lab ->
      Alg_expr.ALG_VAR (convert_alg_var ?max_allowed_var algs lab pos)
    | Alg_expr.TOKEN_ID tk_nme ->
      Alg_expr.TOKEN_ID (convert_token_name tk_nme tok pos)
    | Alg_expr.DIFF_KAPPA_INSTANCE (expr, ast) ->
      Alg_expr.DIFF_KAPPA_INSTANCE
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var expr,
          mixture_of_ast ~warning ~syntax_version sigs pos ast )
    | Alg_expr.DIFF_TOKEN (expr, tk_nme) ->
      Alg_expr.DIFF_TOKEN
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            ?max_allowed_var expr,
          convert_token_name tk_nme tok pos )
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

let assemble_rule ~warning ~syntax_version ~r_editStyle sigs tok algs r_mix
    r_created rm_tk add_tk rate un_rate =
  let tks =
    List.rev_map
      (fun (al, (tk, pos)) ->
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs
            (Locality.dummy_annot (Alg_expr.UN_ALG_OP (Operator.UMINUS, al))),
          convert_token_name tk tok pos ))
      rm_tk
  in
  let tks' =
    List_util.rev_map_append
      (fun (al, (tk, pos)) ->
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs al,
          convert_token_name tk tok pos ))
      add_tk tks
  in
  {
    LKappa.r_mix;
    r_created;
    r_editStyle;
    r_delta_tokens = List.rev tks';
    r_rate = alg_expr_of_ast ~warning ~syntax_version sigs tok algs rate;
    r_un_rate =
      (let r_dist d =
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
         un_rate);
  }

let modif_expr_of_ast ~warning ~syntax_version sigs tok algs contact_map modif
    acc =
  match modif with
  | Ast.APPLY (nb, (r, pos)) ->
    let (mix, cmix), rm_tok, add_tok, r_editStyle =
      match r.Ast.rewrite with
      | Ast.Edit e ->
        ( annotate_edit_mixture ~warning ~syntax_version:Ast.V4 ~is_rule:true
            sigs ~contact_map e.Ast.mix,
          [],
          e.Ast.delta_token,
          true )
      | Ast.Arrow a ->
        ( annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
            a.Ast.lhs a.Ast.rhs,
          a.Ast.rm_token,
          a.Ast.add_token,
          false )
    in
    let mix, cmix = Counters_compiler.remove_counter_rule sigs mix cmix in
    ( Ast.APPLY
        ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs nb,
          ( assemble_rule ~warning ~syntax_version ~r_editStyle sigs tok algs
              mix cmix rm_tok add_tok r.Ast.k_def r.Ast.k_un,
            pos ) ),
      acc )
  | Ast.UPDATE ((lab, pos), how) ->
    let i =
      match Mods.StringMap.find_option lab algs with
      | Some i -> i
      | None ->
        raise
          (ExceptionDefn.Malformed_Decl
             ("Variable " ^ lab ^ " is not defined", pos))
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
    ((alarm, pre, mods, post), pos) up_vars =
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
      if Alg_expr.has_mix (fst k) then
        ( (Locality.dummy_annot rate_var, k) :: acc,
          Locality.dummy_annot (Alg_expr.ALG_VAR rate_var) )
      else
        acc, k
    in
    acc_un, Some (k', dist)

let name_and_purify_rule ~warning ~syntax_version sigs ~contact_map
    (pack, acc, rules) (label_opt, (r, r_pos)) =
  let pack', label =
    give_rule_label r.Ast.bidirectional pack Ast.print_ast_rule r label_opt
  in
  let acc', k_def =
    if Alg_expr.has_mix (fst r.Ast.k_def) then (
      let rate_var = label ^ "_rate" in
      ( (Locality.dummy_annot rate_var, r.Ast.k_def) :: acc,
        Locality.dummy_annot (Alg_expr.ALG_VAR rate_var) )
    ) else
      acc, r.Ast.k_def
  in
  let acc'', k_un = add_un_variable r.Ast.k_un acc' (label ^ "_un_rate") in
  match r.Ast.rewrite with
  | Ast.Edit e ->
    let () =
      if r.Ast.bidirectional || r.Ast.k_op <> None || r.Ast.k_op_un <> None then
        raise
          (ExceptionDefn.Malformed_Decl
             ("Rules in edit notation cannot be bidirectional", r_pos))
    in
    let mix, created =
      annotate_edit_mixture ~warning ~syntax_version ~is_rule:true sigs
        ~contact_map e.Ast.mix
    in
    ( pack',
      acc'',
      (label_opt, true, mix, created, [], e.Ast.delta_token, k_def, k_un, r_pos)
      :: rules )
  | Ast.Arrow a ->
    let mix, created =
      annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
        a.Ast.lhs a.Ast.rhs
    in
    let rules' =
      ( label_opt,
        false,
        mix,
        created,
        a.Ast.rm_token,
        a.Ast.add_token,
        k_def,
        k_un,
        r_pos )
      :: rules
    in
    let acc''', rules'' =
      match r.Ast.bidirectional, r.Ast.k_op with
      | true, Some k when Alg_expr.has_mix (fst k) ->
        let rate_var = Ast.flip_label label ^ "_rate" in
        let rate_var_un = Ast.flip_label label ^ "_un_rate" in
        let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
        let mix, created =
          annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
            a.Ast.rhs a.Ast.lhs
        in
        ( (Locality.dummy_annot rate_var, k) :: acc_un,
          ( Option_util.map (fun (l, p) -> Ast.flip_label l, p) label_opt,
            false,
            mix,
            created,
            a.Ast.add_token,
            a.Ast.rm_token,
            Locality.dummy_annot (Alg_expr.ALG_VAR rate_var),
            k_op_un,
            r_pos )
          :: rules' )
      | true, Some rate ->
        let rate_var_un = Ast.flip_label label ^ "_un_rate" in
        let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
        let mix, created =
          annotate_lhs_with_diff ~warning ~syntax_version sigs ~contact_map
            a.Ast.rhs a.Ast.lhs
        in
        ( acc_un,
          ( Option_util.map (fun (l, p) -> Ast.flip_label l, p) label_opt,
            false,
            mix,
            created,
            a.Ast.add_token,
            a.Ast.rm_token,
            rate,
            k_op_un,
            r_pos )
          :: rules' )
      | false, None -> acc'', rules'
      | false, Some _ | true, None ->
        raise
          (ExceptionDefn.Malformed_Decl
             ( "Incompatible arrow and kinectic rate for inverse definition",
               r_pos ))
    in
    pack', acc''', rules''

let create_t sites incr_info =
  let aux, counters =
    List.fold_right
      (fun site (acc, counts) ->
        match site with
        | Ast.Port p ->
          ( ( p.Ast.port_nme,
              ( NamedDecls.create
                  (Tools.array_map_of_list
                     (function
                       | Some x, pos -> (x, pos), ()
                       | None, pos ->
                         raise
                           (ExceptionDefn.Malformed_Decl
                              ( "Forbidden internal state inside signature \
                                 definition",
                                pos )))
                     p.Ast.port_int),
                List.fold_left
                  (fun acc' -> function
                    | (LKappa.LNK_FREE | LKappa.ANY_FREE | LKappa.LNK_ANY), _ ->
                      acc'
                    | (LKappa.LNK_SOME | LKappa.LNK_VALUE _), pos ->
                      raise
                        (ExceptionDefn.Malformed_Decl
                           ( "Forbidden link status inside signature definition",
                             pos ))
                    | LKappa.LNK_TYPE (a, b), _ -> (a, b) :: acc')
                  [] p.Ast.port_lnk,
                None ) )
            :: acc,
            counts )
        | Ast.Counter c ->
          (match c.Ast.count_test with
          | None ->
            let n, pos = c.Ast.count_nme in
            raise
              (ExceptionDefn.Internal_Error
                 ("Counter " ^ n ^ " should have a test in signature", pos))
          | Some (test, pos) ->
            (match test with
            | Ast.CVAR _ ->
              raise
                (ExceptionDefn.Internal_Error
                   ("Counter should not have a var in signature", pos))
            | Ast.CGTE _ ->
              raise
                (ExceptionDefn.Internal_Error
                   ("Counter should not have >= in signature", pos))
            | Ast.CEQ j ->
              ( ( c.Ast.count_nme,
                  ( NamedDecls.create [||],
                    [ incr_info ],
                    Some (j, fst c.Ast.count_delta) ) )
                :: acc,
                c.Ast.count_nme :: counts ))))
      sites ([], [])
  in
  NamedDecls.create (Array.of_list aux), counters

let create_sig l =
  let with_contact_map =
    List.fold_left
      (fun contact -> function
        | Ast.Absent pos ->
          raise
            (ExceptionDefn.Malformed_Decl
               ("Absent agent are forbidden in signature", pos))
        | Ast.Present (_, sites, _) ->
          List.fold_left
            (fun contact' site ->
              match site with
              | Ast.Counter _ -> contact'
              | Ast.Port p ->
                contact'
                || List.fold_left
                     (fun acc -> function
                       | (LKappa.LNK_FREE | LKappa.ANY_FREE | LKappa.LNK_ANY), _
                         ->
                         acc
                       | (LKappa.LNK_SOME | LKappa.LNK_VALUE _), pos ->
                         raise
                           (ExceptionDefn.Malformed_Decl
                              ( "Forbidden link status inside a definition of \
                                 signature",
                                pos ))
                       | LKappa.LNK_TYPE (_, _), _ -> true)
                     false p.Ast.port_lnk)
            contact sites)
      false l
  in
  let annot = Locality.dummy in
  let sigs, counters =
    List.fold_right
      (fun ag (acc, counters) ->
        match ag with
        | Ast.Absent _ -> acc, counters
        | Ast.Present (name, sites, _) ->
          let lnks, counters_ag =
            create_t sites (("b", annot), ("__incr", annot))
          in
          let counters' =
            if counters_ag = [] then
              counters
            else
              (name, counters_ag) :: counters
          in
          (name, lnks) :: acc, counters')
      l ([], [])
  in
  Signature.create ~counters with_contact_map sigs

let init_of_ast ~warning ~syntax_version sigs contact_map tok algs inits =
  List.map
    (fun (expr, ini) ->
      ( alg_expr_of_ast ~warning ~syntax_version sigs tok algs expr,
        init_of_ast ~warning ~syntax_version sigs tok contact_map ini ))
    inits

let compil_of_ast ~warning ~debugMode ~syntax_version overwrite c =
  let c, with_counters = Counters_compiler.compile ~warning ~debugMode c in
  let c =
    if c.Ast.signatures = [] && c.Ast.tokens = [] then
      if with_counters then
        raise
          (ExceptionDefn.Malformed_Decl
             ("implicit signature is incompatible with counters", Locality.dummy))
      else
        Ast.implicit_signature c
    else
      c
  in
  let sigs = create_sig c.Ast.signatures in
  let contact_map =
    Array.init (Signature.size sigs) (fun i ->
        Array.init (Signature.arity sigs i) (fun s ->
            ( Tools.recti
                (fun a k -> Mods.IntSet.add k a)
                Mods.IntSet.empty
                (Signature.internal_states_number i s sigs),
              Mods.Int2Set.empty )))
  in
  let (_, rule_names), extra_vars, cleaned_rules =
    List.fold_left
      (name_and_purify_rule ~warning ~syntax_version sigs ~contact_map)
      ((0, Mods.StringSet.empty), [], [])
      c.Ast.rules
  in
  let overwrite_overwritten =
    List.fold_left (fun (over, acc) (((x, _), _) as e) ->
        match List.partition (fun (x', _) -> x = x') over with
        | [], over' -> over', e :: acc
        | [ (x, v) ], over' ->
          over', (Locality.dummy_annot x, Alg_expr.const v) :: acc
        | (x, _) :: _ :: _, _ ->
          raise
            (ExceptionDefn.Malformed_Decl
               ( "variable '" ^ x ^ "' is overwritten more than once",
                 Locality.dummy )))
  in
  let overwrite', rev_algs =
    overwrite_overwritten
      (overwrite_overwritten (overwrite, []) c.Ast.variables)
      extra_vars
  in
  let alg_vars_over =
    List_util.rev_map_append
      (fun (x, v) -> Locality.dummy_annot x, Alg_expr.const v)
      overwrite' (List.rev rev_algs)
  in
  let alg_vars_array = Array.of_list alg_vars_over in
  let algs =
    (NamedDecls.create ~forbidden:rule_names alg_vars_array).NamedDecls.finder
  in
  let tk_nd =
    NamedDecls.create (Tools.array_map_of_list (fun x -> x, ()) c.Ast.tokens)
  in
  let tok = tk_nd.NamedDecls.finder in
  let () =
    if with_counters then
      Counters_compiler.add_counter_to_contact_map sigs
        (add_link_contact_map ~contact_map)
  in
  let perts', updated_vars =
    List_util.fold_right_map
      (perturbation_of_ast ~warning ~syntax_version sigs tok algs contact_map)
      c.Ast.perturbations []
  in
  let perts'' =
    if with_counters then
      Counters_compiler.counters_perturbations sigs [ c.Ast.signatures ]
      @ perts'
    else
      perts'
  in
  let rules =
    List.rev_map
      (fun ( label,
             r_editStyle,
             mix,
             created,
             rm_tk,
             add_tk,
             rate,
             un_rate,
             r_pos ) ->
        let mix, created =
          Counters_compiler.remove_counter_rule sigs mix created
        in
        ( label,
          ( assemble_rule ~warning ~syntax_version ~r_editStyle sigs tok algs
              mix created rm_tk add_tk rate un_rate,
            r_pos ) ))
      cleaned_rules
  in
  ( sigs,
    contact_map,
    tk_nd,
    algs,
    updated_vars,
    {
      Ast.filenames = c.Ast.filenames;
      Ast.variables =
        Tools.array_fold_righti
          (fun i (lab, expr) acc ->
            ( lab,
              alg_expr_of_ast ~warning ~syntax_version ~max_allowed_var:(pred i)
                sigs tok algs expr )
            :: acc)
          alg_vars_array [];
      Ast.rules;
      Ast.observables =
        List.rev_map
          (fun expr ->
            alg_expr_of_ast ~warning ~syntax_version sigs tok algs expr)
          (List.rev c.Ast.observables);
      Ast.init =
        init_of_ast ~warning ~syntax_version sigs contact_map tok algs
          c.Ast.init;
      Ast.perturbations = perts'';
      Ast.volumes = c.Ast.volumes;
      Ast.tokens = c.Ast.tokens;
      Ast.signatures = c.Ast.signatures;
      Ast.configurations = c.Ast.configurations;
    } )
