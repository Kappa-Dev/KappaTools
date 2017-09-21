(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let build_l_type sigs pos dst_ty dst_p switch =
  let ty_id = Signature.num_of_agent dst_ty sigs in
  let p_id = Signature.id_of_site dst_ty dst_p sigs in
  ((Ast.LNK_TYPE (p_id,ty_id),pos),switch)

let add_link_contact_map ?contact_map sty sp dty dp =
  match contact_map with
  | None -> ()
  | Some contact_map ->
    let si, sl = contact_map.(sty).(sp) in
    let di,dl = contact_map.(dty).(dp) in
    let () = contact_map.(sty).(sp) <-
        si, List_util.merge_uniq Mods.int_pair_compare sl [dty,dp] in
    contact_map.(dty).(dp) <-
      di, List_util.merge_uniq Mods.int_pair_compare dl [sty,sp]

let build_link sigs ?contact_map pos i ag_ty p_id switch (links_one,links_two) =
  if Mods.IntMap.mem i links_two then
    raise (ExceptionDefn.Malformed_Decl
             ("This is the third occurence of link '"^string_of_int i
              ^"' in the same mixture.",pos))
  else match Mods.IntMap.pop i links_one with
    | None,one' ->
      let new_link = match switch with
        | LKappa.Linked (j,_) -> Some j
        | LKappa.Freed | LKappa.Erased | LKappa.Maintained -> None in
      ((Ast.LNK_VALUE (i,(-1,-1)),pos),switch),
      (Mods.IntMap.add i (ag_ty,p_id,new_link,pos) one',links_two)
    | Some (dst_ty,dst_p,dst_id,_),one' ->
      if Signature.allowed_link ag_ty p_id dst_ty dst_p sigs then
        let () = add_link_contact_map ?contact_map ag_ty p_id dst_ty dst_p in
        let maintained = match switch with
          | LKappa.Linked (j,_) -> Some j = dst_id
          | LKappa.Freed | LKappa.Erased | LKappa.Maintained -> false in
        ((Ast.LNK_VALUE (i,(dst_p,dst_ty)),pos),
         if maintained then LKappa.Maintained else switch),
        (one',Mods.IntMap.add i (ag_ty,p_id,maintained) links_two)
      else
        raise (ExceptionDefn.Malformed_Decl
                 (Format.asprintf
                    "Forbidden link to a %a.%a from signature declaration"
                    (Signature.print_site sigs dst_ty) dst_p
                    (Signature.print_agent sigs) dst_ty,
                  pos))

let annotate_dropped_agent
    ~syntax_version sigs links_annot (agent_name, _ as ag_ty) intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, LKappa.Erased) in
  let internals =
    Array.init arity
      (fun i ->
         match Signature.default_internal_state ag_id i sigs with
         | None -> LKappa.I_ANY | Some _ -> LKappa.I_ANY_ERASED) in
  let lannot,_ =
    List.fold_left
      (fun (lannot,pset) p ->
         let (_,p_pos as p_na) = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             LKappa.several_occurence_of_site agent_name p.Ast.port_nme in
         let () = LKappa.forbid_modification p_pos p.Ast.port_lnk_mod in
         let () = LKappa.forbid_modification p_pos p.Ast.port_int_mod in

         let () = match p.Ast.port_int with
           | [] -> ()
           | [ va ] ->
             internals.(p_id) <-
               LKappa.I_VAL_ERASED (Signature.num_of_internal_state p_id va sign)
           | _ :: (_, pos) :: _ -> LKappa.several_internal_states pos in
         match p.Ast.port_lnk with
         | [Ast.LNK_ANY, pos] ->
           let () =
             ports.(p_id) <- ((Ast.ANY_FREE,pos), LKappa.Erased) in (lannot,pset')
         | [Ast.LNK_SOME, pos_lnk] ->
           let (na,pos) = p.Ast.port_nme in
           let () =
             ExceptionDefn.warning
               ~pos
               (fun f ->
                  Format.fprintf
                    f "breaking a semi-link on site '%s' will induce a side effect"
                    na) in
           let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), LKappa.Erased) in
           (lannot,pset')
         | [Ast.LNK_TYPE (dst_p, dst_ty),pos_lnk] ->
           let (na,pos) = p.Ast.port_nme in
           let () =
             ExceptionDefn.warning
               ~pos
               (fun f ->
                  Format.fprintf
                    f "breaking a semi-link on site '%s' will induce a side effect"
                    na) in
           let () = ports.(p_id) <-
               build_l_type sigs pos_lnk dst_ty dst_p LKappa.Erased in
           (lannot,pset')
         | [Ast.ANY_FREE,_] | [] when syntax_version = Ast.V4 ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.ANY_FREE, LKappa.Erased in
           (lannot,pset')
         | [Ast.ANY_FREE,_] | [] ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.LNK_FREE, LKappa.Erased in
           (lannot,pset')
         | [Ast.LNK_FREE,_] ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.LNK_FREE, LKappa.Erased in
           (lannot,pset')
         | [Ast.LNK_VALUE (i,()), pos] ->
           let va,lannot' =
             build_link sigs pos i ag_id p_id LKappa.Erased lannot in
           let () = ports.(p_id) <- va in (lannot',pset')
         | _::(_,pos)::_ ->
           raise (ExceptionDefn.Malformed_Decl
                    ("Several link state for a single site",pos)))
      (links_annot,Mods.IntSet.empty) intf in
  let ra =
    { LKappa.ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = true;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  Counters_compiler.annotate_dropped_counters
    sign counts ra arity agent_name None,lannot

let annotate_created_agent
    ~syntax_version sigs ?contact_map rannot (agent_name, _ as ag_ty) intf =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Raw_mixture.FREE) in
  let internals =
    Array.init arity
      (fun i ->
         Signature.default_internal_state ag_id i sigs) in
  let _,rannot =
    List.fold_left
      (fun (pset,rannot) p ->
         let (_,p_pos as p_na) = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             LKappa.several_occurence_of_site agent_name p.Ast.port_nme in
         let () = LKappa.forbid_modification p_pos p.Ast.port_lnk_mod in
         let () = LKappa.forbid_modification p_pos p.Ast.port_int_mod in
         let () = match p.Ast.port_int with
           | [] -> ()
           | [ va ] ->
             internals.(p_id) <-
               Some (Signature.num_of_internal_state p_id va sign)
           | _ :: (_, pos) :: _ -> LKappa.several_internal_states pos in
         match p.Ast.port_lnk with
         | ([Ast.LNK_ANY, _] | [Ast.LNK_SOME, _] |
            [Ast.LNK_TYPE _, _] | _::_::_) ->
           LKappa.not_enough_specified agent_name p_na
         | [Ast.ANY_FREE, _] when syntax_version = Ast.V4 ->
           LKappa.not_enough_specified agent_name p_na
         | [Ast.LNK_VALUE (i,()), pos] ->
           let () = ports.(p_id) <- Raw_mixture.VAL i in
           let _,rannot' =
             build_link sigs ?contact_map pos i ag_id p_id LKappa.Freed rannot in
           pset',rannot'
         | [(Ast.ANY_FREE | Ast.LNK_FREE), _] | [] -> pset',rannot
      ) (Mods.IntSet.empty,rannot) intf in
  rannot,
  { Raw_mixture.a_type = ag_id;
    Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; }

let translate_modification sigs ?contact_map ag_id p_id
    ?warn (lhs_links,rhs_links as links_annot) = function
  | None -> LKappa.Maintained,links_annot
  | Some x ->
    let () =
      match warn with
      | None -> ()
      | Some (na,pos) ->
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
    match x with
    | None ->  LKappa.Freed,links_annot
    | Some (j,pos_j) ->
      let _,rhs_links' =
        build_link sigs ?contact_map pos_j j ag_id p_id LKappa.Freed rhs_links in
      LKappa.Linked (j,pos_j),(lhs_links,rhs_links')

let annotate_edit_agent
    ~syntax_version ~is_rule sigs ?contact_map (agent_name, _ as ag_ty)
    links_annot intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, LKappa.Maintained) in
  let internals = Array.make arity LKappa.I_ANY in
  let scan_port (links_annot,pset) p =
    let (p_na,_) = p.Ast.port_nme in
    let p_id = Signature.num_of_site ~agent_name p.Ast.port_nme sign in
    let pset' = Mods.IntSet.add p_id pset in
    let () = if pset == pset' then
        LKappa.several_occurence_of_site agent_name p.Ast.port_nme in
    let links_annot' =
      match p.Ast.port_lnk with
      | [Ast.LNK_SOME, pos as x] ->
        let (modif,links_annot') = translate_modification
            ~warn:(p_na,pos) sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- (x, modif) in
        links_annot'
      | [(Ast.LNK_ANY, pos)] ->
        let (modif,links_annot') = translate_modification
            ~warn:(p_na,pos) sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- ((Ast.ANY_FREE,pos), modif) in
        links_annot'
      | ([] | [Ast.ANY_FREE, _]) when syntax_version = Ast.V4 ->
        let (modif,links_annot') = translate_modification
            ~warn:p.Ast.port_nme sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- (Locality.dummy_annot Ast.ANY_FREE, modif) in
        links_annot'
      | [Ast.LNK_TYPE (dst_p,dst_ty), pos] ->
        let (modif,links_annot') = translate_modification
            ~warn:(p_na,pos) sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p modif in
        links_annot'
      | ([] | [(Ast.LNK_FREE | Ast.ANY_FREE), _]) ->
        let (modif,links_annot') = translate_modification
            ?warn:None sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- (Locality.dummy_annot Ast.LNK_FREE, modif) in
        links_annot'
      | [Ast.LNK_VALUE (i,()), pos] ->
        let (modif,(lhs_links,rhs_links)) = translate_modification
            ?warn:None sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let va,lhs_links' =
          build_link
            sigs ?contact_map:(if is_rule then None else contact_map)
            pos i ag_id p_id modif lhs_links in
        let () = ports.(p_id) <- va in
        (lhs_links',rhs_links)
      | _::(_,pos)::_ ->
        raise (ExceptionDefn.Malformed_Decl
                 ("Several link state for a single site",pos)) in
    let () =
      match p.Ast.port_int,p.Ast.port_int_mod with
      | [], None -> ()
      | [ va ], Some va' ->
        internals.(p_id) <-
          LKappa.I_VAL_CHANGED (Signature.num_of_internal_state p_id va sign,
                         Signature.num_of_internal_state p_id va' sign)
      | [], Some (_,pos as va) ->
        let () =
          ExceptionDefn.warning
            ~pos
            (fun f ->
               Format.fprintf
                 f
                 "internal state of site '%s' of agent '%s' is modified \
                  although it is left unpecified in the left hand side"
                 p_na agent_name) in
        internals.(p_id) <-
          LKappa.I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
      | [ va ], None ->
        let i_id = Signature.num_of_internal_state p_id va sign in
        internals.(p_id) <- LKappa.I_VAL_CHANGED (i_id,i_id)
      | _ :: (_,pos) :: _, _ -> LKappa.several_internal_states pos in
    (links_annot',pset') in
  let annot',_ =
    List.fold_left scan_port (links_annot,Mods.IntSet.empty) intf in
  let ra =
    { LKappa.ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  Counters_compiler.annotate_edit_counters
    sigs ag_ty counts ra (add_link_contact_map ?contact_map),annot'

let annotate_agent_with_diff
    sigs ?contact_map (agent_name,_ as ag_ty) links_annot lp rp lc rc =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, LKappa.Maintained) in
  let internals = Array.make arity LKappa.I_ANY in
  let register_port_modif p_id lnk1 p' (lhs_links,rhs_links as links_annot) =
    let () = LKappa.forbid_modification (snd p'.Ast.port_nme) p'.Ast.port_lnk_mod in
    match lnk1,p'.Ast.port_lnk with
    | [Ast.LNK_ANY,pos], [Ast.LNK_ANY,_] ->
      let () = ports.(p_id) <- ((Ast.ANY_FREE,pos), LKappa.Maintained) in
      links_annot
    | [Ast.LNK_SOME,pos], [Ast.LNK_SOME,_] ->
      let () = ports.(p_id) <- ((Ast.LNK_SOME,pos), LKappa.Maintained) in
      links_annot
    | [Ast.LNK_TYPE ((dst_p'',_ as dst_p),(dst_ty'',_ as dst_ty)),pos],
      [Ast.LNK_TYPE ((dst_p',_),(dst_ty',_)),_]
      when dst_p'' = dst_p' && dst_ty'' = dst_ty' ->
      let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p LKappa.Maintained in
      links_annot
    | _, ([Ast.LNK_ANY,_] | [Ast.LNK_SOME,_] | [Ast.LNK_TYPE _,_]) ->
      LKappa.not_enough_specified agent_name p'.Ast.port_nme
    | [Ast.LNK_ANY,pos], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let () = ports.(p_id) <- ((Ast.LNK_ANY,pos), LKappa.Freed) in
      links_annot
    | [Ast.LNK_SOME,pos_lnk], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), LKappa.Freed) in
      links_annot
    | [Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk],
      ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p LKappa.Freed in
      links_annot
    | ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []),
      ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let () =
        ports.(p_id) <- (Locality.dummy_annot Ast.LNK_FREE, LKappa.Maintained) in
      links_annot
    | [Ast.LNK_VALUE (i,()),pos], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let va,lhs_links' =
        build_link sigs pos i ag_id p_id LKappa.Freed lhs_links in
      let () = ports.(p_id) <- va in (lhs_links',rhs_links)
    | [Ast.LNK_ANY,pos_lnk], [Ast.LNK_VALUE (i,()),pos] ->
      let () = ports.(p_id) <- ((Ast.LNK_ANY,pos_lnk), LKappa.Linked (i,pos)) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos i ag_id p_id LKappa.Freed rhs_links in
      lhs_links,rhs_links'
    | [Ast.LNK_SOME,pos_lnk], [Ast.LNK_VALUE (i,()),pos'] ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), LKappa.Linked (i,pos')) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos' i ag_id p_id LKappa.Freed rhs_links in
      lhs_links,rhs_links'
    | [Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk], [Ast.LNK_VALUE (i,()),pos'] ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <-
          build_l_type sigs pos_lnk dst_ty dst_p (LKappa.Linked (i,pos')) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos' i ag_id p_id LKappa.Freed rhs_links in
      lhs_links,rhs_links'
    | ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []), [Ast.LNK_VALUE (i,()),pos] ->
      let () =
        ports.(p_id) <- (Locality.dummy_annot Ast.LNK_FREE, LKappa.Linked (i,pos)) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos i ag_id p_id LKappa.Freed rhs_links in
      lhs_links,rhs_links'
    | [Ast.LNK_VALUE (i,()),pos_i], [Ast.LNK_VALUE (j,()),pos_j] ->
      let va,lhs_links' = build_link
          sigs pos_i i ag_id p_id (LKappa.Linked (j,pos_j)) lhs_links in
      let _,rhs_links' =
        build_link sigs ?contact_map pos_j j ag_id p_id LKappa.Freed rhs_links in
      let () = ports.(p_id) <- va in (lhs_links',rhs_links')
    | _::(_,pos)::_, _ ->
      raise (ExceptionDefn.Malformed_Decl
               ("Several link state for a single site",pos))
    | _, _::(_,pos)::_ ->
      raise (ExceptionDefn.Malformed_Decl
               ("Several link state for a single site",pos)) in
  let register_internal_modif p_id int1 p' =
    let () = LKappa.forbid_modification (snd p'.Ast.port_nme) p'.Ast.port_int_mod in
    match int1,p'.Ast.port_int with
    | [], [] -> ()
    | [ va ], [ va' ] ->
      internals.(p_id) <-
        LKappa.I_VAL_CHANGED (Signature.num_of_internal_state p_id va sign,
                       Signature.num_of_internal_state p_id va' sign)
    | [], [ va ] ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f
               "internal state of site '%s' of agent '%s' is modified \
although it is left unpecified in the left hand side"
               na agent_name) in
      internals.(p_id) <-
        LKappa.I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
    | [ _ ], [] ->
      let (na,pos) = p'.Ast.port_nme in
      raise (ExceptionDefn.Malformed_Decl
               ("The internal state of port '"^na^
                "' is underspecified on the right hand side", pos))
    | (_ :: (_,pos) :: _, _ | _, _ :: (_,pos) :: _) ->
      LKappa.several_internal_states pos in
  let find_in_r (na,pos) rp =
    let (p',r) =
      List.partition (fun p -> String.compare (fst p.Ast.port_nme) na = 0) rp in
    match p' with
    | [p'] -> (p',r)
    | [] -> LKappa.not_enough_specified agent_name (na,pos)
    | _ :: _ -> LKappa.several_occurence_of_site agent_name (na,pos) in
  let rp_r,annot,_ =
    List.fold_left
      (fun (rp,annot,pset) p ->
         let (_,p_pos as p_na) = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             LKappa.several_occurence_of_site agent_name p.Ast.port_nme in
         let () = LKappa.forbid_modification p_pos p.Ast.port_lnk_mod in
         let () = LKappa.forbid_modification p_pos p.Ast.port_int_mod in

         let p',rp' = find_in_r p_na rp in
         let annot' = register_port_modif
             p_id p.Ast.port_lnk p' annot in
         let () = register_internal_modif p_id p.Ast.port_int p' in
         (rp',annot',pset')) (rp,links_annot,Mods.IntSet.empty) lp in
  let annot' =
    List.fold_left
      (fun annot p ->
         let p_na = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let () = register_internal_modif p_id [] p in
         register_port_modif p_id [Locality.dummy_annot Ast.LNK_ANY] p annot)
      annot rp_r in

  let ra =
    { LKappa.ra_type = ag_id; ra_ports = ports; ra_ints = internals;ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  Counters_compiler.annotate_counters_with_diff
    sigs ag_ty lc rc ra (add_link_contact_map ?contact_map),annot'

let refer_links_annot links_annot mix =
  List.iter
    (fun ra ->
       Array.iteri
         (fun i -> function
            | (Ast.LNK_VALUE (j,(-1,-1)),pos),mods ->
              begin
                match Mods.IntMap.find_option j links_annot with
                | None -> ()
                | Some (dst_ty,dst_p,maintained) ->
                  ra.LKappa.ra_ports.(i) <-
                    ((Ast.LNK_VALUE (j,(dst_p,dst_ty)),pos),
                     if maintained then LKappa.Maintained else mods)
              end
            | ((Ast.LNK_VALUE _ | Ast.LNK_ANY | Ast.LNK_SOME
               | Ast.LNK_TYPE _ | Ast.LNK_FREE | Ast.ANY_FREE),_),_ -> ())
         ra.LKappa.ra_ports) mix

let separate_sites ls =
  let (a,b) =
    List.fold_left
      (fun (ps,cs) -> function
        | Ast.Port p -> (p::ps,cs)
        | Ast.Counter c -> (ps,c::cs)) ([],[]) ls in
  (List.rev a,b)

(*
Is responsible for the check that:
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice
*)
let annotate_lhs_with_diff sigs ?contact_map lhs rhs =
  let rec aux links_annot acc lhs rhs =
    match lhs,rhs with
    | ((lag_na,lpos as ag_ty),lag_s,lmod)::lt, ((rag_na,rpos),rag_s,rmod)::rt
      when String.compare lag_na rag_na = 0 &&
           Ast.no_more_site_on_right true lag_s rag_s ->
      let () = LKappa.forbid_modification lpos lmod in
      let () = LKappa.forbid_modification rpos rmod in
      let (lag_p,lag_c) = separate_sites lag_s in
      let (rag_p,rag_c) = separate_sites rag_s in
      let ra,links_annot' =
        annotate_agent_with_diff
          sigs ?contact_map ag_ty links_annot lag_p rag_p lag_c rag_c in
      aux links_annot' (ra::acc) lt rt
    | erased, added ->
      let () =
        if added <> [] then
          List.iter (fun ((lag,pos),lag_p,_) ->
              if List.exists
                  (fun ((rag,_),rag_p,_) ->
                     String.compare lag rag = 0 &&
                     Ast.no_more_site_on_right false lag_p rag_p) added then
                ExceptionDefn.warning ~pos
                  (fun f ->
                     Format.fprintf
                       f "Rule induced deletion AND creation of the agent %s"
                       lag))
            erased in
      let syntax_version=Ast.V3 in
      let mix,(lhs_links_one,lhs_links_two) =
        List.fold_left
          (fun (acc,lannot) ((_,pos as na),sites,modif) ->
             let () = LKappa.forbid_modification pos modif in
             let intf,counts = separate_sites sites in
             let ra,lannot' = annotate_dropped_agent
                 ~syntax_version sigs lannot na intf counts in
             (ra::acc,lannot'))
          (acc,fst links_annot) erased in
      let () =
        match Mods.IntMap.root lhs_links_one with
        | None -> ()
        | Some (i,(_,_,_,pos)) -> LKappa.link_only_one_occurence i pos in
      let () = refer_links_annot lhs_links_two (List.map (fun r -> r.LKappa.ra) mix) in
      let cmix,(rhs_links_one,_) =
        List.fold_left
          (fun (acc,rannot) ((_,pos as na),sites,modif) ->
             let () = LKappa.forbid_modification pos modif in
             let intf,counts = separate_sites sites in
             let rannot',x' = annotate_created_agent
                 ~syntax_version sigs ?contact_map rannot na intf in
             let x'' =
               Counters_compiler.annotate_created_counters
                 sigs na counts (add_link_contact_map ?contact_map) x' in
             x''::acc,rannot')
          ([],snd links_annot) added in
      let () =
        match Mods.IntMap.root rhs_links_one with
        | None -> ()
        | Some (i,(_,_,_,pos)) -> LKappa.link_only_one_occurence i pos in
      List.rev mix, List.rev cmix in
  aux
    ((Mods.IntMap.empty,Mods.IntMap.empty),(Mods.IntMap.empty,Mods.IntMap.empty))
    [] lhs rhs

let annotate_edit_mixture ~syntax_version ~is_rule sigs ?contact_map m =
  let ((lhs_links_one,lhs_links_two),(rhs_links_one,_)),mix,cmix =
    List.fold_left
      (fun (lannot,acc,news) (ty,sites,modif) ->
         let (intf,counts) = separate_sites sites in
         match modif with
         | None ->
           let a,lannot' = annotate_edit_agent
               ~syntax_version ~is_rule sigs
               ?contact_map ty lannot intf counts in
           (lannot',a::acc,news)
         | Some Ast.Create ->
           let rannot',x' = annotate_created_agent
               ~syntax_version sigs ?contact_map (snd lannot) ty intf in
           let x'' =
              Counters_compiler.annotate_created_counters
               sigs ty counts (add_link_contact_map ?contact_map) x' in
           ((fst lannot,rannot'),acc,x''::news)
         | Some Ast.Erase ->
           let ra,lannot' = annotate_dropped_agent
               ~syntax_version sigs (fst lannot) ty intf counts in
           ((lannot',snd lannot),ra::acc,news))
      (((Mods.IntMap.empty,Mods.IntMap.empty),
        (Mods.IntMap.empty,Mods.IntMap.empty)),[],[])
      m in
  let () =
    match Mods.IntMap.root lhs_links_one with
    | None -> ()
    | Some (i,(_,_,_,pos)) -> LKappa.link_only_one_occurence i pos in
  let () = refer_links_annot lhs_links_two (List.map (fun r -> r.LKappa.ra) mix) in
  let () =
    match Mods.IntMap.root rhs_links_one with
    | None -> ()
    | Some (i,(_,_,_,pos)) -> LKappa.link_only_one_occurence i pos in
  (List.rev mix, List.rev cmix)

let give_rule_label bidirectional (id,set) printer r = function
  | None ->
    (succ id,set), Format.asprintf "r%i: %a" id printer r
  | Some (lab,pos) ->
    let set' = Mods.StringSet.add lab set in
    if set == set' then
      raise
        (ExceptionDefn.Malformed_Decl
           ("A rule named '"^lab^"' already exists.",pos))
    else if bidirectional then
      let set'' =
        Mods.StringSet.add (Ast.flip_label lab) set' in
      if set' == set'' then
        raise
          (ExceptionDefn.Malformed_Decl
             ("A rule named '"^Ast.flip_label lab^"' already exists.",pos))
      else (id,set''),lab
    else (id,set'),lab

let add_un_variable k_un acc rate_var =
  match k_un with
  | None -> (acc,None)
  | Some (k,dist) ->
    let acc_un,k' = if Alg_expr.has_mix (fst k) then
        ((Locality.dummy_annot rate_var,k)::acc,
         Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
      else (acc,k) in
    (acc_un,Some (k',dist))

let name_and_purify_edit_rule (label_opt,(r,r_pos)) (pack,acc,rules) =
  let pack',label =
    give_rule_label false pack Ast.print_ast_edit_rule r label_opt in
  let acc',act =
    if Alg_expr.has_mix (fst r.Ast.act) then
      let rate_var = label^"_rate" in
      ((Locality.dummy_annot rate_var,r.Ast.act)::acc,
       Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
    else (acc,r.Ast.act) in
  let acc'',un_act = add_un_variable r.Ast.un_act acc' (label^"_un_rate") in
  (pack',acc'',
   (label_opt,
    {Ast.mix = r.Ast.mix; Ast.delta_token = r.Ast.delta_token;
     Ast.act; Ast.un_act},r_pos)::rules)

let name_and_purify_rule (label_opt,(r,r_pos)) (pack,acc,rules) =
  let pack',label = give_rule_label
      r.Ast.bidirectional pack Ast.print_ast_rule r label_opt in
  let acc',k_def =
    if Alg_expr.has_mix (fst r.Ast.k_def) then
      let rate_var = label^"_rate" in
      ((Locality.dummy_annot rate_var,r.Ast.k_def)::acc,
       Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
    else (acc,r.Ast.k_def) in
  let acc'',k_un = add_un_variable r.Ast.k_un acc' (label^"_un_rate") in
  let acc''',rules' =
    match r.Ast.bidirectional,r.Ast.k_op with
    | true, Some k when Alg_expr.has_mix (fst k) ->
      let rate_var = (Ast.flip_label label)^"_rate" in
      let rate_var_un = (Ast.flip_label label)^"_un_rate" in
      let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
      ((Locality.dummy_annot rate_var,k)::acc_un,
       (Option_util.map (fun (l,p) -> (Ast.flip_label l,p)) label_opt,
        r.Ast.rhs,r.Ast.lhs,r.Ast.add_token,r.Ast.rm_token,
        Locality.dummy_annot (Alg_expr.ALG_VAR rate_var),k_op_un,r_pos)::rules)
    | true, Some rate ->
      let rate_var_un = (Ast.flip_label label)^"_un_rate" in
      let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
      (acc_un,
       (Option_util.map (fun (l,p) -> (Ast.flip_label l,p)) label_opt,
        r.Ast.rhs,r.Ast.lhs,r.Ast.add_token,r.Ast.rm_token,
        rate,k_op_un,r_pos)::rules)
    | false, None -> (acc'',rules)
    | (false, Some _ | true, None) ->
       raise
         (ExceptionDefn.Malformed_Decl
            ("Incompatible arrow and kinectic rate for inverse definition",
             r_pos)) in
  (pack',acc''',
   (label_opt,r.Ast.lhs,r.Ast.rhs,r.Ast.rm_token,r.Ast.add_token,
    k_def,k_un,r_pos)
   ::rules')

let mixture_of_ast ~syntax_version sigs ?contact_map ~with_counters pos mix =
  match annotate_edit_mixture
          ~syntax_version ~is_rule:false sigs ?contact_map mix with
  | r, [] -> fst (Counters_compiler.remove_counter_rule sigs with_counters r [])
  | _, _ -> raise (ExceptionDefn.Internal_Error
                     ("A mixture cannot create agents",pos))

let raw_mixture_of_ast ~syntax_version sigs ?contact_map ~with_counters mix =
  let created =
    List.map (fun (ty,sites,_) -> (ty,sites, Some Ast.Create)) mix in
  let (a,b) =
    annotate_edit_mixture
      ~syntax_version ~is_rule:false sigs ?contact_map created in
  snd (Counters_compiler.remove_counter_rule sigs with_counters a b)

let convert_alg_var ?max_allowed_var algs lab pos =
  let i =
    match Mods.StringMap.find_option lab algs with
    | Some x -> x
    | None ->
      raise (ExceptionDefn.Malformed_Decl
               (lab ^" is not a declared variable",pos)) in
  let () =
    match max_allowed_var with
    | Some j when j < i ->
      raise (ExceptionDefn.Malformed_Decl
               ("Reference to not yet defined '"^lab ^"' is forbidden.",
                pos))
    | None | Some _ -> ()
  in
  i

let convert_token_name tk_nme tok pos =
  match Mods.StringMap.find_option tk_nme tok with
  | Some x -> x
  | None ->
    raise (ExceptionDefn.Malformed_Decl
             (tk_nme ^ " is not a declared token",pos))

let rec alg_expr_of_ast
    ~syntax_version sigs tok algs ?max_allowed_var ~with_counters (alg,pos) =
  ((match alg with
      | Alg_expr.KAPPA_INSTANCE ast ->
        Alg_expr.KAPPA_INSTANCE
          (mixture_of_ast ~syntax_version sigs ~with_counters pos ast)
      | Alg_expr.ALG_VAR lab ->
        Alg_expr.ALG_VAR (convert_alg_var ?max_allowed_var algs lab pos)
      | Alg_expr.TOKEN_ID tk_nme ->
        Alg_expr.TOKEN_ID (convert_token_name tk_nme tok pos)
      | Alg_expr.DIFF_KAPPA_INSTANCE(expr,ast) ->
        Alg_expr.DIFF_KAPPA_INSTANCE
          (alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var expr ~with_counters,
           mixture_of_ast ~syntax_version sigs pos ast ~with_counters)
      | Alg_expr.DIFF_TOKEN(expr,tk_nme) ->
        Alg_expr.DIFF_TOKEN
          (alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var expr ~with_counters,
           convert_token_name tk_nme tok pos)
      | (Alg_expr.STATE_ALG_OP _ | Alg_expr.CONST _) as x -> x
      | Alg_expr.BIN_ALG_OP (op, a, b) ->
        Alg_expr.BIN_ALG_OP
          (op,
           alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var a ~with_counters,
           alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var b ~with_counters)
      | Alg_expr.UN_ALG_OP (op,a) ->
        Alg_expr.UN_ALG_OP
          (op,alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var a ~with_counters)
      | Alg_expr.IF (cond,yes,no) ->
        Alg_expr.IF
          (bool_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var ~with_counters cond,
           alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var yes ~with_counters,
           alg_expr_of_ast
             ~syntax_version sigs tok algs ?max_allowed_var no ~with_counters)
    ),
   pos)
and bool_expr_of_ast ~syntax_version sigs tok algs ?max_allowed_var ~with_counters = function
  | (Alg_expr.TRUE | Alg_expr.FALSE),_ as x -> x
  | Alg_expr.BIN_BOOL_OP (op,x,y),pos ->
    Alg_expr.BIN_BOOL_OP
      (op, bool_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var ~with_counters x,
       bool_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var ~with_counters y),
    pos
  | Alg_expr.UN_BOOL_OP (op,x),pos ->
    Alg_expr.UN_BOOL_OP
      (op, bool_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var ~with_counters x),
    pos
  | Alg_expr.COMPARE_OP (op,x,y),pos ->
    Alg_expr.COMPARE_OP
      (op,alg_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var  x ~with_counters,
       alg_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var y ~with_counters),pos

let print_expr_of_ast ~syntax_version ~with_counters sigs tok algs = function
  | Primitives.Str_pexpr _ as x -> x
  | Primitives.Alg_pexpr x ->
    Primitives.Alg_pexpr
      (alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters x)

let modif_expr_of_ast
    ~syntax_version sigs tok algs contact_map ~with_counters modif acc =
  match modif with
  | Ast.INTRO (how,(who,pos)) ->
    Ast.INTRO
      (alg_expr_of_ast ~syntax_version sigs tok algs how ~with_counters,
       (raw_mixture_of_ast
          ~syntax_version sigs ~contact_map who ~with_counters,pos)),
    acc
  | Ast.DELETE (how,(who,pos)) ->
    Ast.DELETE
      (alg_expr_of_ast ~syntax_version sigs tok algs how ~with_counters,
       (mixture_of_ast ~syntax_version sigs pos who ~with_counters,pos)),
    acc
  | Ast.UPDATE ((lab,pos),how) ->
    let i =
      match Mods.StringMap.find_option lab algs with
      | Some i -> i
      | None ->
        raise (ExceptionDefn.Malformed_Decl
                 ("Variable " ^ (lab ^ " is not defined"),pos)) in
    Ast.UPDATE
      ((i,pos),
       alg_expr_of_ast ~syntax_version sigs tok algs how ~with_counters),
    i::acc
  | Ast.UPDATE_TOK ((lab,pos),how) ->
    let i =
      match Mods.StringMap.find_option lab tok with
      | Some x -> x
      | None ->
        raise (ExceptionDefn.Malformed_Decl
                 (lab ^" is not a declared token",pos)) in
    Ast.UPDATE_TOK
      ((i,pos),
       alg_expr_of_ast ~syntax_version sigs tok algs how ~with_counters),
    acc
  | Ast.STOP p ->
    Ast.STOP
      (List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p),acc
  | Ast.SNAPSHOT p ->
    Ast.SNAPSHOT
      (List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p),acc
  | Ast.FLUX (rel,p) ->
    Ast.FLUX
      (rel,
       List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p),acc
  | Ast.FLUXOFF p ->
    Ast.FLUXOFF
      (List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p),acc
  | (Ast.PLOTENTRY | Ast.CFLOWLABEL (_,_ ) as x) -> x,acc
  | Ast.PRINT (p,p') ->
    Ast.PRINT
      (List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p,
       List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p'),
    acc
  | Ast.CFLOWMIX (b,(m,pos)) ->
    Ast.CFLOWMIX
      (b,(mixture_of_ast ~syntax_version sigs pos m ~with_counters,pos)),acc
  | Ast.SPECIES_OF (b,p,(m,pos)) ->
    Ast.SPECIES_OF
      (b,List.map
         (print_expr_of_ast ~syntax_version sigs tok algs ~with_counters) p,
       (mixture_of_ast ~syntax_version sigs pos ~with_counters m,pos)),acc

let perturbation_of_ast
    ~syntax_version sigs tok algs contact_map ~with_counters
    ((alarm,pre,mods,post),pos) up_vars =
  let mods',up_vars' =
    List_util.fold_right_map
      (modif_expr_of_ast
         ~syntax_version sigs tok algs contact_map ~with_counters)
      mods up_vars in
  let max_allowed_var = None in
  ((alarm,
    Option_util.map
      (bool_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var ~with_counters)
      pre,mods',
    Option_util.map
      (bool_expr_of_ast
         ~syntax_version sigs tok algs ?max_allowed_var ~with_counters)
      post),pos),
  up_vars'

let init_of_ast ~syntax_version sigs tok contact_map ~with_counters = function
  | Ast.INIT_MIX who,pos ->
    Ast.INIT_MIX
      (raw_mixture_of_ast
         ~syntax_version sigs ~contact_map who ~with_counters),pos
  | Ast.INIT_TOK lab,pos ->
    match Mods.StringMap.find_option lab tok with
    | Some x -> Ast.INIT_TOK x,pos
    | None ->
      raise (ExceptionDefn.Malformed_Decl
               (lab ^" is not a declared token",pos))

let assemble_rule ~syntax_version ~r_editStyle ~with_counters
    sigs tk_nd algs r_mix r_created rm_tk add_tk rate un_rate =
  let tok = tk_nd.NamedDecls.finder in
  let tks =
    List.rev_map (fun (al,tk) ->
        (alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters
           (Locality.dummy_annot (Alg_expr.UN_ALG_OP (Operator.UMINUS,al))),
         NamedDecls.elt_id ~kind:"token" tk_nd tk))
      rm_tk in
  let tks' =
    List_util.rev_map_append (fun (al,tk) ->
          (alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters al,
           NamedDecls.elt_id ~kind:"token" tk_nd tk))
      add_tk tks in
  { LKappa.r_mix; r_created; r_editStyle;
    r_delta_tokens = List.rev tks';
    r_rate = alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters rate;
    r_un_rate =
      let r_dist d =
        alg_expr_of_ast
          ~syntax_version sigs tok algs ?max_allowed_var:None ~with_counters d in
      Option_util.map
        (fun (un_rate',dist) ->
           let un_rate'' =
             alg_expr_of_ast ~syntax_version sigs tok algs
               ?max_allowed_var:None ~with_counters un_rate' in
           match dist with
           | Some d -> (un_rate'', Some (r_dist d))
           | None -> (un_rate'', None))
        un_rate;
  }

let create_t sites incr_info =
  let (aux,counters) =
    List.fold_right
    (fun site (acc,counts) ->
      match site with
      | Ast.Port p ->
         (p.Ast.port_nme,
          (NamedDecls.create
             (Tools.array_map_of_list (fun x -> (x,())) p.Ast.port_int),
           List.fold_left
             (fun acc' -> function
               | (Ast.LNK_FREE | Ast.ANY_FREE | Ast.LNK_ANY), _ -> acc'
               | (Ast.LNK_SOME | Ast.LNK_VALUE _), pos ->
                  raise
                    (ExceptionDefn.Malformed_Decl
                       ("Forbidden link status inside a definition of signature",
                        pos))
               | Ast.LNK_TYPE (a,b), _ -> (a,b) :: acc')
             [] p.Ast.port_lnk,
           None))::acc,counts
      | Ast.Counter c ->
         match c.Ast.count_test with
         | None ->
            let (n,pos) = c.Ast.count_nme in
            raise (ExceptionDefn.Internal_Error
                     ("Counter "^n^" should have a test by now",pos))
         | Some (test,pos) ->
            match test with
            | Ast.CGTE _ | Ast.CVAR _ ->
               raise (ExceptionDefn.Internal_Error
                        ("Counter should not have a var by now",pos))
            | Ast.CEQ j ->
               (c.Ast.count_nme,
                (NamedDecls.create [||],
                 [incr_info],
                 Some (j,(fst c.Ast.count_delta))))::acc,
          c.Ast.count_nme::counts)
    sites ([],[]) in
  NamedDecls.create (Array.of_list aux),counters

let create_sig l =
  let (with_counters,with_contact_map) =
    List.fold_left
      (fun (count,contact) (_,sites,_) ->
        List.fold_left
          (fun (count',contact') site ->
            match site with
            | Ast.Counter _ -> (true,contact')
            | Ast.Port p ->
               let contact'' =
                 List.fold_left (fun acc -> function
                 | (Ast.LNK_FREE | Ast.ANY_FREE | Ast.LNK_ANY), _ -> acc
                 | (Ast.LNK_SOME | Ast.LNK_VALUE _), pos ->
                 raise
                   (ExceptionDefn.Malformed_Decl
                      ("Forbidden link status inside a definition of signature",
                       pos))
                 | Ast.LNK_TYPE (_,_), _ -> true) false p.Ast.port_lnk in
               (count',contact''||contact')) (count,contact) sites)
      (false,false) l in
  let annot = Locality.dummy in
  let (sigs,counters) =
    List.fold_right
      (fun (name,sites,_) (acc,counters) ->
        let (lnks,counters') =
          create_t sites (("b",annot),("__incr",annot)) in
        ((name,lnks)::acc,(name,counters')::counters))
       l ([],[]) in
  let sigs' = if with_counters then (Counters_compiler.add_incr counters)::sigs else sigs in
  let t = Array.of_list sigs' in
  Signature.create with_contact_map t

let compil_of_ast ~syntax_version overwrite c =
  let (c,with_counters) = Counters_compiler.compile c in
  let c =
    if c.Ast.signatures = [] && c.Ast.tokens = []
    then
      if with_counters then
        raise (ExceptionDefn.Malformed_Decl
                 ("implicit signature is incompatible with counters",
                  Locality.dummy))
      else Ast.implicit_signature c
    else c in
  let sigs = create_sig c.Ast.signatures in
  let contact_map =
    Array.init
      (Signature.size sigs)
      (fun i -> Array.init (Signature.arity sigs i)
          (fun s -> (Tools.recti
                       (fun a k -> k::a) []
                       (Signature.internal_states_number i s sigs),[]))) in
  let (name_pack,var_pack,cleaned_rules) =
    List.fold_right
      name_and_purify_rule c.Ast.rules ((0,Mods.StringSet.empty),[],[]) in
  let ((_,rule_names),extra_vars,cleaned_edit_rules) =
    List.fold_right
      name_and_purify_edit_rule c.Ast.edit_rules (name_pack,var_pack,[]) in
  let alg_vars_over =
    List_util.rev_map_append
      (fun (x,v) -> (Locality.dummy_annot x,
                     Alg_expr.const v)) overwrite
      (List.filter
         (fun ((x,_),_) ->
            List.for_all (fun (x',_) -> x <> x') overwrite)
         (c.Ast.variables@extra_vars)) in
  let algs =
    (NamedDecls.create
       ~forbidden:rule_names (Array.of_list alg_vars_over)).NamedDecls.finder in
  let tk_nd = NamedDecls.create
      (Tools.array_map_of_list (fun x -> (x,())) c.Ast.tokens) in
  let tok = tk_nd.NamedDecls.finder in
  let () = if with_counters then
             Counters_compiler.add_counter_to_contact_map
               sigs (add_link_contact_map ~contact_map) in
  let perts',updated_vars =
    List_util.fold_right_map
      (perturbation_of_ast
         ~syntax_version sigs tok algs contact_map ~with_counters)
      c.Ast.perturbations [] in
  let perts'' =
    if with_counters then
      (Counters_compiler.counters_perturbations sigs c.Ast.signatures)@perts'
    else perts' in
  let old_style_rules =
    List.map (fun (label,lhs,rhs,rm_tk,add_tk,rate,un_rate,r_pos) ->
        let mix,created = annotate_lhs_with_diff sigs ~contact_map lhs rhs in
        let mix,created =
          Counters_compiler.remove_counter_rule sigs with_counters mix created in
        label,
        (assemble_rule
           ~syntax_version ~r_editStyle:false ~with_counters
           sigs tk_nd algs mix created rm_tk add_tk rate un_rate,
         r_pos))
      cleaned_rules in
  let edit_rules =
    List.rev_map (fun (label,r,r_pos) ->
        let mix,cmix = annotate_edit_mixture
            ~syntax_version:Ast.V4 ~is_rule:true sigs ~contact_map r.Ast.mix in
        let mix,cmix =
          Counters_compiler.remove_counter_rule sigs with_counters mix cmix in
        (label,
           (assemble_rule
              ~syntax_version:Ast.V4 ~r_editStyle:true ~with_counters
              sigs tk_nd algs mix cmix [] r.Ast.delta_token
              r.Ast.act r.Ast.un_act,r_pos)))
      cleaned_edit_rules in
  let edit_rules = List.rev_append edit_rules old_style_rules in
  sigs,contact_map,tk_nd,algs,updated_vars,
  {
    Ast.filenames = c.Ast.filenames;
    Ast.variables =
      List_util.mapi
        (fun i (lab,expr) ->
           (lab,alg_expr_of_ast
              ~syntax_version ~max_allowed_var:(pred i) ~with_counters
              sigs tok algs expr))
        alg_vars_over;
    Ast.rules = [];
    Ast.edit_rules;
    Ast.observables =
      List.map (fun expr ->
          alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters expr)
        c.Ast.observables;
    Ast.init =
      List.map (fun (lab,expr,ini) ->
          lab,alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters expr,
          init_of_ast ~syntax_version sigs tok contact_map ~with_counters ini)
        c.Ast.init;
    Ast.perturbations = perts'';
    Ast.volumes = c.Ast.volumes;
    Ast.tokens = c.Ast.tokens;
    Ast.signatures = c.Ast.signatures;
    Ast.configurations = c.Ast.configurations;
  }

let init_of_ast ~syntax_version sigs contact_map ~with_counters tok algs inits =
  List.map (fun (lab,expr,ini) ->
      lab,alg_expr_of_ast ~syntax_version sigs tok algs ~with_counters expr,
      init_of_ast ~syntax_version sigs tok contact_map ~with_counters ini)
    inits
