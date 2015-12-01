type switching =
  | Linked of int Location.annot | Freed | Maintained | Erased

type rule_internal =
  | I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int
type rule_agent =
  { ra_type: int;
    ra_erased: bool;
    ra_ports: ((int,int*int) Ast.link Location.annot * switching) array;
    ra_ints: rule_internal array;
    ra_syntax: (((int,int*int) Ast.link Location.annot * switching) array *
		  rule_internal array) option;
  }

let print_rule_internal sigs ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED j ->
     Format.fprintf f "~>>%a" (Signature.print_internal_state sigs ag_ty site) j
  | I_ANY_ERASED -> Format.fprintf f "~--"
  | I_VAL_CHANGED (i,j) ->
     if i <> j then
       Format.fprintf
	 f "~%a>>%a" (Signature.print_internal_state sigs ag_ty site) i
	 (Signature.print_internal_state sigs ag_ty site) j
     else
       Format.fprintf f "~%a" (Signature.print_internal_state sigs ag_ty site) i
  | I_VAL_ERASED i ->
     Format.fprintf
       f "~%a--" (Signature.print_internal_state sigs ag_ty site) i

let print_switching f = function
  | Linked (i,_) -> Format.fprintf f ">>%i" i
  | Freed -> Format.fprintf f ">>%t" Pp.bottom
  | Maintained -> ()
  | Erased -> Format.fprintf f "--"

let print_rule_link sigs f ((e,_),s) =
  Format.fprintf
    f "%a%a"
    (Ast.print_link Format.pp_print_int
		    (fun f (s,a) ->
		     Format.fprintf f "(*%a.%a*)"
				    (Signature.print_site sigs a) s
				    (Signature.print_agent sigs) a))
    e
    print_switching s

let print_rule_intf sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
	  | (Ast.LNK_ANY, _), Maintained ->  ints.(i) <> I_ANY
	  | ((Ast.LNK_ANY, _), (Erased | Freed | Linked _) |
	     ((Ast.LNK_SOME | Ast.FREE |
	       Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true) then
	let () = Format.fprintf
		   f "%t%a%a%a" (if empty then Pp.empty else Pp.comma)
		   (Signature.print_site sigs ag_ty) i
		   (print_rule_internal sigs ag_ty i)
		   ints.(i) (print_rule_link sigs) ports.(i) in
	aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_rule_agent sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.ra_type
		 (print_rule_intf sigs ag.ra_type) (ag.ra_ports,ag.ra_ints)

let print_rule_mixture sigs f mix =
  Pp.list Pp.comma (print_rule_agent sigs) f mix

let build_l_type sigs pos dst_ty dst_p switch =
  let ty_id = Signature.num_of_agent dst_ty sigs in
  let p_id = Signature.id_of_site dst_ty dst_p sigs in
  ((Ast.LNK_TYPE (p_id,ty_id),pos),switch)

let build_link pos i ag_ty p_id switch (links_one,links_two) =
  if Mods.IntMap.mem i links_two then
    raise (ExceptionDefn.Malformed_Decl
	     ("This is the third occurence of link '"^string_of_int i
	      ^"' in the same mixture.",pos))
  else match Mods.IntMap.pop i links_one with
       | None,one' ->
	  let new_link = match switch with
	    | Linked (j,_) -> Some j
	    | Freed | Erased | Maintained -> None in
	  ((Ast.LNK_VALUE (i,(-1,-1)),pos),switch),
	  (Mods.IntMap.add i (ag_ty,p_id,new_link,pos) one',links_two)
       | Some (dst_ty,dst_p,dst_id,_),one' ->
	  let maintained = match switch with
	    | Linked (j,_) -> Some j = dst_id
	    | Freed | Erased | Maintained -> false in
	  ((Ast.LNK_VALUE (i,(dst_p,dst_ty)),pos),
	   if maintained then Maintained else switch),
	  (one',Mods.IntMap.add i (ag_ty,p_id,maintained) links_two)

let internal_state_failure pos =
  raise (ExceptionDefn.Internal_Error
	   ("Internal state of site is problematic! Either Sanity.mixture is"^
	      "broken or you don't use it!",pos))

let site_occurence_failure ag_na (na,pos) =
  raise (ExceptionDefn.Internal_Error
	   ("Site '"^na^"' of agent '"^ag_na^
	      "' is problematic! Either Sanity.mixture is"^
		"broken or you don't use it!",pos))

let annotate_dropped_agent sigs links_annot ((agent_name, _ as ag_ty),intf) =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Location.dummy_annot Ast.LNK_ANY, Erased) in
  let internals =
    Array.init arity
               (fun i ->
		match Signature.default_internal_state ag_id i sigs with
		| None -> I_ANY | Some _ -> I_ANY_ERASED) in
  let lannot =
    List.fold_left
      (fun lannot p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () =
	 if ports.(p_id) <> (Location.dummy_annot Ast.LNK_ANY, Erased) ||
	      match Signature.default_internal_state ag_id p_id sigs with
	      | None -> internals.(p_id) <> I_ANY
	      | Some _ -> internals.(p_id) <> I_ANY_ERASED
	 then site_occurence_failure agent_name p_na in

       let () = match p.Ast.port_int with
	 | [] -> ()
	 | [ va ] ->
	    internals.(p_id) <-
	      I_VAL_ERASED (Signature.num_of_internal_state p_id va sign)
	 | _ :: (_, pos) :: _ -> internal_state_failure pos in
       match p.Ast.port_lnk with
       | (Ast.LNK_ANY, pos) ->
	  let () = ports.(p_id) <- ((Ast.LNK_ANY,pos), Erased) in lannot
       | (Ast.LNK_SOME, pos_lnk) ->
	  let (na,pos) = p.Ast.port_nme in
	  let () =
	    ExceptionDefn.warning
	      ~pos
	      (fun f ->
	       Format.fprintf
		 f "breaking a semi-link on site '%s' will induce a side effect"
		 na) in
	  let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), Erased) in lannot
       | (Ast.LNK_TYPE (dst_p, dst_ty),pos_lnk) ->
	  let (na,pos) = p.Ast.port_nme in
	  let () =
	    ExceptionDefn.warning
	      ~pos
	      (fun f ->
	       Format.fprintf
		 f "breaking a semi-link on site '%s' will induce a side effect"
		 na) in
	  let () = ports.(p_id) <-
		     build_l_type sigs pos_lnk dst_ty dst_p Erased in
	  lannot
       | (Ast.FREE, pos) ->
	  let () = ports.(p_id) <- (Ast.FREE,pos), Erased in lannot
       | (Ast.LNK_VALUE (i,()), pos) ->
	  let va,lannot' = build_link pos i ag_id p_id Erased lannot in
	  let () = ports.(p_id) <- va in lannot')
      links_annot intf in
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = true;
    ra_syntax = Some (Array.copy ports, Array.copy internals);},lannot

let annotate_created_agent id sigs ((agent_name, pos as ag_ty),intf) =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Raw_mixture.FREE) in
  let internals =
    Array.init arity
	       (fun i ->
		Signature.default_internal_state ag_id i sigs) in
  let () =
    List.iter
      (fun p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () =
	 if ports.(p_id) <> Raw_mixture.FREE ||
	      internals.(p_id) <>
		Signature.default_internal_state ag_id p_id sigs
	 then site_occurence_failure agent_name p_na in
       let () = match p.Ast.port_int with
	 | [] -> ()
	 | [ va ] ->
	    internals.(p_id) <-
	      Some (Signature.num_of_internal_state p_id va sign)
	 | _ :: (_, pos) :: _ -> internal_state_failure pos in
       match p.Ast.port_lnk with
       | ((Ast.LNK_ANY, _) | (Ast.LNK_SOME, _) | (Ast.LNK_TYPE _,_)) ->
	  site_occurence_failure agent_name p_na
       | (Ast.LNK_VALUE (i,()), _) ->  ports.(p_id) <- Raw_mixture.VAL i
       | (Ast.FREE, _) -> ()
      ) intf in
  ({ Raw_mixture.a_id = id; Raw_mixture.a_type = ag_id;
     Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; },
   pos)

let annotate_agent_with_diff sigs (agent_name, _ as ag_ty) links_annot lp rp =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Location.dummy_annot Ast.LNK_ANY, Maintained) in
  let internals = Array.make arity I_ANY in
  let register_port_modif p_id lnk1 p' links_annot =
    match lnk1,p'.Ast.port_lnk with
    | (Ast.LNK_ANY,_), (Ast.LNK_ANY,_) -> links_annot
    | (Ast.LNK_SOME,pos), (Ast.LNK_SOME,_) ->
       let () = ports.(p_id) <- ((Ast.LNK_SOME,pos), Maintained) in
       links_annot
    | (Ast.LNK_TYPE ((dst_p'',_ as dst_p),(dst_ty'',_ as dst_ty)),pos),
      (Ast.LNK_TYPE ((dst_p',_),(dst_ty',_)),_)
	 when dst_p'' = dst_p' && dst_ty'' = dst_ty' ->
       let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p Maintained in
       links_annot
    | _, (Ast.LNK_ANY,_ | Ast.LNK_SOME,_ | Ast.LNK_TYPE _,_) ->
       site_occurence_failure agent_name p'.Ast.port_nme
    | (Ast.LNK_ANY,pos), (Ast.FREE,_) ->
       let () = ports.(p_id) <- ((Ast.LNK_ANY,pos), Freed) in
       links_annot
    | (Ast.LNK_SOME,pos_lnk), (Ast.FREE,_) ->
       let (na,pos) = p'.Ast.port_nme in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "breaking a semi-link on site '%s' will induce a side effect"
	      na) in
       let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), Freed) in
       links_annot
    | (Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk), (Ast.FREE,_) ->
       let (na,pos) = p'.Ast.port_nme in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "breaking a semi-link on site '%s' will induce a side effect"
	      na) in
       let () = ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p Freed in
       links_annot
    | (Ast.FREE,pos), (Ast.FREE,_) ->
       let () = ports.(p_id) <- ((Ast.FREE,pos), Maintained) in
       links_annot
    | (Ast.LNK_VALUE (i,()),pos), (Ast.FREE,_) ->
       let va,links_annot' = build_link pos i ag_id p_id Freed links_annot in
       let () = ports.(p_id) <- va in links_annot'
    | (Ast.LNK_ANY,pos_lnk), (Ast.LNK_VALUE (i,()),pos) ->
       let () = ports.(p_id) <- ((Ast.LNK_ANY,pos_lnk), Linked (i,pos)) in
       links_annot
    | (Ast.LNK_SOME,pos_lnk), (Ast.LNK_VALUE (i,()),pos') ->
       let (na,pos) = p'.Ast.port_nme in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "breaking a semi-link on site '%s' will induce a side effect"
	      na) in
       let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), Linked (i,pos')) in
       links_annot
    | (Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk), (Ast.LNK_VALUE (i,()),pos') ->
       let (na,pos) = p'.Ast.port_nme in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "breaking a semi-link on site '%s' will induce a side effect"
	      na) in
       let () = ports.(p_id) <-
		  build_l_type sigs pos_lnk dst_ty dst_p (Linked (i,pos')) in
       links_annot
    | (Ast.FREE,pos_lnk), (Ast.LNK_VALUE (i,()),pos) ->
       let () = ports.(p_id) <- ((Ast.FREE,pos_lnk), Linked (i,pos)) in
       links_annot
    | (Ast.LNK_VALUE (i,()),pos_i), (Ast.LNK_VALUE (j,()),pos_j) ->
       let va,links_annot' =
	 build_link pos_i i ag_id p_id (Linked (j,pos_j)) links_annot in
       let () = ports.(p_id) <- va in links_annot' in
  let register_internal_modif p_id int1 p' =
    match int1,p'.Ast.port_int with
    | [], [] -> ()
    | [ va ], [ va' ] ->
       internals.(p_id) <-
	 I_VAL_CHANGED (Signature.num_of_internal_state p_id va sign,
			Signature.num_of_internal_state p_id va' sign)
    | [], [ va ] ->
       let (na,pos) = p'.Ast.port_nme in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "internal state of site '%s' of agent '%s' is modified although it is left unpecified in the left hand side"
	      na agent_name) in
       internals.(p_id) <-
	 I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
    | [ _ ], [] ->
       let (na,pos) = p'.Ast.port_nme in
       raise (ExceptionDefn.Malformed_Decl
		("The internal state of port '"^na^
		   "' is underspecified on the right hand side", pos))
    | (_ :: (_,pos) :: _, _ | _, _ :: (_,pos) :: _) ->
       internal_state_failure pos in
  let find_in_rp (na,pos) rp =
    let (p',r) =
      List.partition (fun p -> String.compare (fst p.Ast.port_nme) na = 0) rp in
    match p' with
    | [p'] -> (p',r)
    | [] -> site_occurence_failure agent_name (na,pos)
    | _ :: _ -> site_occurence_failure agent_name (na,pos) in
  let rp_r,lannot =
    List.fold_left
      (fun (rp,lannot) p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () =
	 if ports.(p_id) <> (Location.dummy_annot Ast.LNK_ANY, Maintained)
	    || internals.(p_id) <> I_ANY
	 then site_occurence_failure agent_name p_na in
       let p',rp' = find_in_rp p_na rp in
       let lannot' = register_port_modif p_id p.Ast.port_lnk p' lannot in
       let () = register_internal_modif p_id p.Ast.port_int p' in
       (rp',lannot')) (rp,links_annot) lp in
  let lannot' =
    List.fold_left
      (fun lannot p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () = register_internal_modif p_id [] p in
       register_port_modif p_id (Location.dummy_annot Ast.LNK_ANY) p lannot)
      lannot rp_r in
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = false;
    ra_syntax = Some (Array.copy ports, Array.copy internals);},lannot'

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
		      ra.ra_ports.(i) <-
			((Ast.LNK_VALUE (j,(dst_p,dst_ty)),pos),
			 if maintained then Maintained else mods)
		 end
	      | ((Ast.LNK_VALUE _ | Ast.LNK_ANY |
		       Ast.LNK_SOME | Ast.LNK_TYPE _ | Ast.FREE),_),_ -> ())
       ra.ra_ports) mix

let annotate_lhs_with_diff sigs lhs rhs =
  let rec aux links_annot acc lhs rhs =
    match lhs,rhs with
    | ((lag_na,_ as ag_ty),lag_p)::lt, ((rag_na,_),rag_p)::rt
	 when String.compare lag_na rag_na = 0 &&
		Ast.no_more_site_on_right false lag_p rag_p ->
       let ra,links_annot' =
	 annotate_agent_with_diff sigs ag_ty links_annot lag_p rag_p in
       aux links_annot' (ra::acc) lt rt
    | erased, added ->
       let mix,(links_one,links_two) =
	 List.fold_left
	   (fun (acc,lannot) x ->
	    let ra,lannot' = annotate_dropped_agent sigs lannot x in
	    (ra::acc,lannot'))
	   (acc,links_annot) erased in
       let () =
	 match Mods.IntMap.root links_one with
	 | None -> ()
	 | Some (i,(_,_,_,pos)) ->
	    raise (ExceptionDefn.Malformed_Decl
		     ("The link '"^string_of_int i^
			"' occurs only one time in the mixture.", pos)) in
       let () = refer_links_annot links_two mix in
       mix,
       List.fold_left
	 (fun (id,acc) x ->
	  succ id, annotate_created_agent id sigs x::acc) (0,[]) added in
  aux (Mods.IntMap.empty,Mods.IntMap.empty) [] lhs rhs
