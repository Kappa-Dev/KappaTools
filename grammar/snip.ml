open Mods

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
		    (fun f (s,a) -> Format.fprintf f "(*%a.%a*)"
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
	     ((Ast.LNK_SOME | Ast.FREE | Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) ->
	     true) then
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

let link_occurence_failure key pos =
  raise (ExceptionDefn.Internal_Error
	   ("Link "^string_of_int key^
	      " is problematic! Either Sanity.mixture is broken"^
		" or you don't use it!",pos))

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
	      internals.(p_id) <> Signature.default_internal_state ag_id p_id sigs
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

let rec annotate_lhs_with_diff sigs links_annot acc lhs rhs =
  match lhs,rhs with
  | ((lag_na,_ as ag_ty),lag_p)::lt, ((rag_na,_),rag_p)::rt
       when String.compare lag_na rag_na = 0 &&
	      Ast.no_more_site_on_right false lag_p rag_p ->
     let ra,links_annot' =
       annotate_agent_with_diff sigs ag_ty links_annot lag_p rag_p in
     annotate_lhs_with_diff sigs links_annot' (ra::acc) lt rt
  | erased, added ->
     let mix,(links_one,links_two) =
       List.fold_left (fun (acc,lannot) x ->
		       let ra,lannot' = annotate_dropped_agent sigs lannot x in
		       (ra::acc,lannot'))
		      (acc,links_annot) erased in
     let () =
       match Mods.IntMap.root links_one with
       | None -> ()
       | Some (i,(_,_,_,pos)) ->
	  raise (ExceptionDefn.Malformed_Decl
		   ("The link '"^string_of_int i^"' occurs only one time in the mixture.",
		    pos)) in
     let () = refer_links_annot links_two mix in
     mix,
     List.fold_left
       (fun (id,acc) x ->
	succ id, annotate_created_agent id sigs x::acc) (0,[]) added

let ports_from_contact_map sigs contact_map ty_id p_id =
  let ty_na = Format.asprintf "%a" (Signature.print_agent sigs) ty_id in
  let p_na = Format.asprintf "%a" (Signature.print_site sigs ty_id) p_id in
  let cand = snd (Export_to_KaSim.String2Map.find_default
		    ([],[]) (ty_na,p_na) contact_map) in
  List.map (fun (ty_na,p_na) ->
	    let ty_id =
	      Signature.num_of_agent (Location.dummy_annot ty_na) sigs in
	    (ty_id,Signature.id_of_site (Location.dummy_annot ty_na)
					(Location.dummy_annot p_na) sigs))
	   cand

let find_implicit_infos sigs contact_map ags =
  let max_s m = function
    | Linked (i,_) -> max i m
    | Freed | Maintained | Erased -> m in
  let new_switch = function
    | Maintained -> Maintained
    | Freed | Linked _ | Erased -> Freed in
  let rec aux_one ag_tail ty_id max_id ports i =
    let or_ty = (i,ty_id) in
    if i = Array.length ports
    then List.map (fun (f,a,c) -> (f,ports,a,c)) (aux_ags max_id ag_tail)
    else
     match ports.(i) with
     | (Ast.LNK_TYPE (p,a),_),s ->
	List.map (fun (free_id,ports,ags,cor) ->
		  let () =
		    ports.(i) <-
		      (Location.dummy_annot (Ast.LNK_VALUE (free_id,(p,a))),s) in
		  (succ free_id, ports, ags, (free_id,(a,p),or_ty,new_switch s)::cor))
		 (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
     | (Ast.LNK_SOME,_), s ->
	Tools.list_map_flatten
	  (fun (free_id,ports,ags,cor) ->
	   List.map (fun x ->
		     let ports' = Array.copy ports in
		     let () =
		       ports'.(i) <-
			 (Location.dummy_annot (Ast.LNK_VALUE (free_id,x)),s) in
		     (succ free_id, ports', ags, (free_id,x,or_ty,new_switch s)::cor))
		    (ports_from_contact_map sigs contact_map ty_id i))
	  (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
     | (Ast.LNK_VALUE (j,_),_),s ->
	  aux_one ag_tail ty_id (max_s (max j max_id) s) ports (succ i)
     | (Ast.FREE, pos), Maintained ->
	let () = (* Do not make test is being free is the only possibility *)
	  match ports_from_contact_map sigs contact_map ty_id i with
	  | [] -> ports.(i) <- (Ast.LNK_ANY,pos), Maintained
	  | _ :: _ -> () in
	aux_one ag_tail ty_id max_id ports (succ i)
     | (Ast.FREE, _), (Erased | Linked _ | Freed as s) -> aux_one ag_tail ty_id (max_s max_id s) ports (succ i)
     | (Ast.LNK_ANY,_), Maintained -> aux_one ag_tail ty_id max_id ports (succ i)
     | (Ast.LNK_ANY,pos), (Erased | Linked _ | Freed as s) ->
	match ports_from_contact_map sigs contact_map ty_id i with
	| [] when s = Freed ->
	   (* Do not make test is being free is the only possibility *)
	   let () = ports.(i) <- (Ast.LNK_ANY,pos), Maintained in
	   aux_one ag_tail ty_id max_id ports (succ i)
	| pfcm ->
	   Tools.list_map_flatten
	     (fun (free_id,ports,ags,cor) ->
	      let () = ports.(i) <-
			 (Location.dummy_annot Ast.FREE,
			  if s = Freed then Maintained else s) in
	      (free_id, ports, ags, cor) ::
		List.map (fun x ->
			  let ports' = Array.copy ports in
			  let () =
			    ports'.(i) <-
			      (Location.dummy_annot (Ast.LNK_VALUE (free_id,x)),s) in
			  (succ free_id, ports', ags, (free_id,x,or_ty,new_switch s)::cor))
			 pfcm)
	     (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
  and aux_ags max_id = function
    | [] -> [succ max_id,[],[]]
    | ag :: ag_tail ->
       List.map
	 (fun (free_id,ports,ags,cor) ->
	  (free_id,
	   {ra_type = ag.ra_type; ra_ports = ports; ra_ints = ag.ra_ints;
	    ra_erased = ag.ra_erased; ra_syntax = ag.ra_syntax}::ags,
	   cor)
	 )
	 (aux_one ag_tail ag.ra_type max_id ag.ra_ports 0)
  in List.rev @@ List.rev_map (fun (_,mix,todo) -> (mix,todo)) (aux_ags 0 ags)

let complete_with_candidate ag id todo p_id dst_info p_switch =
  Tools.array_fold_lefti
    (fun i acc port ->
     if i <> p_id then acc else
       match port with
       | (Ast.LNK_ANY,_), s ->
	  assert (s = Maintained);
	  let ports' = Array.copy ag.ra_ports in
	  let () = ports'.(i) <-
		     (Location.dummy_annot (Ast.LNK_VALUE (id,dst_info)),p_switch) in
	  ({ ra_type = ag.ra_type; ra_ports = ports'; ra_ints = ag.ra_ints;
	     ra_erased = ag.ra_erased; ra_syntax = ag.ra_syntax;}, todo)
	  :: acc
       | (Ast.LNK_VALUE (k,x),_),s when k > id ->
	  begin
	    match
	      List.partition
		(fun (j,(a',p'),_,sw') ->
		 j=k && i=p' && a'= ag.ra_type && sw' = p_switch) todo with
	    | [ _ ], todo' ->
	       let ports' = Array.copy ag.ra_ports in
	       let () = assert (x = dst_info) in
	       let () = ports'.(i) <-
			  (Location.dummy_annot (Ast.LNK_VALUE (id,x)),s) in
	       ({ ra_type = ag.ra_type; ra_ports = ports'; ra_ints = ag.ra_ints;
		  ra_erased = ag.ra_erased; ra_syntax = ag.ra_syntax;},
		todo') :: acc
	    |_ -> acc
	  end
       | ((Ast.LNK_VALUE _ | Ast.LNK_TYPE _ | Ast.FREE | Ast.LNK_SOME),_), _ -> acc
    ) [] ag.ra_ports

let new_agent_with_one_link sigs ty_id port link dst_info switch =
  let arity = Signature.arity sigs ty_id in
  let ports = Array.make arity (Location.dummy_annot Ast.LNK_ANY, Maintained) in
  let internals = Array.make arity I_ANY in
  let () = ports.(port) <-
	     (Location.dummy_annot (Ast.LNK_VALUE (link,dst_info)),switch) in
  { ra_type = ty_id; ra_ports = ports; ra_ints = internals;
    ra_erased = false; ra_syntax = None;}

let rec add_one_implicit_info sigs id ((ty_id,port),dst_info,s as info) todo = function
  | [] -> [[new_agent_with_one_link sigs ty_id port id dst_info s],todo]
  | ag :: ag_tail ->
     let out_tail = add_one_implicit_info sigs id info todo ag_tail in
     let extra_ags =
       if ty_id = ag.ra_type then
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
	   (List.rev_append (add_one_implicit_info sigs id (info,dst_info,s) todo' m) t)
  in aux [] l

let is_linked_on_port me i id = function
  | (Ast.LNK_VALUE (j,_),_),_ when i = j -> id <> me
  | ((Ast.LNK_VALUE _ | Ast.FREE | Ast.LNK_TYPE _ |
      Ast.LNK_ANY | Ast.LNK_SOME),_),_ -> false

let is_linked_on i ag =
  Tools.array_filter (is_linked_on_port (-1) i) ag.ra_ports <> []

let define_full_transformation
      sigs (removed,added as transf) links_transf place site dst switch =
  let cand = match dst with
    | None -> Primitives.Transformation.Freed (place,site)
    | Some (dst,_) -> Primitives.Transformation.Linked ((place,site),dst) in
  let cands l = match dst with
    | None -> Primitives.Transformation.Freed (place,site)::l
    | Some (dst,pos) ->
       let sort = Agent_place.get_type place in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "rule induces a link permutation on site '%a' of agent '%a'"
	      (Signature.print_site sigs sort) site
	      (Signature.print_agent sigs) sort) in
       Primitives.Transformation.Linked ((place,site),dst) ::
	 Primitives.Transformation.Linked (dst,(place,site)) :: l in
  match switch with
  | Freed ->
     ((cand::removed, (Primitives.Transformation.Freed(place,site)::added)),
      links_transf)
  | Maintained ->
     (transf,links_transf)
  | Erased ->
     ((cand::removed,added),links_transf)
  | Linked (i,pos) ->
     match IntMap.find_option i links_transf with
     | None ->
       let links_transf' = IntMap.add i ((place,site),dst=None) links_transf in
       ((cands removed,added),links_transf')
     | Some ((place',site' as dst'),safe) ->
       let links_transf' = IntMap.remove i links_transf in
       match dst with
       | Some (dst,_) when dst = dst' -> assert false
       | Some (_) ->
	  let () =
	    if not safe then
	      let sort = Agent_place.get_type place' in
	      ExceptionDefn.warning
		~pos
		(fun f ->
		 Format.fprintf
		   f "rule induces a link permutation on site '%a' of agent '%a'"
		   (Signature.print_site sigs sort) site'
		   (Signature.print_agent sigs) sort) in
	  ((cands removed,
	    Primitives.Transformation.Linked((place,site),dst')::added),
	   links_transf')
       | None ->
	  ((cands removed,
	    Primitives.Transformation.Linked((place,site),dst')::added),
	   links_transf')

let define_positive_transformation
      sigs (removed,added as transf) links_transf place site switch =
  match switch with
  | Freed ->
     ((removed,
       Primitives.Transformation.Freed (place,site)::added),links_transf)
  | Erased -> (transf,links_transf)
  | Maintained -> (transf,links_transf)
  | Linked (i,pos) ->
     match IntMap.find_option i links_transf with
     | None ->
       let links_transf' = IntMap.add i ((place,site),false) links_transf in
       (transf,links_transf')
     | Some (dst',_) ->
       let links_transf' = IntMap.remove i links_transf in
       let sort = Agent_place.get_type place in
       let () =
	 ExceptionDefn.warning
	   ~pos
	   (fun f ->
	    Format.fprintf
	      f "rule induces a link permutation on site '%a' of agent '%a'"
	      (Signature.print_site sigs sort) site
	      (Signature.print_agent sigs) sort) in
       ((removed,
	 Primitives.Transformation.Linked((place,site),dst')::added),
	links_transf')

let add_instantiation_free actions pl s = function
  | Freed -> Instantiation.Free (pl,s) :: actions
  | (Linked _ | Maintained | Erased) -> actions
let add_side_site side_sites bt pl s = function
  | (Freed | Linked _ | Erased) -> ((pl,s),bt)::side_sites
  | Maintained -> side_sites
let add_freed_side_effect side_effects pl s = function
  | (Ast.LNK_VALUE _,_),Freed -> (pl,s)::side_effects
  | (Ast.LNK_VALUE _,_),(Maintained | Erased | Linked _)
  | ((Ast.FREE | Ast.LNK_ANY | Ast.LNK_SOME | Ast.LNK_TYPE _),_),_ -> side_effects
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
let make_instantiation
      place links (tests,(actions,side_sites,side_effects)) ref_ports is_erased =
  function
  | None ->
     (tests,
      (actions,side_sites,add_extra_side_effects side_effects place ref_ports))
  | Some (ports, ints) ->
     let rec aux site_id tests actions side_sites side_effects links =
       if site_id < 0
       then (Instantiation.Is_Here place :: tests,
	     ((if is_erased
	       then Instantiation.Remove place :: actions
	       else actions),
	      side_sites,side_effects))
       else
	 let tests',actions' =
	   match ints.(site_id) with
	   | (I_ANY | I_ANY_ERASED) -> tests,actions
	   | I_ANY_CHANGED j ->
	      tests,
	      Instantiation.Mod_internal ((place,site_id),j) :: actions
	   | I_VAL_CHANGED (i,j) ->
	      Instantiation.Has_Internal ((place,site_id),i) :: tests,
	      if i <> j then
		Instantiation.Mod_internal ((place,site_id),j) :: actions
	      else actions
	   | I_VAL_ERASED i ->
	      Instantiation.Has_Internal ((place,site_id),i) :: tests,
	      actions in
	 let tests'',actions'',side_sites',side_effects',links' =
	   match ports.(site_id) with
	   | (Ast.LNK_ANY,_), s ->
	      let side_effects' =
		match s with
		| Maintained ->
		   add_freed_side_effect
		     side_effects place site_id ref_ports.(site_id)
		| Erased | Linked _ | Freed -> side_effects in
	      tests', add_instantiation_free actions' place site_id s,
	      add_side_site side_sites Instantiation.ANY
			    place site_id s,
	      side_effects',
	      links
	   | (Ast.FREE,_), s ->
	      (Instantiation.Is_Free (place,site_id) :: tests'),
	      add_instantiation_free actions' place site_id s,side_sites,
	      side_effects, links
	   | (Ast.LNK_SOME,_), s ->
	      Instantiation.Is_Bound (place,site_id) :: tests',
	      add_instantiation_free actions' place site_id s,
	      add_side_site side_sites Instantiation.BOUND
			    place site_id s,
	      side_effects, links
	   | (Ast.LNK_TYPE (b,a),_),s ->
	      Instantiation.Has_Binding_type ((place,site_id),(a,b))
	      :: tests',
	      add_instantiation_free actions' place site_id s,
	      add_side_site
		side_sites (Instantiation.BOUND_TYPE (a,b))
			    place site_id s,
	      side_effects, links
	   | (Ast.LNK_VALUE (i,_),_),s ->
	      match IntMap.find_option i links with
	      | Some x -> x :: tests',
		add_instantiation_free actions' place site_id s,
		side_sites, side_effects, IntMap.remove i links
	      | None ->
		tests', add_instantiation_free actions' place site_id s,
		side_sites, side_effects, links in
	 aux (pred site_id) tests'' actions'' side_sites' side_effects' links' in
     aux (pred (Array.length ports)) tests actions side_sites side_effects links

let rec add_agents_in_cc sigs id wk registered_links (removed,added as transf)
			 links_transf instantiations remains =
  function
  | [] ->
     begin match IntMap.root registered_links with
	   | None -> (wk,transf,links_transf,instantiations,remains)
	   | Some (key,_) -> link_occurence_failure key Location.dummy
     end
  | ag :: ag_l ->
     let (node,wk) = Connected_component.new_node wk ag.ra_type in
     let place = Agent_place.Existing (node,id) in
     let transf' =
       if ag.ra_erased
       then Primitives.Transformation.Agent place::removed,added
       else transf in
     let rec handle_ports wk r_l c_l (removed,added) l_t re acc site_id =
       if site_id = Array.length ag.ra_ports
       then
	 let instantiations' =
	   make_instantiation
	     place c_l instantiations ag.ra_ports ag.ra_erased ag.ra_syntax in
	 add_agents_in_cc
	   sigs id wk r_l (removed,added) l_t instantiations' re acc
       else
	 let transf,wk' = match ag.ra_ints.(site_id) with
	   | I_ANY -> (removed,added),wk
	   | I_ANY_ERASED ->
	      (Primitives.Transformation.NegativeInternalized (place,site_id)::removed,added),
	      wk
	   | I_ANY_CHANGED j ->
	      (Primitives.Transformation.NegativeInternalized (place,site_id)::removed,
	       Primitives.Transformation.PositiveInternalized (place,site_id,j)::added),
	      wk
	   | I_VAL_CHANGED (i,j) ->
	      (if i = j then (removed,added)
	       else
		 Primitives.Transformation.NegativeInternalized (place,site_id)::removed,
		 Primitives.Transformation.PositiveInternalized (place,site_id,j)::added),
		Connected_component.new_internal_state wk (node,site_id) i
	   | I_VAL_ERASED i ->
	      (Primitives.Transformation.NegativeInternalized (place,site_id)::removed,added),
	      Connected_component.new_internal_state wk (node,site_id) i
	 in
	 match ag.ra_ports.(site_id) with
	 | (Ast.LNK_ANY,_), Maintained ->
	    handle_ports wk' r_l c_l transf l_t re acc (succ site_id)
	 | (Ast.FREE,_), s ->
	    let wk'' = Connected_component.new_free wk' (node,site_id) in
	    let transf',l_t' =
	      define_full_transformation sigs transf l_t place site_id None s in
	    handle_ports
	      wk'' r_l c_l transf' l_t' re acc (succ site_id)
	 | ((Ast.LNK_SOME | Ast.LNK_TYPE _),_),_
	 | ((Ast.LNK_ANY,_), ( Erased | Linked _ | Freed))->
	    raise (ExceptionDefn.Internal_Error
		     (Location.dummy_annot
			"Try to create the connected components of an ambiguous mixture."))
	 | (Ast.LNK_VALUE (i,_),pos),s ->
	    match IntMap.find_option i r_l with
	    | Some (node',site' as dst) ->
	      let dst_place = Agent_place.Existing (node',id),site' in
	      let wk'' = Connected_component.new_link wk' (node,site_id) dst in
	      let c_l' =
		IntMap.add
		  i (Instantiation.Is_Bound_to ((place,site_id),dst_place))
		  c_l in
	      let transf',l_t' =
		define_full_transformation
		  sigs transf l_t place site_id (Some (dst_place,pos)) s in
	      handle_ports wk'' (IntMap.remove i r_l) c_l' transf'
			   l_t' re acc (succ site_id)
	    | None ->
		 match Tools.array_filter (is_linked_on_port site_id i) ag.ra_ports with
		 | [site_id'] (* link between 2 sites of 1 agent *)
		      when List.for_all (fun x -> not(is_linked_on i x)) acc &&
			     List.for_all (fun x -> not(is_linked_on i x)) re ->
		    let wk'' =
		      if site_id' > site_id then
			Connected_component.new_link
			  wk' (node,site_id) (node,site_id')
		      else wk' in
		    let transf',l_t' =
		      define_full_transformation
			sigs transf l_t place site_id
			(Some ((place,site_id'),pos)) s in
		    let transf'',l_t'' =
		      define_full_transformation
			sigs transf' l_t' place site_id'
			(Some ((place,site_id),pos)) s in
		    let c_l' =
		      IntMap.add
			i (Instantiation.Is_Bound_to ((place,site_id),(place,site_id')))
			c_l in
		    handle_ports
		      wk'' r_l c_l' transf'' l_t'' re acc (succ site_id)
		 | _ :: _ ->
		    link_occurence_failure i pos
		 | [] -> (* link between 2 agents *)
		    let r_l' = IntMap.add i (node,site_id) r_l in
		    let transf',l_t' =
		      define_positive_transformation
			sigs transf l_t place site_id s in
		    match List.partition (is_linked_on i) re with
		    | [], re' ->
		       if Tools.list_exists_uniq (is_linked_on i) acc then
		         handle_ports
			   wk' r_l' c_l transf' l_t' re' acc (succ site_id)
		       else
			 link_occurence_failure i pos
		    | [n], re' when List.for_all
				      (fun x -> not(is_linked_on i x)) acc ->
		       handle_ports
			 wk' r_l' c_l transf' l_t' re' (n::acc) (succ site_id)
		    | _, _ -> link_occurence_failure i pos
     in handle_ports wk registered_links IntMap.empty transf' links_transf remains ag_l 0

let rec complete_with_creation
	  sigs (removed,added) links_transf create_actions actions fresh =
  function
  | [] ->
     begin match IntMap.root links_transf with
	   | None -> List.rev_append create_actions actions,
		     (List.rev removed, List.rev added)
	   | Some (i,_) -> link_occurence_failure i Location.dummy
     end
  | (ag,pos) :: ag_l ->
     let place = Agent_place.Fresh (ag.Raw_mixture.a_type,fresh) in
     let rec handle_ports added l_t actions intf site_id =
       if site_id = Array.length ag.Raw_mixture.a_ports then
	 let create_actions' =
	   Instantiation.Create (place,List.rev intf)
	   :: create_actions in
	 complete_with_creation
	   sigs (removed,added) l_t create_actions' actions (succ fresh) ag_l
       else
	 let added',point =
	   match ag.Raw_mixture.a_ints.(site_id) with
	   | None -> added,(site_id,None)
	   | Some i ->
	      Primitives.Transformation.PositiveInternalized (place,site_id,i)::added,
	      (site_id,Some i) in
	 let added'',actions',l_t' =
	   match ag.Raw_mixture.a_ports.(site_id) with
	   | Raw_mixture.FREE ->
	      Primitives.Transformation.Freed (place,site_id)::added',
	      (Instantiation.Free (place,site_id) :: actions),
	      l_t
	   | Raw_mixture.VAL i ->
	      match IntMap.pop i l_t with
	      | Some ((place',site' as dst),safe),l_t' ->
		 let () =
		  if not safe then
		    let sort = Agent_place.get_type place' in
		    ExceptionDefn.warning
		      ~pos
		      (fun f ->
		       Format.fprintf
			 f "rule induces a link permutation on site '%a' of agent '%a'"
			 (Signature.print_site sigs sort) site'
			 (Signature.print_agent sigs) sort) in
		Primitives.Transformation.Linked((place,site_id),dst)::added',
		(Instantiation.Bind_to((place,site_id),dst)
		 ::(Instantiation.Bind_to((dst,(place,site_id))))::actions),
		l_t'
	      | None,l_t ->
		let l_t' = IntMap.add i ((place,site_id),true) l_t in
		(added',actions,l_t') in
	 handle_ports added'' l_t' actions' (point::intf) (succ site_id) in
     handle_ports
       (Primitives.Transformation.Agent place::added) links_transf actions [] 0

let incr_origin = function
  | ( Operator.ALG _ | Operator.PERT _  as x) -> x
  | Operator.RULE i -> Operator.RULE (succ i)

let connected_components_of_mixture created (env,origin) mix =
  let sigs = Connected_component.Env.sigs env in
  let rec aux env transformations instantiations links_transf acc id = function
    | [] ->
       let removed,added = transformations in
       let tests,(actions,side_sites,side_effects) = instantiations in
       let actions' =
	 List.fold_left
	   (fun acs -> function
		    | Primitives.Transformation.Linked (x,y)
			 when Agent_place.is_site_from_fresh x ||
				Agent_place.is_site_from_fresh y ->
		       Instantiation.Bind_to (x,y) :: acs
		    | Primitives.Transformation.Linked (x,y) ->
		       Instantiation.Bind (x,y) :: acs
		    | (Primitives.Transformation.Freed _ |
		       Primitives.Transformation.PositiveInternalized _ |
		       Primitives.Transformation.NegativeInternalized _ |
		       Primitives.Transformation.Agent _) -> acs)
	   actions added in
       let transformations' = (List.rev removed, List.rev added) in
       let actions'',transformations'' =
	 complete_with_creation
	   sigs transformations' links_transf [] actions' 0 created in
       ((env,Tools.option_map incr_origin origin),
	(origin,Tools.array_rev_of_list acc,
	 (tests,(actions'',side_sites,side_effects)), transformations''))
    | h :: t ->
       let wk = Connected_component.begin_new env in
       let (wk_out,(removed,added),l_t,event, remains) =
	 add_agents_in_cc
	   sigs id wk IntMap.empty transformations
	   links_transf instantiations t [h] in
       let (env',inj, cc) =
	 Connected_component.finish_new ?origin wk_out in
     let added' =
       Tools.list_smart_map
	 (Primitives.Transformation.rename wk_out id cc inj) added in
     let removed' =
       Tools.list_smart_map
	 (Primitives.Transformation.rename wk_out id cc inj) removed in
     let event' =
       Instantiation.rename_abstract_event wk_out id cc inj event in
     let l_t' = IntMap.map
		  (fun (((p,s),b) as x) ->
		   let p' = Agent_place.rename wk id cc inj p in
		   if p == p' then x else ((p',s),b)) l_t in
     aux env' (removed',added') event' l_t' (cc::acc) (succ id) remains
  in aux env ([],[]) ([],([],[],[]))
	 IntMap.empty [] 0 mix

let rule_mixtures_of_ambiguous_rule contact_map sigs lhs rhs =
  let precomp_mixs,created = annotate_lhs_with_diff sigs (Mods.IntMap.empty,Mods.IntMap.empty) [] lhs rhs in
  add_implicit_infos
    sigs (find_implicit_infos sigs contact_map (List.rev precomp_mixs)),
  created

let aux_connected_components_sum_of_ambiguous_rule contact_map env ?origin lhs rhs =
  let sigs = Connected_component.Env.sigs env in
  let all_mixs,(_,created) =
    rule_mixtures_of_ambiguous_rule contact_map sigs lhs rhs in
  let () =
    if !Parameter.compileModeOn then
      Format.eprintf "@[<v>_____(%i)@,%a@]@."
		     (List.length all_mixs)
		     (Pp.list
			Pp.cut
			(fun f x ->
			 Format.fprintf
			   f "@[%a%t@]"
			   (print_rule_mixture sigs) x
			   (fun f ->
			    match created with
			    | [] -> ()
			    | _ -> Format.fprintf
				     f "@ (+%t) %a" Pp.nu
				     (Raw_mixture.print sigs)
				     (List.map fst created))))
		     all_mixs in
  Tools.list_fold_right_map (connected_components_of_mixture created)
			    (env,origin) all_mixs

let connected_components_sum_of_ambiguous_rule contact_map env ?origin lhs rhs =
  aux_connected_components_sum_of_ambiguous_rule contact_map env ?origin lhs rhs
let connected_components_sum_of_ambiguous_mixture contact_map env ?origin mix =
  let (cc_env,_),rules =
    aux_connected_components_sum_of_ambiguous_rule contact_map env ?origin mix mix in
  (cc_env, List.map
	     (function _, l, (tests,_), ([],[]) -> l,tests
		     | _ -> assert false) rules)
