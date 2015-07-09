open Mods

type switching =
  | Linked of int Term.with_pos | Freed | Maintained
  | Erased

type rule_internal =
    I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int
type rule_link =
    L_ANY of switching
  | L_FREE of switching
  | L_SOME of switching
  | L_TYPE of int * int * switching (** ty_id,p_id,switch *)
  | L_VAL of int Term.with_pos * switching
type rule_agent =
    { ra_type: int;
      ra_ports: rule_link array;
      ra_ints: rule_internal array;
      ra_syntax: (rule_link array * rule_internal array) option;
    }
type rule_mixture = rule_agent list

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

let print_rule_link f = function
  | L_ANY s ->
     Format.fprintf f "?%a" print_switching s
  | L_FREE s -> Format.fprintf f "%a" print_switching s
  | L_SOME s -> Format.fprintf f "!_%a" print_switching s
  | L_TYPE (ag,p,s) -> Format.fprintf f "!%i.%i%a"
				      p ag
				      print_switching s
  | L_VAL ((i,_),s) -> Format.fprintf f "!%i%a" i print_switching s

let print_rule_intf sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (ports.(i) <> L_ANY Maintained || ints.(i) <> I_ANY)
	 && (i <> 0 || ports.(i) <> L_FREE Maintained) then
	let () = Format.fprintf
		   f "%t%a%a%a" (if empty then Pp.empty else Pp.comma)
		   (Signature.print_site sigs ag_ty) i
		   (print_rule_internal sigs ag_ty i)
		   ints.(i) print_rule_link ports.(i) in
	aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_rule_agent sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.ra_type
		 (print_rule_intf sigs ag.ra_type) (ag.ra_ports,ag.ra_ints)

let print_rule_mixture sigs f mix =
  Pp.list Pp.comma (print_rule_agent sigs) f mix

let build_l_type sigs dst_ty dst_p switch =
  let ty_id = Signature.num_of_agent dst_ty sigs in
  let p_id = Signature.id_of_site dst_ty dst_p sigs in
  L_TYPE (ty_id,p_id,switch)

let several_internal_states pos =
  raise (ExceptionDefn.Malformed_Decl
	   ("In a pattern, a site cannot have several internal state ",pos))

let too_much_or_not_enough too_much agent_name (na,pos) =
  let spec = if too_much then "over" else "under" in
  raise (ExceptionDefn.Malformed_Decl
	   ("The link status of agent '"^agent_name^"', site '"^na
	    ^"' on the right hand side is "^spec^"specified",pos))

let annotate_dropped_agent sigs ((agent_name, _ as ag_ty),intf) =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.init arity (fun i -> if i = 0 then L_FREE Erased else L_ANY Erased) in
  let internals =
    Array.init arity
               (fun i ->
		match Signature.default_internal_state ag_id i sigs with
		| None -> I_ANY | Some _ -> I_ANY_ERASED) in
  let () =
    List.iter
      (fun p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () = match p.Ast.port_int with
	 | [] -> ()
	 | [ va ] ->
	    internals.(p_id) <-
	      I_VAL_ERASED (Signature.num_of_internal_state p_id va sign)
	 | _ :: (_, pos) :: _ -> several_internal_states pos in
       match p.Ast.port_lnk with
       | (Ast.LNK_ANY, _) -> ports.(p_id) <- L_ANY Erased
       | (Ast.LNK_SOME, _) -> ports.(p_id) <- L_SOME Erased
       | (Ast.LNK_TYPE (dst_p, dst_ty),_) ->
	  ports.(p_id) <- build_l_type sigs dst_ty dst_p Erased
       | (Ast.LNK_VALUE i, pos) -> ports.(p_id) <- L_VAL ((i,pos),Erased)
       | (Ast.FREE, _) -> ports.(p_id) <- L_FREE Erased
      ) intf in
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals;
    ra_syntax = Some (ports, internals);}

let annotate_created_agent id sigs ((agent_name, _ as ag_ty),intf) =
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
       let () = match p.Ast.port_int with
	 | [] -> ()
	 | [ va ] ->
	    internals.(p_id) <-
	      Some (Signature.num_of_internal_state p_id va sign)
	 | _ :: (_, pos) :: _ -> several_internal_states pos in
       match p.Ast.port_lnk with
       | (Ast.LNK_ANY, _) -> ()
       | ((Ast.LNK_SOME, _) | (Ast.LNK_TYPE _,_)) ->
	  too_much_or_not_enough false agent_name p_na
       | (Ast.LNK_VALUE i, _) ->  ports.(p_id) <- Raw_mixture.VAL i
       | (Ast.FREE, _) -> ()
      ) intf in
  { Raw_mixture.a_id = id; Raw_mixture.a_type = ag_id;
    Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; }

let annotate_agent_with_diff sigs (agent_name, _ as ag_ty) lp rp =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.init
      arity (fun i -> if i = 0 then L_FREE Maintained else L_ANY Maintained) in
  let internals = Array.make arity I_ANY in
  let register_port_modif p_id lnk1 p' =
    match lnk1,p'.Ast.port_lnk with
    | (Ast.LNK_ANY,_), (Ast.LNK_ANY,_) -> ()
    | (Ast.LNK_SOME,_), (Ast.LNK_SOME,_) -> ports.(p_id) <- L_SOME Maintained
    | (Ast.LNK_TYPE ((dst_p'',_ as dst_p),(dst_ty'',_ as dst_ty)),_),
      (Ast.LNK_TYPE ((dst_p',_),(dst_ty',_)),_)
	 when dst_p'' = dst_p' && dst_ty'' = dst_ty' ->
       ports.(p_id) <- build_l_type sigs dst_ty dst_p Maintained
    | _, (Ast.LNK_ANY,_ | Ast.LNK_SOME,_ | Ast.LNK_TYPE _,_) ->
       too_much_or_not_enough false agent_name p'.Ast.port_nme
    | (Ast.LNK_ANY,_), (Ast.FREE,_) -> ports.(p_id) <- L_ANY Freed
    | (Ast.LNK_SOME,_), (Ast.FREE,_) -> ports.(p_id) <- L_SOME Freed
    | (Ast.LNK_TYPE (dst_p,dst_ty),_), (Ast.FREE,_) ->
       ports.(p_id) <- build_l_type sigs dst_ty dst_p Freed
    | (Ast.FREE,_), (Ast.FREE,_) -> ports.(p_id) <- L_FREE Maintained
    | (Ast.LNK_VALUE i,pos), (Ast.FREE,_) ->
       ports.(p_id) <- L_VAL ((i,pos),Freed)
    | (Ast.LNK_ANY,_), (Ast.LNK_VALUE i,pos) ->
       ports.(p_id) <- L_ANY (Linked (i,pos))
    | (Ast.LNK_SOME,_), (Ast.LNK_VALUE i,pos) ->
       ports.(p_id) <- L_SOME (Linked (i,pos))
    | (Ast.LNK_TYPE (dst_p,dst_ty),_), (Ast.LNK_VALUE i,pos) ->
       ports.(p_id) <- build_l_type sigs dst_ty dst_p (Linked (i,pos))
    | (Ast.FREE,_), (Ast.LNK_VALUE i,pos) ->
       ports.(p_id) <- L_FREE (Linked (i,pos))
    | (Ast.LNK_VALUE i,pos_i), (Ast.LNK_VALUE j,pos_j) ->
       ports.(p_id) <- L_VAL ((i,pos_i),Linked (j,pos_j)) in
  let register_internal_modif p_id int1 p' =
    match int1,p'.Ast.port_int with
    | [], [] -> ()
    | [ va ], [ va' ] ->
       internals.(p_id) <-
	 I_VAL_CHANGED (Signature.num_of_internal_state p_id va sign,
			Signature.num_of_internal_state p_id va' sign)
    | [], [ va ] ->
       internals.(p_id) <-
	 I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
    | [ _ ], [] ->
       let (na,pos) = p'.Ast.port_nme in
       raise (ExceptionDefn.Malformed_Decl
		("The internal state of port '"^na^
		   "' is underspecified on the right hand side", pos))
    | (_ :: (_,pos) :: _, _ | _, _ :: (_,pos) :: _) ->
       several_internal_states pos
  in
  let find_in_rp (na,pos) rp =
    let (p',r) =
      List.partition (fun p -> String.compare (fst p.Ast.port_nme) na = 0) rp in
    match p' with
    | [p'] -> (p',r)
    | l -> too_much_or_not_enough (l <> []) agent_name (na,pos) in
  let rp_r =
    List.fold_left
      (fun rp p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let p',rp' = find_in_rp p_na rp in
       let () = register_port_modif p_id p.Ast.port_lnk p' in
       let () = register_internal_modif p_id p.Ast.port_int p' in
       rp') rp lp in
  let () =
    List.iter
      (fun p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () = register_internal_modif p_id [] p in
       register_port_modif p_id (Term.with_dummy_pos Ast.LNK_ANY) p) rp_r in
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals;
    ra_syntax = Some (ports, internals);}

let rec annotate_lhs_with_diff sigs acc lhs rhs =
  match lhs,rhs with
  | ((lag_na,_ as ag_ty),lag_p)::lt, ((rag_na,_),rag_p)::rt
       when String.compare lag_na rag_na = 0 ->
     annotate_lhs_with_diff
       sigs (annotate_agent_with_diff sigs ag_ty lag_p rag_p::acc) lt rt
  | erased, added ->
     List.fold_left (fun acc x ->
		     annotate_dropped_agent sigs x::acc)
		    acc erased,
     List.fold_left
       (fun (id,acc) x ->
	succ id, annotate_created_agent id sigs x::acc) (0,[]) added


let ports_from_contact_map sigs contact_map ty_id p_id =
  let ty_na = Format.asprintf "%a" (Signature.print_agent sigs) ty_id in
  let p_na = Format.asprintf "%a" (Signature.print_site sigs ty_id) p_id in
  let cand =
    try snd (Export_to_KaSim.String2Map.find (ty_na,p_na) contact_map)
    with Not_found -> [] in
  List.map (fun (ty_na,p_na) ->
	    let ty_id =
	      Signature.num_of_agent (Term.with_dummy_pos ty_na) sigs in
	    (ty_id,Signature.id_of_site (Term.with_dummy_pos ty_na)
					(Term.with_dummy_pos p_na) sigs))
	   cand

let internals_from_contact_map sigs contact_map ty_id p_id =
  let sign = Signature.get sigs ty_id in
  let ty_na = Format.asprintf "%a" (Signature.print_agent sigs) ty_id in
  let p_na = Format.asprintf "%a" (Signature.print_site sigs ty_id) p_id in
  let cand =
    try fst (Export_to_KaSim.String2Map.find (ty_na,p_na) contact_map)
    with Not_found -> [] in
  List.map
    (fun i_na ->
     Signature.num_of_internal_state p_id (Term.with_dummy_pos i_na) sign)
    cand

let find_implicit_infos sigs contact_map ags =
  let max_s m = function
    | Linked (i,_) -> max i m
    | Freed | Maintained | Erased -> m in
  let rec aux_internals ty_id ints acc i =
    if i = Array.length ints then acc
    else
      let acc' =
	match ints.(i) with
	| (I_ANY | I_VAL_CHANGED _ | I_VAL_ERASED _) -> acc
	| I_ANY_CHANGED j ->
	   Tools.list_map_flatten
	     (fun ints' ->
	      List.map (fun x ->
			let ints'' = Array.copy ints' in
			let () = ints''.(i) <- I_VAL_CHANGED (x,j) in
			ints'')
		       (internals_from_contact_map sigs contact_map ty_id i))
	     acc
	| I_ANY_ERASED ->
	   Tools.list_map_flatten
	     (fun ints' ->
	      List.map (fun x ->
			let ints'' = Array.copy ints' in
			let () = ints''.(i) <- I_VAL_ERASED x in
			ints'')
		       (internals_from_contact_map sigs contact_map ty_id i))
	     acc in
      aux_internals ty_id ints acc' (succ i) in
  let new_switch free_id = function
    | Maintained -> Linked (Term.with_dummy_pos free_id)
    | Freed | Linked _ | Erased -> Freed in
  let old_switch free_id = function
    | Maintained -> Linked (Term.with_dummy_pos free_id)
    | Freed | Linked _ | Erased as s -> s in
  let rec aux_one ag_tail ty_id max_id ports i =
    if i = Array.length ports
    then List.map (fun (f,a,c) -> (f,ports,a,c)) (aux_ags max_id ag_tail)
    else
     match ports.(i) with
     | L_TYPE (a,p,s) ->
	List.map (fun (free_id,ports,ags,cor) ->
		  let () =
		    ports.(i) <- L_VAL (Term.with_dummy_pos free_id,old_switch free_id s) in
		  (succ free_id, ports, ags, (free_id,(a,p),new_switch free_id s)::cor))
		 (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
     | L_SOME s ->
	Tools.list_map_flatten
	  (fun (free_id,ports,ags,cor) ->
	   List.map (fun x ->
		     let ports' = Array.copy ports in
		     let () =
		       ports'.(i) <- L_VAL (Term.with_dummy_pos free_id,old_switch free_id s) in
		     (succ free_id, ports', ags, (free_id,x,new_switch free_id s)::cor))
		    (ports_from_contact_map sigs contact_map ty_id i))
	  (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
     | L_VAL ((j,_),s) ->
	  aux_one ag_tail ty_id (max_s (max j max_id) s) ports (succ i)
     | L_FREE s -> aux_one ag_tail ty_id (max_s max_id s) ports (succ i)
     | L_ANY Maintained -> aux_one ag_tail ty_id max_id ports (succ i)
     | L_ANY (Erased | Linked _ | Freed as s) ->
	Tools.list_map_flatten
	  (fun (free_id,ports,ags,cor) ->
	   let () = ports.(i) <- L_FREE (if s = Freed then Maintained else s) in
	   (free_id, ports, ags, cor) ::
	     List.map (fun x ->
		       let ports' = Array.copy ports in
		       let () =
			 ports'.(i) <- L_VAL (Term.with_dummy_pos free_id,old_switch free_id s) in
		       (succ free_id, ports', ags, (free_id,x,new_switch free_id s)::cor))
		      (ports_from_contact_map sigs contact_map ty_id i))
	  (aux_one ag_tail ty_id (max_s max_id s) ports (succ i))
  and aux_ags max_id = function
    | [] -> [succ max_id,[],[]]
    | ag :: ag_tail ->
       Tools.list_map_flatten
	 (fun (free_id,ports,ags,cor) ->
	  List.map (fun ints ->
		    (free_id,
		     {ra_type = ag.ra_type; ra_ports = ports; ra_ints = ints;
		      ra_syntax = ag.ra_syntax}::ags,
		     cor))
		   (aux_internals ag.ra_type ag.ra_ints [ag.ra_ints] 0)
	 )
	 (aux_one ag_tail ag.ra_type max_id ag.ra_ports 0)
  in List.map (fun (_,mix,todo) -> (mix,todo)) (aux_ags 0 ags)

let complete_with_candidate ag id todo p_id p_switch =
  Tools.array_fold_lefti
    (fun i acc port ->
     if i <> p_id then acc else
       match port with
       | L_ANY s ->
	  assert (s = Maintained);
	  let ports' = Array.copy ag.ra_ports in
	  let () = ports'.(i) <- L_VAL (Term.with_dummy_pos id,p_switch) in
	  ({ ra_type = ag.ra_type; ra_ports = ports'; ra_ints = ag.ra_ints;
	     ra_syntax = ag.ra_syntax;}, todo)
	  :: acc
       | L_VAL ((k,_),s) when k > id ->
	  begin
	    match
	      List.partition
		(fun (j,(a',p'),sw') ->
		 j=k && i=p' && a'= ag.ra_type && sw' = p_switch) todo with
	    | [ _ ], todo' ->
	       let ports' = Array.copy ag.ra_ports in
	       let () = ports'.(i) <- L_VAL (Term.with_dummy_pos id,s) in
	       ({ ra_type = ag.ra_type; ra_ports = ports'; ra_ints = ag.ra_ints;
		  ra_syntax = ag.ra_syntax;},
		todo') :: acc
	    |_ -> acc
	  end
       | (L_VAL _ | L_TYPE _ | L_FREE _ | L_SOME _) -> acc
    ) [] ag.ra_ports

let new_agent_with_one_link sigs ty_id port link switch =
  let arity = Signature.arity sigs ty_id in
  let ports =
    Array.init
      arity (fun i -> if i = 0 then L_FREE Maintained else L_ANY Maintained) in
  let internals = Array.make arity I_ANY in
  let () = ports.(port) <- L_VAL (Term.with_dummy_pos link,switch) in
  { ra_type = ty_id; ra_ports = ports; ra_ints = internals;
    ra_syntax = None;}

let rec add_one_implicit_info sigs id ((ty_id,port),s as info) todo = function
  | [] -> [[new_agent_with_one_link sigs ty_id port id s],todo]
  | ag :: ag_tail ->
     let out_tail = add_one_implicit_info sigs id info todo ag_tail in
     let extra_ags =
       if ty_id = ag.ra_type then
	 (List.map
	    (fun (ag',todo') -> ag'::ag_tail,todo')
	    (complete_with_candidate ag id todo port s))
       else [] in
     List.fold_left (fun l (x,todo') -> ((ag::x,todo')::l)) extra_ags out_tail

let add_implicit_infos sigs l =
  let rec aux acc = function
    | [] -> acc
    | (m,[]) :: t -> aux (m::acc) t
    | (m,((id,info,s) :: todo')) :: t ->
       aux acc
	   (List.rev_append (add_one_implicit_info sigs id (info,s) todo' m) t)
  in aux [] l

let is_linked_on_port me i id = function
  | L_VAL ((j,_),_) when i = j -> id <> me
  | (L_VAL _ | L_FREE _ | L_TYPE _ | L_ANY _ | L_SOME _) -> false

let is_linked_on i ag =
  Tools.array_filter (is_linked_on_port (-1) i) ag.ra_ports <> []

let dangling_link side key =
  raise (ExceptionDefn.Malformed_Decl
	   (Term.with_dummy_pos ("At least link "^string_of_int key^
				   " is dangling on the " ^side)))

let define_full_transformation
      (removed,added as transf) links_transf
      place site dst switch =
  let cand = match dst with
    | None -> Primitives.Transformation.Freed (place,site)
    | Some dst -> Primitives.Transformation.Linked ((place,site),dst) in
  let cands l = match dst with
    | None -> Primitives.Transformation.Freed (place,site)::l
    | Some dst ->
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
  | Linked (i,_) ->
     try
       let dst' = IntMap.find i links_transf in
       let links_transf' = IntMap.remove i links_transf in
       if Some dst' = dst then
	 (transf,links_transf')
       else
	 ((cands removed,
	   Primitives.Transformation.Linked((place,site),dst')::added),
	  links_transf')
     with Not_found ->
       let links_transf' = IntMap.add i ((place,site)) links_transf in
       ((cands removed,added),links_transf')

let define_positive_transformation (removed,added as transf) links_transf
				   place site switch =
  match switch with
  | Freed ->
     ((removed,Primitives.Transformation.Freed (place,site)::added),links_transf)
  | Erased ->
     (transf,links_transf)
  | Maintained -> assert false
  | Linked (i,_) ->
     try
       let dst' = IntMap.find i links_transf in
       let links_transf' = IntMap.remove i links_transf in
       ((removed,
	 Primitives.Transformation.Linked((place,site),dst')::added),
	links_transf')
     with Not_found ->
       let links_transf' = IntMap.add i ((place,site)) links_transf in
       (transf,links_transf')

let make_instantiation place links acc = function
  | None -> acc,None
  | Some (ports, ints) ->
     let rec aux site_id acc links =
       if site_id < 0
       then Primitives.Instantiation.Is_Here place :: acc
       else
	 let test =
	   match ints.(site_id) with
	   | (I_ANY | I_ANY_ERASED | I_ANY_CHANGED _) -> acc
	   | (I_VAL_CHANGED (i,_) | I_VAL_ERASED i) ->
	      Primitives.Instantiation.Has_Internal ((place,site_id),i) :: acc
	 in
	 let test',links' =
	   match ports.(site_id) with
	   | L_ANY _ -> test,links
	   | L_FREE _ ->
	      Primitives.Instantiation.Is_Free (place,site_id) :: test,links
	   | L_SOME _ ->
	      Primitives.Instantiation.Is_Bound (place,site_id) :: test,links
	   | L_TYPE (a,b,_) ->
	      Primitives.Instantiation.Has_Binding_type ((place,site_id),(a,b))
	      :: test,links
	   | L_VAL ((i,_),_) ->
	      try IntMap.find i links :: test, IntMap.remove i links
	      with Not_found -> test, links in
	 aux (pred site_id) test' links' in
     (aux (pred (Array.length ports)) acc links,
      match ports.(0) with
      | L_FREE Erased -> Some place
      | L_FREE (Maintained | Linked _ | Freed)
      | L_ANY _ | L_SOME _ | L_TYPE _ | L_VAL _ -> None)


let rec add_agents_in_cc id wk registered_links transf links_transf
			 instantiations remains =
  function
  | [] ->
     begin match IntMap.root registered_links with
	   | None -> (wk,transf,links_transf,instantiations,remains)
	   | Some (key,_) -> dangling_link "left" key
     end
  | ag :: ag_l ->
     let (node,wk) = Connected_component.new_node wk ag.ra_type in
     let place = Primitives.Place.Existing (node,id) in
     let rec handle_ports wk r_l c_l (removed,added) l_t re acc site_id =
       if site_id = Array.length ag.ra_ports
       then
	 let tests,actions = instantiations in
	 let tests',delete = make_instantiation place c_l tests ag.ra_syntax in
	 let actions' = match delete with
	     None -> actions
	   | Some p -> Primitives.Instantiation.Remove p :: actions in
	 add_agents_in_cc id wk r_l (removed,added) l_t (tests',actions') re acc
       else
	 let transf,wk' = match ag.ra_ints.(site_id) with
	   | I_ANY -> (removed,added),wk
	   | I_VAL_CHANGED (i,j) ->
	      (if i = j then (removed,added)
	       else Primitives.Transformation.Internalized (place,site_id,i)::removed,
		    Primitives.Transformation.Internalized (place,site_id,j)::added),
		Connected_component.new_internal_state wk (node,site_id) i
	   | I_VAL_ERASED i ->
	      (Primitives.Transformation.Internalized (place,site_id,i)::removed,added),
	      Connected_component.new_internal_state wk (node,site_id) i
	   | (I_ANY_ERASED | I_ANY_CHANGED _) ->
	      raise (ExceptionDefn.Internal_Error
		       (Term.with_dummy_pos
			  "Try to create the connected components of an ambiguous mixture."))
	 in
	 match ag.ra_ports.(site_id) with
	 | L_ANY Maintained ->
	    handle_ports wk' r_l c_l transf l_t re acc (succ site_id)
	 | L_FREE s ->
	    let wk'' = if site_id = 0 then wk'
		       else Connected_component.new_free wk' (node,site_id) in
	    let transf',l_t' =
	      define_full_transformation transf l_t place site_id None s in
	    handle_ports
	      wk'' r_l c_l transf' l_t' re acc (succ site_id)
	 | (L_SOME _ | L_TYPE _ | L_ANY ( Erased | Linked _ | Freed))->
	    raise (ExceptionDefn.Internal_Error
		     (Term.with_dummy_pos
			"Try to create the connected components of an ambiguous mixture."))
	 | L_VAL ((i,pos),s) ->
	    try
	      let (node',site' as dst) = IntMap.find i r_l in
	      let dst_place = Primitives.Place.Existing (node',id),site' in
	      let wk'' = Connected_component.new_link wk' (node,site_id) dst in
	      let c_l' =
		IntMap.add
		  i (Primitives.Instantiation.Is_Bound_to ((place,site_id),dst_place))
		  c_l in
	      let transf',l_t' =
		define_full_transformation
		  transf l_t place site_id (Some dst_place) s in
	      handle_ports wk'' (IntMap.remove i r_l) c_l' transf'
			   l_t' re acc (succ site_id)
	    with Not_found ->
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
			transf l_t place site_id (Some (place,site_id')) s in
		    let transf'',l_t'' =
		      define_full_transformation
			transf' l_t' place site_id' (Some (place,site_id)) s in
		    let c_l' =
		      IntMap.add
			i (Primitives.Instantiation.Is_Bound_to ((place,site_id),(place,site_id')))
			c_l in
		    handle_ports
		      wk'' r_l c_l' transf'' l_t'' re acc (succ site_id)
		 | _ :: _ ->
		    raise (ExceptionDefn.Malformed_Decl
			     ("There are more than two sites using link "^
				string_of_int i,pos))
		 | [] -> (* link between 2 agents *)
		    let r_l' = IntMap.add i (node,site_id) r_l in
		    let transf',l_t' =
		      define_positive_transformation
			transf l_t place site_id s in
		    match List.partition (is_linked_on i) re with
		    | [], re'
			 when Tools.list_exists_uniq (is_linked_on i) acc ->
		       handle_ports wk' r_l' c_l transf' l_t' re' acc (succ site_id)
		    | [n], re' when List.for_all
				      (fun x -> not(is_linked_on i x)) acc ->
		       handle_ports wk' r_l' c_l transf' l_t' re' (n::acc) (succ site_id)
		    | _, _ ->
		       raise (ExceptionDefn.Malformed_Decl
				("There are more than two agents using link "^
				   string_of_int i,pos))
     in handle_ports wk registered_links IntMap.empty transf links_transf remains ag_l 0

let rec complete_with_creation (removed,added) links_transf actions fresh =
  function
  | [] ->
     begin match IntMap.root links_transf with
	   | None -> actions,(List.rev removed, List.rev added)
	   | Some (i,_) -> dangling_link "right" i
     end
  | ag :: ag_l ->
     let place = Primitives.Place.Fresh (ag.Raw_mixture.a_type,fresh) in
     let rec handle_ports added l_t actions site_id =
       if site_id = Array.length ag.Raw_mixture.a_ports then
	 let actions' = Primitives.Instantiation.Create (place,[]) :: actions in
	 complete_with_creation (removed,added) l_t actions' (succ fresh) ag_l
       else
	 let added' =
	   match ag.Raw_mixture.a_ints.(site_id) with
	   | None -> added
	   | Some i ->
	      Primitives.Transformation.Internalized (place,site_id,i)::added in
	 let added'',actions',l_t' =
	   match ag.Raw_mixture.a_ports.(site_id) with
	   | Raw_mixture.FREE ->
	      Primitives.Transformation.Freed (place,site_id)::added',actions,
	      l_t
	   | Raw_mixture.VAL i ->
	      try
		let dst = IntMap.find i l_t in
		let l_t' = IntMap.remove i l_t in
		Primitives.Transformation.Linked((place,site_id),dst)::added',
		(Primitives.Instantiation.Bind((place,site_id),dst)::actions),
		l_t'
	      with Not_found ->
		let l_t' = IntMap.add i ((place,site_id)) l_t in
		(added',actions,l_t') in
	 handle_ports added'' l_t' actions' (succ site_id) in
     handle_ports added links_transf actions 0

let connected_components_of_mixture created id_incr (env,rule_id) mix =
  let rec aux env transformations instantiations links_transf acc id = function
    | [] ->
       let removed,added = transformations in
       let tests,del_actions = instantiations in
       let actions =
	 List.map Primitives.Instantiation.abstract_action_of_transformation
		  added in
       let transformations' = (List.rev removed, List.rev added) in
       let actions',transformations'' =
	 complete_with_creation
	   transformations' links_transf actions 0 created in
       ((env,Tools.option_map id_incr rule_id),(Tools.array_rev_of_list acc,
	     (tests,List.rev_append del_actions actions'),
	     transformations''))
    | h :: t ->
       let wk = Connected_component.begin_new env in
       let (wk_out,(removed,added),l_t,(tests,actions),remains) =
	 add_agents_in_cc id wk IntMap.empty transformations
			  links_transf instantiations t [h] in
       let origin = Tools.option_map (fun i -> Term.RULE i) rule_id in
       let (env',inj, cc) =
	 Connected_component.finish_new ?origin wk_out in
     let added' =
       Tools.list_smart_map
	 (Primitives.Transformation.rename wk_out id cc inj) added in
     let removed' =
       Tools.list_smart_map
	 (Primitives.Transformation.rename wk_out id cc inj) removed in
     let tests' =
       Tools.list_smart_map
	 (Primitives.Instantiation.rename_abstract_test wk_out id cc inj)
	 tests in
     let actions' =
       Tools.list_smart_map
	 (Primitives.Instantiation.rename_abstract_action wk_out id cc inj)
	 actions in
     let l_t' = IntMap.map
		  (fun (p,s as x) ->
		   let p' = Primitives.Place.rename wk id cc inj p in
		   if p == p' then x else (p',s)) l_t in
     aux env' (removed',added') (tests',actions')
	 l_t' (cc::acc) (succ id) remains
  in aux env ([],[]) ([],[])
	 IntMap.empty [] 0 mix

let rule_mixtures_of_ambiguous_rule contact_map sigs lhs rhs =
  let precomp_mixs,created = annotate_lhs_with_diff sigs [] lhs rhs in
  add_implicit_infos
    sigs (find_implicit_infos sigs contact_map (List.rev precomp_mixs)),
  created

let aux_connected_components_sum_of_ambiguous_rule id_incr contact_map env ?rule_id lhs rhs =
  let () =
    if !Parameter.compileModeOn then
      Format.eprintf "@[<v>_____@,"(*"@[<2>%a@]@," Expr.print_ast_mix lhs*) in
  let sigs = Connected_component.Env.sigs env in
  let all_mixs,(_,created) =
    rule_mixtures_of_ambiguous_rule contact_map sigs lhs rhs in
  let () =
    if !Parameter.compileModeOn then
      Format.eprintf "%a@]@."
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
				     (Raw_mixture.print sigs) created)))
		     all_mixs in
  Tools.list_fold_right_map (connected_components_of_mixture created id_incr)
			    (env,rule_id) all_mixs

let connected_components_sum_of_ambiguous_rule contact_map env ?rule_id lhs rhs =
  aux_connected_components_sum_of_ambiguous_rule succ contact_map env ?rule_id lhs rhs
let connected_components_sum_of_ambiguous_mixture contact_map env ?rule_id mix =
  let (cc_env,_),rules =
    aux_connected_components_sum_of_ambiguous_rule
      (fun x -> x) contact_map env ?rule_id mix mix in
  (cc_env, List.map
	     (function l, _, ([],[]) -> l | _ -> assert false) rules)
