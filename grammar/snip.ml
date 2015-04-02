open Mods

type switching =
  | Linked of int Term.with_pos | Freed | Maintained
  | Erased | Created

type rule_internal =
    I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int
  | I_CREATED of int
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
    }
type rule_mixture = rule_agent list

type internal = int option
type link = ANY | FREE | VAL of int
type agent =
    { a_id: int; a_type: int; a_ports: link array; a_ints: internal array; }
type mixture = agent list

let print_link f = function
  | ANY -> Format.pp_print_string f "?"
  | FREE -> ()
  | VAL i -> Format.fprintf f "!%i" i

let print_intf sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if ports.(i) <> ANY || ints.(i) <> None then
	let () = Format.fprintf
		   f "%t%a%a" (if empty then Pp.empty else Pp.comma)
		   (Signature.print_site_internal_state sigs ag_ty i)
		   ints.(i) print_link ports.(i) in
	aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_agent sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.a_type
		 (print_intf sigs ag.a_type) (ag.a_ports,ag.a_ints)

let print_mixture sigs f mix =
  Pp.list Pp.comma (print_agent sigs) f mix

let full_agent_of_rule_agent config rm =
  let links_of_rule_links l =
    Array.map
      (fun l ->
       match l,config with
       | L_ANY Maintained,_ -> ANY
       | (L_ANY _ | L_SOME _ | L_TYPE _), _ ->
	  failwith "attempt to make an agent from an ambiguous rule agent"
       | (L_FREE Maintained | L_VAL (_,Maintained)), Some _ -> ANY
       | (L_FREE (Linked i) | L_VAL (_,Linked i)),Some true -> VAL (fst i)
       | L_VAL (_,(Freed | Created)), Some true -> FREE
       | (L_FREE Erased | L_VAL (_,Erased)), Some true -> ANY
       | (L_FREE Created | L_VAL (_,Created)), _ -> ANY
       | L_FREE _, _ -> FREE
       | L_VAL (i,_), (Some false | None) -> VAL (fst i)
      ) l in
  let ints_of_rule_ints i =
    Array.map
      (fun i ->
       match i, config with
       | I_ANY,_ -> None
       | I_VAL_CHANGED (i,j), Some _ when i = j -> None
       | I_VAL_CHANGED (_,i), Some true -> Some i
       | I_VAL_CHANGED (i,_), _ -> Some i
       | I_CREATED i, Some true -> Some i
       | I_VAL_ERASED i, _ -> Some i
       | (I_ANY_CHANGED _ | I_ANY_ERASED), _ ->
	  failwith "attempt to make an agent from an ambiguous rule agent"
       | I_CREATED _, _ -> None
      ) i in
  List.fold_right (fun rag (free_id,acc) ->
		   let ports = links_of_rule_links rag.ra_ports in
		   let ints = ints_of_rule_ints rag.ra_ints in
		   if Array.fold_left (fun b e -> b || e <> ANY) false ports ||
			Array.fold_left (fun b e -> b || e <> None) false ints
		   then
		     succ free_id,
		     {
		       a_id = free_id;
		       a_type = rag.ra_type;
		       a_ports = ports;
		       a_ints = ints;
		     }:: acc
		   else succ free_id,acc) rm (0,[])

let agent_of_rule_agent_positive rm =
  snd (full_agent_of_rule_agent (Some true) rm)
let agent_of_rule_agent_negative rm =
  snd (full_agent_of_rule_agent (Some false) rm)
let agent_of_rule_agent rm =
  snd (full_agent_of_rule_agent None rm)

let agent_is_linked_on_port me i id = function
  | VAL j when i = j -> id <> me
  | (VAL _ | FREE | ANY) -> false

let agent_is_linked_on forbidden i ag =
  Tools.array_filter (agent_is_linked_on_port forbidden i) ag.a_ports <> []

let rec agents_are_compatibles don remains = function
  | [] -> Some (don,remains)
  | (o,p)::q ->
     if o.a_type = p.a_type then
       let ints = Array.copy o.a_ints in
       let modif,i_ok =
	 Tools.array_fold_left2i
	   (fun i (modif,b) x y ->
	    if b then match x,y with
		      | Some a, Some b -> ints.(i) <- None; (true,(a = b))
		      | (Some _ | None), _ -> (modif,true)
	    else (modif,b))
	   (false,true) o.a_ints p.a_ints in
       if i_ok then
	 let ports = Array.copy o.a_ports in
	 match Tools.array_fold_left2i
		 (fun i c x y ->
		  match c with
		  | _,None -> c
		  | modif,(Some (todo,(g,h)) as op) ->
		     match x, y with
		     | (FREE, VAL _ | VAL _, FREE) -> (modif,None)
		     | ANY, VAL _ -> c
		     | VAL _, ANY -> c
		     | (ANY, _ | _, ANY) -> ports.(i) <- ANY; (modif||i<>0,op)
		     | FREE, FREE -> ports.(i) <- ANY; (modif||i<>0,op)
		     | VAL a, VAL b ->
			ports.(i) <- ANY;
			match List.partition (agent_is_linked_on (-1) a) g,
			      List.partition (agent_is_linked_on (-1) b) h with
			| ([],_), ([],_) -> c
			   (* if List.exists *)
			   (* 	(fun (o,p) -> *)
			   (* 	 agent_is_linked_on i a o && agent_is_linked_on i b p) *)
			   (* 	todos *)
			   (* then c *)
			   (* else None *)
			| ([x],g'), ([y],h') -> true,Some ((x,y)::todo,(g',h'))
			| _, _ -> true,None
		 )
	      (modif,Some (q,remains)) o.a_ports p.a_ports with
	 | true,Some (todo',rem') ->
	    agents_are_compatibles ({a_id = o.a_id; a_type = o.a_type;
				     a_ports = ports; a_ints = ints }::don)
				   rem' todo'
	 | _,_ -> None
       else None
     else None


let rec differences_bundles o pre p =
  match o,p with
  | [],_ | _,[] -> []
  | oh :: ot, ph :: pt ->
     (match agents_are_compatibles [] (ot,List.rev_append pre pt) [oh,ph] with
      | None -> []
      | Some (don,(o',_)) -> [don@o']
     ) @ List.fold_right (fun out acc -> (if out == ot then o else oh::out)::acc)
			 (differences_bundles ot pre p)
			 (differences_bundles o (ph::pre) pt)

let differences o p =
  List.filter (fun x -> x != o) (differences_bundles o [] p)

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
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; }

let annotate_created_agent sigs ((agent_name, _ as ag_ty),intf) =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (L_FREE Created) in
  let internals =
    Array.init arity
	       (fun i ->
		match Signature.default_internal_state ag_id i sigs with
		| None -> I_ANY | Some int -> I_CREATED int) in
  let () =
    List.iter
      (fun p ->
       let p_na = p.Ast.port_nme in
       let p_id = Signature.num_of_site ~agent_name p_na sign in
       let () = match p.Ast.port_int with
	 | [] -> ()
	 | [ va ] ->
	    internals.(p_id) <-
	      I_CREATED (Signature.num_of_internal_state p_id va sign)
	 | _ :: (_, pos) :: _ -> several_internal_states pos in
       match p.Ast.port_lnk with
       | (Ast.LNK_ANY, _) -> ()
       | ((Ast.LNK_SOME, _) | (Ast.LNK_TYPE _,_)) ->
	  too_much_or_not_enough false agent_name p_na
       | (Ast.LNK_VALUE i, pos) ->  ports.(p_id) <- L_VAL ((i,pos),Created)
       | (Ast.FREE, _) -> ()
      ) intf in
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; }

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
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; }

let rec annotate_lhs_with_diff sigs acc lhs rhs =
  match lhs,rhs with
  | ((lag_na,_ as ag_ty),lag_p)::lt, ((rag_na,_),rag_p)::rt
       when String.compare lag_na rag_na = 0 ->
     annotate_lhs_with_diff
       sigs (annotate_agent_with_diff sigs ag_ty lag_p rag_p::acc) lt rt
  | erased, added ->
     List.fold_left (fun acc x ->
		     annotate_dropped_agent sigs x::acc)
		    (List.fold_left
		       (fun acc x ->
			annotate_created_agent sigs x::acc) acc added)
		    erased

let ports_from_contact_map sigs contact_map ty_id p_id =
  let ty_na = Format.asprintf "%a" (Signature.print_agent sigs) ty_id in
  let p_na = Format.asprintf "%a" (Signature.print_site sigs ty_id) p_id in
  let cand = snd (Export_to_KaSim.String2Map.find (ty_na,p_na) contact_map) in
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
  let cand = fst (Export_to_KaSim.String2Map.find (ty_na,p_na) contact_map) in
  List.map
    (fun i_na ->
     Signature.num_of_internal_state p_id (Term.with_dummy_pos i_na) sign)
    cand

let find_implicit_infos sigs contact_map ags =
  let rec aux_internals ty_id ints acc i =
    if i = Array.length ints then acc
    else
      let acc' =
	match ints.(i) with
	| (I_ANY | I_CREATED _ | I_VAL_CHANGED _ | I_VAL_ERASED _) -> acc
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
  let rec aux_one ag_tail ty_id max_id ports i =
    if i = Array.length ports
    then List.map (fun (f,a,c) -> (f,ports,a,c)) (aux_ags max_id ag_tail)
    else
     match ports.(i) with
     | L_TYPE (a,p,s) ->
	List.map (fun (free_id,ports,ags,cor) ->
		  let () =
		    ports.(i) <- L_VAL (Term.with_dummy_pos free_id,s) in
		  (succ free_id, ports, ags, (free_id,(a,p),s)::cor))
		 (aux_one ag_tail ty_id max_id ports (succ i))
     | L_SOME s ->
	Tools.list_map_flatten
	  (fun (free_id,ports,ags,cor) ->
	   List.map (fun x ->
		     let ports' = Array.copy ports in
		     let () =
		       ports'.(i) <- L_VAL (Term.with_dummy_pos free_id,s) in
		     (succ free_id, ports', ags, (free_id,x,s)::cor))
		    (ports_from_contact_map sigs contact_map ty_id i))
	  (aux_one ag_tail ty_id max_id ports (succ i))
     | L_VAL ((j,_),s) ->
	  aux_one ag_tail ty_id (max j max_id) ports (succ i)
     | L_FREE s -> aux_one ag_tail ty_id max_id ports (succ i)
     | L_ANY Maintained -> aux_one ag_tail ty_id max_id ports (succ i)
     | L_ANY (Erased | Linked _ | Freed | Created as s) ->
	Tools.list_map_flatten
	  (fun (free_id,ports,ags,cor) ->
	   let () = ports.(i) <- L_FREE s in
	   (free_id, ports, ags, cor) ::
	     List.map (fun x ->
		       let ports' = Array.copy ports in
		       let () =
			 ports'.(i) <- L_VAL (Term.with_dummy_pos free_id,s) in
		       (succ free_id, ports', ags, (free_id,x,s)::cor))
		      (ports_from_contact_map sigs contact_map ty_id i))
	  (aux_one ag_tail ty_id max_id ports (succ i))
  and aux_ags max_id = function
    | [] -> [succ max_id,[],[]]
    | ag :: ag_tail ->
       Tools.list_map_flatten
	 (fun (free_id,ports,ags,cor) ->
	  List.map (fun ints ->
		    (free_id,
		     {ra_type = ag.ra_type; ra_ports = ports; ra_ints = ints}::ags,
		     cor))
		   (aux_internals ag.ra_type ag.ra_ints [ag.ra_ints] 0)
	 )
	 (aux_one ag_tail ag.ra_type max_id ag.ra_ports 0)
  in List.map (fun (_,mix,todo) -> (mix,todo)) (aux_ags 0 ags)

let complete_with_candidate ag id todo p_id =
  Tools.array_fold_lefti
    (fun i acc port ->
     if i <> p_id then acc else
       match port with
       | L_ANY s ->
	  let ports' = Array.copy ag.ra_ports in
	  let () = ports'.(i) <- L_VAL (Term.with_dummy_pos id,s) in
	  ({ ra_type = ag.ra_type; ra_ports = ports'; ra_ints = ag.ra_ints; }, todo)
	  :: acc
       | L_VAL ((k,_),s) when k > id ->
	  begin
	    match
	      List.partition
		(fun (j,(a',p'),_) -> j=k && i=p' && a'= ag.ra_type) todo with
	    | [ _ ], todo' ->
	       let ports' = Array.copy ag.ra_ports in
	       let () = ports'.(i) <- L_VAL (Term.with_dummy_pos id,s) in
	       ({ ra_type = ag.ra_type; ra_ports = ports'; ra_ints = ag.ra_ints; },
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
  { ra_type = ty_id; ra_ports = ports; ra_ints = internals; }

let rec add_one_implicit_info sigs id ((ty_id,port),s as info) todo = function
  | [] -> [[new_agent_with_one_link sigs ty_id port id s],todo]
  | ag :: ag_tail ->
     let out_tail = add_one_implicit_info sigs id info todo ag_tail in
     let extra_ags =
       if ty_id = ag.ra_type then
	 (List.map
	    (fun (ag',todo') -> ag'::ag_tail,todo')
	    (complete_with_candidate ag id todo port))
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

let rec add_agents_in_cc wk registered_links remains = function
  | [] ->
     begin match IntMap.root registered_links with
	   | None -> (wk,remains)
	   | Some (key,_) ->
	      raise (ExceptionDefn.Malformed_Decl
		       (Term.with_dummy_pos ("At least link "^string_of_int key^
					       " is dangling")))
     end
  | ag :: ag_l ->
     let (node,wk) = Connected_component.new_node wk ag.ra_type in
     let rec handle_ports wk r_l re acc site_id =
       if site_id = Array.length ag.ra_ports
       then add_agents_in_cc wk r_l re acc
       else
	 let wk' = match ag.ra_ints.(site_id) with
	   | (I_ANY | I_CREATED _) -> wk
	   | (I_VAL_CHANGED (i,_) | I_VAL_ERASED i ) ->
	      Connected_component.new_internal_state wk (node,site_id) i
	   | (I_ANY_ERASED | I_ANY_CHANGED _) ->
	      raise (ExceptionDefn.Internal_Error
		       (Term.with_dummy_pos
			  "Try to create the connected components of an ambiguous mixture."))
	 in
	 match ag.ra_ports.(site_id) with
	 | L_ANY Maintained -> handle_ports wk' r_l re acc (succ site_id)
	 | L_FREE s ->
	    let wk'' = if site_id = 0 then wk'
		       else Connected_component.new_free wk' (node,site_id) in
	    handle_ports wk'' r_l re acc (succ site_id)
	 | (L_SOME _ | L_TYPE _ | L_ANY (Created | Erased | Linked _ | Freed))->
	    raise (ExceptionDefn.Internal_Error
		     (Term.with_dummy_pos
			"Try to create the connected components of an ambiguous mixture."))
	 | L_VAL ((i,pos),s) ->
	    try
	      let dst = IntMap.find i r_l in
	      let wk'' = Connected_component.new_link wk' (node,site_id) dst in
	      handle_ports wk'' (IntMap.remove i r_l) re acc (succ site_id)
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
		    handle_ports wk'' r_l re acc (succ site_id)
		 | _ :: _ ->
		    raise (ExceptionDefn.Malformed_Decl
			     ("There are more than two sites using link "^
				string_of_int i,pos))
		 | [] -> (* link between 2 agents *)
		    let r_l' = IntMap.add i (node,site_id) r_l in
		    match List.partition (is_linked_on i) re with
		    | [], re'
			 when Tools.list_exists_uniq (is_linked_on i) acc ->
		       handle_ports wk' r_l' re' acc (succ site_id)
		    | [n], re' when List.for_all
				      (fun x -> not(is_linked_on i x)) acc ->
		       handle_ports wk' r_l' re' (n::acc) (succ site_id)
		    | _, _ ->
		       raise (ExceptionDefn.Malformed_Decl
				("There are more than two agents using link "^
				   string_of_int i,pos))
     in handle_ports wk registered_links remains ag_l 0

let connected_components_of_mixture env mix =
  let rec aux env acc = function
  | [] -> (env,acc)
  | h :: t ->
     let wk = Connected_component.begin_new env in
     let (wk_out,remains) = add_agents_in_cc wk IntMap.empty t [h] in
     let (env',_, cc) = Connected_component.finish_new wk_out in
     aux env' (cc::acc) remains
  in aux env [] mix

let rule_mixtures_of_ambiguous_rule contact_map sigs lhs rhs =
  let precomp_mixs = List.rev (annotate_lhs_with_diff sigs [] lhs rhs) in
  add_implicit_infos
    sigs (find_implicit_infos sigs contact_map precomp_mixs)

let connected_components_sum_of_ambiguous_rule contact_map env lhs rhs =
  let sigs = Connected_component.Env.sigs env in
  let all_mixs = rule_mixtures_of_ambiguous_rule contact_map sigs lhs rhs in
  let (env',ccs) =
    Tools.list_fold_right_map connected_components_of_mixture env all_mixs in
  let () =
    if !Parameter.compileModeOn then
      let boxed_cc f cc =
	let () = Format.pp_open_box f 2 in
	let () = Connected_component.print
		   true (Connected_component.Env.sigs env') f cc in
	Format.pp_close_box f () in
      let one_ccs f x =
	Format.fprintf f "---@,%a"
		       (Pp.list ~trailing:Pp.space Pp.space boxed_cc) x in
      Format.eprintf "@[<v>-----@,@[<2>%a@]@,%a@,@]@." Expr.print_ast_mix lhs
		     (Pp.list Pp.space one_ccs) ccs in
  (env',ccs)

let connected_components_sum_of_ambiguous_mixture contact_map env mix =
  connected_components_sum_of_ambiguous_rule contact_map env mix mix
