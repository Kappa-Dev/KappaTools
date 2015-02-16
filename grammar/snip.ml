open Mods

let find_implicit_infos contact_map ags =
  let rec aux_one ag_tail ag_na max_id = function
  | [] -> aux_ags max_id ag_tail
  | port :: ports_l ->
     match port.Ast.port_lnk with
     | Ast.LNK_TYPE (p,a), pos ->
	let (free_id, ports_l', ags', cor) =
	  aux_one ag_tail ag_na max_id ports_l in
	(succ free_id,
	 {port with Ast.port_lnk = (Ast.LNK_VALUE free_id,pos)}::ports_l',
	 ags',(free_id,[fst a, fst p])::cor)
     | Ast.LNK_SOME, pos ->
	let (free_id, ports_l', ags', cor) =
	  aux_one ag_tail ag_na max_id ports_l in
	(succ free_id,
	 {port with Ast.port_lnk = (Ast.LNK_VALUE free_id,pos)}::ports_l',ags',
	 (free_id,
	  snd (Export_to_KaSim.String2Map.find
		 (ag_na,fst port.Ast.port_nme) contact_map))::cor)
     | Ast.LNK_VALUE i, pos ->
	let (free_id, ports_l', ags', cor) =
	  aux_one ag_tail ag_na (max i max_id) ports_l in
	(free_id, port::ports_l', ags', cor)
     | _ ->
	let (free_id, ports_l', ags', cor) =
	  aux_one ag_tail ag_na max_id ports_l in
	(free_id, port::ports_l', ags', cor)
  and aux_ags max_id = function
    | [] -> succ max_id,[],[],[]
    | ((ag_na,ag_pos),ag_port_l) :: ag_tail ->
       let (free_id,ports_l',ags',cor) =
	 aux_one ag_tail ag_na max_id ag_port_l in
       (free_id,[],((ag_na,ag_pos),ports_l')::ags',cor)
  in aux_ags 0 ags

let new_port_l_from_info port id =
  [{Ast.port_nme = Term.with_dummy_pos port;Ast.port_int = [];
   Ast.port_lnk = Term.with_dummy_pos (Ast.LNK_VALUE id);}]

let complete_with_candidates ag_na id todo cand l =
  let rec aux candidates = function
    | [] ->
       List.map (fun (_,port) -> (new_port_l_from_info port id,todo)) candidates
    | port :: ports_l ->
       match List.partition (fun (_,p) -> p = fst port.Ast.port_nme) candidates
       with
       | [],_ ->
	  List.map (fun (l, todo') -> (port::l, todo')) (aux candidates ports_l)
       | (a,b)::t,others ->
	  assert (t = []);
	  let out' =
	    List.map (fun (l,todo') -> (port::l,todo')) (aux others ports_l) in
	  match port.Ast.port_lnk with
	  | Ast.LNK_ANY, pos ->
	     ({Ast.port_nme= port.Ast.port_nme; Ast.port_int= port.Ast.port_int;
	       Ast.port_lnk = Ast.LNK_VALUE id, pos;}::ports_l,todo)::out'
	  | Ast.LNK_VALUE k, pos when k > id ->
	     begin
	       match List.partition (fun (j,_) -> j=k) todo with
	       | [(_,possible_links)], todo' ->
		  if List.mem (ag_na, fst port.Ast.port_nme) possible_links
		  then
		    ({Ast.port_nme= port.Ast.port_nme;
		      Ast.port_int= port.Ast.port_int;
		      Ast.port_lnk = Ast.LNK_VALUE id, pos;}::ports_l,todo')
		    ::out'
		  else out'
	       |_, _ -> assert false
	     end
	  | _ -> out'
  in aux cand l

let rec add_one_implicit_info id info todo = function
  | [] -> List.map (fun (ag,port) ->
		    ([(Term.with_dummy_pos ag, new_port_l_from_info port id)]
		    ,todo))
		   info
  | ((ag_na,ag_pos),ag_port_l as ag) :: ag_tail ->
     let out_tail = add_one_implicit_info id info todo ag_tail in
     let canditates = List.filter (fun (a,_) -> a = ag_na) info in
     List.fold_left (fun l (x,todo') -> ((ag::x,todo')::l))
		    (List.map
		       (fun (pl,todo') -> ((ag_na,ag_pos),pl)::ag_tail,todo')
		       (complete_with_candidates ag_na id todo
						 canditates ag_port_l))
		    out_tail

let add_implicit_infos (_,_,mix,todo) =
  let rec aux acc = function
    | [] -> acc
    | (m,[]) :: t -> aux (m::acc) t
    | (m,((id,info) :: todo')) :: t ->
       aux acc (List.rev_append (add_one_implicit_info id info todo' m) t)
  in aux [] [mix,List.rev todo]

let rec declare_internal_state wk node site_id = function
  | [] -> wk
  | [ (va,pos) ] -> Connected_component.new_internal_state wk (node,site_id) va
  | _ :: (_,pos) :: _ ->
     raise (ExceptionDefn.Malformed_Decl
	      ("In a pattern, a site cannot have several internal state ",pos))

let is_linked_on_port i p =
  match p.Ast.port_lnk with
  |Ast.LNK_VALUE j,_ when i = j -> true
  |_ -> false

let is_linked_on i (_,ag_port_l) =
  List.exists (is_linked_on_port i) ag_port_l

let rec add_agents_in_cc wk registered_links remains = function
  | [] ->
     begin match IntMap.root registered_links with
	   | None -> (wk,remains)
	   | Some (key,v) ->
	      raise (ExceptionDefn.Malformed_Decl
		       (Term.with_dummy_pos ("At least link "^string_of_int key^
					       " is dangling")))
     end
  | ((ag_na,pos),ag_port_l) :: ag_l ->
     let (node,wk) = Connected_component.new_node ~pos wk ag_na in
     let rec handle_ports wk r_l re acc = function
       | [] -> add_agents_in_cc wk r_l re acc
       | port :: ports_l ->
	  let (site,pos) = port.Ast.port_nme in
	  let site_id = Connected_component.get_site_id ~pos wk node site in
	  let wk' = declare_internal_state wk node site_id port.Ast.port_int in
	  match port.Ast.port_lnk with
	  | Ast.LNK_ANY,_ -> handle_ports wk' r_l re acc ports_l
	  | Ast.FREE,pos ->
	     let wk'' = Connected_component.new_free ~pos wk' (node,site_id) in
	     handle_ports wk'' r_l re acc ports_l
	  | (Ast.LNK_SOME,pos | Ast.LNK_TYPE _,pos) ->
	     raise (ExceptionDefn.Internal_Error
		      ("Try to create the connected components of an ambiguous mixture.",pos))
	  | Ast.LNK_VALUE i,pos ->
	     try let dst = IntMap.find i r_l in
		 let wk'' =
		   Connected_component.new_link ~pos wk' (node,site_id) dst in
		 handle_ports wk'' (IntMap.remove i r_l) re acc ports_l
	     with Not_found ->
		  match List.partition (is_linked_on_port i) ports_l with
		  | [site'], ports_l' (* link between 2 sites of 1 agent *)
		       when List.for_all (fun x -> not(is_linked_on i x)) acc &&
		     List.for_all (fun x -> not(is_linked_on i x)) re ->
		     let site_id' =
		       Connected_component.get_site_id
			 ~pos wk node (fst site'.Ast.port_nme) in
		     let wk'' =
		       Connected_component.new_link
			 ~pos wk' (node,site_id) (node,site_id') in
		     handle_ports wk'' r_l re acc ports_l'
		  | _ :: _, _ ->
		     raise (ExceptionDefn.Malformed_Decl
			      ("There are more than two sites using link "^
				 string_of_int i,pos))
		  | [], _ -> (* link between 2 agents *)
		     let r_l' = IntMap.add i (node,site_id) r_l in
		     match List.partition (is_linked_on i) re with
		     | [], re'
			  when Tools.list_exists_uniq (is_linked_on i) acc ->
			handle_ports wk' r_l' re' acc ports_l
		     | [n], re' when List.for_all
				       (fun x -> not(is_linked_on i x)) acc ->
			handle_ports wk' r_l' re' (n::acc) ports_l
		     | _, _ ->
			raise (ExceptionDefn.Malformed_Decl
				 ("There are more than two agents using link "^
				    string_of_int i,pos))
     in handle_ports wk registered_links remains ag_l ag_port_l

let connected_components_of_mixture env mix =
  let rec aux env acc = function
  | [] -> (env,acc)
  | h :: t ->
     let wk = Connected_component.begin_new env in
     let (wk_out,remains) = add_agents_in_cc wk IntMap.empty t [h] in
     let (env',cc) = Connected_component.finish_new wk_out in
     aux env' (cc::acc) remains
  in aux env [] mix

let connected_components_sum_of_ambiguous_mixture contact_map env mix =
  let all_mixs = add_implicit_infos (find_implicit_infos contact_map mix) in
  Tools.list_fold_right_map connected_components_of_mixture env all_mixs
