open Mods

type link = UnSpec | Free | Link of int * int (** node_id, site_id *)

(** The link of site k of node i is stored in links(i).(k).

The internal state of site k of node i is store in internals(i).(k). A
negative number means UnSpec. *)
type cc = {
  id: int;
  nodes_by_type: int list array;
  links: link array IntMap.t;
  internals: int array IntMap.t;
}
type t = cc

type 'id raw_place =
    Existing of 'id
  | Fresh of int * int (* type, id *)

type edge = ToNode of int raw_place * int | ToNothing | ToInternal of int

type son = {
  extra_edge: ((int raw_place*int)*edge);
  dst: int (** t.id *);
  inj: Dipping.t; (* From dst To ("this" cc + extra edge) *)
  above_obs: int list;
}

type point = {
  cc: t;
  is_obs: bool;
  fathers: int (** t.id *) list;
  sons: son list;
}

type work = {
  sigs: Signature.s;
  cc_env: point IntMap.t;
  reserved_id: int list array;
  used_id: int list array;
  free_id: int;
  cc_id: int;
  cc_links: link array IntMap.t;
  cc_internals: int array IntMap.t;
  dangling: int; (* node_id *)
}

module Node = struct
  type t = int * int * int (** (cc_id,type_id,node_id) *)

  let compare (cc,_,n) (cc',_,n') =
    let c = int_compare cc cc' in
    if c = 0 then int_compare n n' else c

  let rename wk cc inj (n_cc,n_ty,n_id as node) =
    if wk.cc_id = n_cc then (cc.id,n_ty, Dipping.apply inj n_id)
    else node

  let get_sort (_,ty,_) = ty

  let print ?sigs f (cc,ty,i) =
    match sigs with
    | Some sigs ->
       Format.fprintf f "%a/*%i*/" (Signature.print_agent sigs) ty i
    | None -> Format.fprintf f "cc%in%i" cc i

  let print_site ?sigs (cc,agent,i) f id =
    match sigs with
    | Some sigs ->
       Signature.print_site sigs agent f id
    | None -> Format.fprintf f "cc%in%is%i" cc i id

  let print_internal ?sigs (_,agent,_) site f id =
    match sigs with
    | Some sigs ->
       Signature.print_site_internal_state sigs agent site f (Some id)
    | None -> Format.pp_print_int f id
end

let raw_find_ty tys id =
  let rec aux i =
    assert (i >= 0);
    if List.mem id tys.(i) then i else aux (pred i)
  in aux (Array.length tys - 1)

let find_ty cc id = raw_find_ty cc.nodes_by_type id

(** Errors *)
let already_specified ?sigs x i =
  ExceptionDefn.Malformed_Decl
    (Term.with_dummy_pos
       (Format.asprintf "Site %a of agent %a already specified"
			(Node.print_site ?sigs x) i (Node.print ?sigs) x))

let dangling_node ~sigs tys x =
  ExceptionDefn.Malformed_Decl
    (Term.with_dummy_pos
       (Format.asprintf
	  "Cannot proceed because last declared agent %a/*%i*/%a"
	  (Signature.print_agent sigs) (raw_find_ty tys x) x
	  Format.pp_print_string " is not linked to its connected component."))

let identity_injection cc =
  Dipping.identity
    (Array.fold_left (fun x y -> List.rev_append y x) [] cc.nodes_by_type)

let print_node_id sigs f = function
  | Existing id -> Format.pp_print_int f id
  | Fresh (ty,id) ->
     Format.fprintf f "!%a-%i" (Signature.print_agent sigs) ty id

let print_edge sigs f = function
  | (source,site), ToNothing ->
     Format.fprintf f "-%a_%i-%t->" (print_node_id sigs) source site Pp.bottom
  | (source,site), ToNode (id,port) ->
     Format.fprintf f "-%a_%i-%a_%i->" (print_node_id sigs) source site
		    (print_node_id sigs) id port
  | (source,site), ToInternal i ->
     Format.fprintf f "-%a_%i-~%i->" (print_node_id sigs) source site i

let find_root cc =
  let rec aux ty =
    if ty = Array.length cc.nodes_by_type
    then None
    else match cc.nodes_by_type.(ty) with
	 | [] -> aux (succ ty)
	 | h::t ->
	    let x = List.fold_left (fun _ x -> x) h t in
	    Some(ty,x)
  in aux 0

let to_navigation full cc =
  let rec build_for out don = function
    | [] -> List.rev out
    | h ::  t ->
       let out_ints =
	 if full then
	   Tools.array_fold_lefti
	     (fun i acc v ->
	      if v < 0 then acc else ((Existing h,i),ToInternal v)::acc)
	     out (IntMap.find h cc.internals)
	 else out in
       let news,out_lnk,todo =
	 Tools.array_fold_lefti
	   (fun i (news,ans,re as acc) ->
	    function
	    | UnSpec -> acc
	    | Free ->
	       if full && i > 0
	       then (news,((Existing h,i),ToNothing)::ans,re)
	       else acc
	    | Link (n,l) ->
	       if List.mem n don then acc
	       else if n = h || List.mem n re
	       then
		 if full then (news,((Existing h,i),ToNode (Existing n,l))::ans ,re)
		 else acc
	       else
		 ((Existing h,i),(n,l))::news, ans, n::re)
	   ([],[],t) (IntMap.find h cc.links) in
       let out' =
	 List.fold_left (fun acc (x,(n,s)) ->
			 (x,ToNode(Fresh(find_ty cc n,n),s))::acc) out_ints
			(List.sort (fun (_,(a,_)) (_,(b,_)) ->
				    Mods.int_compare a b) news) in
       let out'' = List.rev_append out_lnk out' in
       build_for out'' (h::don) todo in
  match find_root cc with
  | None -> []
  | Some (i,x) ->
     build_for [(Fresh (i,x),0),ToNothing] [] [x]

let print with_id sigs f cc =
  let print_intf (_,_,ag_i as ag) link_ids internals neigh =
    snd
      (Tools.array_fold_lefti
	 (fun p (not_empty,(free,link_ids as out)) el ->
	  if p = 0 then (not_empty, out)
	  else
	    let () =
	      if internals.(p) >= 0
	      then Format.fprintf f "%t%a"
				  (if not_empty then Pp.comma else Pp.empty)
				  (Node.print_internal ~sigs ag p) internals.(p)
	      else
		if  el <> UnSpec then
		  Format.fprintf f "%t%a"
				 (if not_empty then Pp.comma else Pp.empty)
				 (Node.print_site ~sigs ag) p in
	    match el with
	    | UnSpec ->
	       if internals.(p) >= 0
	       then let () = Format.fprintf f "?" in (true,out)
	       else (not_empty,out)
	    | Free -> true,out
	    | Link (dst_a,dst_p) ->
	       let i,out' =
		 try (Int2Map.find (dst_a,dst_p) link_ids, out)
		 with Not_found ->
		   (free,(succ free, Int2Map.add (ag_i,p) free link_ids)) in
	       let () = Format.fprintf f "!%i" i in
	       true,out') (false,link_ids) neigh) in
  let () = Format.pp_open_box f 2 in
  let () = if with_id then Format.fprintf f "/*cc%i*/@ " cc.id in
  let (_,_) =
    IntMap.fold
      (fun x el (not_empty,link_ids) ->
       let ag_x = (cc.id,find_ty cc x,x) in
       let () =
	 Format.fprintf
	   f "%t@[<h>%a("
	   (if not_empty then Pp.comma else Pp.empty)
	   (Node.print ~sigs) ag_x in
       let out = print_intf ag_x link_ids (IntMap.find x cc.internals) el in
       let () = Format.fprintf f ")@]" in
       true,out) cc.links (false,(1,Int2Map.empty)) in
  Format.pp_close_box f ()

let print_dot sigs f cc =
  let pp_one_node x i f = function
    | UnSpec -> ()
    | Free ->
       let n = (cc.id,find_ty cc x,x) in
       if i <> 0 then
	 let () = Format.fprintf
		    f "@[%a@ [label=\"%t\",@ height=\".1\",@ width=\".1\""
		    (Node.print_site ?sigs:None n) i Pp.bottom in
	 let () =
	   Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
	 let () = Format.fprintf
		    f "@[<b>%a ->@ %a@ @[[headlabel=\"%a\",@ weight=\"25\""
		    (Node.print_site ?sigs:None n) i (Node.print ?sigs:None) n
		    (Node.print_site ~sigs n) i in
	 Format.fprintf f",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@,"
       else Format.fprintf f "@[%a [label=\"%a\"]@];@,"
			   (Node.print ?sigs:None) n (Node.print ~sigs) n
    | Link (y,j) ->
       let n = (cc.id,find_ty cc x,x) in
       let n' = (cc.id,find_ty cc y,y) in
       if x<y || (x=y && i<j) then
	 let () = Format.fprintf
		    f
		    "@[<b>%a ->@ %a@ @[[taillabel=\"%a\",@ headlabel=\"%a\""
		    (Node.print ?sigs:None) n (Node.print ?sigs:None) n'
		    (Node.print_site ~sigs n) i (Node.print_site ~sigs n') j in
	 Format.fprintf
	   f ",@ arrowhead=\"odot\",@ arrowtail=\"odot\",@ dir=\"both\"]@];@]@,"
  in
  let pp_one_internal x i f k =
    let n = (cc.id,find_ty cc x,x) in
    if k >= 0 then
      let () = Format.fprintf
		 f "@[%ai@ [label=\"%a\",@ height=\".1\",@ width=\".1\""
		 (Node.print_site ?sigs:None n) i
		 (Node.print_internal ~sigs n i) k in
      let () =
	Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
      let () = Format.fprintf
		 f "@[<b>%ai ->@ %a@ @[[headlabel=\"%a\",@ weight=25"
		 (Node.print_site ?sigs:None n) i (Node.print ?sigs:None) n
		 (Node.print_site ~sigs n) i in
      Format.fprintf f ",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@," in
  let pp_slot pp_el f (x,a) =
    Pp.array (fun _ -> ()) (pp_el x) f a in
  Format.fprintf
    f "@[<v>subgraph %i {@,%a%a}@]" cc.id
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
	    IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (pp_slot pp_one_node)) cc.links
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
	    IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (pp_slot pp_one_internal)) cc.internals

let print_sons_dot sigs cc_id f sons =
  let pp_edge f ((n,p),e) =
    match e with
    | ToNode (n',p') ->
       Format.fprintf f "(%a,%i) -> (%a,%i)"
		      (print_node_id sigs) n p (print_node_id sigs) n' p'
    | ToNothing ->
       Format.fprintf f "(%a,%i) -> %t" (print_node_id sigs) n p Pp.bottom
    | ToInternal i ->
       Format.fprintf f "(%a,%i)~%i" (print_node_id sigs) n p i in
  Pp.list Pp.space ~trailing:Pp.space
	  (fun f son -> Format.fprintf f "@[cc%i -> cc%i [label=\"%a %a\"];@]"
				       cc_id son.dst pp_edge son.extra_edge
				       Dipping.print son.inj)
	  f sons

let print_point_dot sigs f (id,point) =
  let style = if point.is_obs then "octagon" else "box" in
  Format.fprintf f "@[cc%i [label=\"%a\", shape=\"%s\"];@]@,%a"
		 point.cc.id (print false sigs) point.cc
		 style (print_sons_dot sigs id) point.sons

module Env : sig
  type t

  val fresh : Signature.s -> int list array -> int -> point IntMap.t -> t
  val empty : Signature.s -> t
  val sigs : t -> Signature.s
  val find : t -> cc -> (int * Dipping.t * point) option
  val navigate :
    t -> ((int raw_place*int)*edge) list -> (int * Dipping.t * point) option
  val get : t -> int -> point
  val check_vitality : t -> unit
  val cc_map : t -> cc IntMap.t
  val add_point : int -> point -> t -> t
  val to_work : t -> work
  val fresh_id : t -> int
  val nb_ag : t -> int
  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit
end = struct
  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: point IntMap.t;
    mutable used_by_a_begin_new: bool;
  }

let fresh sigs id_by_type nb_id domain =
  {
    sig_decl = sigs;
    id_by_type = id_by_type;
    nb_id = nb_id;
    domain = domain;
    used_by_a_begin_new = false;
  }

let empty sigs =
  let nbt = Array.make (Signature.size sigs) [] in
  let nbt' = Array.make (Signature.size sigs) [] in
  let empty_cc = {id = 0; nodes_by_type = nbt;
		  links = IntMap.empty; internals = IntMap.empty;} in
  let empty_point =
    {cc = empty_cc; is_obs = false; fathers = []; sons = [];} in
  fresh sigs nbt' 1 (IntMap.add 0 empty_point IntMap.empty)

let check_vitality env = assert (env.used_by_a_begin_new = false)

let cc_map env = IntMap.fold (fun i x out ->
			      if x.is_obs then IntMap.add i x.cc out else out)
			     env.domain IntMap.empty
let print f env =
  Format.fprintf
    f "@[<v>%a@]"
    (Pp.set ~trailing:Pp.space IntMap.bindings Pp.space
	    (fun f (_,p) ->
	     Format.fprintf f "@[<hov 2>(%a)@ -> @[<h>%a@]@ -> @[(%a)@]@]"
			    (Pp.list Pp.space Format.pp_print_int) p.fathers
			    (print true env.sig_decl) p.cc
			    (Pp.list Pp.space
				     (fun f s -> Format.fprintf
						   f "%a(@[%a@])%i"
						   (print_edge env.sig_decl)
						   s.extra_edge
						   Dipping.print s.inj s.dst))
			    p.sons))
    env.domain

let add_point id el env =
  {
    sig_decl = env.sig_decl;
    id_by_type = env.id_by_type;
    nb_id = env.nb_id;
    domain = IntMap.add id el env.domain;
    used_by_a_begin_new = false;
  }

let fresh_id env =
  if IntMap.is_empty env.domain then 0
  else succ (IntMap.max_key env.domain)

let sigs env = env.sig_decl

let to_work env =
  let () = check_vitality env in
  let () = env.used_by_a_begin_new <- true in
  {
    sigs = env.sig_decl;
    cc_env = env.domain;
    reserved_id = env.id_by_type;
    used_id = Array.make (Array.length env.id_by_type) [];
    free_id = env.nb_id;
    cc_id = fresh_id env;
    cc_links = IntMap.empty;
    cc_internals = IntMap.empty;
    dangling = 0;
  }

let navigate env nav =
  let compatible_point inj = function
    | ((Existing id,site), ToNothing), e ->
       if e = ((Existing (Dipping.apply inj id),site),ToNothing)
       then Some inj else None
    | ((Existing id,site), ToInternal i), e ->
       if e = ((Existing (Dipping.apply inj id),site),ToInternal i)
       then Some inj else None
    | ((Existing id,site), ToNode (Existing id',site')), e ->
       if e =
	    ((Existing (Dipping.apply inj id),site),
	     ToNode (Existing (Dipping.apply inj id'),site'))
	  || e =
	       ((Existing (Dipping.apply inj id'),site'),
		ToNode (Existing (Dipping.apply inj id),site))
       then Some inj else None
    | (
      ((Existing id,site),ToNode (Fresh (ty,id'),site')),
      ((Existing sid,ssite), ToNode (Fresh(ty',sid'),ssite'))
    | ((Fresh (ty,id'),site),ToNode (Existing id,site')),
      ((Existing sid,ssite), ToNode (Fresh(ty',sid'),ssite'))
    | ((Existing id,site),ToNode (Fresh (ty,id'),site')),
      ((Fresh(ty',sid'),ssite), ToNode (Existing sid,ssite'))
    | ((Fresh (ty,id'),site),ToNode (Existing id,site')),
      ((Fresh(ty',sid'),ssite), ToNode (Existing sid,ssite'))
    ) ->
       if sid = Dipping.apply inj id && ssite = site
	  && ty' = ty && ssite' = site'
       then Some (Dipping.add id' sid' inj) else None
    | ((Existing _,_), ToNode (Fresh _,_)),
      (((Fresh _ | Existing _), _), _) -> None
    | ((Fresh (ty,id),site), ToNothing), ((Fresh (ty',id'),site'),x) ->
       if ty = ty' && site = site' && x = ToNothing && not (Dipping.mem id inj)
       then Some (Dipping.add id id' inj) else None
    | ((Fresh (ty,id),site), ToInternal i), ((Fresh (ty',id'),site'),x) ->
       if ty = ty' && site = site' &&
	    x = ToInternal i && not (Dipping.mem id inj)
       then Some (Dipping.add id id' inj) else None
    | ((Fresh (ty,id),site), ToNode (Fresh (ty',id'),site')),
      ((Fresh (sty,sid),ssite), ToNode (Fresh (sty',sid'),ssite')) ->
       if not (Dipping.mem id inj) && not (Dipping.mem id inj) then
	 if ty = sty && site = ssite && ty' = sty' && site' = ssite'
	 then Some (Dipping.add id' sid' (Dipping.add id sid inj))
	 else if ty = sty && site = ssite && ty' = sty' && site' = ssite'
	 then Some (Dipping.add id' sid (Dipping.add id sid' inj))
	 else None
       else None
    | ((Fresh _,_), _), ((Fresh _,_),_) -> None
    | ((Fresh _,_), _), ((Existing _,_),_) -> None in
  let rec aux inj_dst2nav i = function
    | [] -> Some (i,inj_dst2nav,IntMap.find i env.domain)
    | e :: t ->
       let rec find_good_edge = function
	 | [] -> None
	 | s :: tail ->
	    match compatible_point inj_dst2nav (s.extra_edge,e) with
	    | Some inj' -> aux (Dipping.compose s.inj inj') s.dst t
	    | None -> find_good_edge tail in
       find_good_edge (IntMap.find i env.domain).sons
  in aux Dipping.empty 0 nav

let find env cc =
  let nav = to_navigation true cc in
(*  let () = Format.eprintf
	     "@[[%a]@]@,%a@." (Pp.list Pp.space (print_edge (sigs env))) nav
	     print env in*)
  navigate env nav

let get env cc_id = IntMap.find cc_id env.domain

let nb_ag env = env.nb_id

let print_dot f env =
  let () = Format.fprintf f "@[<v>strict digraph G {@," in
  let () =
    Pp.set ~trailing:Pp.space IntMap.bindings Pp.space
	   (print_point_dot (sigs env)) f env.domain in
  Format.fprintf f "}@]@."
end

let propagate_add_obs obs_id env cc_id =
  let rec aux son_id domain cc_id =
    let cc = Env.get domain cc_id in
    let sons' =
      Tools.list_smart_map
	(fun s -> if s.dst = son_id && not (List.mem obs_id s.above_obs)
		  then {s with above_obs = obs_id::s.above_obs}
		  else s) cc.sons in
    if sons' == cc.sons then domain
    else
      let env' =
	Env.add_point cc_id {cc with sons = sons'} domain in
      List.fold_left (aux cc_id) env' cc.fathers in
  List.fold_left (aux cc_id) env (Env.get env cc_id).fathers

exception Found

let update_cc cc_id cc ag_id links internals =
  { id = cc_id;
    nodes_by_type = cc.nodes_by_type;
    internals = IntMap.add ag_id internals cc.internals;
    links = IntMap.add ag_id links cc.links;}

let remove_ag_cc cc_id cc ag_id =
  let ty = find_ty cc ag_id in
  match cc.nodes_by_type.(ty) with
  | [] -> assert false
  | max :: tail ->
     let rec build_subst subst pre = function
       | _ when pre = ag_id -> Dipping.add pre max subst
       | [] -> assert false
       | h :: t -> build_subst (Dipping.add pre h subst) h t in
     let to_subst = build_subst (identity_injection cc) max tail in
     let new_nbt =
       Array.mapi (fun i l -> if i = ty then tail else l) cc.nodes_by_type in
     if Dipping.is_identity to_subst then
       { id = cc_id;
	 nodes_by_type = new_nbt;
	 links = IntMap.remove ag_id cc.links;
	 internals = IntMap.remove ag_id cc.internals;},to_subst
     else
       let swip map =
	 let map' =
	   List.fold_right
	     (fun (s,d) map ->
	      if s = max || s = d then map else
		let tmp = IntMap.find max map in
		let map' = IntMap.add max (IntMap.find s map) map in
		IntMap.add s tmp map') (Dipping.to_list to_subst) map in
	 IntMap.remove max map' in
       let new_ints = swip cc.internals in
       let prelinks = swip cc.links in
       let new_links =
	 IntMap.map
	   (fun a -> Array.map (function
				 | (UnSpec | Free) as x -> x
				 | Link (n,s) as x ->
				    try Link (Dipping.apply to_subst n,s)
				    with Not_found -> x) a) prelinks in
       { id = cc_id; nodes_by_type = new_nbt;
	 links = new_links; internals = new_ints;},to_subst

let compute_cycle_edges cc =
  let rec aux don acc path ag_id =
    Tools.array_fold_lefti
      (fun i (don,acc as out) ->
	     function
	     | UnSpec | Free -> out
	     | Link (n',i') ->
		if List.mem n' don then out
		else
		  let edge = ((Existing ag_id,i),ToNode(Existing n',i')) in
		  if ag_id = n' then (don, edge::acc)
		  else
		    let rec extract_cycle acc' = function
		      | ((Existing n,i),_ as e) :: t ->
			 if n' = n then
			   if i' = i then out
			   else (don,edge::e::acc')
			 else extract_cycle (e::acc') t
		      | ((Fresh _,_),_) :: _ -> assert false
		      | [] ->
			 let (don',acc') = aux don acc (edge::path) n' in
			 (n'::don',acc') in
		    extract_cycle acc path)
      (don,acc) (IntMap.find ag_id cc.links) in
  let rec element i t =
    if i = Array.length t then [] else
      match t.(i) with
      | [] -> element (succ i) t
      | h :: _ -> snd (aux [] [] [] h) in
  element 0 cc.nodes_by_type

let remove_cycle_edges complete_domain_with obs_id dst env free_id cc =
  let rec aux ((f_id,env'),out as acc) = function
    | ((Existing n,i),ToNode(Existing n',i') as e) :: q ->
       let links = IntMap.find n cc.links in
       let int = IntMap.find n cc.internals in
       let links' = Array.copy links in
       let () = links'.(i) <- UnSpec in
       let cc_tmp = update_cc f_id cc n links' int in
       let links_dst = IntMap.find n' cc_tmp.links in
       let int_dst = IntMap.find n' cc_tmp.internals in
       let links_dst' = Array.copy links_dst in
       let () = links_dst'.(i') <- UnSpec in
       let cc' = update_cc f_id cc_tmp n' links_dst' int_dst in
       let pack,ans =
	 complete_domain_with obs_id dst env' (succ f_id)
			      cc' e (identity_injection cc) in
       aux (pack,ans::out) q
    | l -> assert (l = []); acc in
  aux ((free_id,env),[]) (compute_cycle_edges cc)

let compute_father_candidates complete_domain_with obs_id dst env free_id cc =
  let agent_is_removable lp links internals =
    try
      let () = Array.iter (fun el -> if el >= 0 then raise Found) internals in
      let () =
	Array.iteri
	  (fun i el -> if i>0 && i<>lp && el<>UnSpec then raise Found) links in
      true
    with Found -> false in
  let remove_one_internal acc ag_id links internals =
    Tools.array_fold_lefti
      (fun i ((f_id,env'), out as acc) el ->
       if el >= 0 then
	 let int' = Array.copy internals in
	 let () = int'.(i) <- -1 in
	 let pack,ans =
	   complete_domain_with
	     obs_id dst env' (succ f_id) (update_cc f_id cc ag_id links int')
	     ((Existing ag_id,i),ToInternal el) (identity_injection cc) in
	 (pack,ans::out)
       else acc)
      acc internals in
  let remove_one_frontier acc ag_id links internals =
    Tools.array_fold_lefti
      (fun i ((f_id,env'),out as acc) ->
       function
       | UnSpec -> acc
       | Free ->
	  if i = 0 then acc else
	    let links' = Array.copy links in
	    let () = links'.(i) <- UnSpec in
	    let pack,ans =
	      complete_domain_with
		obs_id dst env' (succ f_id)
		(update_cc f_id cc ag_id links' internals)
		((Existing ag_id,i),ToNothing) (identity_injection cc) in
	    (pack,ans::out)
       | Link (n',i') ->
	  if not (agent_is_removable i links internals) then acc else
	    let links_dst = IntMap.find n' cc.links in
	    let int_dst = IntMap.find n' cc.internals in
	    let links_dst' = Array.copy links_dst in
	    let () = links_dst'.(i') <- UnSpec in
	    let cc',inj2cc' =
	      remove_ag_cc f_id (update_cc f_id cc n' links_dst' int_dst)
			   ag_id in
	    let pack,ans =
	      complete_domain_with
		obs_id dst env' (succ f_id) cc'
		((Existing (Dipping.apply inj2cc' n'),i'),
		 ToNode
		   (Fresh(find_ty cc ag_id,Dipping.apply inj2cc' ag_id),i))
		inj2cc' in
	    (pack,ans::out))
      (remove_one_internal acc ag_id links internals) links in
  let remove_or_remove_one ((f_id,env'),out as acc) ag_id links internals =
    if agent_is_removable 0 links internals then
      let pack,ans =
	complete_domain_with
	  obs_id dst env' (succ f_id) (fst (remove_ag_cc f_id cc ag_id))
	  ((Fresh(find_ty cc ag_id,ag_id),0),ToNothing) (identity_injection cc) in
      (pack,ans::out)
    else remove_one_frontier acc ag_id links internals in
  IntMap.fold (fun i links acc ->
	       remove_or_remove_one acc i links (IntMap.find i cc.internals))
	      cc.links
	      (remove_cycle_edges complete_domain_with obs_id dst env free_id cc)

let rec complete_domain_with obs_id dst env free_id cc edge inj_dst2cc =
  let new_son inj_found2cc =
    { dst = dst;
      extra_edge = edge;
      inj = Dipping.compose inj_dst2cc (Dipping.inverse inj_found2cc);
      above_obs = [obs_id];} in
  let known_cc = Env.find env cc in
  match known_cc with
  | Some (cc_id, inj_cc_id2cc, point') ->
     let point'' = {point' with sons = new_son inj_cc_id2cc :: point'.sons} in
     let completed =
       propagate_add_obs obs_id (Env.add_point cc_id point'' env) cc_id in
     (free_id,completed), cc_id
  | None ->
     let son = new_son (identity_injection cc) in
     add_new_point obs_id env free_id [son] cc
and add_new_point obs_id env free_id sons cc =
  let (free_id'',env'),fathers =
    compute_father_candidates complete_domain_with obs_id cc.id env free_id cc in
  let completed =
    Env.add_point
      cc.id
      {cc = cc; is_obs = cc.id = obs_id; sons=sons; fathers = fathers;}
      env' in
  ((free_id'',completed),cc.id)

let add_domain env cc =
  let known_cc = Env.find env cc in
  match known_cc with
  | Some (id,inj,point) ->
     (if point.is_obs then env
      else propagate_add_obs id env id),inj,point.cc
  | None ->
     let (_,env'),_ = add_new_point cc.id env (succ cc.id) [] cc in
     (env',identity_injection cc, cc)

(** Operation to create cc *)
let check_dangling wk =
  if wk.dangling <> 0 then
    raise (dangling_node ~sigs:wk.sigs wk.used_id wk.dangling)
let check_node_adequacy ~pos wk cc_id =
  if wk.cc_id <> cc_id then
    raise (
	ExceptionDefn.Malformed_Decl
	  (Format.asprintf
		"A node from a different connected component has been used."
	  ,pos))

let begin_new env = Env.to_work env

let finish_new wk =
  let () = check_dangling wk in
  (** rebuild env **)
  let () =
    Tools.iteri
      (fun i ->
       wk.reserved_id.(i) <- List.rev_append wk.used_id.(i) wk.reserved_id.(i))
      (Array.length wk.used_id) in
  let cc_candidate =
    { id = wk.cc_id; nodes_by_type = wk.used_id;
      links = wk.cc_links; internals = wk.cc_internals; } in
  let env = Env.fresh wk.sigs wk.reserved_id wk.free_id wk.cc_env in
  add_domain env cc_candidate


let new_link wk ((cc1,_,x as n1),i) ((cc2,_,y as n2),j) =
  let pos = (Lexing.dummy_pos,Lexing.dummy_pos) in
  let () = check_node_adequacy ~pos wk cc1 in
  let () = check_node_adequacy ~pos wk cc2 in
  let x_n = IntMap.find x wk.cc_links in
  let y_n = IntMap.find y wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n1 i)
  else if y_n.(j) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n2 j)
  else
    let () = x_n.(i) <- Link (y,j) in
    let () = y_n.(j) <- Link (x,i) in
    if wk.dangling = x || wk.dangling = y
    then { wk with dangling = 0 }
    else wk

let new_free wk ((cc,_,x as n),i) =
  let () = check_node_adequacy ~pos:(Lexing.dummy_pos,Lexing.dummy_pos) wk cc in
  let x_n = IntMap.find x wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- Free in
    wk

let new_internal_state wk ((cc,_,x as n), i) va =
  let () = check_node_adequacy ~pos:(Lexing.dummy_pos,Lexing.dummy_pos) wk cc in
  let x_n = IntMap.find x wk.cc_internals in
  if x_n.(i) >= 0 then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- va in
    wk

let new_node wk type_id =
  let () = check_dangling wk in
  let arity = Signature.arity wk.sigs type_id in
  match wk.reserved_id.(type_id) with
  | h::t ->
     let () = wk.used_id.(type_id) <- h :: wk.used_id.(type_id) in
     let () = wk.reserved_id.(type_id) <- t in
     let node = (wk.cc_id,type_id,h) in
     (node,
      new_free
	{ wk with
	  dangling = if IntMap.is_empty wk.cc_links then 0 else h;
	  cc_links = IntMap.add h (Array.make arity UnSpec) wk.cc_links;
	  cc_internals = IntMap.add h (Array.make arity (-1)) wk.cc_internals;
	} (node,0))
  | [] ->
     let () = wk.used_id.(type_id) <- wk.free_id :: wk.used_id.(type_id) in
     let node = (wk.cc_id, type_id, wk.free_id) in
     (node,
      new_free
	{ wk with
	  free_id = succ wk.free_id;
	  dangling = if IntMap.is_empty wk.cc_links then 0 else wk.free_id;
	  cc_links =
	    IntMap.add wk.free_id (Array.make arity UnSpec) wk.cc_links;
	  cc_internals =
	    IntMap.add wk.free_id (Array.make arity (-1)) wk.cc_internals;
	} (node,0))

module NodeMap = MapExt.Make(Node)

let injection_for_one_more_edge inj graph = function
  | ((Fresh _,_),_) -> None
  | ((Existing id,site),ToNothing) ->
     if Edges.is_free (Dipping.apply inj id) site graph then Some inj else None
  | ((Existing id,site),ToInternal i) ->
     if Edges.is_internal i (Dipping.apply inj id) site graph
     then Some inj else None
  | ((Existing id,site),ToNode (Existing id',site')) ->
     if Edges.link_exists (Dipping.apply inj id) site
			  (Dipping.apply inj id') site' graph
     then Some inj else None
  | ((Existing id,site),ToNode (Fresh (ty,id'),site')) ->
     match Edges.exists_fresh (Dipping.apply inj id) site ty site' graph with
     | None -> None
     | Some node -> Some (Dipping.add id' node inj)

module Matching = struct
  type t = int NodeMap.t * IntSet.t

  let empty = (NodeMap.empty, IntSet.empty)

  let from_dipping cc =
    Dipping.fold
      (fun src dst -> function
      | None -> None
      | Some (acc,co) ->
	 if IntSet.mem src co then None
	 else
	   Some (NodeMap.add (cc.id,find_ty cc dst,dst) src acc, IntSet.add src co))

  let reconstruct graph inj cc root =
    match find_root cc with
    | None -> Some inj
    | Some (_,node) ->
       let dip = Dipping.add node root Dipping.empty in
       let full_dip =
	 List.fold_left
	   (fun inj_op nav ->
	    match inj_op with
	    | None -> None
	    | Some inj -> injection_for_one_more_edge inj graph nav)
	   (Some dip) (List.tl (to_navigation false cc)) in
       match full_dip with
       | None -> failwith "Matching.reconstruct error"
       | Some dip -> from_dipping cc dip (Some inj)

  let get node (t,_) = NodeMap.find node t

  let from_edge domain graph edge =
    let rec aux cache acc = function
      | [] -> acc
      | (point,inj_point2graph) :: remains ->
	 let acc' =
	   if point.is_obs
	   then match find_root point.cc with
		| None -> assert false
		| Some (_,root) ->
		   (point.cc,Dipping.apply inj_point2graph root) :: acc
	   else acc in
	 let remains',cache' =
	   List.fold_left
	     (fun (re,ca as acc) son ->
	      match injection_for_one_more_edge inj_point2graph graph son.extra_edge with
	      | None -> acc
	      | Some inj' ->
		 if
		   try Dipping.equal (IntMap.find son.dst cache) inj'
		   with Not_found -> false
		 then acc
		 else
		   (Env.get domain son.dst,inj')::re,IntMap.add son.dst inj' ca)
	     (remains,cache) point.sons in
	 aux cache' acc' remains' in
    match Env.navigate domain [edge] with
    | None -> []
    | Some (cc_id,inj,point) ->
       aux (IntMap.add cc_id inj IntMap.empty) [] [(point,inj)]

  let observables_from_free domain graph ty node_id site =
    from_edge domain graph ((Fresh (ty,node_id),site),ToNothing)
  let observables_from_internal domain graph ty node_id site id =
    from_edge domain graph ((Fresh (ty,node_id),site),ToInternal id)
  let observables_from_link domain graph ty n_id site  ty' n_id' site' =
    from_edge
      domain graph ((Fresh (ty,n_id),site),ToNode (Fresh (ty',n_id'),site'))
end

module ForState = struct
  type t = cc
  let compare cc cc' = Mods.int_compare cc.id cc'.id
end

module Map = MapExt.Make(ForState)
