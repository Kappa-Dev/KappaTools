open Mods

type link = UnSpec | Free | Link of int * int (** node_id, site_id *)

(** The link of site k of node i is stored in links(i).(k).

The internal state of site k of node i is store in internals(i).(k). A
negative number means UnSpec. *)
type cc = {
  id: int;
  nodes_by_type: int list array;
  links: link array IntMap.t; (*pattern graph id -> [|... link_j...|] i.e agent_id on site_j has a link*)
  internals: int array IntMap.t; (*internal state id -> [|... state_j...|] i.e agent_id on site_j has internal state state_j*)
}

type t = cc

type transition = {
  next: Navigation.step;
  dst: int (* id of cc and also address in the Env.domain map*);
  inj: Renaming.t list; (* From dst To ("this" cc + extra edge) *)
  above_obs: IntSet.t;
}

type point = {
  content: cc;
  is_obs_of: Operator.DepSet.t option;
  fathers: int (* t.id *) list;
  sons: transition list;
}

type work = {
  sigs: Signature.s;
  cc_env: point IntMap.t;
  cc_single_ag: (cc * Operator.DepSet.t) IntMap.t;
  reserved_id: int list array;
  used_id: int list array;
  free_id: int;
  cc_id: int;
  cc_links: link array IntMap.t;
  cc_internals: int array IntMap.t;
  dangling: int; (* node_id *)
}

module ContentAgent = struct
  type t = int * int * int (** (cc_id,type_id,node_id) *)

  let compare (cc,_,n) (cc',_,n') =
    let c = int_compare cc cc' in
    if c = 0 then int_compare n n' else c

  let rename wk cc inj (n_cc,n_ty,n_id as node) =
    if wk.cc_id = n_cc then (cc.id,n_ty, Renaming.apply inj n_id)
    else node

  let get_sort (_,ty,_) = ty

  let print ?sigs ?with_id f (cc,ty,i) =
    match sigs with
    | Some sigs ->
       Format.fprintf f "%a%t" (Signature.print_agent sigs) ty
		      (fun f -> match with_id with
				| None -> ()
				| Some () -> Format.fprintf f "/*%i*/" i)
    | None -> Format.fprintf f "cc%in%i" cc i

  let print_site ?sigs (cc,agent,i) f id =
    match sigs with
    | Some sigs ->
       Signature.print_site sigs agent f id
    | None -> Format.fprintf f "cc%in%is%i" cc i id

  let print_internal ?sigs (cc,agent,i) site f id =
    match sigs with
    | Some sigs ->
       Signature.print_site_internal_state sigs agent site f (Some id)
    | None -> Format.fprintf f "cc%in%is%i~%i" cc i site id
end

let raw_find_ty tys id =
  let rec aux i =
    assert (i >= 0);
    if List.mem id tys.(i) then i else aux (pred i)
  in aux (Array.length tys - 1)

let find_ty cc id = raw_find_ty cc.nodes_by_type id

let add_origin deps = function
  | None -> deps
  | Some x -> Operator.DepSet.add x deps

(** Errors *)
let already_specified ?sigs x i =
  ExceptionDefn.Malformed_Decl
    (Location.dummy_annot
       (Format.asprintf "Site %a of agent %a already specified"
			(ContentAgent.print_site ?sigs x) i
			(ContentAgent.print ?sigs ?with_id:None) x))

let dangling_node ~sigs tys x =
  ExceptionDefn.Malformed_Decl
    (Location.dummy_annot
       (Format.asprintf
	  "Cannot proceed because last declared agent %a/*%i*/%a"
	  (Signature.print_agent sigs) (raw_find_ty tys x) x
	  Format.pp_print_string " is not linked to its connected component."))

let identity_injection cc =
  Renaming.identity
    (Array.fold_left (fun x y -> List.rev_append y x) [] cc.nodes_by_type)

(** pick a root in the CC. Any root works.
In this case pick the last node of smallest type *)
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

let find_root_type cc = Tools.option_map fst (find_root cc)

(*turns a cc into a path(:list) in the domain*)
let to_navigation (full:bool) cc =
  let rec build_for (_,out as acc) don = function
    | [] -> List.rev out
    | h ::  t ->
       let first_ints,out_ints =
	   Tools.array_fold_lefti
	     (fun i (first,out as acc) v ->
	      if (full || first) && v >= 0 then
		(false,
		 (((if first
		    then Navigation.Fresh (h,find_ty cc h)
		    else Navigation.Existing h),i),
		  Navigation.ToInternal v)::out)
	      else acc)
	     acc (IntMap.find_default [||] h cc.internals)
       in
       let first_lnk,out'',todo =
	 Tools.array_fold_lefti
	   (fun i (first,ans,re as acc) ->
	    function
	    | UnSpec -> acc
	    | Free ->
	       if full || first
	       then (false,
		     (((if first
			then Navigation.Fresh (h,find_ty cc h)
			else Navigation.Existing h),i),
		      Navigation.ToNothing)::ans,re)
	       else acc
	    | Link (n,l) ->
	       if List.mem n don then acc
	       else if n = h || List.mem n re
	       then
		 if full
		 then (false,
		       (((if first
			  then Navigation.Fresh (h,find_ty cc h)
			  else Navigation.Existing h),i),
			Navigation.ToNode (Navigation.Existing n,l))::ans ,re)
		 else acc
	       else
		 (false,
		  (((if first
		     then Navigation.Fresh (h,find_ty cc h)
		     else Navigation.Existing h),i),
		   Navigation.ToNode(Navigation.Fresh(n,find_ty cc n),l))::ans,
		  n::re))
	   (first_ints,out_ints,t) (IntMap.find_default [||] h cc.links) in
       build_for (first_lnk,out'') (h::don) todo
  in
  match find_root cc with
  | None -> [] (*empty path for x0*)
  | Some (_,x) -> (*(ag_sort,ag_id)*)
     build_for (true,[]) (*wip*) [] (*already_done*) [x] (*todo*)

let print ?sigs ?with_id f cc =
  let print_intf (_,_,ag_i as ag) link_ids internals neigh =
    snd
      (Tools.array_fold_lefti
	 (fun p (not_empty,(free,link_ids as out)) el ->
	  let () =
	    if internals.(p) >= 0
	    then Format.fprintf f "%t%a"
				(if not_empty then Pp.comma else Pp.empty)
				(ContentAgent.print_internal ?sigs ag p) internals.(p)
	    else
	      if  el <> UnSpec then
		Format.fprintf f "%t%a"
			       (if not_empty then Pp.comma else Pp.empty)
			       (ContentAgent.print_site ?sigs ag) p in
	  match el with
	  | UnSpec ->
	     if internals.(p) >= 0
	     then let () = Format.fprintf f "?" in (true,out)
	     else (not_empty,out)
	  | Free -> true,out
	  | Link (dst_a,dst_p) ->
	     let i,out' =
	       match Int2Map.find_option (dst_a,dst_p) link_ids with
	       | Some x -> (x, out)
	       | None ->
		  (free,(succ free, Int2Map.add (ag_i,p) free link_ids)) in
	     let () = Format.fprintf f "!%i" i in
	     true,out') (false,link_ids) neigh) in
  let () = Format.pp_open_box f 2 in
  let () = match with_id with
    | Some () -> Format.fprintf f "/*cc%i*/@ " cc.id
    | None -> () in
  let (_,_) =
    IntMap.fold
      (fun x el (not_empty,link_ids) ->
       let ag_x = (cc.id,find_ty cc x,x) in
       let () =
	 Format.fprintf
	   f "%t@[<h>%a("
	   (if not_empty then Pp.comma else Pp.empty)
	   (ContentAgent.print ?sigs ?with_id) ag_x in
       let out = print_intf ag_x link_ids (IntMap.find_default [||] x cc.internals) el in
       let () = Format.fprintf f ")@]" in
       true,out) cc.links (false,(1,Int2Map.empty)) in
  Format.pp_close_box f ()

let print_dot sigs f cc =
  let pp_one_node x i f = function
    | UnSpec -> ()
    | Free ->
       let n = (cc.id,find_ty cc x,x) in
       let () = Format.fprintf
		  f "@[%a@ [label=\"%t\",@ height=\".1\",@ width=\".1\""
		  (ContentAgent.print_site ?sigs:None n) i Pp.bottom in
       let () =
	 Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
       let () = Format.fprintf
		  f "@[<b>%a ->@ %a@ @[[headlabel=\"%a\",@ weight=\"25\""
		  (ContentAgent.print_site ?sigs:None n) i
		  (ContentAgent.print ?sigs:None ?with_id:None) n
		  (ContentAgent.print_site ~sigs n) i in
       Format.fprintf f",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@,"
    | Link (y,j) ->
       let n = (cc.id,find_ty cc x,x) in
       let n' = (cc.id,find_ty cc y,y) in
       if x<y || (x=y && i<j) then
	 let () = Format.fprintf
		    f
		    "@[<b>%a ->@ %a@ @[[taillabel=\"%a\",@ headlabel=\"%a\""
		    (ContentAgent.print ?sigs:None ?with_id:None) n
		    (ContentAgent.print ?sigs:None ?with_id:None) n'
		    (ContentAgent.print_site ~sigs n) i
		    (ContentAgent.print_site ~sigs n') j in
	 Format.fprintf
	   f ",@ arrowhead=\"odot\",@ arrowtail=\"odot\",@ dir=\"both\"]@];@]@,"
  in
  let pp_one_internal x i f k =
    let n = (cc.id,find_ty cc x,x) in
    if k >= 0 then
      let () = Format.fprintf
		 f "@[%ai@ [label=\"%a\",@ height=\".1\",@ width=\".1\""
		 (ContentAgent.print_site ?sigs:None n) i
		 (ContentAgent.print_internal ~sigs n i) k in
      let () =
	Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
      let () = Format.fprintf
		 f "@[<b>%ai ->@ %a@ @[[headlabel=\"%a\",@ weight=25"
		 (ContentAgent.print_site ?sigs:None n) i
		 (ContentAgent.print ?sigs:None ?with_id:None) n
		 (ContentAgent.print_site ~sigs n) i in
      Format.fprintf f ",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@," in
  let pp_slot pp_el f (x,a) =
    Pp.array (fun _ -> ()) (pp_el x) f a in
  Format.fprintf
    f "@[<v>subgraph %i {@,%a%a%a}@]" cc.id
    (Pp.array (fun f -> Format.pp_print_cut f ())
	      (fun _ -> Pp.list (fun f -> Format.pp_print_cut f ())
				(fun f x ->
				 let n = (cc.id,find_ty cc x,x) in
				 Format.fprintf
				   f "@[%a [label=\"%a\"]@];@,"
				   (ContentAgent.print ?sigs:None ?with_id:None) n
				   (ContentAgent.print ~sigs ~with_id:()) n)))
    cc.nodes_by_type
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
	    IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (pp_slot pp_one_node)) cc.links
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
	    IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (pp_slot pp_one_internal)) cc.internals

let print_sons_dot sigs cc_id cc f sons =
  Pp.list Pp.space ~trailing:Pp.space
	  (fun f son -> Format.fprintf
			  f "@[cc%i -> cc%i [label=\"%a %a\"];@]" cc_id son.dst
			  (Navigation.print_step sigs (find_ty cc)) son.next
			  (Pp.list Pp.space Renaming.print) son.inj)
	  f sons

let print_point_dot sigs f (id,point) =
  let style = match point.is_obs_of with | Some _ -> "octagon" | None -> "box" in
  Format.fprintf f "@[cc%i [label=\"%a\", shape=\"%s\"];@]@,%a"
		 point.content.id (print ~sigs  ?with_id:None) point.content
		 style (print_sons_dot sigs id point.content) point.sons

module Env : sig
  type t

  val fresh :
    Signature.s -> int list array -> int -> point IntMap.t ->
    (cc * Operator.DepSet.t) IntMap.t -> t
  val empty : Signature.s -> t
  val finalize : t -> t
  val sigs : t -> Signature.s
  val find : t -> cc -> (int * Renaming.t list * point) option
  val navigate :
    t -> Navigation.t -> (int * Renaming.t list * point) option
  val get : t -> int -> point
  val add_point : int -> point -> t -> t
  val add_single_agent : int -> cc -> Operator.rev_dep option -> t -> cc * t
  val get_single_agent : int -> t -> (cc * Operator.DepSet.t) option
  val to_work : t -> work
  val nb_ag : t -> int
  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit
  end = struct
    type domain = Provisional of point IntMap.t | Final of point array

  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: domain;
    single_agent_points: (cc*Operator.DepSet.t) IntMap.t;
    mutable used_by_a_begin_new: bool;
  }

let fresh sigs id_by_type nb_id domain single_agent_points =
  {
    sig_decl = sigs;
    id_by_type = id_by_type;
    nb_id = nb_id;
    domain = Provisional domain;
    single_agent_points = single_agent_points;
    used_by_a_begin_new = false;
  }

let empty_point sigs =
  let nbt = Array.make (Signature.size sigs) [] in
  let empty_cc = {id = 0; nodes_by_type = nbt;
		  links = IntMap.empty; internals = IntMap.empty;} in
  {content = empty_cc; is_obs_of = None; fathers = []; sons = [];}

let empty sigs =
  let nbt' = Array.make (Signature.size sigs) [] in
  fresh sigs nbt' 1 (IntMap.add 0 (empty_point sigs) IntMap.empty) IntMap.empty

let finalize env =
  match env.domain with
  | Final _ -> env
  | Provisional s ->
     let si = match IntMap.max_key s with Some i -> succ i | None -> 0 in
     let out = Array.make si (empty_point env.sig_decl) in
     let () = IntMap.iter (fun i p -> out.(i) <- p) s in
     { env with domain = Final out}

let check_vitality env = assert (env.used_by_a_begin_new = false)

let print f env =
  let pp_point f p =
    Format.fprintf
      f "@[<hov 2>(%a)@ -> @[<h>%a@]@ %t-> @[(%a)@]@]"
      (Pp.list Pp.space Format.pp_print_int) p.fathers
      (print ~sigs:env.sig_decl ~with_id:()) p.content
      (fun f ->
       match p.is_obs_of with
       | None -> ()
       | Some deps ->
	  Format.fprintf
	    f "@[[%a]@]@ "
	    (Pp.set Operator.DepSet.elements Pp.space Operator.print_rev_dep)
	    deps)
      (Pp.list
	 Pp.space
	 (fun f s ->
	  Format.fprintf
	    f "@[%a%a@ %i[@[%a@]]@]"
	    (Navigation.print_step env.sig_decl (find_ty p.content)) s.next
	    (Pp.list
	       Pp.space
	       (fun f -> Format.fprintf f "(@[%a@])" Renaming.print))
	    s.inj s.dst
	    (Pp.set IntSet.elements Pp.space Format.pp_print_int)
	    s.above_obs))
      p.sons in
  Format.fprintf
    f "@[<v>%a%a@]"
    (Pp.set IntMap.bindings Pp.space ~trailing:Pp.space
	    (fun f (_,(cc,deps)) ->
	     Format.fprintf f "@[<h>%a@] @[[%a]@]"
			    (print ~sigs:env.sig_decl ~with_id:()) cc
			    (Pp.set Operator.DepSet.elements Pp.space Operator.print_rev_dep)
			    deps))
    env.single_agent_points
    (fun f -> function
	   | Provisional s ->
	      Pp.set IntMap.bindings Pp.space (fun f (_,p) -> pp_point f p) f s
	   | Final a -> Pp.array Pp.space (fun _ -> pp_point) f a)
    env.domain

let add_point id el env =
  {
    sig_decl = env.sig_decl;
    id_by_type = env.id_by_type;
    nb_id = env.nb_id;
    domain =
      (match env.domain with
      | Final _ -> failwith "Connected_component.Env.add_point on finalized env"
      | Provisional s -> Provisional (IntMap.add id el s));
    single_agent_points = env.single_agent_points;
    used_by_a_begin_new = false;
  }

let add_single_agent ty cc origin env =
  let (cc',deps) =
    IntMap.find_default (cc,Operator.DepSet.empty) ty env.single_agent_points in
  cc',
  {
    sig_decl = env.sig_decl;
    id_by_type = env.id_by_type;
    nb_id = env.nb_id;
    domain = env.domain;
    single_agent_points =
      IntMap.add ty (cc', add_origin deps origin) env.single_agent_points;
    used_by_a_begin_new = false;
  }

let get_single_agent ty env =
  IntMap.find_option ty env.single_agent_points

let get env cc_id =
  match env.domain with
  | Final a -> a.(cc_id)
  | Provisional s ->
     match IntMap.find_option cc_id s with
     | Some x -> x
     | None -> raise Not_found

let fresh_id env =
  let max_id_single =
    IntMap.fold (fun _ (cc,_) x -> max x cc.id) env.single_agent_points 0 in
  match match env.domain with
	| Final _ ->
	   failwith "Connected_component.Env.fresh_id on finalized env"
	| Provisional s -> IntMap.max_key s with
  | Some i -> succ (max max_id_single i)
  | None -> succ max_id_single

let sigs env = env.sig_decl

let to_work env =
  let () = check_vitality env in
  let () = env.used_by_a_begin_new <- true in
  {
    sigs = env.sig_decl;
    cc_env = (match env.domain with
	      | Provisional s -> s
	      | Final _ ->
		 failwith "Connected_component.Env.fresh_id on finalized env");
    cc_single_ag = env.single_agent_points;
    reserved_id = env.id_by_type;
    used_id = Array.make (Array.length env.id_by_type) [];
    free_id = env.nb_id;
    cc_id = fresh_id env;
    cc_links = IntMap.empty;
    cc_internals = IntMap.empty;
    dangling = 0;
  }

let navigate env nav =
  let rec aux injs_dst2nav pt_i = function
    | [] -> Some (pt_i,injs_dst2nav, get env pt_i)
    | e :: t ->
       let rec find_good_edge = function (*one should use a hash here*)
	 | [] -> None
	 | s :: tail ->
	    match Navigation.compatible_point injs_dst2nav s.next e with
	    | [] ->  find_good_edge tail
	    | inj' ->
	       aux (Tools.list_map_flatten
		      (fun x -> List.map (Renaming.compose false x) inj') s.inj) s.dst t
       in find_good_edge (get env pt_i).sons
  in aux [Renaming.empty] 0 nav

let find env cc =
  let nav = to_navigation true cc in
  (* let () = Format.eprintf *)
  (* 	     "@[[%a]@]@,%a@." (Pp.list Pp.space (print_edge (sigs env) cc)) nav *)
  (* 	     print env in *)
  navigate env nav

let nb_ag env = env.nb_id

let print_dot f env =
  let () = Format.fprintf f "@[<v>strict digraph G {@," in
  let () =
    match env.domain with
    | Provisional s ->
       Pp.set ~trailing:Pp.space IntMap.bindings Pp.space
	      (print_point_dot (sigs env)) f s
    | Final a ->
       Pp.array ~trailing:Pp.space Pp.space
		(fun i f s -> print_point_dot (sigs env) f (i,s)) f a
  in
  Format.fprintf f "}@]@."
end

let propagate_add_obs obs_id env cc_id =
  let rec aux son_id domain cc_id =
    let cc = Env.get domain cc_id in
    let sons' =
      Tools.list_smart_map
	(fun s -> if s.dst = son_id && not (IntSet.mem obs_id s.above_obs)
		  then {s with above_obs = IntSet.add obs_id s.above_obs}
		  else s) cc.sons in
    if sons' == cc.sons then domain
    else
      let env' =
	Env.add_point cc_id {cc with sons = sons'} domain in
      List.fold_left (aux cc_id) env' cc.fathers in
  List.fold_left (aux cc_id) env (Env.get env cc_id).fathers

exception Found

let remove_ag_cc inj2cc cc_id cc ag_id =
  let ty = find_ty cc ag_id in
  match cc.nodes_by_type.(ty) with
  | [] -> assert false
  | max :: tail as list ->
     let cycle =
       Renaming.cyclic_permutation_from_list
	 ~stop_at:ag_id list in
     let to_subst = Renaming.compose true inj2cc cycle in
     let new_nbt =
       Array.mapi (fun i l -> if i = ty then tail else l) cc.nodes_by_type in
     if Renaming.is_identity to_subst then
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
		let tmp = IntMap.find_default [||] max map in
		let map' = IntMap.add max (IntMap.find_default [||] s map) map in
		IntMap.add s tmp map') (Renaming.to_list cycle) map in
	 IntMap.remove max map' in
       let new_ints = swip cc.internals in
       let prelinks = swip cc.links in
       let new_links =
	 IntMap.map
	   (fun a -> Array.map (function
				 | (UnSpec | Free) as x -> x
				 | Link (n,s) as x ->
				    try Link (Renaming.apply cycle n,s)
				    with Renaming.Undefined -> x) a) prelinks in
       { id = cc_id; nodes_by_type = new_nbt;
	 links = new_links; internals = new_ints;},to_subst

let update_cc inj2cc cc_id cc ag_id links internals =
  if
    Array.fold_left
      (fun x -> function UnSpec -> x | (Free | Link _) -> false) true links
    && Array.fold_left (fun x i -> x && i < 0) true internals
  then true,remove_ag_cc inj2cc cc_id cc ag_id
  else
    false,({ id = cc_id;
      nodes_by_type = cc.nodes_by_type;
      internals = IntMap.add ag_id internals cc.internals;
      links = IntMap.add ag_id links cc.links;}, inj2cc)

let compute_cycle_edges cc =
  let rec aux don acc path ag_id =
    Tools.array_fold_lefti
      (fun i (don,acc as out) ->
	     function
	     | UnSpec | Free -> out
	     | Link (n',i') ->
		if List.mem n' don then out
		else
		  let edge = ((Navigation.Existing ag_id,i),
			      Navigation.ToNode(Navigation.Existing n',i')) in
		  if ag_id = n' then (don, edge::acc)
		  else
		    let rec extract_cycle acc' = function
		      | ((Navigation.Existing n,i),_ as e) :: t ->
			 if n' = n then
			   if i' = i then out
			   else (don,edge::e::acc')
			 else extract_cycle (e::acc') t
		      | ((Navigation.Fresh _,_),_) :: _ -> assert false
		      | [] ->
			 let (don',acc') = aux don acc (edge::path) n' in
			 (n'::don',acc') in
		    extract_cycle acc path)
      (don,acc) (IntMap.find_default [||] ag_id cc.links) in
  let rec element i t =
    if i = Array.length t then [] else
      match t.(i) with
      | [] -> element (succ i) t
      | h :: _ -> snd (aux [] [] [] h) in
  element 0 cc.nodes_by_type

let remove_cycle_edges complete_domain_with obs_id dst env free_id cc =
  let rec aux ((f_id,env'),out as acc) = function
    | ((Navigation.Existing n,i),
       Navigation.ToNode(Navigation.Existing n',i')) :: q ->
       let links = IntMap.find_default [||] n cc.links in
       let int = IntMap.find_default [||] n cc.internals in
       let links' = Array.copy links in
       let () = links'.(i) <- UnSpec in
       let has_removed,(cc_tmp,inj2cc) =
	 update_cc (identity_injection cc) f_id cc n links' int in
       let new_n' = Renaming.apply inj2cc n' in
       let links_dst = IntMap.find_default [||] new_n' cc_tmp.links in
       let int_dst = IntMap.find_default [||] new_n' cc_tmp.internals in
       let links_dst' = Array.copy links_dst in
       let () = links_dst'.(i') <- UnSpec in
       let has_removed',(cc',inj2cc') =
	 update_cc inj2cc f_id cc_tmp new_n' links_dst' int_dst in
       let e' =
	 if n = n' && has_removed'
	 then
	   (Navigation.Fresh (Renaming.apply inj2cc' n,find_ty cc n),i),
	   Navigation.ToNode (Navigation.Existing (Renaming.apply inj2cc' n),i')
	 else
	   ((if has_removed
	     then Navigation.Fresh (Renaming.apply inj2cc' n,find_ty cc n)
	     else Navigation.Existing (Renaming.apply inj2cc' n)),i),
	   Navigation.ToNode
	     ((if has_removed'
	       then Navigation.Fresh (Renaming.apply inj2cc' n',find_ty cc n')
	       else Navigation.Existing (Renaming.apply inj2cc' n')),i') in
       let pack,ans =
	 complete_domain_with obs_id dst env' f_id
			      cc' e' inj2cc' in
       aux (pack,ans::out) q
    | [] -> acc
    | (((Navigation.Existing _,_),Navigation.ToNode(Navigation.Fresh _,_)) |
       ((Navigation.Existing _,_),
	(Navigation.ToInternal _ | Navigation.ToNothing)) |
       ((Navigation.Fresh _,_),_))::_ -> assert false in
  aux ((free_id,env),[]) (compute_cycle_edges cc)

let compute_father_candidates complete_domain_with obs_id dst env free_id cc =
  let agent_is_removable lp links internals =
    try
      let () = Array.iter (fun el -> if el >= 0 then raise Found) internals in
      let () =
	Array.iteri
	  (fun i el -> if i<>lp && el<>UnSpec then raise Found) links in
      true
    with Found -> false in
  let remove_one_internal acc ag_id links internals =
    Tools.array_fold_lefti
      (fun i ((f_id,env'), out as acc) el ->
       if el >= 0 then
	 let int' = Array.copy internals in
	 let () = int'.(i) <- -1 in
	 let has_removed,(cc',inj2cc') =
	   update_cc (identity_injection cc) f_id cc ag_id links int' in
	 let pack,ans =
	   complete_domain_with
	     obs_id dst env' f_id cc'
	     (((if has_removed
		then Navigation.Fresh (Renaming.apply inj2cc' ag_id, find_ty cc ag_id)
		else Navigation.Existing (Renaming.apply inj2cc' ag_id)),i),Navigation.ToInternal el)
	     inj2cc' in
	 (pack,ans::out)
       else acc)
      acc internals in
  let remove_one_frontier acc ag_id links internals =
    Tools.array_fold_lefti
      (fun i ((f_id,env'),out as acc) ->
       function
       | UnSpec -> acc
       | Free ->
	  let links' = Array.copy links in
	  let () = links'.(i) <- UnSpec in
	  let has_removed,(cc',inj2cc') =
	    update_cc (identity_injection cc) f_id cc ag_id links' internals in
	  let pack,ans =
	    complete_domain_with
	      obs_id dst env' f_id cc'
	      (((if has_removed
		 then Navigation.Fresh (Renaming.apply inj2cc' ag_id, find_ty cc ag_id)
		 else Navigation.Existing (Renaming.apply inj2cc' ag_id)),i),Navigation.ToNothing)
		 inj2cc' in
	  (pack,ans::out)
       | Link (n',i') ->
	  if not (agent_is_removable i links internals) then acc else
	    let links_dst = IntMap.find_default [||] n' cc.links in
	    let int_dst = IntMap.find_default [||] n' cc.internals in
	    let links_dst' = Array.copy links_dst in
	    let () = links_dst'.(i') <- UnSpec in
	    let has_removed,(cc',inj2cc') =
	      update_cc (identity_injection cc) f_id cc n' links_dst' int_dst in
	    let cc'',inj2cc'' =
	      remove_ag_cc inj2cc' f_id cc' (Renaming.apply inj2cc' ag_id) in
	    let pack,ans =
	      complete_domain_with
		obs_id dst env' f_id cc''
		(((if has_removed
		   then Navigation.Fresh (Renaming.apply inj2cc'' n', find_ty cc n')
		   else Navigation.Existing (Renaming.apply inj2cc'' n')),i'),
		 Navigation.ToNode
		   (Navigation.Fresh(Renaming.apply inj2cc'' ag_id,find_ty cc ag_id),i))
		inj2cc'' in
	    (pack,ans::out))
      (remove_one_internal acc ag_id links internals) links in
  let remove_or_remove_one acc ag_id links internals =
    remove_one_frontier acc ag_id links internals in
  IntMap.fold (fun i links acc ->
	       remove_or_remove_one acc i links (IntMap.find_default [||] i cc.internals))
	      cc.links
	      (remove_cycle_edges complete_domain_with obs_id dst env free_id cc)

let rec complete_domain_with obs_id dst env free_id cc edge inj_dst2cc =
  let rec new_son inj_cc2found = function
    | [] ->
       [{ dst = dst;
	  next = Navigation.rename_step inj_cc2found edge;
	  inj = [Renaming.compose true inj_dst2cc inj_cc2found];
	  above_obs = IntSet.singleton obs_id;}]
    | h :: t when h.dst = dst && (h.next = edge) ->
       {h with inj = (Renaming.compose true inj_dst2cc inj_cc2found) :: h.inj} :: t
    | h :: t -> h :: new_son inj_cc2found t in
  let known_cc = Env.find env cc in
  match known_cc with
  | Some (cc_id, inj_cc_id2cc, point') ->
     let point'' =
       {point' with
	 sons = new_son (Renaming.inverse (List.hd inj_cc_id2cc)) point'.sons} in
     let completed =
       propagate_add_obs obs_id (Env.add_point cc_id point'' env) cc_id in
     (free_id,completed), cc_id
  | None ->
     let son = new_son (identity_injection cc) [] in
     add_new_point ~origin:None obs_id env free_id son cc
and add_new_point ~origin obs_id env free_id sons cc =
  let (free_id'',env'),fathers =
    compute_father_candidates complete_domain_with obs_id cc.id env (succ free_id) cc in
  let completed =
    Env.add_point
      cc.id
      {content = cc;sons=sons; fathers = fathers;
       is_obs_of =
	 if cc.id = obs_id then Some (add_origin Operator.DepSet.empty origin) else None;}
      env' in
  ((free_id'',completed),cc.id)

let add_domain ?origin env cc =
  let nav = to_navigation true cc in
  if nav = [] then
    match find_root cc with
    | None -> assert false
    | Some (ty,_) ->
       let cc',env' = Env.add_single_agent ty cc origin env in
       env',identity_injection cc',cc'
  else
    let known_cc = Env.navigate env nav in
    match known_cc with
    | Some (id,inj,point) ->
       (match point.is_obs_of with
	| Some deps ->
	   let point' =
	     { point with
	       is_obs_of = Some (add_origin deps origin)} in
	   Env.add_point id point' env
	| None ->
	   let point' =
	     { point with
	       is_obs_of = Some (add_origin Operator.DepSet.empty origin)} in
	   propagate_add_obs id (Env.add_point id point' env) id),
       List.hd inj,point.content
    | None ->
       let (_,env'),_ = add_new_point ~origin cc.id env cc.id [] cc in
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

let finish_new ?origin wk =
  let () = check_dangling wk in
  (* rebuild env *)
  let () =
    Tools.iteri
      (fun i ->
       wk.reserved_id.(i) <- List.rev_append wk.used_id.(i) wk.reserved_id.(i))
      (Array.length wk.used_id) in
  let cc_candidate =
    { id = wk.cc_id; nodes_by_type = wk.used_id;
      links = wk.cc_links; internals = wk.cc_internals; } in
  let env =
    Env.fresh wk.sigs wk.reserved_id wk.free_id wk.cc_env wk.cc_single_ag in
  add_domain ?origin env cc_candidate


let new_link wk ((cc1,_,x as n1),i) ((cc2,_,y as n2),j) =
  let pos = Location.dummy in
  let () = check_node_adequacy ~pos wk cc1 in
  let () = check_node_adequacy ~pos wk cc2 in
  let x_n = IntMap.find_default [||] x wk.cc_links in
  let y_n = IntMap.find_default [||] y wk.cc_links in
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
  let () = check_node_adequacy ~pos:Location.dummy wk cc in
  let x_n = IntMap.find_default [||] x wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- Free in
    wk

let new_internal_state wk ((cc,_,x as n), i) va =
  let () = check_node_adequacy ~pos:Location.dummy wk cc in
  let x_n = IntMap.find_default [||] x wk.cc_internals in
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
      { wk with
	dangling = if IntMap.is_empty wk.cc_links then 0 else h;
	cc_links = IntMap.add h (Array.make arity UnSpec) wk.cc_links;
	cc_internals = IntMap.add h (Array.make arity (-1)) wk.cc_internals;
      })
  | [] ->
     let () = wk.used_id.(type_id) <- wk.free_id :: wk.used_id.(type_id) in
     let node = (wk.cc_id, type_id, wk.free_id) in
     (node,
      { wk with
	free_id = succ wk.free_id;
	dangling = if IntMap.is_empty wk.cc_links then 0 else wk.free_id;
	cc_links =
	  IntMap.add wk.free_id (Array.make arity UnSpec) wk.cc_links;
	cc_internals =
	  IntMap.add wk.free_id (Array.make arity (-1)) wk.cc_internals;
      })

module NodeSetMap = SetMap.Make(ContentAgent)
module NodeMap = NodeSetMap.Map

module Matching = struct
  type t = int NodeMap.t IntMap.t * IntSet.t
  (* (map,set)
      map: point_i -> (node_j(i) -> id_node_graph_in_current_matching)
      set:codomain of current matching *)

  let empty = (IntMap.empty, IntSet.empty)

  let debug_print f (m,_co) =
    Format.fprintf
      f "@[(%a)@]"
      (Pp.set IntMap.bindings Pp.comma
	      (fun f (ccid,nm) ->
	       Pp.set NodeMap.bindings Pp.comma
		      (fun f (node,dst) ->
		       Format.fprintf
			 f "%i:%a->%i" ccid
			 (ContentAgent.print ?sigs:None ~with_id:()) node dst
		      ) f nm)) m

  (**)
  let from_renaming cc =
    Renaming.fold
      (fun src dst -> function
      | None -> None (*in case of clash*)
      | Some (acc,co) ->
	 if IntSet.mem dst co then None
	 else
	   Some (NodeMap.add (cc.id,find_ty cc src,src) dst acc, IntSet.add dst co))

  (*- rm - reconstruct: Edges.t -> t -> int -> cc -> int -> t option*)
  let reconstruct graph inj id cc root =
    (* -rm - full_rename: Renaming.t option *)
    let _,full_rename =
      (*- rm - to_navigation: bool -> cc -> list *)
      match to_navigation false cc with
      | _::_ as nav ->
	 List.fold_left
           (fun (root,inj_op) nav ->
            match inj_op with
            | None -> None,None
            | Some inj ->
	       None,Navigation.injection_for_one_more_edge ?root inj graph nav)
           (Some root,Some Renaming.empty) nav
	   (*- rm - find_root: cc -> (type, node) option *)
      | [] -> match find_root cc with
	      | None -> failwith "Matching.reconstruct cc error"
	      (*- rm - add : int -> int -> Renaming.t -> Renaming.t *)
	      | Some (_,id) -> None, Some (Renaming.add id root Renaming.empty) in
    match full_rename with
    | None -> failwith "Matching.reconstruct renaming error"
    | Some rename ->
       match from_renaming cc rename (Some (NodeMap.empty, snd inj)) with
       | None -> None
       | Some (inj',co) -> Some (IntMap.add id inj' (fst inj),co)

  (* get : (ContentAgent.t * int) -> t -> int *)
  let get (node,id) (t,_) =
    match NodeMap.find_option node (IntMap.find_default NodeMap.empty id t) with
    | Some x -> x
    | None -> raise Not_found

  (*edges: list of concrete edges,
returns the roots of observables that are above in the domain*)

  (* get_all : Edges.t -> t -> cc -> int -> int list *)
  let get_all graph inj cc root =
    let cc_id_in_rule = 1 in
    match reconstruct graph inj cc_id_in_rule cc root with
    | None -> []
    | Some match_nodes_cc_graph ->
       let rec aux ty =
	 if ty = Array.length cc.nodes_by_type then []
	 else List.append
		(List.map (fun node_id ->
			   let node = (cc.id, ty, node_id) in
			   get (node, cc_id_in_rule) match_nodes_cc_graph)
			  cc.nodes_by_type.(ty))
		(aux (succ ty))
       in aux 0

  (* get_all_with_types : Edges.t -> t -> cc -> int -> int *int list *)
  let get_all_with_types graph inj cc root =
    let cc_id_in_rule = 1 in
    match reconstruct graph inj cc_id_in_rule cc root with
    | None -> []
    | Some match_nodes_cc_graph ->
       let rec aux ty =
	 if ty = Array.length cc.nodes_by_type then []
	 else List.append
		(List.map (fun node_id ->
			   let node = (cc.id, ty, node_id) in
			   (get (node, cc_id_in_rule) match_nodes_cc_graph,ty))
			  cc.nodes_by_type.(ty))
		(aux (succ ty))
       in aux 0

  module Cache =
    struct
      type t = int * point * Renaming.t
      let compare (a,_,a') (b,_,b') =
	let c = Mods.int_compare a b in
	if c = 0 then Renaming.compare a' b' else c
    end
  module CacheSetMap = SetMap.Make(Cache)

  type cache = CacheSetMap.Set.t
  let empty_cache = CacheSetMap.Set.empty

  let from_edge domain graph acc edges =
    let get_root inj point =
      match find_root point.content with
      | None -> assert false
      | Some (root_type,root) -> Renaming.apply inj root,root_type in
    let rec aux cache (obs,rev_deps as acc) = function
      | [] -> acc,cache
      | (_pid,point,inj_point2graph as current) :: remains ->
	 let root_bundle =
	   get_root inj_point2graph point in
	 let acc' =
	   match point.is_obs_of with
	   | None -> acc
	   | Some ndeps ->
	       ((point.content,root_bundle) :: obs,
		Operator.DepSet.union rev_deps ndeps) in
	 let remains' =
	   List.fold_left
	     (fun re son ->
	      match Navigation.injection_for_one_more_edge
		      inj_point2graph graph son.next with
	      | None -> re
	      | Some inj' ->
		 let p' = Env.get domain son.dst in
		 List.fold_left
		   (fun remains renaming ->
		    let rename = Renaming.compose false renaming inj' in
		    let next = (son.dst,p',rename) in
		    if CacheSetMap.Set.mem next cache
		    then remains
		    else next::remains) re son.inj)
	     remains point.sons in
	 aux (CacheSetMap.Set.add current cache) acc' remains' in
    match Env.navigate domain edges with
    | None -> acc
    | Some (pid,injs,point) ->
       List.fold_left
	 (fun (out,cache) inj -> aux cache out [(pid,point,inj)])
	 acc injs

  let observables_from_agent
	domain graph ((obs,rdeps),cache as acc) (_,ty as node) =
    if Edges.is_agent node graph
    then match Env.get_single_agent ty domain with
	 | Some (cc,deps) ->
	    ((cc,node)::obs,Operator.DepSet.union rdeps deps),cache
	 | None -> acc
    else acc

  let observables_from_free domain graph acc node site =
    from_edge domain graph acc
	      [(Navigation.Fresh node,site),Navigation.ToNothing]
  let observables_from_internal domain graph acc node site id =
    from_edge domain graph acc
	      [(Navigation.Fresh node,site),Navigation.ToInternal id]
  let observables_from_link domain graph acc n site  n' site' =
    from_edge domain graph acc
	      [(Navigation.Fresh n,site),
	       Navigation.ToNode (Navigation.Fresh n',site')]
end

let compare_canonicals cc cc' = Mods.int_compare cc.id cc'.id
let is_equal_canonicals cc cc' = compare_canonicals cc cc' = 0

module ForState = struct
    type t = cc
    let compare = compare_canonicals
end

module SetMap = SetMap.Make(ForState)
module Set = SetMap.Set
module Map = SetMap.Map
