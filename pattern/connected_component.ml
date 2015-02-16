open Mods

type node = int * int * int (** (cc_id,type_id,node_id) *)
type link = UnSpec | Free | Link of node * int

(** The link of site k of node i is stored in links(i).(k).

The internal state of site k of node i is store in internals(i).(k). A
negative number means UnSpec. *)
type t = {
  id: int;
  nodes_by_type: int list array;
  links: link array IntMap.t;
  internals: int array IntMap.t;
}

type work = {
  sigs: Signature.s;
  cc_env: t IntMap.t;
  reserved_id: int list array;
  used_id: int list array;
  free_id: int;
  cc_id: int;
  cc_links: link array IntMap.t;
  cc_internals: int array IntMap.t;
  dangling: node;
}

type env = {
  sig_decl: Signature.s;
  id_by_type: int list array;
  nb_id: int;
  ccs: t IntMap.t;
  mutable used_by_a_begin_new: bool;
}

(** Errors *)
let print_bot f = Format.pp_print_string f "\xE2\x8A\xA5"
let print_site ?sigs (cc,agent,i) f id =
  match sigs with
  | Some sigs ->
     Signature.print_site sigs agent f id
  | None -> Format.fprintf f "cc%in%is%i" cc i id
let print_node ?sigs f (cc,ty,i) =
  match sigs with
  | Some sigs -> Format.fprintf f "%a/*%i*/" (Signature.print_agent sigs) ty i
  | None -> Format.fprintf f "cc%in%i" cc i
let print_internal ?sigs (_,agent,_) site f id =
  match sigs with
  | Some sigs ->
     Signature.print_site_agent sigs agent site f id
  | None -> Format.pp_print_int f id

let already_specified ?sigs x i =
  ExceptionDefn.Malformed_Decl
    (Term.with_dummy_pos
       (Format.asprintf "Site %a of agent %a already specified"
			(print_site ?sigs x) i (print_node ?sigs) x))

let dangling_node ?sigs x =
  ExceptionDefn.Malformed_Decl
    (Term.with_dummy_pos
       (Format.asprintf
	  "Cannot proceed because last declared agent %a%a" (print_node ?sigs) x
	  Format.pp_print_string " is not linked to its connected component."))

(** Operation on env *)
let new_env sigs id_by_type nb_id ccs =
  {
    sig_decl = sigs;
    id_by_type = id_by_type;
    nb_id = nb_id;
    ccs = ccs;
    used_by_a_begin_new = false;
  }

let empty_env sigs =
  new_env sigs (Array.make (NamedDecls.size sigs) []) 0 IntMap.empty

let check_vitality env = assert (env.used_by_a_begin_new = false)

let cc_map env = env.ccs

(** Operations on cc *)
let find_ty env id =
  let rec aux i =
    if List.mem id env.nodes_by_type.(i) then i else aux (succ i)
  in aux 0

let print env f cc =
  let sigs = env.sig_decl in
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
				  (print_internal ~sigs ag p) internals.(p)
	      else
		if  el <> UnSpec then
		  Format.fprintf f "%t%a"
				 (if not_empty then Pp.comma else Pp.empty)
				 (print_site ~sigs ag) p in
	    match el with
	    | UnSpec ->
	       if internals.(p) >= 0
	       then let () = Format.fprintf f "?" in (true,out)
	       else (not_empty,out)
	    | Free -> true,out
	    | Link ((_,_,dst_a),dst_p) ->
	       let i,out' =
		 try (Int2Map.find (dst_a,dst_p) link_ids, out)
		 with Not_found ->
		   (free,(succ free, Int2Map.add (ag_i,p) free link_ids)) in
	       let () = Format.fprintf f "!%i" i in
	       true,out') (false,link_ids) neigh) in
  let () = Format.pp_open_box f 2 in
  let (_,_) =
    IntMap.fold
      (fun x el (not_empty,link_ids) ->
       let ag_x = (cc.id,find_ty cc x,x) in
       let () =
	 Format.fprintf
	   f "%t@[<h>%a("
	   (if not_empty then Pp.comma else Pp.empty)
	   (print_node ~sigs) ag_x in
       let out = print_intf ag_x link_ids (IntMap.find x cc.internals) el in
       let () = Format.fprintf f ")@]" in
       true,out) cc.links (false,(1,Int2Map.empty)) in
  Format.pp_close_box f ()

let print_dot env f cc =
  let sigs = env.sig_decl in
  let pp_one_node x i f = function
    | UnSpec -> ()
    | Free ->
       let n = (cc.id,find_ty cc x,x) in
       if i <> 0 then
	 let () = Format.fprintf
		    f "@[%a@ [label=\"%t\",@ height=\".1\",@ width=\".1\""
		    (print_site ?sigs:None n) i print_bot in
	 let () =
	   Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
	 let () = Format.fprintf
		    f "@[<b>%a ->@ %a@ @[[headlabel=\"%a\",@ weight=\"25\""
		    (print_site ?sigs:None n) i (print_node ?sigs:None) n
		    (print_site ~sigs n) i in
	 Format.fprintf f",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@,"
       else Format.fprintf f "@[%a [label=\"%a\"]@];@,"
			   (print_node ?sigs:None) n (print_node ~sigs) n
    | Link ((_,_,y as n'),j) ->
       let n = (cc.id,find_ty cc x,x) in
       if x<y || (x=y && i<j) then
	 let () = Format.fprintf
		    f
		    "@[<b>%a ->@ %a@ @[[taillabel=\"%a\",@ headlabel=\"%a\""
		    (print_node ?sigs:None) n (print_node ?sigs:None) n'
		    (print_site ~sigs n) i (print_site ~sigs n') j in
	 Format.fprintf
	   f ",@ arrowhead=\"odot\",@ arrowtail=\"odot\",@ dir=\"both\"]@];@]@,"
  in
  let pp_one_internal x i f k =
    let n = (cc.id,find_ty cc x,x) in
    if k >= 0 then
      let () = Format.fprintf
		 f "@[%ai@ [label=\"%a\",@ height=\".1\",@ width=\".1\""
		 (print_site ?sigs:None n) i (print_internal ~sigs n i) k in
      let () =
	Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
      let () = Format.fprintf
		 f "@[<b>%ai ->@ %a@ @[[headlabel=\"%a\",@ weight=25"
		 (print_site ?sigs:None n) i (print_node ?sigs:None) n
		 (print_site ~sigs n) i in
      Format.fprintf f ",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@," in
  let pp_slot pp_el f (x,a) =
    Pp.array (fun f -> ()) (pp_el x) f a in
  Format.fprintf
    f "@[<v>subgraph %i {@,%a%a}@]" cc.id
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
	    IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (pp_slot pp_one_node)) cc.links
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
	    IntMap.bindings (fun f -> Format.pp_print_cut f ())
	    (pp_slot pp_one_internal)) cc.internals

let equal cc1 cc2 =
  let always_equal_min_but_not_null _ (l,_,_ as p) l1 l2 =
    let l' = List.length l1 in
    if l' <> List.length l2 then raise Not_found
    else if l = 0 || (l' > 0 && l' < l) then (l',l1,l2) else p in
  let internals_are_ok iso =
    IntMap.fold
      (fun k a out->
       out &&
	 let a' = IntMap.find (IntMap.find k iso) cc2.internals in
	 Tools.array_fold_left2i (fun _ b x y -> b && x = y) true a a')
      cc1.internals true in
  let rec admissible_mapping iso = function
    | [] -> internals_are_ok iso
    | (x,y) :: t ->
       try IntMap.find x iso = y && admissible_mapping iso t
       with Not_found ->
	 let iso' = IntMap.add x y iso in
	 let n_x = IntMap.find x cc1.links in
	 let n_y = IntMap.find y cc2.links in
	 try
	   let remains =
	     Tools.array_fold_left2i
	       (fun _ out a b -> match a,b with
			       | ((UnSpec, UnSpec) | (Free, Free)) -> out
			       | Link ((_,_,a),i), Link ((_,_,b),j)
				    when i = j -> (a,b)::out
			       | _, _ -> raise Not_found) t n_x n_y in
	   admissible_mapping iso' remains
	 with Not_found -> false
  in cc1 == cc2 ||
       try
	 let (_,l1,l2) =
	   Tools.array_fold_left2i always_equal_min_but_not_null (0,[],[])
				  cc1.nodes_by_type cc2.nodes_by_type in
	 match l1 with
	 | [] -> true
	 | h :: _ ->
	    List.exists (fun x -> admissible_mapping IntMap.empty [(h,x)]) l2
       with Not_found -> false

(** Operation to create cc *)
let check_dangling wk =
  if wk.dangling <> (0,0,0) then
    raise (dangling_node ~sigs:wk.sigs wk.dangling)
let check_node_adequacy ~pos wk cc_id =
  if wk.cc_id <> cc_id then
    raise (
	ExceptionDefn.Malformed_Decl
	  (Format.asprintf
		"A node from a different connected component has been used."
	  ,pos))

let begin_new env =
  let () = check_vitality env in
  let () = env.used_by_a_begin_new <- true in
  {
    sigs = env.sig_decl;
    cc_env = env.ccs;
    reserved_id = env.id_by_type;
    used_id = Array.make (Array.length env.id_by_type) [];
    free_id = env.nb_id;
    cc_id = IntMap.size env.ccs;
    cc_links = IntMap.empty;
    cc_internals = IntMap.empty;
    dangling = (0,0,0);
  }

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
  match IntMap.fold (fun _ cc -> function
			      | Some _ as x -> x
			      | None ->
				 if equal cc_candidate cc then Some cc else None)
		    wk.cc_env None with
  | None ->
     (new_env wk.sigs wk.reserved_id wk.free_id
	      (IntMap.add wk.cc_id cc_candidate wk.cc_env), cc_candidate)
  | Some cc -> (new_env wk.sigs wk.reserved_id wk.free_id wk.cc_env, cc)

let get_site_id ?(pos=(Lexing.dummy_pos,Lexing.dummy_pos)) wk (_,ty,_) site =
  Signature.num_of_site (site,pos) (snd wk.sigs.NamedDecls.decls.(ty))

let new_link ?(pos=(Lexing.dummy_pos,Lexing.dummy_pos))
	     wk ((cc1,_,x as n1),i) ((cc2,_,y as n2),j) =
  let () = check_node_adequacy ~pos wk cc1 in
  let () = check_node_adequacy ~pos wk cc2 in
  let x_n = IntMap.find x wk.cc_links in
  let y_n = IntMap.find y wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n1 i)
  else if y_n.(j) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n2 j)
  else
    let () = x_n.(i) <- Link (n2,j) in
    let () = y_n.(j) <- Link (n1,i) in
    if wk.dangling = n1 || wk.dangling = n2
    then { wk with dangling = (0,0,0) }
    else wk

let new_free ?(pos=(Lexing.dummy_pos,Lexing.dummy_pos))
	     wk ((cc,_,x as n),i) =
  let () = check_node_adequacy ~pos wk cc in
  let x_n = IntMap.find x wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- Free in
    wk

let raw_new_internal_state wk ((cc,_,x as n), i) va =
  let () = check_node_adequacy ~pos:(Lexing.dummy_pos,Lexing.dummy_pos)
			       wk cc in
  let x_n = IntMap.find x wk.cc_internals in
  if x_n.(i) >= 0 then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- va in
    wk

let new_internal_state ?(pos=(Lexing.dummy_pos,Lexing.dummy_pos))
		       wk ((_,ty,_),site_id as place) va =
  let internal_state_id =
    Signature.num_of_internal_state site_id (va,pos)
				    (snd wk.sigs.NamedDecls.decls.(ty)) in
  raw_new_internal_state wk place internal_state_id

let raw_new_node wk type_id =
  let () = check_dangling wk in
  let arity = Signature.arity (snd wk.sigs.NamedDecls.decls.(type_id)) in
  match wk.reserved_id.(type_id) with
  | h::t ->
     let () = wk.used_id.(type_id) <- h :: wk.used_id.(type_id) in
     let () = wk.reserved_id.(type_id) <- t in
     let node = (wk.cc_id,type_id,h) in
     (node,
      new_free
	{ wk with
	  dangling = if IntMap.is_empty wk.cc_links then (0,0,0) else node;
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
	  dangling = if IntMap.is_empty wk.cc_links then (0,0,0) else node;
	  cc_links =
	    IntMap.add wk.free_id (Array.make arity UnSpec) wk.cc_links;
	  cc_internals =
	    IntMap.add wk.free_id (Array.make arity (-1)) wk.cc_internals;
	} (node,0))

let new_node ?(pos=(Lexing.dummy_pos,Lexing.dummy_pos)) wk typ =
  raw_new_node wk (NamedDecls.elt_id ~kind:"type of node" wk.sigs (typ,pos))
