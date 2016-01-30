type internal = int option
type link = FREE | VAL of int
type agent =
    { a_id: int; a_type: int; a_ports: link array; a_ints: internal array; }
type t = agent list

let print_link f = function
  | FREE -> ()
  | VAL i -> Format.fprintf f "!%i" i

let print_intf compact with_link sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      let () = Format.fprintf
		 f "%t%a%a"
		 (if empty then Pp.empty
		  else if compact then Pp.compact_comma else Pp.comma)
		 (Signature.print_site_internal_state sigs ag_ty i)
		 ints.(i) (if with_link then print_link else (fun _ _ -> ()))
			  ports.(i) in
      aux false (succ i) in
  aux true 0

let print_agent compact link sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.a_type
		 (print_intf compact link sigs ag.a_type) (ag.a_ports,ag.a_ints)

let get_color =
  let store = Hashtbl.create 10 in
  fun i ->
  try Hashtbl.find store i
  with Not_found ->
    let v = Format.sprintf "#%x%x%x" (Random.int 255)
			   (Random.int 255) (Random.int 255) in
    let () = Hashtbl.add store i v in v

let print_dot sigs nb_cc f mix =
  Pp.listi
    Pp.empty
    (fun i f ag ->
     Format.fprintf
       f "node%d_%d [label = \"@[<h>%a@]\", color = \"%s\", style=filled];@,"
       nb_cc i (print_agent true false sigs) ag
       (get_color ag.a_type);
     Format.fprintf
       f "node%d_%d -> counter%d [style=invis];@," nb_cc i nb_cc) f mix;
  ignore @@
    List.fold_left
      (fun (a,acc) ag ->
       let acc' =
	 Tools.array_fold_lefti
	   (fun s acc ->
	    function
	    | FREE -> acc
	    | VAL i ->
	       match Mods.IntMap.pop i acc with
	       | None,acc -> Mods.IntMap.add i (a,ag,s) acc
	       | Some (a',ag',s'),acc' ->
		  Format.fprintf
		    f
		    "node%d_%d -> node%d_%d [taillabel=\"%a\", headlabel=\"%a\", dir=none];@,"
		    nb_cc a nb_cc a' (Signature.print_site sigs ag.a_type) s
		    (Signature.print_site sigs ag'.a_type) s';
		  acc')
	   acc ag.a_ports in
       (succ a,acc'))
      (0,Mods.IntMap.empty) mix

let print ~compact sigs f mix =
  Pp.list Pp.comma (print_agent compact true sigs) f mix

let get_destination_of i l =
  let get_port_linked_on i ports =
    Tools.array_fold_lefti
      (fun s acc ->
       function
       | FREE -> acc
       | VAL j ->
	  if i = j then
	    match acc with
	    | None -> Some s
	    | Some _ ->
	       failwith "Raw_mixture.get_destination_of"
	  else acc)
      None ports in
  List.fold_left
    (fun (oui,non) ag ->
     match get_port_linked_on i ag.a_ports with
     | None -> (oui,ag::non)
     | Some s -> ((s,ag)::oui,non)) ([],[]) l

let rec agents_are_compatibles remains = function
  | [] -> remains = ([],[])
  | (o,p)::q ->
     o.a_type = p.a_type &&
       let i_ok =
	 Tools.array_fold_left2i
	   (fun _ b x y ->
	    b && match x,y with
		 | Some a, Some b -> (a = b)
		 | None, None -> true
		 | (Some _ | None), _ -> false) true o.a_ints p.a_ints in
       i_ok &&
	 match Tools.array_fold_left2i
		 (fun _ c x y ->
		  match c with
		  | None -> c
		  | Some (todo,(g,h)) ->
		     match x, y with
		     | (FREE, VAL _ | VAL _, FREE) -> None
		     | FREE, FREE -> c
		     | VAL a, VAL b ->
			match get_destination_of a g,get_destination_of b h with
			| ([],_), ([],_) -> c
			| ([s,x],g'), ([s',y],h')
			  when s = s' -> Some ((x,y)::todo,(g',h'))
			| _, _ -> None
		 )
		 (Some (q,remains)) o.a_ports p.a_ports with
	 | Some (todo',rem') -> agents_are_compatibles rem' todo'
	 | _ -> false

let classify_by_type mix =
  let rec classify ag = function
    | [] -> [ag.a_type,1,[ag]]
    | (ty,nb,ags as h)::t as l ->
       if ag.a_type < ty
       then (ag.a_type,1,[ag])::l
       else if ag.a_type = ty
       then (ty,succ nb,ag::ags)::t
       else h::classify ag t
  in List.fold_left (fun acc x -> classify x acc) [] mix

let min_not_null l1 l2 =
  let rec aux va out = function
    | [],[] -> Some out
    | [], _::_ | _::_, [] -> None
    | (ty1,nb1,ag1)::t1, (ty2,nb2,ag2)::t2 ->
       if ty1 <> ty2 || nb1 <> nb2 then None
       else if nb1 < va then aux nb1 (ag1,ag2) (t1,t2)
       else aux va out (t1,t2) in
  match (l1,l2) with
  | [],[] -> Some ([],[])
  | [], _::_ | _::_, [] -> None
  | (ty1,nb1,ag1)::t1, (ty2,nb2,ag2)::t2 ->
     if ty1 <> ty2 || nb1 <> nb2 then None
     else aux nb1 (ag1,ag2) (t1,t2)

let equal a b =
  let rem_me x l = List.filter (fun y -> x != y) l in
  match min_not_null (classify_by_type a) (classify_by_type b) with
  | None -> false
  | Some ([],ags) -> ags = []
  | Some (h1::_,ags) ->
     let a' = rem_me h1 a in
     List.fold_left
       (fun bool ag -> bool || agents_are_compatibles (a',rem_me ag b) [h1,ag])
       false ags
