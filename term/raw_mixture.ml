type internal = int option
type link = FREE | VAL of int
type agent =
    { a_id: int; a_type: int; a_ports: link array; a_ints: internal array; }
type t = agent list

let print_link f = function
  | FREE -> ()
  | VAL i -> Format.fprintf f "!%i" i

let print_intf sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      let () = Format.fprintf
		 f "%t%a%a" (if empty then Pp.empty else Pp.comma)
		 (Signature.print_site_internal_state sigs ag_ty i)
		 ints.(i) print_link ports.(i) in
      aux false (succ i) in
  aux true 1

let print_agent sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.a_type
		 (print_intf sigs ag.a_type) (ag.a_ports,ag.a_ints)

let print sigs f mix =
  Pp.list Pp.comma (print_agent sigs) f mix

let agent_is_linked_on_port me i id = function
  | VAL j when i = j -> id <> me
  | (VAL _ | FREE) -> false

let agent_is_linked_on forbidden i ag =
  Tools.array_filter (agent_is_linked_on_port forbidden i) ag.a_ports <> []

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
			match List.partition (agent_is_linked_on (-1) a) g,
			      List.partition (agent_is_linked_on (-1) b) h with
			| ([],_), ([],_) -> c
			| ([x],g'), ([y],h') -> Some ((x,y)::todo,(g',h'))
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
