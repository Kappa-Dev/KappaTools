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
      if ints.(i) <> None then
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

let print sigs f mix =
  Pp.list Pp.comma (print_agent sigs) f mix

let agent_is_linked_on_port me i id = function
  | VAL j when i = j -> id <> me
  | (VAL _ | FREE) -> false

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
		     | FREE, FREE -> (modif||i<>0,op)
		     | VAL a, VAL b ->
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
