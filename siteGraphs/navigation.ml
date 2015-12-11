type id_upto_alpha =
    Existing of int
  | Fresh of int * int (* type, id *)

type port = id_upto_alpha * int

type arrow = ToNode of port | ToNothing | ToInternal of int

type step = port * arrow

type t = step list

let print_id sigs f = function
  | Existing id -> Format.pp_print_int f id
  | Fresh (ty,id) ->
     Format.fprintf f "!%a-%i" (Signature.print_agent sigs) ty id

let print_id_site ?source sigs find_ty n =
  let ty =
    match n with
    | Fresh (ty,_) -> ty
    | Existing id ->
       match source with
       | Some (Fresh (ty,id')) when id = id' -> ty
       | (None | Some (Fresh _ | Existing _)) -> find_ty id in
  Signature.print_site sigs ty

let print_id_internal_state sigs find_ty n =
  Signature.print_site_internal_state
    sigs (match n with Existing id -> find_ty id | Fresh (ty,_) -> ty)

let print_step sigs find_ty f = function
  | (source,site), ToNothing ->
     Format.fprintf f "-%a_%a-%t->" (print_id sigs) source
		    (print_id_site sigs find_ty source) site Pp.bottom
  | (source,site), ToNode (id,port) ->
     Format.fprintf f "-%a_%a-%a_%a->" (print_id sigs) source
		    (print_id_site sigs find_ty source) site
		    (print_id sigs) id
		    (print_id_site ~source sigs find_ty id) port
  | (source,site), ToInternal i ->
     Format.fprintf
       f "-%a_%a->" (print_id sigs) source
       (print_id_internal_state sigs find_ty source site) (Some i)

let compatible_point injs e e' =
  match e,e' with
  | ((Existing id,site), ToNothing), e ->
     List.filter
       (fun inj -> e = ((Existing (Renaming.apply inj id),site),ToNothing))
       injs
  | ((Existing id,site), ToInternal i), e ->
     List.filter
       (fun inj -> e = ((Existing (Renaming.apply inj id),site),ToInternal i))
       injs
  | ((Existing id,site), ToNode (Existing id',site')), e ->
     List.filter
       (fun inj ->
	e =
	  ((Existing (Renaming.apply inj id),site),
	   ToNode (Existing (Renaming.apply inj id'),site'))
	|| e =
	     ((Existing (Renaming.apply inj id'),site'),
	      ToNode (Existing (Renaming.apply inj id),site)))
       injs
  | (((Existing id,site),ToNode (Fresh (ty,id'),site')),
     ((Existing sid,ssite), ToNode (Fresh(ty',sid'),ssite'))
    | ((Fresh (ty,id'),site),ToNode (Existing id,site')),
      ((Existing sid,ssite), ToNode (Fresh(ty',sid'),ssite'))
    | ((Existing id,site),ToNode (Fresh (ty,id'),site')),
      ((Fresh(ty',sid'),ssite), ToNode (Existing sid,ssite'))
    | ((Fresh (ty,id'),site),ToNode (Existing id,site')),
      ((Fresh(ty',sid'),ssite), ToNode (Existing sid,ssite'))) ->
     List.map (Renaming.add id' sid')
	      (List.filter
		 (fun inj -> sid = Renaming.apply inj id && ssite = site
			     && ty' = ty && ssite' = site') injs)
  | ((Existing _,_), ToNode (Fresh _,_)),
    (((Fresh _ | Existing _), _), _) -> []
  | ((Fresh (ty,id),site), ToNothing), ((Fresh (ty',id'),site'),x) ->
     List.map (Renaming.add id id')
	      (List.filter
		 (fun inj ->
		  ty = ty' && site = site' && x = ToNothing
		  && not (Renaming.mem id inj)) injs)
  | ((Fresh (ty,id),site), ToInternal i), ((Fresh (ty',id'),site'),x) ->
     List.map (Renaming.add id id')
	      (List.filter
		 (fun inj -> ty = ty' && site = site' &&
			       x = ToInternal i && not (Renaming.mem id inj))
		 injs)
  | ((Fresh (ty,id),site), ToNode (Fresh (ty',id'),site')),
    ((Fresh (sty,sid),ssite), ToNode (Fresh (sty',sid'),ssite')) ->
     List.fold_left
       (fun acc inj ->
	if not (Renaming.mem id inj) && not (Renaming.mem id inj) then
	  if ty = sty && site = ssite && ty' = sty' && site' = ssite'
	  then (Renaming.add id' sid' (Renaming.add id sid inj))::acc
	  else if ty = sty && site = ssite && ty' = sty' && site' = ssite'
	  then (Renaming.add id' sid (Renaming.add id sid' inj))::acc
	  else acc
	else acc) [] injs
  | ((Fresh _,_), _), ((Fresh _,_),_) -> []
  | ((Fresh _,_), _), ((Existing _,_),_) -> []

let rename_id inj2cc = function
  | Fresh _ as x -> x
  | Existing n -> Existing (Renaming.apply inj2cc n)

let rename_step inj2cc = function
  | ((x,i), (ToNothing | ToInternal _ as a)) -> ((rename_id inj2cc x,i),a)
  | ((x,i),ToNode (y,j)) ->
     ((rename_id inj2cc x,i),ToNode (rename_id inj2cc y,j))

let check_edge graph = function
  | ((Fresh (_,id),site),ToNothing) -> Edges.is_free id site graph
  | ((Fresh (_,id),site),ToInternal i) -> Edges.is_internal i id site graph
  | ((Fresh (_,id),site),ToNode (Existing id',site')) ->
     Edges.link_exists id site id' site' graph
  | ((Fresh (_,id),site),ToNode (Fresh (_,id'),site')) ->
     Edges.link_exists id site id' site' graph
  | ((Existing id,site),ToNothing) -> Edges.is_free id site graph
  | ((Existing id,site),ToInternal i) -> Edges.is_internal i id site graph
  | ((Existing id,site),ToNode (Existing id',site')) ->
     Edges.link_exists id site id' site' graph
  | ((Existing id,site),ToNode (Fresh (_,id'),site')) ->
     Edges.link_exists id site id' site' graph

(*inj is the partial injection built so far: inj:abs->concrete*)
let dst_is_okay inj' graph root site = function
  | ToNothing ->
     if Edges.is_free root site graph then Some inj' else None
  | ToInternal i ->
     if Edges.is_internal i root site graph then Some inj' else None
  | ToNode (Existing id',site') ->
     if Edges.link_exists root site
			  (Renaming.apply inj' id') site' graph
     then Some inj' else None
  | ToNode (Fresh (ty,id'),site') ->
     match Edges.exists_fresh root site ty site' graph with
     | None -> None
     | Some node -> Some (Renaming.add id' node inj')

let injection_for_one_more_edge ?root inj graph = function
  | ((Existing id,site),dst) ->
     dst_is_okay inj graph (Renaming.apply inj id) site dst
  | ((Fresh (_,id),site),dst) ->
     match root with
     | None -> None
     | Some root ->
	dst_is_okay (Renaming.add id root inj) graph root site dst
