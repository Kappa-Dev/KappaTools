type t =
  Existing of Agent.t * int
  | Fresh of int * int (* type, id *)

let rename id inj = function
  | Existing (n, id') as x ->
     if id <> id' then x else
       let n' = Agent.rename inj n in
       if n == n' then x else Existing (n',id')
  | Fresh _ as x -> x

let print ?sigs f = function
  | Existing (n,id) ->
     Format.fprintf
       f "%a/*%i*/" (Agent.print ?sigs ~with_id:true) n id
  | Fresh (ty,i) ->
     Format.fprintf f "%a/*%t %i*/"
		    (match sigs with
		     | None -> Format.pp_print_int
		     | Some sigs -> Signature.print_agent sigs) ty Pp.nu i

let print_site ?sigs place f site =
  match place with
  | Existing (n,_) -> Agent.print_site ?sigs n f site
  | Fresh (ty,_) ->
     match sigs with
     | None -> Format.pp_print_int f site
     | Some sigs -> Signature.print_site sigs ty f site

let print_internal ?sigs place site f id =
  match place with
  | Existing (n,_) -> Agent.print_internal ?sigs n site f id
  | Fresh (ty,_) ->
     match sigs with
     | None -> Format.fprintf f "%i~%i" site id
     | Some sigs ->
	Signature.print_site_internal_state sigs ty site f (Some id)

let get_type = function
  | Existing (n,_) -> Agent.sort n
  | Fresh (i,_) -> i

let same_connected_component p p' =
  match p,p' with
  | (Existing _, Fresh _ | Fresh _, Existing _) -> false
  | Fresh (_,i), Fresh (_,i') -> i=i'
  | Existing (_,id), Existing (_,id') -> id=id'

let is_site_from_fresh = function
  | (Existing _,_) -> false
  | (Fresh _, _) -> true

let concretize (inj_nodes,inj_fresh) = function
  | Existing (n,id) ->
     (Connected_component.Matching.get (n,id) inj_nodes,Agent.sort n)
  | Fresh (ty,id) ->
     match Mods.IntMap.find_option id inj_fresh with
     | Some x -> (x,ty)
     | None -> failwith "Instantiation.from_place"
