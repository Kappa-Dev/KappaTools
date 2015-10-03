type t =
  Existing of Connected_component.ContentAgent.t * int
  | Fresh of int * int (* type, id *)

let rename wk id cc inj = function
  | Existing (n, id') as x ->
     if id <> id' then x else
       let n' = Connected_component.ContentAgent.rename wk cc inj n in
       if n == n' then x else Existing (n',id')
  | Fresh _ as x -> x

let print ?sigs f = function
  | Existing (n,id) ->
     Format.fprintf f "%a/*%i*/"
		    (Connected_component.ContentAgent.print ?sigs) n id
  | Fresh (ty,i) ->
     Format.fprintf f "%a/*%t %i*/"
		    (match sigs with
		     | None -> Format.pp_print_int
		     | Some sigs -> Signature.print_agent sigs) ty Pp.nu i

let print_site ?sigs place f site =
  match place with
  | Existing (n,_) ->
     Connected_component.ContentAgent.print_site ?sigs n f site
  | Fresh (ty,_) ->
     match sigs with
     | None -> Format.pp_print_int f ty
     | Some sigs -> Signature.print_site sigs ty f site

let print_internal ?sigs place site f id =
  match place with
  | Existing (n,_) ->
     Connected_component.ContentAgent.print_internal ?sigs n site f id
  | Fresh (ty,_) ->
     match sigs with
     | None -> Format.pp_print_int f id
     | Some sigs ->
	Signature.print_site_internal_state sigs ty site f (Some id)

let get_type = function
  | Existing (n,_) -> Connected_component.ContentAgent.get_sort n
  | Fresh (i,_) -> i

let same_connected_component p p' =
  match p,p' with
  | (Existing _, Fresh _ | Fresh _, Existing _) -> false
  | Fresh (_,i), Fresh (_,i') -> i=i'
  | Existing (_,id), Existing (_,id') -> id=id'

let is_site_from_fresh = function
  | (Existing _,_) -> false
  | (Fresh _, _) -> true
