type place =
    Existing of Connected_component.node
  | Fresh of int * int (* type_id, node_id *)
type t =
    Freed of place * int
  | Linked of (place * int) * (place * int)
  | Internalized of place * int * int

let rename_place wk cc inj = function
  | Existing n as x ->
     let n' = Connected_component.rename_node wk cc inj n in
     if n == n' then x else Existing n'
  | Fresh _ as x -> x

let rename wk cc inj = function
  | Freed (p,s) as x ->
     let p' = rename_place wk cc inj p in
     if p == p' then x else Freed (p',s)
  | Linked ((p1,s1),(p2,s2)) as x ->
     let p1' = rename_place wk cc inj p1 in
     let p2' = rename_place wk cc inj p2 in
     if p1 == p1' && p2 == p2' then x else Linked ((p1',s1),(p2',s2))
  | Internalized (p,s,i) as x ->
     let p' = rename_place wk cc inj p in
     if p == p' then x else Internalized (p',s,i)

let print_place sigs f = function
  | Existing n -> Connected_component.print_node ~sigs f n
  | Fresh (ty,i) ->
     Format.fprintf f "%a/*%t %i*/" (Signature.print_agent sigs) ty Pp.nu i

let print_site_place sigs place f site =
  match place with
  | Existing n -> Connected_component.print_site ~sigs n f site
  | Fresh (ty,_) ->
     Signature.print_site sigs ty f site

let print_internal_place sigs place site f id =
  match place with
  | Existing n -> Connected_component.print_internal ~sigs n site f id
  | Fresh (ty,_) ->
     Signature.print_site_internal_state sigs ty site f (Some id)

let print sigs f = function
  | Freed (p,s) ->
     Format.fprintf f "@[%a.%a = %t@]"
		    (print_place sigs) p (print_site_place sigs p) s
		    Pp.bottom
  | Linked ((p1,s1),(p2,s2)) ->
     Format.fprintf f "@[%a.%a = %a.%a@]"
		    (print_place sigs) p1 (print_site_place sigs p1) s1
		    (print_place sigs) p2 (print_site_place sigs p2) s2
  | Internalized (p,s,i) ->
     Format.fprintf f "@[%a.%a =@]"
		    (print_place sigs) p (print_internal_place sigs p s) i
