type t = ((unit NamedDecls.t) option) NamedDecls.t

let fold f sign cont =
  Tools.array_fold_lefti
    (fun i cont ((na,_),_) -> f i na cont) cont
    sign.NamedDecls.decls

let num_of_site ?agent_name site_name sign =
  let kind = match agent_name with
    | None -> "site name"
    | Some agent_name -> "site name for agent "^agent_name
  in NamedDecls.elt_id ~kind sign site_name

let site_of_num addr sign =
  try NamedDecls.elt_name sign addr
  with Invalid_argument _ -> raise Not_found

let num_of_internal_state site_id state sign =
  try
    let (na,_),values_opt = sign.NamedDecls.decls.(site_id) in
    match values_opt with
    | None -> raise Not_found
    | Some nd ->
       NamedDecls.elt_id ~kind:("internal state for site "^na) nd state
  with
  | Invalid_argument _ -> raise Not_found

let internal_state_of_num site_num val_num sign =
  try
    let _,values_opt = sign.NamedDecls.decls.(site_num) in
    match values_opt with
    | None -> raise Not_found
    | Some nd -> fst (fst nd.NamedDecls.decls.(val_num))
  with
  | Invalid_argument _ -> raise Not_found

let create_t ast_intf =
  NamedDecls.create (
    Tools.array_map_of_list
      (fun p ->
       match p.Ast.port_lnk with
       | (Ast.FREE,_) ->
	  (p.Ast.port_nme,
	   match p.Ast.port_int with
	   | [] -> None
	   | l ->
	      Some (NamedDecls.create
		      (Tools.array_map_of_list (fun x -> (x,())) l))
	  )
       | ((Ast.LNK_SOME | Ast.LNK_ANY |
	   Ast.LNK_TYPE _ | Ast.LNK_VALUE _), pos) ->
	  raise (ExceptionDefn.Malformed_Decl
		("Link status inside a definition of signature", pos))
      ) ast_intf)

let print_one f sign =
  Format.fprintf
    f "@[%a@]"
    (NamedDecls.print
       ~sep:(fun f -> Format.fprintf f ",@,")
       (fun _ name f ints ->
	let pp_int f =
	  match ints with
	  | None -> ()
	  | Some nd ->
	     NamedDecls.print
	       ~sep:(fun _ -> ())
	       (fun _ na f () -> Format.fprintf f "~%s" na) f nd
	in
	Format.fprintf f "%s%t" name pp_int))
    sign

type s = t NamedDecls.t

let size sigs = NamedDecls.size sigs
let get sigs agent_id = snd sigs.NamedDecls.decls.(agent_id)
let arity sigs agent_id = NamedDecls.size (get sigs agent_id)
let max_arity sigs =
  NamedDecls.fold (fun _ _ x a -> max x (NamedDecls.size a)) 0 sigs

let agent_of_num i sigs = NamedDecls.elt_name sigs i
let num_of_agent name sigs =
  NamedDecls.elt_id ~kind:"agent" sigs name

let id_of_site (agent_name,_ as agent_ty) site_name sigs =
  let n = num_of_agent agent_ty sigs in
  num_of_site ~agent_name site_name (get sigs n)

let site_of_id agent_id site_id sigs =
  site_of_num site_id (get sigs agent_id)

let id_of_internal_state (agent_name,_ as agent_ty) site_name state sigs =
  let n = num_of_agent agent_ty sigs in
  let sign = get sigs n in
  let site_id =
    num_of_site ~agent_name site_name sign in
  num_of_internal_state site_id state sign

let internal_state_of_id agent_id id_site id_state sigs =
  internal_state_of_num id_site id_state (get sigs agent_id)

let internal_states_number agent_id site_num sigs =
  try
    let _,values_opt = (get sigs agent_id).NamedDecls.decls.(site_num) in
    match values_opt with
    | None -> 0
    | Some nd -> NamedDecls.size nd
  with
  | Invalid_argument _ -> raise Not_found

let default_internal_state agent_id site_id sigs =
  try
    match (get sigs agent_id).NamedDecls.decls.(site_id) with
    | _, None -> None
    | _, Some _ -> Some 0
  with
  | Invalid_argument _ ->
     invalid_arg "Signature.default_num_value: invalid site identifier"

let create l =
  NamedDecls.create (Tools.array_map_of_list
		       (fun (name,intf) -> (name,create_t intf))
		       l)

let print_agent sigs f ag_ty =
 Format.pp_print_string f @@ agent_of_num ag_ty sigs
let print_site sigs ag_ty f id =
 Format.pp_print_string f @@ site_of_id ag_ty id sigs
let print_internal_state sigs ag_ty site f id =
  Format.pp_print_string f @@ internal_state_of_id ag_ty site id sigs
let print_site_internal_state sigs ag_ty site f = function
  | None -> print_site sigs ag_ty f site
  | Some id ->
     Format.fprintf f "%s~%s" (site_of_id ag_ty site sigs)
		    (internal_state_of_id ag_ty site id sigs)

let print f sigs =
  Format.fprintf
    f "@[<v>%a@]"
    (NamedDecls.print ~sep:Pp.space
		      (fun _ n f si -> Format.fprintf f "%s(%a)" n print_one si))
    sigs
