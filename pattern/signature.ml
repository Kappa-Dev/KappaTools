open Mods
open ExceptionDefn

type t = ((unit NamedDecls.t) option) NamedDecls.t

let fold f sign cont =
  let cont,_ = Array.fold_left
		 (fun (cont,i) ((na,_),_) -> (f i na cont,i+1)) (cont,0)
		 sign.NamedDecls.decls
  in cont

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

let internal_states_number site_num sign =
  try
    let _,values_opt = sign.NamedDecls.decls.(site_num) in
    match values_opt with
    | None -> 0
    | Some nd -> NamedDecls.size nd
  with
  | Invalid_argument _ -> raise Not_found

let arity sign = NamedDecls.size sign

let default_num_value num_site sign =
  try
    let _,values_opt = sign.NamedDecls.decls.(num_site) in
    match values_opt with
    | None -> None
    | Some _ -> Some 0
  with
  | Invalid_argument _ ->
     invalid_arg "Signature.default_num_value: invalid site identifier"

let create ast_intf =
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
       | (_, pos) ->
	  raise (ExceptionDefn.Malformed_Decl
		("Link status inside a definition of signature", pos))
      ) ({Ast.port_nme =Term.with_dummy_pos "_";
	  Ast.port_int=[];
	  Ast.port_lnk =Term.with_dummy_pos Ast.FREE;}
	 :: ast_intf))

let print f sign =
  Format.fprintf
    f "(%a)"
    (Pp.array
       (fun f -> Format.pp_print_string f ",")
       (fun i f ((name,_),_) ->
	if name = "_" then ()
	else
	  let int =
	    match default_num_value i sign with
	    | None -> ""
	    | Some n ->  "~"^(internal_state_of_num i n sign)
	  in
	  Format.fprintf f "%s%s" name int))
    sign.NamedDecls.decls
