open Mods
open ExceptionDefn

type t = ((unit NamedDecls.t) option) NamedDecls.t

let fold f sign cont =
  let cont,_ = Array.fold_left (fun (cont,i) _ -> (f i cont,i+1)) (cont,0)
			      sign.NamedDecls.decls
  in cont

let num_of_site site_name sign =
  StringMap.find site_name sign.NamedDecls.finder

let site_of_num addr sign =
  try NamedDecls.elt_name sign addr
  with Invalid_argument _ -> raise Not_found

let num_of_internal_state site_id state sign =
  try
    let _,values_opt = sign.NamedDecls.decls.(site_id) in
    match values_opt with
    | None -> raise Not_found
    | Some nd -> StringMap.find state nd.NamedDecls.finder
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

let to_string sign =
  let str_of_assoc assoc =
    let cont = ref [] in
    Array.iteri (fun i ((name,_),_) ->
		 if name = "_" then ()
		 else
		   let int =
		     match default_num_value i sign with
		     | None -> ""
		     | Some n ->  "~"^(internal_state_of_num i n sign)
		   in
		   cont:=(name^int)::!cont
		) assoc ;
    String.concat "," (List.rev !cont)
  in
  Printf.sprintf "(%s)"	(str_of_assoc sign.NamedDecls.decls)
