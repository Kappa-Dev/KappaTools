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

let num_of_internal_state site_name state sign =
  try
    let _,values_opt = sign.NamedDecls.decls.(num_of_site site_name sign) in
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

let create ag_name intf_map =
  let assoc = Array.make (StringMap.size intf_map)
			 (("",(Lexing.dummy_pos,Lexing.dummy_pos)),None) in
  let _ =
    StringMap.fold
      (fun site_name (int_state_list,_,pos) cpt ->
       let ar_opt =
	 match int_state_list with
	 | [] -> None
	 | l ->
	    let a = Array.of_list l in
	    Some (NamedDecls.create (Array.map (fun x -> (x,())) a))
       in
       let cpt,cpt' =
	 if site_name = "_" then (0,cpt) else (cpt,cpt+1) in
       (*making sure that "_" gets assigned to 0*)
       assoc.(cpt) <- ((site_name,pos),ar_opt) ;
       cpt'
      )
      intf_map 1
  in NamedDecls.create assoc

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
