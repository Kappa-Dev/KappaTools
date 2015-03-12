open Mods

type 'a t =
    { decls : (string Term.with_pos *'a) array;
      finder : int StringMap.t }

let name_map_of_array a =
  fst (Array.fold_left
	 (fun (map,i) ((x,pos),_) ->
	  if StringMap.mem x map then
	    raise (ExceptionDefn.Malformed_Decl
		     ("Label '"^x^"' already defined", pos))
	  else StringMap.add x i map,succ i)
	 (StringMap.empty,0) a)

let create a = { decls = a; finder = name_map_of_array a}

let size nd = Array.length nd.decls

let elt_name nd i = fst (fst nd.decls.(i))

let elt_id ?(kind="element") nd (s,pos) =
  try StringMap.find s nd.finder
  with Not_found ->
    raise (ExceptionDefn.Malformed_Decl
	     (Format.asprintf "\"%s\" is not a declared %s." s kind,pos))

let print pr f nd =
  Pp.array Pp.space (fun i f ((n,_),el) ->
		     Format.fprintf f "@[%i>%s: @[<2>%a@]@]"
				    i n pr el) f nd.decls
