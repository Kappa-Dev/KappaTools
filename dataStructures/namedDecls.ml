open Mods

type 'a t =
    { decls : (string Term.with_pos *'a) array;
      finder : int StringMap.t }

let name_map_of_array a =
  fst (Array.fold_left
	 (fun (map,i) ((x,(pos,_)),_) ->
	  if StringMap.mem x map then
	    raise (ExceptionDefn.Semantics_Error
		     (Tools.pos_of_lex_pos pos, (Printf.sprintf "Label '%s' already defined" x)))
	  else StringMap.add x i map,succ i)
	 (StringMap.empty,0) a)

let create a = { decls = a; finder = name_map_of_array a}

let size nd = Array.length nd.decls

let elt_name nd i = fst (fst nd.decls.(i))
