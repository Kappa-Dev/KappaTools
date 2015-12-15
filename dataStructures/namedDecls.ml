open Mods

type 'a t =
    { decls : (string Location.annot *'a) array;
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
  match StringMap.find_option s nd.finder with
  | Some x -> x
  | None ->
    raise (ExceptionDefn.Malformed_Decl
	     (Format.asprintf "\"%s\" is not a declared %s." s kind,pos))

let print ~sep pp f nd =
  Pp.array sep (fun i f ((n,_),el) -> pp i n f el) f nd.decls
let debug_print pr f nd =
  print ~sep:Pp.space (fun i n f el ->
		       Format.fprintf f "@[%i>%s: @[<2>%a@]@]"
				      i n pr el)
	f nd

let fold f acc nd =
  Tools.array_fold_lefti
    (fun i acc ((na,_),x) -> f i na acc x)
    acc nd.decls
