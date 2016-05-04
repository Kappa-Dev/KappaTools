open Mods

type 'a t =
    { decls : (string Location.annot *'a) array;
      finder : int StringMap.t }

let name_map_of_array ?forbidden a =
  let bad = match forbidden with
    | None -> fun _ -> false
    | Some s -> fun x -> StringSet.mem x s in
  Tools.array_fold_lefti
    (fun i map ((x,pos),_) ->
     let map' = StringMap.add x i map in
     if bad x || map == map' then
       raise (ExceptionDefn.Malformed_Decl
		("Label '"^x^"' already defined", pos))
     else map')
    StringMap.empty a

let create ?forbidden a = { decls = a; finder = name_map_of_array ?forbidden a}

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
