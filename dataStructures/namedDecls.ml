type 'a t =
  { decls : (string Location.annot *'a) array;
    finder : int Mods.StringMap.t }

let name_map_of_array ?forbidden a =
  let bad = match forbidden with
    | None -> fun _ -> false
    | Some s -> fun x -> Mods.StringSet.mem x s in
  Tools.array_fold_lefti
    (fun i map ((x,pos),_) ->
       if bad x || Mods.StringMap.mem x map then
         raise (ExceptionDefn.Malformed_Decl
                  ("Label '"^x^"' already defined", pos))
       else Mods.StringMap.add x i map)
    Mods.StringMap.empty a

let create ?forbidden a = { decls = a; finder = name_map_of_array ?forbidden a}

let size nd = Array.length nd.decls

let elt_name nd i = fst (fst nd.decls.(i))

let elt_id ?(kind="element") nd (s,pos) =
  match Mods.StringMap.find_option s nd.finder with
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

let to_json aux nd =
  `List
    (Array.fold_right
       (fun ((x,_),a) acc -> `Assoc ([("name",`String x);("decl",aux a)]):: acc)
       nd.decls
       [])

let of_json aux = function
  | `List l ->
    let decls =
      Tools.array_map_of_list
        (function
          | (`Assoc ([("name",`String x);("decl",a)]) |
             `Assoc ([("decl",a);("name",`String x)])) ->
            (Location.dummy_annot x, aux a)
          | x ->
            raise (Yojson.Basic.Util.Type_error
                     ("Not a valid NamedDecl element",x)))
        l in
    create decls
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a valid NamedDecl",x))
