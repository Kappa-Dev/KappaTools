(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type 'a t = { decls: (string * 'a) array; finder: int Mods.StringMap.t }

let name_map_of_array ?forbidden a =
  let bad =
    match forbidden with
    | None -> fun _ -> false
    | Some s -> fun x -> Mods.StringSet.mem x s
  in
  Tools.array_fold_lefti
    (fun i map ((x, pos), _) ->
      if bad x || Mods.StringMap.mem x map then
        raise
          (ExceptionDefn.Malformed_Decl
             ("Label '" ^ x ^ "' already defined", pos))
      else
        Mods.StringMap.add x i map)
    Mods.StringMap.empty a

let create ?forbidden a =
  {
    decls = Array.map (fun ((x, _), y) -> x, y) a;
    finder = name_map_of_array ?forbidden a;
  }

let create_from_list ?forbidden l = create ?forbidden (Array.of_list l)

(* TODO see if we should keep this *)
let create_no_loc ?forbidden a =
  Array.map (fun (x, y) -> (x, Loc.dummy), y) a |> create ?forbidden

let size nd = Array.length nd.decls
let elt_name nd i = fst nd.decls.(i)

let elt_id ?(kind = "element") nd (s, pos) =
  match Mods.StringMap.find_option s nd.finder with
  | Some x -> x
  | None ->
    raise
      (ExceptionDefn.Malformed_Decl
         (Format.asprintf "\"%s\" is not a declared %s." s kind, pos))

let print ~sep pp f nd =
  Pp.array sep (fun i f (n, el) -> pp i n f el) f nd.decls

let debug_print pr f nd =
  print ~sep:Pp.space
    (fun i n f el -> Format.fprintf f "@[%i>%s: @[<2>%a@]@]" i n pr el)
    f nd

let fold f acc nd =
  Tools.array_fold_lefti (fun i acc (na, x) -> f i na acc x) acc nd.decls

let map f nd =
  { decls = Array.map (fun (s, v) -> s, f s v) nd.decls; finder = nd.finder }

let mapi f nd =
  {
    decls = Array.mapi (fun i (s, v) -> s, f i s v) nd.decls;
    finder = nd.finder;
  }

let elt_val nd i = snd nd.decls.(i)

let to_json aux nd =
  `List
    (Array.fold_right
       (fun (x, a) acc -> `Assoc [ "name", `String x; "decl", aux a ] :: acc)
       nd.decls [])

let of_json aux = function
  | `List l ->
    let decls =
      Tools.array_map_of_list
        (function
          | `Assoc [ ("name", `String x); ("decl", a) ]
          | `Assoc [ ("decl", a); ("name", `String x) ] ->
            Loc.annot_with_dummy x, aux a
          | x ->
            raise
              (Yojson.Basic.Util.Type_error ("Not a valid NamedDecl element", x)))
        l
    in
    create decls
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a valid NamedDecl", x))
