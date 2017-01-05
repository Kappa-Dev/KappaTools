(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception Undefined
exception NotBijective
exception Clashing

type t = {sigma:int Mods.IntMap.t ; is_identity:bool ; dsts:Mods.IntSet.t}

let empty =
  {sigma = Mods.IntMap.empty ; is_identity=true ; dsts = Mods.IntSet.empty }
let identity l =
  {sigma =
     List.fold_left (fun out x -> Mods.IntMap.add x x out) Mods.IntMap.empty l;
   is_identity = true;
   dsts =
     List.fold_left (fun out x -> Mods.IntSet.add x out) Mods.IntSet.empty l}
let is_identity i = i.is_identity

let to_list i = Mods.IntMap.bindings i.sigma
let image i = i.dsts

let unsafe_add x y i =
  {sigma = Mods.IntMap.add x y i.sigma ;
   is_identity = i.is_identity && x==y ;
   dsts = Mods.IntSet.add y i.dsts}
let add x y i =
  let not_ok = !Parameter.debugModeOn && Mods.IntMap.mem x i.sigma in
  if not_ok then raise Clashing else
    let i' = unsafe_add x y i in
    if i.dsts == i'.dsts then None else Some i'

let rec cyclic_permutation_from_identity max id subst pre = function
  | _ when pre = id -> unsafe_add pre max subst
  | [] -> assert false
  | h :: t ->
    cyclic_permutation_from_identity max id (unsafe_add pre h subst) h t
let cyclic_permutation_from_list ~stop_at = function
  | [] -> failwith "Renaming.cyclic_permutation_from_list"
  | h :: t ->
    cyclic_permutation_from_identity h stop_at empty h t

let mem x i = Mods.IntMap.mem x i.sigma
let fold f i = Mods.IntMap.fold f i.sigma

let apply i x =
  if not i.is_identity || !Parameter.debugModeOn then
    match Mods.IntMap.find_option x i.sigma with
    | Some x -> x
    | None -> raise Undefined
  else x

let compose extensible i i' =
  if not i.is_identity || extensible || !Parameter.debugModeOn then
    let sigma,is_id =
      Mods.IntMap.fold (fun x y (out,is_id) ->
          match Mods.IntMap.find_option y i'.sigma with
          | Some z -> (Mods.IntMap.add x z out,is_id && x==z)
          | None -> (out,is_id && x==y)
        ) i.sigma (i.sigma,true)
    in
    {sigma=sigma ; is_identity=is_id ; dsts = i'.dsts}
  else i'

let inverse i =
  if i.is_identity then i
  else
    Mods.IntMap.fold (fun x y out ->
        if Mods.IntMap.mem y out.sigma then raise NotBijective
        else unsafe_add y x out) i.sigma empty

let compare i i' = Mods.IntMap.compare Mods.int_compare i.sigma i'.sigma
let equal i i' = (compare i i') = 0
let min_elt i = Mods.IntMap.min_elt i.sigma

let print f i =
  ignore
    (Mods.IntMap.fold
       (fun src dst b ->
          if src <> dst then
            let () =
              Format.fprintf f "%t%i->%i" (if b then Pp.comma else Pp.empty)
                src dst in
            true
          else b
       ) i.sigma false)

let print_full f i =
  Format.fprintf
    f "@[(%a)@]"
    (Pp.set Mods.IntMap.bindings Pp.comma
       (fun f (src,dst) -> if src<>dst then Format.fprintf f "%i->%i" src dst
         else Format.pp_print_int f src)) i.sigma

let to_yojson i =
  `List
    (Mods.IntMap.fold
       (fun src dst acc -> `List [`Int src; `Int dst] :: acc)
       i.sigma [])

let of_yojson = function
  | `List l ->
    List.fold_left (fun r -> function
        | `List [ `Int src; `Int dst ] as x ->
          begin match add src dst r with
            | Some r' -> r'
            | None ->
              raise (Yojson.Basic.Util.Type_error ("Incorrect renaming item",x))
          end
        | x ->
          raise (Yojson.Basic.Util.Type_error ("Incorrect renaming item",x))
      ) empty l
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect renaming",x))
