(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception Undefined
exception NotBijective
exception Clashing

let special_val = max_int

type t = {
  mutable immediate: int array;
  mutable delayed: (t * t) option;
  mutable is_identity: bool;
  mutable dsts: Mods.IntSet.t;
}

let empty () =
  {
    immediate = [||];
    delayed = None;
    is_identity = true;
    dsts = Mods.IntSet.empty;
  }

let dummy = empty ()

let identity l =
  let max = List.fold_left max 0 l in
  let immediate = Array.make (succ max) special_val in
  let () = List.iter (fun x -> immediate.(x) <- x) l in
  {
    immediate;
    delayed = None;
    is_identity = true;
    dsts =
      List.fold_left (fun out x -> Mods.IntSet.add x out) Mods.IntSet.empty l;
  }

let is_identity i = i.is_identity

let rec compute k i =
  let v = i.immediate.(k) in
  if v <> special_val then
    v
  else (
    match i.delayed with
    | None -> special_val
    | Some (x, y) ->
      if k >= Array.length x.immediate then
        special_val
      else (
        let v' = compute k x in
        if v' = special_val then
          special_val
        else (
          let v'' = compute v' y in
          let o =
            if v'' = special_val then
              v'
            else
              v''
          in
          let () = i.immediate.(k) <- o in
          o
        )
      )
  )

let force i =
  if i.delayed <> None then (
    let () = Array.iteri (fun k _ -> ignore (compute k i)) i.immediate in
    i.delayed <- None
  )

let to_list i =
  let () = force i in
  Tools.array_fold_lefti
    (fun i acc v ->
      if v <> special_val then
        (i, v) :: acc
      else
        acc)
    [] i.immediate
  |> List.rev

let image i = i.dsts

let unsafe_functionnal_add x y i =
  let l = max (Array.length i.immediate) (x + 1) in
  let immediate = Array.make l special_val in
  let () = Array.blit i.immediate 0 immediate 0 (Array.length i.immediate) in
  let () = immediate.(x) <- y in
  {
    immediate;
    delayed = i.delayed;
    is_identity = i.is_identity && x == y;
    dsts = Mods.IntSet.add y i.dsts;
  }

let add ~debug_mode x y i =
  let not_ok =
    debug_mode && x < Array.length i.immediate && i.immediate.(x) <> special_val
  in
  if not_ok then
    raise Clashing
  else (
    let i' = unsafe_functionnal_add x y i in
    if i.dsts == i'.dsts then
      None
    else
      Some i'
  )

let unsafe_imperative_add x y i =
  let () =
    let l = Array.length i.immediate in
    if x >= l then (
      let immediate = Array.make (succ x) special_val in
      let () = Array.blit i.immediate 0 immediate 0 l in
      let () = immediate.(x) <- y in
      i.immediate <- immediate
    ) else
      i.immediate.(x) <- y
  in
  let () = i.is_identity <- i.is_identity && x == y in
  i.dsts <- Mods.IntSet.add y i.dsts

let imperative_add ~debug_mode x y i =
  let not_ok =
    debug_mode && x < Array.length i.immediate && i.immediate.(x) <> special_val
  in
  if not_ok then
    raise Clashing
  else (
    let origin = i.dsts in
    let () = unsafe_imperative_add x y i in
    not (i.dsts == origin)
  )

let rec cyclic_permutation_from_identity max id subst pre = function
  | _ when pre = id -> unsafe_imperative_add pre max subst
  | [] -> assert false
  | h :: t ->
    let () = unsafe_imperative_add pre h subst in
    cyclic_permutation_from_identity max id subst h t

let cyclic_permutation_from_list ~stop_at = function
  | [] -> failwith "Renaming.cyclic_permutation_from_list"
  | h :: t ->
    let out = empty () in
    let () = cyclic_permutation_from_identity h stop_at out h t in
    out

let mem x i = x < Array.length i.immediate && compute x i <> special_val

let fold f i acc =
  let () = force i in
  Tools.array_fold_lefti
    (fun i acc v ->
      if v = special_val then
        acc
      else
        f i v acc)
    acc i.immediate

let apply ~debug_mode i x =
  if (not i.is_identity) || debug_mode then (
    let c = compute x i in
    if c = special_val then
      raise Undefined
    else
      c
  ) else
    x

let compose ~debug_mode extensible i i' =
  if (not i.is_identity) || extensible || debug_mode then
    {
      immediate = Array.make (Array.length i.immediate) special_val;
      delayed = Some (i, i');
      is_identity = i.is_identity && i'.is_identity;
      dsts =
        Mods.IntSet.fold
          (fun v' set ->
            let v'' = compute v' i' in
            Mods.IntSet.add v'' set)
          i.dsts Mods.IntSet.empty;
    }
  (* let sigma,is_id =
       Mods.IntMap.fold (fun x y (out,is_id) ->
           match Mods.IntMap.find_option y i'.sigma with
           | Some z -> (Mods.IntMap.add x z out,is_id && x==z)
           | None -> (out,is_id && x==y)
         ) i.sigma (i.sigma,true)
     in
     {sigma=sigma ; is_identity=is_id ; dsts = i'.dsts}
  *)
  else
    i'

let inverse i =
  if i.is_identity then
    i
  else (
    let out = empty () in
    let () = force i in
    let () =
      Array.iteri
        (fun x y ->
          if y <> special_val then
            if
              y < Array.length out.immediate && out.immediate.(y) <> special_val
            then
              raise NotBijective
            else
              unsafe_imperative_add y x out)
        i.immediate
    in
    out
  )

let compare i i' =
  let () = force i in
  let () = force i' in
  Tools.array_compare Mods.int_compare i.immediate i'.immediate

let equal i i' = compare i i' = 0

let min_elt i =
  let l = Array.length i.immediate in
  let rec aux_min_elt k =
    if k >= l then
      None
    else (
      let o = compute k i in
      if o = special_val then
        aux_min_elt (succ k)
      else
        Some (k, o)
    )
  in
  aux_min_elt 0

let print f i =
  let () = force i in
  ignore
    (Tools.array_fold_lefti
       (fun src b dst ->
         if src <> dst && dst <> special_val then (
           let () =
             Format.fprintf f "%t%i->%i"
               (if b then
                  Pp.comma
                else
                  Pp.empty)
               src dst
           in
           true
         ) else
           b)
       false i.immediate)

let print_full f i =
  let () = force i in
  Format.fprintf f "@[(%a)@]"
    (Pp.array Pp.comma (fun src f dst ->
         if dst <> special_val then
           if src <> dst then
             Format.fprintf f "%i->%i" src dst
           else
             Format.pp_print_int f src))
    i.immediate

let to_yojson i =
  let () = force i in
  `List
    (Tools.array_fold_lefti
       (fun src acc dst ->
         if dst <> special_val then
           `List [ `Int src; `Int dst ] :: acc
         else
           acc)
       [] i.immediate)

let of_yojson = function
  | `List l ->
    let out = empty () in
    let () =
      List.iter
        (function
          | `List [ `Int src; `Int dst ] as x ->
            if not (imperative_add ~debug_mode:false src dst out) then
              raise
                (Yojson.Basic.Util.Type_error ("Incorrect renaming item", x))
          | x ->
            raise (Yojson.Basic.Util.Type_error ("Incorrect renaming item", x)))
        l
    in
    out
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect renaming", x))
