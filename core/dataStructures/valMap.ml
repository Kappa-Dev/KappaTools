(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type key = int
type t = Empty | Node of t * key * t * int * Int64.t
(*Node(left,key,right,height,acc)*)

let height = function
  | Empty -> 0
  | Node (_, _, _, h, _) -> h

let accval = function
  | Empty -> 0L
  | Node (_, _, _, _, acc) -> acc

let total = accval

let weight = function
  | Empty -> 0L
  | Node (l, _, r, _, acc) -> Int64.sub (Int64.sub acc (accval l)) (accval r)

let rec print f = function
  | Empty -> Pp.empty_set f
  | Node (l, k, r, _, acc) as x ->
    Format.fprintf f "@[<hov 2><%d,%Li(%Li)>@,[%a@,|%a@,]" k acc (weight x)
      print l print r

let create l key acc r =
  let hl = height l in
  let hr = height r in
  Node (l, key, r, succ (min hl hr), acc)

let bal l x w r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then (
    match l with
    | Empty -> invalid_arg "Val_map.bal"
    | Node (ll, lv, lr, _, acc_l) ->
      let acc_r = accval r in
      if height ll >= height lr then
        create ll lv
          (Int64.add (Int64.add acc_l w) acc_r)
          (create lr x (Int64.add (Int64.add w (accval lr)) acc_r) r)
      else (
        match lr with
        | Empty -> invalid_arg "Val_map.bal"
        | Node (lrl, lrv, lrr, _, _) ->
          let acc_lrr = accval lrr in
          create
            (create ll lv (Int64.sub acc_l acc_lrr) lrl)
            lrv
            (Int64.add (Int64.add acc_l w) acc_r)
            (create lrr x (Int64.add (Int64.add acc_lrr w) acc_r) r)
      )
  ) else if hr > hl + 2 then (
    match r with
    | Empty -> invalid_arg "Val_map.bal"
    | Node (rl, rv, rr, _, acc_r) ->
      let acc_l = accval l in
      if height rr >= height rl then
        create
          (create l x (Int64.add (Int64.add acc_l w) (accval rl)) rl)
          rv
          (Int64.add (Int64.add acc_l w) acc_r)
          rr
      else (
        match rl with
        | Empty -> invalid_arg "Val_map.bal"
        | Node (rll, rlv, rlr, _, _) ->
          let acc_rll = accval rll in
          create
            (create l x (Int64.add (Int64.add acc_l w) acc_rll) rll)
            rlv
            (Int64.add (Int64.add acc_l w) acc_r)
            (create rlr rv (Int64.sub acc_r acc_rll) rr)
      )
  ) else (
    let acc_l = accval l in
    let acc_r = accval r in
    create l x (Int64.add (Int64.add acc_l w) acc_r) r
  )

let empty = Empty

let is_empty = function
  | Empty -> true
  | Node _ -> false

let rec add key weight = function
  | Empty -> Node (Empty, key, Empty, 1, Int64.of_int weight)
  | Node (l, key', r, h, acc) ->
    if key = key' then
      Node
        ( l,
          key,
          r,
          h,
          Int64.add (Int64.add (Int64.of_int weight) (accval l)) (accval r) )
    else (
      let weight' = Int64.sub (Int64.sub acc (accval l)) (accval r) in
      if key < key' then
        bal (add key weight l) key' weight' r
      else
        bal l key' weight' (add key weight r)
    )

let rec find_acc aim_acc = function
  | Empty -> raise Not_found
  | Node (l, key, r, _, acc) ->
    if aim_acc >= acc then
      raise Not_found
    else (
      let acc_l = accval l in
      let acc_r = accval r in
      if acc_l > aim_acc then
        find_acc aim_acc l
      else if Int64.add acc_r acc_l > aim_acc then
        find_acc (Int64.sub aim_acc acc_l) r
      else
        key
    )

let rec mem key = function
  | Empty -> false
  | Node (l, key', r, _, _) ->
    let c = Mods.int_compare key key' in
    c = 0
    || mem key
         (if c < 0 then
            l
          else
            r)

let rec min_binding = function
  | Empty -> raise Not_found
  | Node (Empty, x, r, _, acc) -> x, Int64.sub acc (accval r)
  | Node (l, _, _, _, _) -> min_binding l

let rec remove_min_binding = function
  | Empty -> invalid_arg "Val_map.remove_min_elt"
  | Node (Empty, _, r, _, _) -> r
  | Node (l, x, r, _, acc) ->
    let weight = Int64.sub (Int64.sub acc (accval l)) (accval r) in
    bal (remove_min_binding l) x weight r

let merge t1 t2 =
  match t1, t2 with
  | Empty, t -> t
  | t, Empty -> t
  | Node _, Node _ ->
    let x, w = min_binding t2 in
    bal t1 x w (remove_min_binding t2)

let rec remove x = function
  | Empty -> Empty
  | Node (l, v, r, _, acc) ->
    let c = compare x v in
    if c = 0 then
      merge l r
    else (
      let weight = Int64.sub (Int64.sub acc (accval l)) (accval r) in
      if c < 0 then
        bal (remove x l) v weight r
      else
        bal l v weight (remove x r)
    )
(*
let rec iter f = function
  | Empty -> ()
  | Node(l, v, r, _,_) as x -> iter f l; f v (weight x); iter f r

let rec fold f m accu =
  match m with
  | Empty -> accu
  | Node(l, v, r, _,_) as x -> fold f r (f v (weight x) (fold f l accu))
*)

(**Returns (key,value) at random in the tree*)
let random state m =
  try
    let r = Random.State.int64 state (accval m) in
    find_acc r m
  with Invalid_argument _ -> invalid_arg "Val_map.random_val"
