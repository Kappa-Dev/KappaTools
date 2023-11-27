(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let remove_suffix_after_last_occurrence p list =
  let rec aux list buffer output =
    match list with
    | h :: t when p h -> aux t [] ((h :: buffer) :: output)
    | h :: t -> aux t (h :: buffer) output
    | [] -> output
  in
  let rev_concat list =
    List.fold_left (List.fold_left (fun output a -> a :: output)) [] list
  in
  rev_concat (aux list [] [])

let rec last = function
  | [] -> failwith "list_last"
  | [ x ] -> x
  | _ :: l -> last l

let rec aux_pop_last acc = function
  | [] -> failwith "list_pop_last"
  | [ x ] -> List.rev acc, x
  | h :: t -> aux_pop_last (h :: acc) t

let pop_last l = aux_pop_last [] l

let cons_option h t =
  match h with
  | Some x -> x :: t
  | None -> t

let rec smart_filter f = function
  | t :: q as l ->
    let q' = smart_filter f q in
    if f t then
      if q == q' then
        l
      else
        t :: q'
    else
      q'
  | l -> l

let rec smart_map f = function
  | t :: q as l ->
    let q' = smart_map f q in
    let t' = f t in
    if t' == t && q' == q then
      l
    else
      t' :: q'
  | l -> l

let rev_mapi f l =
  let rec aux_mapi i acc = function
    | [] -> acc
    | h :: q -> aux_mapi (pred i) (f i h :: acc) q
  in
  aux_mapi (List.length l - 1) [] l

let rec map_option f = function
  | [] -> []
  | h :: q -> cons_option (f h) (map_option f q)

let exists_uniq f l =
  let rec second = function
    | [] -> true
    | h :: t -> (not (f h)) && second t
  in
  let rec first = function
    | [] -> false
    | h :: t ->
      if f h then
        second t
      else
        first t
  in
  first l

let merge_uniq cmp l1 l2 =
  let rec aux_merge_uniq l1 l2 k =
    match l1, l2 with
    | [], _ -> k l2
    | _, [] -> k l1
    | h1 :: t1, h2 :: t2 ->
      let c = cmp h1 h2 in
      if c < 0 then
        aux_merge_uniq t1 l2 (fun o ->
            if o == t1 then
              k l1
            else
              k (h1 :: o))
      else if c > 0 then
        aux_merge_uniq l1 t2 (fun o ->
            if o == t2 then
              k l2
            else
              k (h2 :: o))
      else
        aux_merge_uniq t1 t2 (fun o ->
            if o == t1 then
              k l1
            else
              k (h1 :: o))
  in
  aux_merge_uniq l1 l2 (fun x -> x)

let rec rev_map_append f l acc =
  match l with
  | [] -> acc
  | h :: t -> rev_map_append f t (f h :: acc)

let rec map_flatten f = function
  (* list_bind *)
  | [] -> []
  | h :: t -> List.append (f h) (map_flatten f t)
(* List.rev
   (List.fold_left (fun x y -> List.rev_append y x) [] (List.rev_map f l))
*)

let remove_consecutive_double l =
  let rec aux last l acc =
    match l with
    | h :: q when last = h -> aux last q acc
    | h :: q -> aux h q (h :: acc)
    | [] -> List.rev acc
  in
  match l with
  | [] -> []
  | h :: q -> aux h q [ h ]

let rec fold_right_map f l x =
  match l with
  | [] -> [], x
  | h :: t ->
    let t', x' = fold_right_map f t x in
    let h', x'' = f h x' in
    h' :: t', x''

let rec fold_left2 f x l1 l2 =
  match l1, l2 with
  | [], [] -> x
  | [], _ :: _ | _ :: _, [] -> raise (Invalid_argument "list_fold_left2")
  | h1 :: t1, h2 :: t2 -> fold_left2 f (f x h1 h2) t1 t2

let random rs l = List.nth l (Random.State.int rs (List.length l))

let find_option (p : 'a -> bool) (l : 'a list) : 'a option =
  try Some (List.find p l) with Not_found -> None

module Infix = struct
  let ( $$ ) = cons_option
end
