(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let float_is_zero x =
  match classify_float x with
  | FP_zero -> true
  | FP_normal | FP_subnormal |FP_infinite | FP_nan -> false

let pow i j =
  let () = assert (0 <= j) in
  let rec aux i k accu =
    if k=0 then accu
    else if k land 1 = 0
    then
      aux i (k/2) accu*accu
    else
      aux i (k/2) (i*accu*accu)
  in
  aux i j 1

let div2 x = Int64.div x (Int64.add Int64.one Int64.one)
let pow64 x n =
  assert (n >= Int64.zero);
  let rec aux k accu =
    if k=Int64.zero then accu
    else if Int64.logand k Int64.one = Int64.zero
    then
      aux (div2 k) (Int64.mul accu accu)
    else
      aux (div2 k) (Int64.mul x (Int64.mul accu accu))
  in
  aux n Int64.one

let read_input () =
  let rec parse acc input =
    match Stream.next input with
    | '\n' -> acc
    | c -> parse (Printf.sprintf "%s%c" acc c) input in
  try
    let user_input = Stream.of_channel stdin in
    parse "" user_input
  with
  | Stream.Failure -> invalid_arg "Tools.Read_input: cannot read stream"

let not_an_id s =
  try
    String.iter
      (fun c ->
         let i = int_of_char c in
         if i < 48 || i > 122 ||
            (i > 57 && (i < 65 || (i > 90 && i <> 95 && i < 97)))
         then raise Not_found)
      s;
    false
  with Not_found -> true

let array_fold_left_mapi f x a =
  let y = ref x in
  let o = Array.init (Array.length a)
      (fun i -> let (y',out) = f i !y a.(i) in
        let () = y := y' in
        out) in
  (!y,o)

let array_map_of_list f l =
  let len = List.length l in
  let rec fill i v = function
    | [] -> ()
    | x :: l ->
      Array.unsafe_set v i (f x);
      fill (succ i) v l in
  match l with
  | [] -> [||]
  | x :: l ->
    let ans = Array.make len (f x) in
    let () = fill 1 ans l in
    ans

let array_rev_of_list = function
  | [] -> [||]
  | h :: t ->
    let l = succ (List.length t) in
    let out = Array.make l h in
    let rec fill i = function
      | [] -> assert (i= -1)
      | h' :: t' ->
        let () = Array.unsafe_set out i h' in
        fill (pred i) t' in
    let () = fill (l - 2) t in
    out

let array_fold_lefti f x a =
  let y = ref x in
  let () = Array.iteri (fun i e -> y := f i !y e) a in
  !y

let array_fold_left2i  f x a1 a2 =
  let l = Array.length a1 in
  if l <> Array.length a2 then raise (Invalid_argument "array_fold_left2i")
  else array_fold_lefti (fun i x e -> f i x e a2.(i)) x a1

let array_filter f a =
  array_fold_lefti (fun i acc x -> if f i x then i :: acc else acc) [] a

let array_min_equal_not_null l1 l2 =
  if Array.length l1 <> Array.length l2 then None
  else
    let rec f j =
      if j = Array.length l1 then Some ([],[])
      else
        let (nb1,ag1) = l1.(j) in
        let (nb2,ag2) = l2.(j) in
        if nb1 <> nb2 then None
        else if nb1 = 0 then f (succ j)
        else
          let rec aux i va out =
            if i = Array.length l1 then Some out
            else
              let (nb1,ag1) = l1.(i) in
              let (nb2,ag2) = l2.(i) in
              if nb1 <> nb2 then None
              else if nb1 > 0 && nb1 < va then aux (succ i) nb1 (ag1,ag2)
              else aux (succ i) va out in
          aux (succ j) nb1 (ag1,ag2) in
    f 0

let iteri f i =
  let rec aux j = if j < i then let () = f j in aux (succ j) in
  aux 0

let rec recti f x i =
  if 0 < i then let i' = pred i in recti f (f x i') i' else x

let min_pos_int_not_zero (keya,dataa) (keyb,datab) =
  if keya = 0 then keyb,datab
  else if keyb = 0 then keya,dataa
  else if compare keya keyb > 0 then keyb,datab
  else keya,dataa

let max_pos_int_not_zero (keya,dataa) (keyb,datab) =
  if compare keya keyb > 0 then keya,dataa else keyb,datab

let fold_over_permutations
    f l accu =
  let rec aux to_do discarded permutation accu =
    match to_do,discarded
    with
    | [],[]   -> f permutation accu
    | [],_::_ -> accu
    | h::t,_ ->
      let to_do1 =
        List.fold_left
          (fun list a -> a::list)
          t discarded
      in
      let accu = aux to_do1 [] (h::permutation) accu in
      let accu = aux t (h::discarded) permutation accu in
      accu
  in
  aux l [] [] accu

let gcd_2 a b =
  let rec aux a b =
    if b = 0 then a
    else aux b (a mod b)
  in
  let a = abs a in
  let b = abs b in
  if a < b then aux b a
  else aux a b

let lcm_2 a b = (abs a)*(abs b)/(gcd_2 a b)
let lcm list =
  match list with
  | [] -> 0
  | h::t ->
    List.fold_left lcm_2 h t

let get_interval_list p i j =
  let add current output =
    match current with
    | None -> output
    | Some p -> p::output
  in
  let insert k current =
    match current with
    | None -> Some (k,k)
    | Some (_,j) -> Some (k,j)
  in
  let rec aux p k current output =
    if k<i
    then add current output
    else if p k
    then aux p (k-1) (insert k current) output
    else aux p (k-1) None (add current output)
  in
  aux p j None []

let lowercase = String.lowercase
let capitalize = String.capitalize

let smash_duplicate_in_ordered_list p l =

  let () = Format.fprintf Format.std_formatter "DUPL \n"  in

  let rec aux tail nocc current accu =
    match tail with
    | [] -> (current,nocc)::accu
    | (h,n)::t when p h current = 0 ->
      (*let () = Format.fprintf Format.std_formatter "DUPL %i\n" (n+nocc) in*)
        aux t (n+nocc) current accu
    | (h,n)::t -> aux t n h ((current,nocc)::accu)
  in
  match (List.rev l) with
  | [] -> []
  | (h,n)::t -> aux t n h []

let default_message_delimter : char = '\x1e' (* "\t" *)
