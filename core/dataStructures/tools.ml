(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let float_is_zero x =
  match classify_float x with
  | FP_zero -> true
  | FP_normal | FP_subnormal | FP_infinite | FP_nan -> false

let pow i j =
  let () = assert (0 <= j) in
  let rec aux i k accu =
    if k = 0 then
      accu
    else if k land 1 = 0 then
      aux i (k / 2) accu * accu
    else
      aux i (k / 2) (i * accu * accu)
  in
  aux i j 1

let fact i =
  let rec aux i accu =
    if i < 2 then
      accu
    else
      aux (i - 1) (i * accu)
  in
  aux i 1

let get_product_image_occ start combine f l =
  let l = List.sort compare l in
  let rec aux l old occ accu =
    match l with
    | h :: t when h = old -> aux t old (1 + occ) accu
    | _ ->
      let accu = combine accu (f occ) in
      (match l with
      | h :: t -> aux t h 1 accu
      | [] -> accu)
  in
  match l with
  | [] -> 1
  | h :: t -> aux t h 1 start

let get_product_image_occ_2 start combine f l1 l2 =
  let l1 = List.sort compare l1 in
  let l2 = List.sort compare l2 in
  let count_head_and_get_tail l =
    match l with
    | [] -> [], 0
    | h :: t ->
      let rec aux l h occ =
        match l with
        | [] -> [], occ
        | h' :: t when h = h' -> aux t h (occ + 1)
        | _ -> l, occ
      in
      aux t h 1
  in
  let rec aux l1 l2 accu =
    match l1, l2 with
    | h1 :: _, h2 :: _ when h1 = h2 ->
      let l1, occ1 = count_head_and_get_tail l1 in
      let l2, occ2 = count_head_and_get_tail l2 in
      aux l1 l2 (combine accu (f occ1 occ2))
    | h1 :: _, h2 :: _ when compare h1 h2 < 0 ->
      let l1, occ1 = count_head_and_get_tail l1 in
      aux l1 l2 (combine accu (f occ1 0))
    | _ :: _, _ :: _ ->
      let l2, occ2 = count_head_and_get_tail l2 in
      aux l1 l2 (combine accu (f 0 occ2))
    | [], _ | _, [] -> accu
  in
  aux l1 l2 start

let div2 x = Int64.div x (Int64.add Int64.one Int64.one)

let pow64 x n =
  assert (n >= Int64.zero);
  let rec aux k accu =
    if k = Int64.zero then
      accu
    else if Int64.logand k Int64.one = Int64.zero then
      aux (div2 k) (Int64.mul accu accu)
    else
      aux (div2 k) (Int64.mul x (Int64.mul accu accu))
  in
  aux n Int64.one

let cantor_pairing x y =
  let s = x + y in
  (succ s * s / 2) + y

let read_input () =
  let rec parse acc input =
    match Stream.next input with
    | '\n' -> acc
    | c -> parse (Printf.sprintf "%s%c" acc c) input
  in
  try
    let user_input = Stream.of_channel stdin in
    parse "" user_input
  with Stream.Failure -> invalid_arg "Tools.Read_input: cannot read stream"

let not_an_id s =
  String.length s = 0
  ||
  let i = int_of_char s.[0] in
  (i < 65 || i > 122 || (i > 90 && (i <> 95 || String.length s = 1) && i < 97))
  ||
  try
    String.iter
      (fun c ->
        let i = int_of_char c in
        if
          i < 48 || i > 122
          || (i > 57 && (i < 65 || (i > 90 && i <> 95 && i < 97)))
        then
          raise Not_found)
      s;
    false
  with Not_found -> true

let array_fold_left_mapi f x a =
  let y = ref x in
  let o =
    Array.init (Array.length a) (fun i ->
        let y', out = f i !y a.(i) in
        let () = y := y' in
        out)
  in
  !y, o

let array_map_of_list =
  let rec fill f i v = function
    | [] -> ()
    | x :: l ->
      Array.unsafe_set v i (f x);
      fill f (succ i) v l
  in
  fun f -> function
    | [] -> [||]
    | x :: l ->
      let len = succ (List.length l) in
      let ans = Array.make len (f x) in
      let () = fill f 1 ans l in
      ans

let array_rev_of_list =
  let rec fill out i = function
    | [] -> assert (i = -1)
    | h' :: t' ->
      let () = Array.unsafe_set out i h' in
      fill out (pred i) t'
  in
  function
  | [] -> [||]
  | h :: t ->
    let l = succ (List.length t) in
    let out = Array.make l h in
    let () = fill out (l - 2) t in
    out

let array_rev_map_of_list =
  let rec fill f out i = function
    | [] -> assert (i = -1)
    | h' :: t' ->
      let () = Array.unsafe_set out i (f h') in
      fill f out (pred i) t'
  in
  fun f -> function
    | [] -> [||]
    | h :: t ->
      let l = succ (List.length t) in
      let out = Array.make l (f h) in
      let () = fill f out (l - 2) t in
      out

let array_fold_lefti f x a =
  let y = ref x in
  let () = Array.iteri (fun i e -> y := f i !y e) a in
  !y

let rec aux_fold_righti i f a x =
  if i < 0 then
    x
  else
    aux_fold_righti (pred i) f a (f i a.(i) x)

let array_fold_righti f a x = aux_fold_righti (Array.length a - 1) f a x

let array_fold_left2i f x a1 a2 =
  let l = Array.length a1 in
  if l <> Array.length a2 then
    raise (Invalid_argument "array_fold_left2i")
  else
    array_fold_lefti (fun i x e -> f i x e a2.(i)) x a1

let array_filter f a =
  array_fold_lefti
    (fun i acc x ->
      if f i x then
        i :: acc
      else
        acc)
    [] a

let array_min_equal_not_null l1 l2 =
  if Array.length l1 <> Array.length l2 then
    None
  else (
    let rec f j =
      if j = Array.length l1 then
        Some ([], [])
      else (
        let nb1, ag1 = l1.(j) in
        let nb2, ag2 = l2.(j) in
        if nb1 <> nb2 then
          None
        else if nb1 = 0 then
          f (succ j)
        else (
          let rec aux i va out =
            if i = Array.length l1 then
              Some out
            else (
              let nb1, ag1 = l1.(i) in
              let nb2, ag2 = l2.(i) in
              if nb1 <> nb2 then
                None
              else if nb1 > 0 && nb1 < va then
                aux (succ i) nb1 (ag1, ag2)
              else
                aux (succ i) va out
            )
          in
          aux (succ j) nb1 (ag1, ag2)
        )
      )
    in
    f 0
  )

let array_compare compare a b =
  let l = Array.length a in
  let l' = Array.length b in
  let d = Stdlib.compare l l' in
  let rec aux_array_compare k =
    if k >= l then
      0
    else (
      let o = compare a.(k) b.(k) in
      if o <> 0 then
        o
      else
        aux_array_compare (succ k)
    )
  in
  if d <> 0 then
    d
  else
    aux_array_compare 0

let iteri f i =
  let rec aux j =
    if j < i then (
      let () = f j in
      aux (succ j)
    )
  in
  aux 0

let rec recti f x i =
  if 0 < i then (
    let i' = pred i in
    recti f (f x i') i'
  ) else
    x

let min_pos_int_not_zero (keya, dataa) (keyb, datab) =
  if keya = 0 then
    keyb, datab
  else if keyb = 0 then
    keya, dataa
  else if compare keya keyb > 0 then
    keyb, datab
  else
    keya, dataa

let max_pos_int_not_zero (keya, dataa) (keyb, datab) =
  if compare keya keyb > 0 then
    keya, dataa
  else
    keyb, datab

let fold_over_permutations f l accu =
  let rec aux to_do discarded permutation accu =
    match to_do, discarded with
    | [], [] -> f permutation accu
    | [], _ :: _ -> accu
    | h :: t, _ ->
      let to_do1 = List.fold_left (fun list a -> a :: list) t discarded in
      let accu = aux to_do1 [] (h :: permutation) accu in
      let accu = aux t (h :: discarded) permutation accu in
      accu
  in
  aux l [] [] accu

let gcd_2 a b =
  let rec aux a b =
    if b = 0 then
      a
    else
      aux b (a mod b)
  in
  let a = abs a in
  let b = abs b in
  if a < b then
    aux b a
  else
    aux a b

let lcm_2 a b = abs a * abs b / gcd_2 a b

let lcm list =
  match list with
  | [] -> 0
  | h :: t -> List.fold_left lcm_2 h t

let get_interval_list p i j =
  let add current output =
    match current with
    | None -> output
    | Some p -> p :: output
  in
  let insert k current =
    match current with
    | None -> Some (k, k)
    | Some (_, j) -> Some (k, j)
  in
  let rec aux p k current output =
    if k < i then
      add current output
    else if p k then
      aux p (k - 1) (insert k current) output
    else
      aux p (k - 1) None (add current output)
  in
  aux p j None []

let lowercase = String.lowercase_ascii
let capitalize = String.capitalize_ascii

let string_split_on_char (delimiter : char) (s : string) :
    string * string option =
  try
    let index = String.index s delimiter in
    let length = String.length s in
    String.sub s 0 index, Some (String.sub s (index + 1) (length - index - 1))
  with Not_found -> s, None

let smash_duplicate_in_ordered_list p l =
  let () = Format.fprintf Format.std_formatter "DUPL \n" in

  let rec aux tail nocc current accu =
    match tail with
    | [] -> (current, nocc) :: accu
    | (h, n) :: t when p h current = 0 ->
      (*let () = Format.fprintf Format.std_formatter "DUPL %i\n" (n+nocc) in*)
      aux t (n + nocc) current accu
    | (h, n) :: t -> aux t n h ((current, nocc) :: accu)
  in
  match List.rev l with
  | [] -> []
  | (h, n) :: t -> aux t n h []

let chop_suffix_or_extension name ext =
  if Filename.check_suffix name ext then
    Filename.chop_suffix name ext
  else
    Filename.remove_extension name

let find_available_name ~already_there name ~facultative ~ext =
  let ext =
    match ext with
    | Some e -> e
    | None -> Filename.extension name
  in
  let base = chop_suffix_or_extension name ext in
  if already_there (base ^ ext) then (
    let base' =
      if facultative <> "" then
        base ^ "_" ^ facultative
      else
        base
    in
    if already_there (base' ^ ext) then (
      let v = ref 0 in
      let () =
        while already_there (base' ^ "~" ^ string_of_int !v ^ ext) do
          incr v
        done
      in
      base' ^ "~" ^ string_of_int !v ^ ext
    ) else
      base' ^ ext
  ) else
    base ^ ext

let default_message_delimter : char = '\x1e' (* "\t" *)

let get_ref ref =
  let i = !ref in
  let () = ref := i + 1 in
  i

let remove_double_elements l =
  let l = List.sort compare l in
  let rec aux l accu old =
    match l, old with
    | [], _ -> accu
    | h :: t, Some h' when h = h' -> aux t accu old
    | h :: t, (None | Some _) -> aux t (h :: accu) (Some h)
  in
  aux l [] None

let from_n_to_0 n =
  let rec aux k acc =
    if k > n then
      acc
    else
      aux (k + 1) (k :: acc)
  in
  aux 0 []

let clear a = Array.iteri (fun i _ -> a.(i) <- []) a

let sort_by_priority f n =
  let a = Array.make (n + 1) [] in
  let keys = from_n_to_0 n in
  let sort l =
    let rec aux l =
      match l with
      | [] -> ()
      | h :: t ->
        let k = f h in
        let () = a.(k) <- h :: a.(k) in
        aux t
    in
    let () = aux l in
    let output =
      List.fold_left
        (fun list key ->
          List.fold_left (fun list elt -> elt :: list) list a.(key))
        [] keys
    in
    let () = clear a in
    output
  in
  sort
