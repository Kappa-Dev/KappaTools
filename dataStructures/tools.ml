let pow x n =
  assert (n >= 0);
  let rec aux x n acc =
    if n = 0 then acc
    else
      aux x (pred n) (x*acc)
  in
  aux x n 1

let pow64 x n =
  assert (n >= Int64.zero);
  let rec aux x n acc =
    if n = Int64.zero then acc
    else
      aux x (Int64.pred n) (Int64.mul x acc)
  in
  aux x n Int64.one

(*number of bits used to represent n in base 2*)
let bit_rep_size n = 
	let rec aux p acc = 
		if p = 0 then acc
		else
			let p' = p/2 in
			aux p' (acc+1)
	in
		aux n 0  

let replace_space str =
  String.map (fun c -> if c=' ' then '_' else c) str

let read_input () =
	let rec parse acc input =
		match Stream.next input with
			| '\n' -> acc
			| c -> parse (Printf.sprintf "%s%c" acc c) input
	in
	try
		let user_input = Stream.of_channel stdin in
		parse "" user_input
	with
		| Stream.Failure -> invalid_arg "Tools.Read_input: cannot read stream"

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let rec list_last = function
  | [] -> failwith "list_last"
  | [ x ] -> x
  | _ :: l -> list_last l

let rec list_smart_filter f = function
  | t :: q as l ->
     let q' = list_smart_filter f q in
     if f t then if q == q' then l else t::q' else q'
  | l -> l

let rec list_smart_map f = function
  | t :: q as l ->
     let q' = list_smart_map f q in
     let t' = f t in
     if t' == t && q' == q then l else t' :: q'
  | l -> l

let list_mapi f l =
  let rec aux i = function
    | [] -> []
    | h :: q -> f i h :: aux (succ i) q in
  aux 0 l

let list_exists_uniq f l =
  let rec second = function
    | [] -> true
    | h :: t -> not (f h) && second t in
  let rec first = function
    | [] -> false
    | h :: t -> if f h then second t else first t in
  first l

let rec list_rev_map_append f l acc =
  match l with
  | [] -> acc
  | h :: t -> list_rev_map_append f t (f h::acc)

let rec list_map_flatten f = function (* list_bind *)
  | [] -> []
  | h :: t -> List.append (f h) (list_map_flatten f t)
(*  List.rev
    (List.fold_left (fun x y -> List.rev_append y x) [] (List.rev_map f l))
 *)

let rec list_fold_right_map f x = function
  | [] -> (x,[])
  | h :: t ->
     let (x',t') = list_fold_right_map f x t in
     let (x'', h') = f x' h in (x'', h'::t')

let rec list_fold_left2 f x l1 l2 =
  match l1, l2 with
  | [], [] -> x
  | [], _ :: _ | _ :: _, [] -> raise (Invalid_argument "list_fold_left2")
  | h1::t1, h2::t2 -> list_fold_left2 f (f x h1 h2) t1 t2

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
       fill (succ i) v l
  in
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

let iteri f i =
  let rec aux j =
  if j < i then let () = f j in aux (succ j)
  in aux 0

let recti f x i =
  let rec aux j =
    if j < i then f j (aux (succ j)) else x
  in aux 0
