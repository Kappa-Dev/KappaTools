type 'a t = Unary of 'a array | Binary of 'a array array

let max_array_size1 = Sys.max_array_length (* 5 *)

let max_array_size2 =
  if float_of_int max_array_size1 > sqrt (float_of_int (max_int))
  then max_int
  else max_array_size1 * max_array_size1

let euclideen p q = (p / q, p mod q)

let create n a =
  if n <= max_array_size1
  then Unary (Array.make n a)
  else
    if n > max_array_size2 then invalid_arg "GenArray: array too large"
    else
      let m =
	let p, q = euclideen n max_array_size1 in
	let l = Array.make max_array_size1 a in
	let m = Array.make (if q = 0 then p else p + 1) l in
	let rec aux k =
	  if k = (- 1) then m
	  else (m.(k) <- Array.make max_array_size1 a; aux (k - 1))
	in
	if q = 0 then aux (p - 1)
	else (m.(p) <- Array.make q a; aux (p - 1))
      in Binary m

let length = function
  | Unary a -> Array.length a
  | Binary a ->
     let p = Array.length a in
     let q = Array.length (Array.unsafe_get a (p - 1)) in
     (p - 1) * max_array_size1 + q

let get2 a p q = Array.unsafe_get (Array.unsafe_get a p) q

let get a i =
  match a with
  | Unary a -> Array.unsafe_get a i
  | Binary a ->
     let p, q = euclideen i max_array_size1 in
     get2 a p q

let set2 a p q j = Array.unsafe_set (Array.unsafe_get a p) q j

let set a i j =
  match a with
  | Unary a -> Array.unsafe_set a i j
  | Binary a ->
     let p, q = euclideen i max_array_size1 in
     set2 a p q j

let make = create

let init n f =
  if n < 0 || n > max_array_size2
  then raise (Invalid_argument ("Big_array.init : "^(string_of_int n)))
  else
    if n <= max_array_size1
    then Unary (Array.init n f)
    else
      let m =
	let p, q = euclideen n max_array_size1 in
	Array.init
	  (if q = 0 then p else p + 1)
	  (fun p' ->
	   if p'= p then
	     Array.init q (fun x -> f ((p * max_array_size1) + x))
	   else
	     Array.init max_array_size1 (fun x -> f((p'* max_array_size1) + x)))
      in
      Binary m

let make_matrix m n a = init m (fun _ -> create n a)

let append a b =
  let lb = length b in
  let la = length a in
  let c = la + lb in
  init c (fun x -> if x < la then get a x else get b (x - la))

let concat l =
  let l = List.filter (fun x -> length x > 0) l in
  match l with
  | [] -> Unary [||]
  | t:: _ ->
     let elt = get t 0 in
     let c = List.fold_left (fun sol a -> sol + length a) 0 l in
     let m = create c elt in
     let rec aux k l =
       match l with
       | [] -> m
       | t:: q ->
	  let s = length t in
	  let rec aux2 offset k =
	    if offset = s then aux k q
	    else (set m k (get t offset); aux2 (offset + 1) (k + 1))
	  in
	  aux2 0 k in
     aux 0 l

let sub a start len =
  let size = length a in
  if start < 0 || len < 0 || start + len > size
  then raise (Invalid_argument "Big_array.sub")
  else if size = 0
  then Unary [||]
  else init len (fun x -> get a (x + start))

let copy = function
  | Unary a -> Unary (Array.copy a)
  | Binary b' ->
     let size = Array.length b' in
     Binary
       (Array.init size (fun x -> Array.copy (b'.(x))))

let fill a start len x =
  let size = length a in
  if start < 0 || len < 0 || start + len > size
  then raise (Invalid_argument "Big_array.fill")
  else
    let rec aux k i =
      if k < len then let () = set a i x in aux (k + 1) (i + 1) in
    aux 0 start

let of_list = function
  | [] -> Unary [||]
  | t::_ as l ->
     let size = List.length l in
     let a = create size t in
     let rec aux k = function
       | [] -> a
       | t:: q -> let () = set a k t in aux (k + 1) q
     in aux 0 l

let iter f = function
  | Unary a -> Array.iter f a
  | Binary a -> Array.iter (Array.iter f) a

let iteri f = function
  | Unary a -> Array.iteri f a
  | Binary a ->
     let g k k' = k*max_array_size1+k' in
     Array.iteri (fun k a -> Array.iteri (fun k' a -> f (g k k') a) a) a

    (*let gen g1 g2 h1 h2 f = function
      | Unary a -> h1 (g1 f a)
      |	Binary a -> h2 (g2 (g1 f) a)

    let map =
      gen Array.map Array.map (fun x -> Unary x) (fun x -> Binary x)

    let geni g1 g2 h1 h2 f = function
      | Unary a -> h1 (g1 f a)
      |	Binary b ->
	 h2
	   (g2
	      (fun p a ->
	       let n = p * max_array_size1 in
	       g1
		 (fun q a -> f (q + n) a)
		 a)
	      b)

    let mapi =
      geni Array.mapi Array.mapi (fun x -> Unary x) (fun x -> Binary x)*)

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
     || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Array.blit"
  else
    if ofs1 < ofs2 then
      (* Top-down copy *)
      for i = len - 1 downto 0 do
	set a2 (ofs2 + i) (get a1 (ofs1 + i))
      done
    else
      (* Bottom-up copy *)
      for i = 0 to len - 1 do
	set a2 (ofs2 + i) (get a1 (ofs1 + i))
      done

let fold_left f init = function
  | Unary a -> Array.fold_left f init a
  | Binary a -> Array.fold_left (Array.fold_left f) init a

let fold_lefti f init a  =
  let g (i,current) k = (i+1,f i current k) in
  snd (fold_left g (0,init) a)

let fold_right f a init  =
  match a with
  | Unary a -> Array.fold_right f a init
  | Binary a -> Array.fold_right (Array.fold_right f) a init

let fold_righti f a init =
  let g k (i,current) = (i-1,f i k current) in
  snd (fold_right g a (length a-1,init))

let print ?(trailing=(fun _ -> ())) pr_sep pr_el f a =
  let rec aux i f =
    if i < length a then
      let () = pr_el i f (get a i) in
      if i < length a - 1 then
	let () = pr_sep f in aux (succ i) f
      else if i > 0 then trailing f
  in aux 0 f
