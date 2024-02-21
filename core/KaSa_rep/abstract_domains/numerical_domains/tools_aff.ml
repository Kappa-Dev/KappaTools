open List

let trace = ref false

let t_int parameters x =
  if Remanent_parameters.get_trace parameters || !trace then
    Loggers.fprintf (Remanent_parameters.get_logger parameters) "%i" x

let t_string parameters x =
  if Remanent_parameters.get_trace parameters || !trace then
    Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" x

let comp_pair (a, b) (x, y) =
  match a < x, a = x, b < y with
  | true, _, _ -> -1
  | _, true, true -> -1
  | _ -> 1

let rec list_it f l b =
  match l with
  | t :: q -> list_it f q (f t b)
  | [] -> b

let rec true_map x y =
  match y with
  | t :: q ->
    let a = x t in
    a :: true_map x q
  | [] -> []

let max_of_list parameters error list =
  match list with
  | [] -> Exception.warn parameters error __POS__ Exit None
  | t :: q -> error, Some (List.fold_left max t q)

let sum_list list = List.fold_left (fun a b -> a + b) 0 list

let rec member x l =
  match l with
  | [] -> false
  | a :: b ->
    if a = x then
      true
    else
      member x b

let rec member2 x l =
  match l with
  | [] -> false
  | (a, _) :: b ->
    if a = x then
      true
    else
      member2 x b

let rec all_differents liste =
  match liste with
  | a :: b ->
    if member a b then
      false
    else
      all_differents b
  | [] -> true

let rec dolist l a =
  match l with
  | t :: q -> dolist q (t a)
  | [] -> a

let concat liste1 liste2 =
  let rec aux liste1 liste2 =
    match liste1 with
    | a :: b -> aux b (a :: liste2)
    | [] -> liste2
  in
  aux (rev liste1) liste2

let applati liste =
  let rec aux1 l rep =
    match l with
    | a :: b -> aux1 b (a :: rep)
    | [] -> rep
  in
  let rec aux2 l rep =
    match l with
    | a :: b -> aux2 b (aux1 a rep)
    | [] -> rep
  in
  rev (aux2 liste [])

let rec forall p l =
  match l with
  | a :: b ->
    if p a then
      forall p b
    else
      false
  | [] -> true

let rec correspondance parameters error ancien nouveau x =
  match ancien, nouveau with
  | a :: _, p :: _ when a = x -> error, Some p
  | _ :: b, _ :: q -> correspondance parameters error b q x
  | [], [] -> error, Some x
  | _ -> Exception.warn parameters error __POS__ Exit None

let transfert parameters error ancien nouveau liste =
  let rec aux parameters error l =
    match l with
    | [] -> error, []
    | a :: b ->
      let error, h = correspondance parameters error ancien nouveau a in
      let error, tl = aux parameters error b in
      error, h :: tl
  in
  aux parameters error liste

let tr parameters error ancien nouveau id =
  match transfert parameters error ancien nouveau [ id ] with
  | error, [ res ] -> error, res
  | error, _ -> Exception.warn parameters error __POS__ Exit None

let paire_of_list liste1 liste2 rep =
  let rec aux1 x l2 rep =
    match l2 with
    | a :: b -> aux1 x b ((x, a) :: rep)
    | [] -> rep
  in
  let rec aux2 l1 l2 rep =
    match l1 with
    | a :: b -> aux2 b l2 (aux1 a l2 rep)
    | [] -> rep
  in
  aux2 liste1 liste2 rep

let produit_predicat p i j =
  match i, j with
  | (a, b), (c, d) -> p a c || (a = c && p b d)

let union_list p liste1 liste2 =
  let rec aux liste1 liste2 sol =
    match liste1, liste2 with
    | a :: b, t :: q when a = t -> aux b q (t :: sol)
    | a :: b, t :: _q when p a t -> aux b liste2 (a :: sol)
    | _a :: _b, t :: q -> aux liste1 q (t :: sol)
    | a :: b, [] -> aux b [] (a :: sol)
    | [], a :: b -> aux b [] (a :: sol)
    | [], [] -> rev sol
  in
  aux liste1 liste2 []

let intersec_list p liste1 liste2 =
  let rec aux liste1 liste2 sol =
    match liste1, liste2 with
    | a :: b, t :: q ->
      if a = t then
        aux b q (a :: sol)
      else if p a t then
        aux b liste2 sol
      else
        aux liste1 q sol
    | _ -> rev sol
  in
  aux liste1 liste2 []

let ajoute t liste =
  let rec aux liste rep =
    match liste with
    | a :: b -> aux b (concat t a :: rep)
    | [] -> rep
  in
  aux (rev liste) []

let flat_map f l =
  let rec aux l rep =
    match l with
    | a :: b -> aux b (f a @ rep)
    | [] -> rep
  in
  aux (rev l) []

let flat_map_zip f l =
  let rec aux l rep1 rep2 =
    match l with
    | a :: b ->
      let c, d = f a in
      aux b (c @ rep1) (d @ rep2)
    | [] -> rep1, rep2
  in
  aux (rev l) [] []

let produit_list liste1 liste2 = flat_map (fun x -> ajoute x liste2) liste1

let rec mix_list liste =
  match liste with
  | [] -> [ [] ]
  | t :: q -> produit_list t (mix_list q)

let insert_list p x liste1 =
  let rec vide reste fait =
    match reste with
    | a :: b -> vide b (a :: fait)
    | [] -> fait
  in
  let rec aux reste vue =
    match reste with
    | a :: b ->
      if p a x then
        aux b (a :: vue)
      else
        vide (x :: vue) reste
    | [] -> vide (x :: vue) []
  in
  aux liste1 []

let list_of_table h =
  let liste = ref [] in
  Hashtbl.iter (fun a b -> liste := (a, b) :: !liste) h;
  !liste

let copy_table h =
  let rep = Hashtbl.create 1 in
  Hashtbl.iter (fun a b -> Hashtbl.add rep a b) h;
  rep

let insert_sort p l k =
  let rec aux l rep =
    match l with
    | t :: q when p t k -> aux q (t :: rep)
    | t :: _q when t = k -> concat (List.rev rep) l
    | _ -> concat (List.rev rep) (k :: l)
  in
  aux l []

let merge p l k =
  let rec aux l1 l2 rep =
    match l1, l2 with
    | t :: q, a :: _b when p t a -> aux q l2 (t :: rep)
    | t :: q, a :: b when p a t -> aux (t :: q) b (a :: rep)
    | t :: q, _a :: b -> aux q b (t :: rep)
    | t :: q, [] -> aux q l2 (t :: rep)
    | [], a :: b -> aux l1 b (a :: rep)
    | _ -> List.rev rep
  in
  aux l k []

let fusion = merge

let sub_list p l k =
  let rec aux l rep =
    match l with
    | t :: q when t = k -> concat (List.rev rep) q
    | t :: q when p t k -> aux q (t :: rep)
    | _ -> concat (List.rev rep) l
  in
  aux l []

let vide f l =
  let rec aux l rep =
    match l with
    | t :: q when f t -> aux q rep
    | t :: q -> aux q (t :: rep)
    | _ -> List.rev rep
  in
  aux l []

let filtre f l = vide (fun x -> not (f x)) l

let rev l =
  let rec aux res sol =
    match res with
    | [] -> sol
    | a :: b -> aux b (a :: sol)
  in
  aux l []

let flap_map parameters error f l =
  let n =
    Working_list_imperative.make
      (Remanent_parameters.get_empty_hashtbl_size parameters)
  in
  let rec aux l =
    match l with
    | t :: q ->
      Working_list_imperative.push (f t) n;
      aux q
    | [] -> ()
  in
  List.iter aux l;
  error, Working_list_imperative.list n

let map_list (f : 'a -> 'b) (l : 'a list) =
  let rec aux (l : 'a list) (rep : 'b list) =
    match l with
    | [] -> rep
    | a :: b ->
      (try
         let r = f a in
         aux b (r :: rep)
       with _ -> aux b rep)
  in
  aux (List.rev l) []

let compte_list parameters error l =
  let a =
    Hashtbl.create (Remanent_parameters.get_empty_hashtbl_size parameters)
  in
  let get x = try Hashtbl.find a x with _ -> 0 in
  let inc x =
    let rep = get x in
    Hashtbl.remove a x;
    Hashtbl.add a x (rep + 1)
  in
  List.iter (fun x -> inc x) l;
  error, list_of_table a
