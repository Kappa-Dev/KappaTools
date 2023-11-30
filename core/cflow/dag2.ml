type 'a superlist = Elt of 'a | Super_list of 'a superlist list list

let rec fold f a list =
  match list with
  | Elt elt -> f a elt
  | Super_list l -> List.fold_left (List.fold_left (fold f)) a l

let rec compare_list compare l1 l2 =
  match l1 with
  | [] ->
    (match l2 with
    | [] -> 0
    | _ -> -1)
  | h1 :: t1 ->
    (match l2 with
    | [] -> 1
    | h2 :: t2 ->
      let cmp = compare h1 h2 in
      (match cmp with
      | 0 -> compare_list compare t1 t2
      | _ -> cmp))

let rec compare_superlist compare a b =
  match a, b with
  | Elt a, Elt b -> compare a b
  | Super_list a, Super_list b ->
    compare_list (compare_list (compare_superlist compare)) a b
  | Elt _, Super_list _ -> -1
  | Super_list _, Elt _ -> 1

let rec sort compare superlist =
  match superlist with
  | Elt a -> Elt a
  | Super_list a ->
    Super_list
      (List.rev_map
         (fun l ->
           let l' = List.rev_map (sort compare) l in
           let l' = List.sort (compare_superlist compare) l' in
           l')
         (List.rev a))

let dump_super_list f s l =
  let rec aux depth f l =
    match l with
    | Elt a -> Format.fprintf f "%s%a@," depth s a
    | Super_list l ->
      Pp.list Pp.cut
        (fun f l ->
          Format.fprintf f "%s@,%a" depth
            (Pp.list Pp.cut (fun f l ->
                 Format.fprintf f "(%s@,%a" depth (aux ("--" ^ depth)) l))
            l)
        f l
  in
  Format.fprintf f "@[<v>%a@]@." (aux "->") l

type 'a info = { pred_1: int list; conflict_1: int list; label: 'a; depth: int }

let compare_fst_triple (a, _, _) (b, _, _) = compare a b

let smash compare l =
  let rec aux to_do old current accu =
    match to_do with
    | [] -> current :: accu
    | h :: t ->
      if compare h old = 0 then
        aux t old (h :: current) accu
      else
        aux t h [ h ] (current :: accu)
  in
  match l with
  | Elt a -> Elt a
  | Super_list l ->
    Super_list
      (List.rev
         (List.fold_left
            (fun accu l ->
              match l with
              | [] -> accu
              | h :: t -> aux t h [ h ] accu)
            [] l))

(*let l =
    Super_list
      [
        [
          Super_list
            [
              [Elt (2,0,0);Elt (3,0,0);Elt (4,0,0)];
              [Elt (1,1,1);Elt (2,1,1);Elt (3,1,1)];
              [Elt (2,2,2);Elt (4,2,2);Elt (5,3,3)];
              [Elt (1,4,4);Elt (2,4,4);Elt (3,4,4)]];
          Super_list
            [[Elt (1,5,5);Elt (2,5,5)]];
          Super_list
            [[Elt (1,6,6);Elt (2,6,6)]]]];;

  let dump_triple (a,b,c) =   ("("^(string_of_int a)^","^(string_of_int b)^","^(string_of_int c)^")")

  let _ = dump_super_list dump_triple l
  let l1 = sort compare_fst_triple l
  let _ = dump_super_list dump_triple l1
  let l2 = smash (compare_superlist compare_fst_triple) l1
  let _ = dump_super_list dump_triple l2 *)
(*
let normal_form root compare f = 
  let deal_with_elt elt = 
    let info = f x in 
    let node = (info.label,info.depth,List.length info.pred_1,List.length info.conflict_1) in 
    node,info.pred_1,info.conflict_1 
  in 

  
  let rec aux current_layer next_layer accu = 
    match 
      current_layer 
    with 
    | List l  -> 

    | Super_list l -> 
        let extended_list = List.rev_map deal_with_elt l in 
        let sorted_list = List.sort compare_fst_triple extended_list in 
        let smash_list = smash compare_fst_triple extended_list in 
        let info_of x = 
          let info = f x in 
          (info.label,info.depth,List.length info.pred_1,List.length info.conflict_1)
        in 
        
          
      end
    | Hub l -> 
      begin 
      end 
        
*)
