(**
    * union_find.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2015, the 11th of March
    * Last modification: 
    * * 
    * This library provides primitives to deal with union find algorithm with
    * path compression
    *  
    * Copyright 2010,2011 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)

type union_find =
    {
      treeArr: int array
    }

let create n =
  {
    treeArr = Array.init n (fun i -> i)
  }

(* findSet(e): which return a pointer to the representative of the set
   containing e. Since the set are disjoint, e containted in one set
   only. Therefore, the returned representative can be uniquely determined.
*)
      
let findSet e list =
  let a = Array.of_list list in
  let union_find = {treeArr = a} in
  let treeArr = union_find.treeArr in
  let pointToRoot root =
    List.iter (fun i -> treeArr.(i) <- root) in
  let rec helper e l =
    let parent = treeArr.(e) in
    if e <> parent
    then
      helper parent (e::l)
    else
      begin
        (* base case: we hit the root node make all collected nodes on the
           path point to the root.  and return the root afterwards *)
        pointToRoot parent l;
        parent;
      end
  in
  helper e []

let union x y l =
  let root_x = findSet x l in
  let root_y = findSet y l in
  let a = Array.of_list l in
  let n = Array.length a - 1 in
  for i = 0 to n do
    let union_find = {treeArr = a} in
    let treeArr = union_find.treeArr in
    treeArr.(root_x) <- root_y
  done;
  let l = Array.to_list a in
  l

let is_equivalence x y l =
  (findSet x l) = (findSet y l)

let union_list l =
  match l with
  | [] -> []
  | x :: tl as l ->
    let a = Array.of_list l in
    List.iter (fun y ->
               let root_x = findSet x l in
               let root_y = findSet y l in
               let union_find = {treeArr = a} in
               let treeArr = union_find.treeArr in
               treeArr.(root_x) <- root_y) tl;
    let l = Array.to_list a in
    l
	
(*convert union_find to dictionary type *)
let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic classes") message exn
                 (fun () -> default)

 let get_id_for_value parameter error t set =
  match  Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None ->
      error, Stochastic_classes_type.Set_list_union.empty_set
    | error, Some ids -> error, ids

 let rec print_list l =
  match l with
  | [] -> print_string "empty"
  | h :: [] ->  print_int h; print_string " "
  | h :: tl ->
     let _ = print_int h; print_string "," in
     print_list tl
       
 let rec print_list_list ls =
  match ls with
  | [] -> ()
  | h :: [] -> print_list h; print_string " "
  | h :: tl ->
     let _ =  print_list h;
              print_string "; " in
     print_list_list tl
       
let store_pointer_backward parameter error id pointer_backward l =
  List.fold_left
    (fun (error,pointer_backward) elt ->
      let error, old_set_id =
	get_id_for_value parameter error elt pointer_backward
      in
      let error,new_set_id =
        Stochastic_classes_type.Set_list_union.add_set
          parameter error id old_set_id
      in
      Int_storage.Nearly_inf_Imperatif.set
        parameter
        error
        elt
        new_set_id
        pointer_backward)
    (error, pointer_backward)
    l
    
let print_remanent_dic parameter error dic =
  Stochastic_classes_type.Dictionary_of_Stochastic_classes.print
    parameter
    error
    (fun parameter error elt l _ _ ->
      let _ = Printf.printf "Stochastic_class_id:%i:" elt in
      let _ =
        print_string "site_type:{";
        let rec print_list l =
          match l with
            | [] -> ()
            | h :: [] -> print_int h; print_string "}"
            | h :: tl ->
              let _ = print_int h; print_string "," in
              print_list tl in
        print_list l
      in
      let _ = print_newline () in
      error
    ) dic.Stochastic_classes_type.dic_union

 let store_new_class parameter error l remanent =
  (*the current remanent information: dictionary, pointer_backward*)
  let good_lists = remanent.Stochastic_classes_type.dic_union in
  let pointer_backward = remanent.Stochastic_classes_type.pointer in
  match l with
  | [] -> error, remanent
  | _ ->
     (*get allocate_id from a dictionary*)
     let error, output =
       Stochastic_classes_type.Dictionary_of_Stochastic_classes.allocate
         parameter
         error
         Misc_sa.compare_unit
         l
         ()
         Misc_sa.const_unit
         good_lists
     in
     let error,(id,dic) =
       match output with
       |Some (al,_,_,dic) -> error,(al,dic)
       | None -> warn
            parameter
            error
            (Some "line 106")
            Exit
            (0,good_lists)
     in
     (*store pointer backward*)
     let error,pointer_backward =
       store_pointer_backward parameter error id pointer_backward l
     in
     error, {
       Stochastic_classes_type.dic_union = dic; 
       Stochastic_classes_type.pointer = pointer_backward}

let union_list_dic parameter error classes =
  let init_dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init()in
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let empty_list =
    {Stochastic_classes_type.dic_union = init_dic;
     Stochastic_classes_type.pointer = init_pointer}
  in
  List.fold_left (fun (error, acc) list ->
    let list = union_list list in
    match list with
      | [] -> error, acc
      | t :: tl ->
	let pointer = acc.Stochastic_classes_type.pointer in
	let error, potential_supersets =
	  get_id_for_value parameter error t pointer
        in
	let rec aux to_visit potential_supersets =
	  match to_visit with
	    | [] -> error, acc
	    | t' :: tl' ->
	      let error, potential_supersets' =
		get_id_for_value parameter error t' pointer
              in
	      if Stochastic_classes_type.Set_list_union.is_empty_set
                potential_supersets'
	      then
		store_new_class parameter error list acc
              else
	        aux tl' potential_supersets'
        in
        (*check the beginning state of a superset*)
        if Stochastic_classes_type.Set_list_union.is_empty_set
          potential_supersets
        then
          (*if it is empty then store it to remanent*)
          store_new_class parameter error list acc
        else
          aux tl potential_supersets
  ) (error, empty_list) classes
    
let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr
