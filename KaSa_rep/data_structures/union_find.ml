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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic classes") message exn
                 (fun () -> default)

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
      
let findSet e a =
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
           path point to the root. And return the root afterwards *)
        pointToRoot parent l;
        parent;
      end
  in
  helper e []

let dump a = Array.iteri (Printf.fprintf stdout "%i:%i") a

let union x y a =
  let root_x = findSet x a in
  let root_y = findSet y a in
  let _ = a.(root_x) <- root_y in
  (*let _ = print_string "dump: "; dump a; print_string "\n" in*)
  a

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

let eq_classes_dic parameter error a =
  let error, classes = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let size = Array.length a in
  let rec aux k (classes,union_list) =
    if  k < 0 
    then
      classes, union_list
    else 
      (*find the parent of the union*)
      let rep = findSet k a in
      (*check if inside classes has already has this parent*)
      let error, old =
        Int_storage.Nearly_inf_Imperatif.get
          parameter
          error
          rep
          classes
      in
      let get_rep =
        match old with
          | None -> []
          | Some r -> r
      in
      (*store the result inside classes*)
      let error, classes = Int_storage.Nearly_inf_Imperatif.set parameter error
        rep (k :: get_rep) classes
      in
      aux (k - 1) (classes, union_list)
  in
  let classes, a = aux (size - 1) (classes, a) in
  classes, a
                (*let classes =
      IntMap.fold 
      (fun k list output -> 
        match list with 
          | [] -> output
          | _ -> 
            let _ = Printf.fprintf stdout "%i:{" k in 
            let _ = List.iter (Printf.fprintf stdout "%i,") list in 
            let _ = Printf.fprintf stdout "\n" in 
            list::output)
      classes []
  in 
  classes,a*)

let get_id_for_value parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None -> error, Stochastic_classes_type.Set_list_fst_pair.empty_set
    | error, Some ids -> error, ids

let store_pointer parameter error id pointer (l:int list Int_storage.Nearly_inf_Imperatif.t) =
  (*let _ = print_string "TEST_ID: "; print_int id; print_string "\n" in*)
  let error, get_list =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      id (*CHECK*)
      l
  in
  let list =
    match get_list with
      | None -> []
      | Some l -> l
  in
  (*let _ = print_string "List: "; print_list list; print_string "\n" in*)
  let error, result = List.fold_left
    (fun (error, pointer) elt ->
      let error, old_set_id =
        get_id_for_value parameter error elt pointer
      in
      let error, new_set_id =
        Stochastic_classes_type.Set_list_fst_pair.add_set
          parameter error id old_set_id
      in
      (*store*)
      Int_storage.Nearly_inf_Imperatif.set
        parameter
        error
        elt
        new_set_id
        pointer)
    (error, pointer) list
  in
  (*let _ = print_string "LIST_RESULT: ";
    print_list result; print_string "\n" in*)
  result
    

let print_dic_remanent parameter error dic =
  Stochastic_classes_type.Dictionary_of_Stochastic_classes.print
    parameter
    error
    (fun parameter error elt (l,a) _ _ ->
      let _ = Printf.printf "Stochastic_class_id:%i:" elt in
      let _ =
        print_string "site_type:{";
        (*print int list t*)
        let print_list_t = Int_storage.Nearly_inf_Imperatif.print
          error
          (fun error parameter l ->
            let _ = print_list l in
            error
          ) parameter l
        in
        print_list_t
        (*(*print union_find*)
        let array = Array.iter (fun i -> Printf.fprintf stdout "%i " i)
          a
        in
        array*)
      in
      let _ = print_string "}"; print_newline () in
      error
    ) dic.Stochastic_classes_type.dic

let store_new_class parameter error (pair: int list Int_storage.Nearly_inf_Imperatif.t * int array) remanent =
  let pair_dic = remanent.Stochastic_classes_type.dic in
  let pointer = remanent.Stochastic_classes_type.pointer in
  (*get allocate from a dictionary*)
  let error, output =
    Stochastic_classes_type.Dictionary_of_Stochastic_classes.allocate
      parameter
      error
      Misc_sa.compare_unit
      pair
      ()
      Misc_sa.const_unit
      pair_dic
  in
  let error, (id, dic) =
    match output with
      | Some (al, _, _, dic) -> error, (al, dic)
      | None -> warn parameter error (Some "line 137") Exit (0, pair_dic)
  in
  (*store pointer*)
  let pointer =
    store_pointer parameter error id pointer (fst pair)
  in
  error,
  {
    Stochastic_classes_type.dic = dic;
    Stochastic_classes_type.pointer = pointer
  }

let empty_remanent parameter error =
  let init_dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init() in
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let empty =
    {Stochastic_classes_type.dic = init_dic;
     Stochastic_classes_type.pointer = init_pointer}
  in empty

let union_dic parameter error classes =
  List.fold_left (fun (error, remanent_acc) l ->
    let size = List.length l in
    let a = Array.init size (fun i -> i) in
    match l with
      | [] | [_] -> error, remanent_acc
      | t :: q ->
        let rec aux to_visit =
          match to_visit with
            | [] -> error, remanent_acc
            | t' :: q' ->
              let union_array = union t t' a in
              let (fst_pair,a) = eq_classes_dic parameter error union_array in
              let _ = 
                print_string "FST_PAIR:";
                Int_storage.Nearly_inf_Imperatif.print
                  error (fun error parameter l ->
                    let _ = print_list l;
                      print_string "\n" in
                    error) parameter fst_pair
              in
              let error, result = store_new_class parameter error (fst_pair,a) remanent_acc in
              let _  = (*print_string "RESULT:\n";*)
                print_dic_remanent parameter error remanent_acc
              in
              result;
              aux q'
        in aux q
  )(error, empty_remanent parameter error) classes

let union_dic2 parameter error classes =
  List.fold_left (fun (error, acc) l ->
    let size = List.length l in
    let a = Array.init size (fun i -> i) in
    match l with
      | [] | [_] -> error, acc
      | t :: q ->
        let rec aux to_visit =
          match to_visit with
            | [] -> error, acc
            | t' :: q' ->
              let a = union t t' a in
              let (dic,a) = eq_classes_dic parameter error a in
              print_string "dic:";
              Int_storage.Nearly_inf_Imperatif.print
                error (fun error parameter l ->
                  let _ = print_list l;
                    print_string "\n" in
                  error) parameter dic;
              aux q' 
        in aux q
  ) (error, empty_remanent parameter error) classes
