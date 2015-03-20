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
                      
(*
let list_of_union {treeArr} =
  let rec list_of_union i res =
    if i < 0
    then res
    else
      list_of_union (i - 1) (Array.unsafe_get treeArr i :: res)
  in
  list_of_union (Array.length treeArr - 1) []

let union_of_list l =
  {treeArr = Array.of_list l}*)

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
  (*| [_] -> []*)
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
                 
let dic_of_union parameter error l =
   let init_dic =
    Stochastic_classes_type.Dictionary_of_Stochastic_classes.init ()
  in
  let init_remanent =
    { Stochastic_classes_type.dic = init_dic}
  in
  let error, output =
    Stochastic_classes_type.Dictionary_of_Stochastic_classes.allocate
      parameter
      error
      Misc_sa.compare_unit
      l
      ()
      Misc_sa.const_unit
      init_dic
  in
  let error, dic =
    match output with
    | None -> warn parameter error (Some "line 92") Exit
                   init_dic
    | Some (_, _, _, dic) -> error, dic  
  in
  {Stochastic_classes_type.dic = dic}

let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr
