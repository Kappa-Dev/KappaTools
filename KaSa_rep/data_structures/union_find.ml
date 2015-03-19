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

let findSet e union_find =
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
        (* we hit the root node make all collected nodes on the
           path point to the root. and return the root afterwards *)
        pointToRoot parent l;
        parent;
      end
  in
  helper e []

let union x y union_find =
  let root_x = findSet x union_find in
  let root_y = findSet y union_find in
  let treeArr = union_find.treeArr in
  treeArr.(root_y) <- root_x;
  union_find

let union_list l union_find =
  match l with
    | [] -> union_find
    | h :: tl ->
       List.fold_left (fun union_c t -> 
                       union h t union_c) union_find tl
                                
let list_of_union {treeArr} =
  let rec list_of_union i res =
    if i < 0
    then res
    else
      list_of_union (i - 1) (Array.unsafe_get treeArr i :: res)
  in
  list_of_union (Array.length treeArr - 1) []

let union_of_list l =
  {treeArr = Array.of_list l}

let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr
