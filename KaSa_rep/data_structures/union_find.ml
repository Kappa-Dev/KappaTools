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

type union_find = int array

let create n = Array.init n (fun i -> i)

(************************************************************************************) 
(* findSet(e): which return a pointer to the representative of the set
   containing e. Since the set are disjoint, e containted in one set
   only. Therefore, the returned representative can be uniquely determined.
*)
      
let findSet e union_find =
  let pointToRoot root =
    List.iter (fun i -> union_find.(i) <- root) in
  let rec helper e l =
    let parent = union_find.(e) in
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

(************************************************************************************)   
(*UNION*)

let union x y a =
  let root_x = findSet x a in
  let root_y = findSet y a in
  let _ = a.(root_x) <- root_y in
  (*let _ = print_string "dump: "; dump a; print_string "\n" in*)
  a
              
let eq_classes_map parameter error a =
  let classes = Cckappa_sig.Site_map_and_set.empty_map in
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
        Cckappa_sig.Site_map_and_set.find_map_option
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
      let error, classes =
	Cckappa_sig.Site_map_and_set.add_map
	  parameter
	  error
          rep
	  (k :: get_rep)
	  classes
      in
      aux (k - 1) (classes, union_list)
  in
  let classes, a = aux (size - 1) (classes, a) in
  classes, a

(************************************************************************************)   
(* compute union-find in a list*)

let union_list parameter error a (list: int list) =
  match list with
  | [] -> error, a
  | t :: q ->
     let rec aux to_visit a =
       match to_visit with
       | [] -> error, a
       | t' :: q' ->
          let union_array = union t t' a in
          aux q' union_array
     in aux q a
