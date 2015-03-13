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

let makeSets n =
  {
    treeArr = Array.init n (fun i -> i)
  }

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
        pointToRoot parent l;
        parent
      end
  in
  helper e []

let union x y union_find =
  let root_x = findSet x union_find in
  let root_y = findSet y union_find in
  let treeArr = union_find.treeArr in
  treeArr.(root_x) <- root_y

let is_equivalence x y union_find =
  (findSet x union_find) = (findSet y union_find)
