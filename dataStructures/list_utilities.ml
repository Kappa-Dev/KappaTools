
(**
   * list_utilities.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * 
   * Creation:                      <2015-11-20 09:21:53 feret>
   * Last modification: Time-stamp: <2015-11-20 09:30:32 feret>
   * * 
   * This library provides primitives to deal with storage functions
   *  
   * Copyright 2010,2011,2012,2013,2014,2015 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   *  under the terms of the GNU Library General Public License *)

let aux p list = 
  let rec aux list buffer output = 
    match 
      list
    with 
    | h::t when p h -> aux t [] ((h::buffer)::output)
    | h::t -> aux t (h::buffer) output
    | [] -> output 
  in 
  let rev_concat list = 
    List.fold_left 
      (	List.fold_left 
	  (fun output a -> a::output))
	  [] list
  in 
  rev_concat (aux list [] [])

