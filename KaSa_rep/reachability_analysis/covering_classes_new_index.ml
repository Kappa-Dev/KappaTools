(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Compute the relations between the left hand site of a rule and its sites.
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Covering_classes_type
open Cckappa_sig
open Int_storage
open Printf
open Site_map_and_set

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(*------------------------------------------------------------------------------*)
(*PART II:
  Re-index the sites in each rule, each agent type and each covering class *)

let position x = (*TODO: change to use array instead*)
  let rec aux k = function
    | [] -> raise Not_found
    | y :: ys ->
      if x = y
      then k
      else aux (k+1) ys
  in aux 0;;

let is_empty_list l =
  match l with
    | [] -> true
    | _ -> false

let re_index_value_list list =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: tl ->
        let nth = position x list in
        (nth + 1) :: aux tl
  in aux list

(*------------------------------------------------------------------------------*)
(*projection site that are modified with new index*)

let project_modified_site value_list modified_map = (*TODO:add state information*)
  let rec aux acc =
    match acc with
      | [] -> []
      | x :: tl ->
        if not (is_empty_map modified_map)
        then
          if mem_map x modified_map
          then
            begin
              if not (is_empty_list value_list)
              then
                let nth_1 = (position x value_list) + 1 in
                let l = nth_1 :: aux tl in
                l
              else []
            end
          else aux tl
        else [] (*if modified_set is empty then nothing*)
  in aux value_list
    
(*------------------------------------------------------------------------------*)
(*compute new index dictionary*)

let new_index_dic parameter error new_index_covering_class good_index =
  let error, output =
    Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      new_index_covering_class
      ()
      Misc_sa.const_unit
      good_index
  in
  let error, (id_dic, store_dic) =
    match output with
    | Some (id, _, _, dic) -> error, (id, dic)
    | None -> warn parameter error (Some "95") Exit (0, good_index)
  in
  error, (id_dic, store_dic)

(*------------------------------------------------------------------------------*)
(*compute tested dictionary with new index *)

let test_new_index_dic parameter error new_id new_dic good_test_dic =
  let error, (value_index_dic, _, _) =
    Misc_sa.unsome
      (Dictionary_of_Covering_class.translate
         parameter
         error
         new_id
         new_dic)
      (fun error -> warn parameter error (Some "line 236") Exit ([], (),()))
  in
  (*return site_test_dic*)
  let error, output =
    Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      value_index_dic
      ()
      Misc_sa.const_unit
      good_test_dic
  in
  let error, (id_dic, store_dic) =
    match output with
    | Some (id, _, _, dic) -> error, (id, dic)
    | None -> warn parameter error (Some "131") Exit (0, good_test_dic)
  in
  error, (id_dic, store_dic)

(*------------------------------------------------------------------------------*)
(*compute modified dictionary with new_index*)

let modified_index_dic parameter error covering_class modified_map 
    good_new_index_modif_dic =
  let modified_value = project_modified_site covering_class modified_map in
  let error, out_modif_dic =
    Dictionary_of_Modified_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      modified_value
      ()
      Misc_sa.const_unit
      good_new_index_modif_dic
  in
  let error, (new_modif_id, modif_index_dic) =
    match out_modif_dic with
      | None -> warn parameter error (Some "line 252") Exit (0, good_new_index_modif_dic)
      | Some (id, _, _, dic) -> error, (id, dic)        
  in
  error, (new_modif_id, modif_index_dic)

(************************************************************************************)   
(*PART II. COMPUTE properties related to covering classes*)

(*------------------------------------------------------------------------------*)
(*return the number of covering classes for each agent type*)

let number_of_covering_classes parameter error store_dic =
  let error, num =
    Dictionary_of_Covering_class.last_entry
      parameter error store_dic
  in num + 1
