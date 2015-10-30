(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 26th of October
  * Last modification: 
  * 
  * Compute the relations between the left hand site of a rule and its sites.
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(*convert a list of covering class, new_index of covering class into a set*)

open Covering_classes_type
open Cckappa_sig
open Site_map_and_set

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*function mapping a list of covering class, return triple
  (list of new_index, map1, map2)
  For example:
  - intput : covering_class_list [0;1]
  - output : 
  {new_index_of_covering_class: [1;2],
  map1 (from 0->1, 1->2),
  map2 (from 1->0, 2->1)
  }
*)

let new_index_pair_map parameter error l =
  let rec aux acc k map1 map2 =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error, map1 =
        add_map parameter error h k map1
      in
      let error, map2 =
        add_map parameter error k h map2
      in
      aux tl (k+1) map1 map2
  in aux l 1 empty_map empty_map

(************************************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  List.fold_left (fun (error, current_set) elt ->
    let error, add_set =
      add_set parameter error elt current_set
    in
    error, add_set
  ) (error, empty_set) list

(************************************************************************************)

let collect_remanent_list2set parameter error store_remanent store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type remanent store_result ->
      (*-------------------------------------------------------------------------*)
      (*get covering classes dictionary*)
      let store_dic = remanent.store_dic in
      let error, (id_list, site_set_list) =
        Dictionary_of_Covering_class.fold
          (fun list _ index (error, (index_list, current_list)) ->
            let error, list2set =
              list2set parameter error list
            in
            let list_set = list2set :: current_list in
            let list_index = index :: index_list in
            error, (List.rev list_index, List.rev list_set)
          ) store_dic (error, ([], []))
      in
      (*-------------------------------------------------------------------------*)
      (*store a mapping function from a list of covering class into a list
      of new index and a pair of map*)
      let error, (id_list_map, list_of_map) =
        Dictionary_of_Covering_class.fold
          (fun list _ index (error, (index_list, current_list)) ->
            let error, store_map =
              new_index_pair_map parameter error list
            in
            let new_list = store_map :: current_list in
            let index_list = index :: index_list in
            error, (List.rev index_list, List.rev new_list)
          ) store_dic (error, ([], []))
      in
      (*-------------------------------------------------------------------------*)
      (*store*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          ((id_list, site_set_list),
           (id_list_map, list_of_map)
          )
          store_result
      in
      error, store_result
    ) store_remanent store_result

(************************************************************************************)   
(*PRINT*)

let print_set set =
  let _ =
    Site_map_and_set.iter_set (fun elt ->
      Printf.fprintf stdout "site_type:%i\n" elt
    ) set
  in
  ()

let print_pair_site_list l l' =
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | id :: tl, site :: tl' ->
      Printf.fprintf stdout 
        "Potential dependencies between sites:Covering_class_id:%i\n"
        id; print_set site;
      aux tl tl'
  in aux l l'

let print_map_functions l l' =
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> ()
    | id :: tl, h :: tl' ->
      Printf.fprintf stdout
        "Potential dependencies between sites:Map-functions:Covering_class_id:%i\n" id; 
      let (map1, map2) = h in
      let _ =
        Site_map_and_set.iter_map
          (fun site site_new ->
            Printf.fprintf stdout "Map1:site_type:%i:site_type':%i\n" site site_new
          ) map1
      in
      let _ =
        Site_map_and_set.iter_map
          (fun site_new site ->
            Printf.fprintf stdout "Map2:site_type':%i:site_type:%i\n" site_new site
          ) map2
      in
      ();
      aux tl tl'
  in
  aux l l'

let print_list2set parameter error result =
  AgentMap.print error (fun error parameter
    ((id_site_list, site_list),
     (id_list, list_of_map)
    ) ->
      let _ =
        let _ =
          print_pair_site_list id_site_list site_list
        in
        print_map_functions id_list list_of_map
      in
      error
  ) parameter result
