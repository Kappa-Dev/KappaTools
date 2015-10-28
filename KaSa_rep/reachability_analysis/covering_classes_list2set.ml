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
      (*TODO*)
      let error, site_set =
        Dictionary_of_Covering_class.fold
          (fun list _ index (error, (_, set)) ->
            let error, list2set =
              list2set parameter error list
            in
            let error, new_set =
              union parameter error list2set set                
            in
            error, (index, new_set)
          ) store_dic (error, (0, empty_set))
      in
      (*-------------------------------------------------------------------------*)
      (*get covering classes dictionary with new index*)
      let store_new_index_dic = remanent.store_new_index_dic in
      let error, site_new_index_set =
        Dictionary_of_Covering_class.fold
          (fun list _ index _ ->
            let error, list2set =
              list2set parameter error list
            in
            error, (index, list2set)
          ) store_new_index_dic (error, (0, empty_set))
      in
      (*-------------------------------------------------------------------------*)
      (*get test rule in covering classes dictionary with new index*)
      let store_test_new_index_dic = remanent.store_test_new_index_dic in
      let error, site_test_new_index_set =
        Dictionary_of_Covering_class.fold
          (fun list _ index _ ->
            let error, list2set =
              list2set parameter error list
            in
            error, (index, list2set)
          ) store_test_new_index_dic (error, (0, empty_set))
      in
      (*-------------------------------------------------------------------------*)
      (*get modification rule in covering classes dictionary with new index*)
      let store_modif_new_index_dic = remanent.store_modif_new_index_dic in
      let error, site_modif_new_index_set =
        Dictionary_of_Modified_class.fold
          (fun list _ index _ ->
            let error, list2set =
              list2set parameter error list
            in
            error, (index, list2set)
          ) store_modif_new_index_dic (error, (0, empty_set))
      in
      (*-------------------------------------------------------------------------*)
      (*store a mapping function from a list of covering class into a list
      of new index and a pair of map*)
      let error, (id, covering_class_list) =
        Dictionary_of_Covering_class.fold
          (fun list _ index _ ->
            error, (index, list)
          ) store_dic (error, (0, []))
      in
      let error, store_new_index_pair_map =
        new_index_pair_map parameter error covering_class_list
      in
      (*-------------------------------------------------------------------------*)
      (*store*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          (site_set,
           site_new_index_set,
           site_test_new_index_set,
           site_modif_new_index_set,
           (id, store_new_index_pair_map)
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

let print_list2set parameter error result =
  AgentMap.print error (fun error parameter
    ((id_site, site_set),
     (id_new_index, site_new_index_set),
     (id_test, site_test_new_index_set),
     (id_modif, site_modif_new_index_set),
     (id, store_new_index_pair_map)
    ) ->
    let _ =
      let _ =
        Printf.fprintf stdout "Potential dependencies between sites:Covering_class_id:%i\n"
        id_site; print_set site_set
      in
      let _ =
        Printf.fprintf stdout
          "Potential dependencies between sites:New-index:Covering_class_id:%i\n"
        id_new_index;
        print_set site_new_index_set
      in
      let _ =
        Printf.fprintf stdout
          "Potential dependencies between sites:TEST:New-index:Covering_class_id:%i\n"
          id_test;
        print_set site_test_new_index_set
      in
      let _ =
        Printf.fprintf stdout
          "Potential dependencies between sites:MODIFICATION-:New-index:Covering_class_id:%i\n"
          id_modif;
        print_set site_modif_new_index_set
      in
      Printf.fprintf stdout "Mapping functions from covering class to new_index of its:\n";
      let (map1, map2) = store_new_index_pair_map in
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
      ()
    in
    error
  ) parameter result
