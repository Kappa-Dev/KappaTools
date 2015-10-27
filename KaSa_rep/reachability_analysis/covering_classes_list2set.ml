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

let list2set parameter error list =
  List.fold_left (fun (error, current_set) elt ->
    let error, add_set =
      add_set parameter error elt current_set
    in
    error, add_set
  ) (error, empty_set) list

let collect_remanent_list2set parameter error store_remanent store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type remanent store_result ->
      (*-------------------------------------------------------------------------*)
      (*get covering classes dictionary*)
      let store_dic = remanent.store_dic in
      let error, site_set =
        Dictionary_of_Covering_class.fold
          (fun list _ index _ ->
            let error, list2set =
              list2set parameter error list
            in
            error, (index, list2set)
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
      (*store*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          (site_set,
           site_new_index_set,
           site_test_new_index_set,
           site_modif_new_index_set)
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
     (id_modif, site_modif_new_index_set)
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
      Printf.fprintf stdout
        "Potential dependencies between sites:MODIFICATION-:New-index:Covering_class_id:%i\n"
      id_modif;
      print_set site_modif_new_index_set
    in
    error
  ) parameter result
