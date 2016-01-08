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
open Site_map_and_set
open Covering_classes_new_index

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*sorted and ordered a covering classes in an descreasing order *)

let length_sorted (l: int list list): int list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists       = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(************************************************************************************)
(*port information for site (state)*)

let int_of_port port = port.site_state.min
    
(************************************************************************************)
(*common function for getting an id in pointer backward*)

let get_id_common_set parameter error t set =
  match Nearly_inf_Imperatif.unsafe_get
    parameter
    error 
    t 
    set
  with
    | error, None -> error, Set.empty
    | error, Some id -> error, id

(************************************************************************************)
(*CLEANING*)

(*------------------------------------------------------------------------------*)
(*store pointer backward *)

let store_pointer_backward parameter error id pointer_backward covering_class =
  List.fold_left (fun (error, pointer_backward) old_id ->
    (*getting an old id in the set*)
    let error, old_id_set =
      get_id_common_set 
        parameter 
        error
        old_id 
        pointer_backward
    in
    (*add the current set of elt into the old set*)
    let error',new_id_set = Set.add parameter error id old_id_set in
    let error = Exception.check warn parameter error error' (Some "line 71") Exit in 
    (*store the result into pointer backward*)
    Nearly_inf_Imperatif.set
      parameter
      error
      old_id
      new_id_set
      pointer_backward)
    (error, pointer_backward)
    covering_class
  
(*------------------------------------------------------------------------------*)
(*compute covering class dictionary*)

let covering_class_dic parameter error covering_class good_covering_class =
  let error, output =
    Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      covering_class
      ()
      Misc_sa.const_unit
      good_covering_class
  in
  (*return id and result as a dictionary type*)
  let error, (id_dic, store_dic) =
    match output with
      | Some (id, _, _, dic) -> error, (id, dic)
      | None -> warn parameter error (Some "line 197") Exit (0, good_covering_class)
  in
  error, (id_dic, store_dic)

(*------------------------------------------------------------------------------*)

let store_remanent parameter error covering_class modified_map remanent =
  (* current state of remanent*)
  let pointer_backward    = remanent.store_pointer_backward in
  let good_covering_class = remanent.store_dic in
  let good_index          = remanent.store_new_index_dic in
  let good_test_dic       = remanent.store_test_new_index_dic in
  let good_modif_dic      = remanent.store_modif_new_index_dic in
  (*------------------------------------------------------------------------------*)
  (*covering class dictionary*)
  let error, (covering_class_id, store_dic) =
    covering_class_dic 
      parameter 
      error 
      covering_class
      good_covering_class 
  in
  (*------------------------------------------------------------------------------*)
  (*store pointer backward*)
  let error, pointer_backward =
    store_pointer_backward 
      parameter
      error
      covering_class_id 
      pointer_backward
      covering_class 
  in
  (*------------------------------------------------------------------------------*)
  (*PART II: compute new_index in covering_class*)
  let new_index_covering_class = re_index_value_list covering_class in
  let error, (new_id, new_dic) =
    new_index_dic 
      parameter 
      error 
      new_index_covering_class 
      good_index 
  in
  (*------------------------------------------------------------------------------*)
  (*PART II: site test with new index*)
  let error, (new_test_id, test_new_index_dic) =
    test_new_index_dic 
      parameter 
      error
      new_id 
      new_dic 
      good_test_dic 
  in     
  (*------------------------------------------------------------------------------*)
  (*PART II: site modified with new_index*)
  let error, (new_modif_id, modif_index_dic) =
    modified_index_dic 
      parameter
      error 
      covering_class
      modified_map
      good_modif_dic 
  in
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    store_pointer_backward    = pointer_backward;
    store_dic                 = store_dic;
    store_new_index_dic       = new_dic;
    store_test_new_index_dic  = test_new_index_dic;
    store_modif_new_index_dic = modif_index_dic;
  }
    
(*------------------------------------------------------------------------------*)
(*CLEAN: In a covering class, it will store the old result of the previous
  covering class of an agent.
  
  For example:
  - rule 0: agent A has a covering class: (0)
  - rule 1: agent A has a covering class: (0,1)
  => Then do the intersection of two covering classes of agent A:
  (0) inter (0,1) -> 0
*)
    
    
let init_dic = Dictionary_of_Covering_class.init ()
  
let clean_classes parameter error covering_classes modified_map =
  let error, init_pointer = Nearly_inf_Imperatif.create parameter error 0 in
  let init_index          = init_dic in
  let init_store_dic      = init_dic in
  let init_test_index     = init_dic in
  let init_modif_index    = Dictionary_of_Modified_class.init() in
  (*------------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent = 
    {
      store_pointer_backward    = init_pointer;
      store_dic                 = init_store_dic;
      store_new_index_dic       = init_index;
      store_test_new_index_dic  = init_test_index;
      store_modif_new_index_dic = init_modif_index;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted covering_classes in
  let is_empty_set = Site_map_and_set.Set.is_empty in
  List.fold_left (fun (error, remanent) covering_class ->
    match covering_class with
      | [] -> 
	if current_covering_classes = [[]] 
        (* if the agent has only an empty covering class, keep it *)
	then 
	  store_remanent 
            parameter 
            error
            covering_class
            modified_map
            remanent
	else 
	  error,remanent
      | t :: tl ->
        let pointer_backward = remanent.store_pointer_backward in
        (* return the set of list(id) containing t.
           For example: current_covering_classes: [[0;1];[0]]
           t = 0 => (id:1;id:2) of type set;
           remanent_type: [(id:1,[0;1]);(id:2,[0])];
           (id:pointer_backward, dic: int list)
        *)
        let error, potential_supersets =
          get_id_common_set
            parameter
            error
            t
            pointer_backward 
        in
        let rec aux to_visit potential_supersets =
          match to_visit with
	    | [] -> error, remanent
	    | t' :: tl' ->
              (* get the set of list(id) containing t' *)
              let error, potential_supersets' =
		get_id_common_set
                  parameter 
                  error
                  t' 
                  pointer_backward 
              in
              (*-------------------------------------------------------------------*)
              (* intersection of two sets *)
              let error',potential_superset =
                Site_map_and_set.Set.inter parameter error 
                  potential_supersets
                  potential_supersets'
              in
	      let error = Exception.check warn parameter error error'
                (Some "line 71") Exit
              in 
              if is_empty_set potential_superset
              then
                let error, result_covering_dic =
                  store_remanent 
                    parameter 
                    error
                    covering_class
                    modified_map
                    remanent
                in
                error, result_covering_dic
              else
                aux tl' potential_superset
        in
        (*-------------------------------------------------------------------*)
        (*check the beginning state of a superset*)
        if is_empty_set potential_supersets
        then
          (*if it is empty then store it to remanent*)
          let error, result_covering_dic =
            store_remanent 
              parameter
              error
              covering_class
              modified_map
              remanent
          in
          error, result_covering_dic
        else
          aux tl potential_supersets
          )
    (error, init_remanent)
    current_covering_classes
