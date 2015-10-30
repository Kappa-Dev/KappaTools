(**
  * bdu_fixpoint_iteration.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 9th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Bdu_analysis_type
open Set_and_map
open Bdu_build
open Bdu_build
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig

(************************************************************************************)
(*fixpoint iteration function*)
(*from an array of rule, build bdu:{of creation rule; test; list of modification}.
  the idea of iterate function: 
  - if working list is empty then return the bdu_remanent_array;
  - pop the first element inside the working list;
  - get 'rule' type from the rule_array;
  - build bdu_creation; bdu_test, and a list of action;
  - build a list of type List_sig.list for modif_list (list of action);
  - check if it  is an enable rule or not:
    it is an enable rule if the result of intersection of test and init is not empty;
    + if it is an enable rule return the result.
    + if it is not an enable rule: call iterate update function:
      ++ bdu_creation intersection (mvbdu_and) with bdu_test, 
         then use the result above: redefine (redefine function in mvbdu) it
         with the list of modif;
      ++ do the union (mvbdu_or) of bdu_assignment (the intersection above) and bdu_creation;
      ++ store this bdu_update in bdu_array, with the index is rule_id.
*)

(************************************************************************************)
(*is enable rule*)

let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then true
  else false
    
let comp_is_enable parameter error handler bdu_init bdu_test bdu_creation =
  let error, handler, bdu_X =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  if not (is_belong bdu_X bdu_init)
  then
    error, true
  else
    error, false

(************************************************************************************)
(*update bdu*)

let compute_update parameter error rule_id handler bdu_test modif_list bdu_creation
    bdu_remanent_array =
  let error, handler, bdu_inter_test_creation =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  let error, handler, bdu_assigment =
    f parameter error bdu_inter_test_creation
      (redefine parameter error parameter handler bdu_inter_test_creation)
      modif_list
  in
  let error, handler, bdu_update =
    f parameter error bdu_assigment
      (boolean_mvbdu_or parameter handler error parameter bdu_assigment)
      bdu_creation
  in
  let bdu_remanent_array =
    bdu_remanent_array.(rule_id) <- bdu_update;
    bdu_remanent_array
  in
  error, bdu_remanent_array
    
(************************************************************************************)
(* From each covering class, with their new index for sites, build 
   (bdu_test, bdu_creation and list of modification).
   Note: not taking sites in the local, because it will be longer.
   - Convert type set of sites into map
*)

(*let compute_a_map_restriction' parameter error set agent map =
  let error, map_restriction =
    Site_map_and_set.fold_map_restriction parameter error
      (fun site port (error, store_result) ->
        let state = port.site_state.min in
        (*find site' with new_index inside map*)
        let error, site' =
          try Site_map_and_set.find_map parameter error site map
          with Not_found -> error, 0
        in
        (*add into a restriction map with state of a new site'*)
        let error, new_pair_map =
          Site_map_and_set.add_map parameter error site' state store_result
        in
        error, new_pair_map
      ) set agent.agent_interface Site_map_and_set.empty_map
  in
  error, map_restriction*)

(*fold it in a list of id*)
let a_map_restriction parameter error id set agent map =
  let error, map_restriction =
    Site_map_and_set.fold_map_restriction parameter error
      (fun site port (error, store_result) ->
        let state = port.site_state.min in
        (*find site' with new_index inside map*)
        let error, site' =
          try Site_map_and_set.find_map parameter error site map
          with Not_found -> error, 0
        in
        (*add into a restriction map with state of a new site'*)
        let error, new_pair_map =
          Site_map_and_set.add_map parameter error site' state store_result
        in
        error, new_pair_map
      ) set agent.agent_interface Site_map_and_set.empty_map
  in
  error, (id, map_restriction)

let a_list_of_map_restriction parameter error id list_set agent list_map =
  (*check inside each covering class id of a list of map*)
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | set :: tl, (map, _) :: tl' ->
      (*compute a map restriction*)
      let error, (id, map_restriction) =
        a_map_restriction
          parameter
          error
          id
          set 
          agent
          map
      in
      (id, map_restriction) :: aux tl tl'
  in
  aux list_set list_map

let compute_map_restriction parameter error id_list list_set agent list_map =
  let rec aux acc =
    match acc with
    | [] -> []
    | id :: tl ->
      let list =
        a_list_of_map_restriction
          parameter
          error
          id
          list_set
          agent
          list_map
      in
      List.concat [list; aux tl]
  in
  aux id_list

(*let compute_a_list_of_map_restriction' parameter error list_set agent list_map =
  (*check inside each covering class id of a list of map*)
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | set :: tl, (map, _) :: tl' ->
      (*compute a map restriction*)
      let error, map_restriction =
        compute_a_map_restriction
          parameter
          error
          set 
          agent
          map
      in
      map_restriction :: aux' tl tl'
  in
  aux list_set list_map*)

(*TODO*)
let collect_restriction_bdu_test parameter error rule covering_class_set store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      match agent with
      | Ghost -> error, store_result
      | Agent agent ->
        let agent_type = agent.agent_name in
        let error, store_result =
          AgentMap.fold parameter error
            (fun parameter error agent_type_cv set store_result ->
              let (id_set_list, list_set), (id_list, list_map) = set in
              let map_restriction_list =
                compute_map_restriction
                  parameter
                  error
                  id_list
                  list_set
                  agent
                  list_map
              in
              (*get old*)
              let error, old_list =
                match AgentMap.unsafe_get parameter error agent_type store_result with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let new_list = List.concat [map_restriction_list; old_list] in
              (*store*)
              let error, store_result =
                AgentMap.set
                  parameter
                  error
                  agent_type
                  (List.rev new_list)
                  store_result
              in
              error, store_result
            ) covering_class_set store_result
        in
        error, store_result
    ) rule.rule_lhs.views store_result
