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
open SetMap
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig
open Site_map_and_set
open Covering_classes_type
open Bdu_build_common
open Bdu_structure

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
(*store the product of [|bdu_creation * bdu_test * modif_list|], rule_id is
  an index, nrules is the length of this array. *)

let collect_bdu_creation_array parameter error handler_sig store_creation_bdu =
  let error, init = AgentMap.create parameter error 0 in
  let error, (handler, bdu_init) = bdu_init parameter error in
  (*create an empty array*)
  let nrules = Handler.nrules parameter error handler_sig in
  let store_array = Array.make nrules bdu_init in
  (*get rule_id in creation_map*)
  let error, store_result =
   AgentMap.fold parameter error
     (fun parameter error agent_type l store_result ->
         let error, result_array =
           List.fold_left (fun (error, current_array) (rule_id, bdu_creation) ->
             current_array.(rule_id) <- bdu_creation;
             error, current_array
           ) (error, store_array) l
         in
         (*--------------------------------------------------------------------------*)
         (*store*)
         let error, old_array =
           match AgentMap.unsafe_get parameter error agent_type store_result with
           | error, None -> error, [||]
           | error, Some a -> error, a
         in
         let new_array = Array.append result_array old_array in
         (*--------------------------------------------------------------------------*)
         (*store*)
         let error, store_result =
           AgentMap.set
             parameter
             error
             agent_type
             new_array
             store_result
         in
         error, store_result
     ) store_creation_bdu init
  in
  error, store_result

(*build creation rule from a map of bdu_creation REMOVE*)

(*let collect_bdu_creation_array_map parameter error handler_sig store_creation_bdu_map
    store_result =
  let error, (handler, bdu_init) = bdu_init parameter error in
  (*create an empty array*)
  let nrules = Handler.nrules parameter error handler_sig in
  let store_array = Array.make nrules bdu_init in
  (*get rule_id in creation_map*)
  let error, store_result =
    Map_creation_bdu.Map.fold (fun (agent_type, rule_id)(l1, l2) (error, _) ->
      List.fold_left (fun (error, current_array) (handler, bdu_creation) ->
        current_array.(rule_id) <- bdu_creation;
        error, current_array
      ) (error, store_array) l2
    ) store_creation_bdu_map (error, [||])
  in
  error, store_result*)

(************************************************************************************)
(*store bdu_test in an array, index of this array is rule_id of test
  rules. The lenght of this array is the number of rules.*)

let collect_bdu_test_array parameter error handler_sig store_test_bdu =
  let error, init = AgentMap.create parameter error 0 in
  let error, (handler, bdu_init) = bdu_init parameter error in
  (*create an empty array*)
  let nrules = Handler.nrules parameter error handler_sig in
  let store_array = Array.make nrules bdu_init in
  (*get rule_id in creation_map*)
  let error, store_result =
   AgentMap.fold parameter error
     (fun parameter error agent_type l store_result ->
         let error, result_array =
           List.fold_left (fun (error, current_array) (rule_id, bdu_test) ->
             current_array.(rule_id) <- bdu_test;
             error, current_array
           ) (error, store_array) l
         in
         (*--------------------------------------------------------------------------*)
        (*store*)
         let error, old_array =
           match AgentMap.unsafe_get parameter error agent_type store_result with
           | error, None -> error, [||]
           | error, Some a -> error, a
         in
         let new_array = Array.append result_array old_array in
         (*--------------------------------------------------------------------------*)
         let error, store_result =
           AgentMap.set
             parameter
             error
             agent_type
             new_array
             store_result
         in
         error, store_result
     ) store_test_bdu init
  in
  error, store_result

(*build test rule from a map of bdu_test REMOVE*)

(*let collect_bdu_test_array_map parameter error handler_sig store_test_bdu_map
   store_result =
  let error, init = AgentMap.create parameter error 0 in
  let error, (handler, bdu_init) = bdu_init parameter error in
  (*create an empty array*)
  let nrules = Handler.nrules parameter error handler_sig in
  let store_array = Array.make nrules bdu_init in
  (*get rule_id in creation_map*)
  let error, store_result =
    Map_test_bdu.Map.fold (fun (agent_type, rule_id)(l1, l2) (error, store_result) ->
      (*FIXME: check the agent_type*)
      let error, result_array =
        List.fold_left (fun (error, current_array) (handler, bdu_test) ->
          store_array.(rule_id) <- bdu_test;
          error, store_array
        ) (error, [||]) l2
      in
      (*--------------------------------------------------------------------------*)
      let error, old_array =
        match AgentMap.unsafe_get parameter error agent_type store_result with
        | error, None -> error, [||]
        | error, Some a -> error, a
      in
      let new_array = Array.append result_array old_array in
      (*--------------------------------------------------------------------------*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          new_array
          store_result
      in
      error, store_result
    ) store_test_bdu_map (error, init)
  in
  error, store_result*)

(************************************************************************************)
(*combine creation and test REMOVE*)

(*let collect_bdu_creation_test_array parameter error handler_sig store_creation_bdu
    store_test_bdu store_result =
  let error, (handler, bdu_init) = bdu_init parameter error in
  (*create an empty array*)
  let nrules = Handler.nrules parameter error handler_sig in
  let store_array = Array.make nrules bdu_init in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type l1 l2 store_result ->
      let result_array =
        List.fold_left (fun _ (rule_id_creation, bdu_creation) ->
          List.fold_left (fun _ (rule_id_test, bdu_test) ->
            store_array.(rule_id_test) <- bdu_test;
            store_array.(rule_id_creation) <- bdu_creation;
            store_array
          ) [||] l2
        ) [||] l1
      in
      (*--------------------------------------------------------------------------*)
      (*store*)
      let error, old_array =
        match AgentMap.unsafe_get parameter error agent_type store_result with
        | error, None -> error, [||]
        | error, Some a -> error, a
      in
      let new_array = Array.append result_array old_array in
      (*--------------------------------------------------------------------------*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          new_array
          store_result
      in
      error, store_result
    ) store_creation_bdu store_test_bdu store_result*)

(************************************************************************************)
