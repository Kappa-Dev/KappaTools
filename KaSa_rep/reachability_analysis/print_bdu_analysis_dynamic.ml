(**
  * print_bdu_analysis.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Remanent_parameters_sig
open Cckappa_sig
open Bdu_analysis_type
open Fifo

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

(************************************************************************************)
(*syntactic contact map*)

(*let print_contact_map_aux parameter error result =
  let b, result1 = result in
  fprintf stdout "A bond is discovered for the first time:%b\n" b;
  Int2Map_syn.Map.iter (fun rule_id set ->
    Set_pair.Set.iter (fun ((agent1, site1, state1), (agent2, site2,state2)) ->
      fprintf stdout "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n" agent1 site1 state1 agent2 site2 state2
    ) set
  ) result1

let print_contact_map parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "(Syntactic) Contact map (only considering rhs) and initital state:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Sites are annotated with the id of binding type:\n";
  let error =
    print_contact_map_aux
      parameter
      error
      result
  in
  error*)

(*TODO*)

let print_contact_map_aux parameter error result =
  Int2Map_CM_Syntactic.Map.iter (fun set1 set2 ->
    Set_triple.Set.iter (fun (agent1, site1, state1) ->
      Set_triple.Set.iter (fun (agent2, site2, state2) ->
        fprintf stdout "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n" agent1 site1 state1 agent2 site2 state2
      ) set2
    )set1
  ) result    

let print_contact_map parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "(Syntactic) Contact map and initital state:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Sites are annotated with the id of binding type:\n";
  let error =
    print_contact_map_aux
      parameter
      error
      result
  in
  error

(************************************************************************************)
(*contact map full information*)

let print_contact_map_full_aux parameter error result =
  Int2Map_CM_state.Map.iter (fun (agent1, site1, state1) set ->
    Set_triple.Set.iter (fun (agent2, site2, state2) ->
      fprintf stdout "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n" agent1 site1 state1 agent2 site2 state2
    ) set
  ) result

let print_contact_map_full parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "(Full) Contact map and initital state:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Sites are annotated with the id of binding type:\n";
  let error =
    print_contact_map_full_aux
      parameter
      error
      result
  in
  error

(************************************************************************************)
(*update (c) function*)

let print_covering_classes_modification_aux parameter error result =
  Int2Map_CV_Modif.Map.iter
    ( fun (x, y) (_, s2) ->
      let _ =
        fprintf parameter.log
          "agent_type:%i:covering_class_id:%i:@set of rule_id:\n" x y
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          fprintf parameter.log "rule_id:%i\n" rule_id
        ) s2
    ) result

let print_covering_classes_modification parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "List of rules to awake when the state of a site is modified and tested:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_covering_classes_modification_aux
    parameter
    error
    result
    
(************************************************************************************)
(*update(c'), when discovered a bond for the first time*)

let print_covering_classes_side_effects parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "List of rules to awake when the state of a site is modified and tested and side effects:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_covering_classes_modification_aux
    parameter
    error
    result

(************************************************************************************)
(*Final update function*)

let print_covering_classes_update_full parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Final list of rules to awake when the state of a site is modified and tested and side effects:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_covering_classes_modification_aux
    parameter
    error
    result

(************************************************************************************)
(*main print*)

let print_result_dynamic parameter error result =
  fprintf (Remanent_parameters.get_log parameter) 
    "\n** Dynamic information:\n";
  (*------------------------------------------------------------------------------*)
  let _ =
    print_contact_map_full
      parameter
      error 
      result.store_contact_map_full
  in
  (*------------------------------------------------------------------------------*)
  let _ =
    print_contact_map
      parameter
      error
      result.store_contact_map
  in
  (*------------------------------------------------------------------------------*)
  let _ =
    print_covering_classes_modification
      parameter
      error
      result.store_covering_classes_modification_update
  in
  (*------------------------------------------------------------------------------*)
  let _ =
    print_covering_classes_side_effects
      parameter
      error
      result.store_covering_classes_modification_side_effects
  in
  (*------------------------------------------------------------------------------*)
  let _ =
    print_covering_classes_update_full
      parameter
      error
      result.store_covering_classes_modification_update_full
  in
  error
