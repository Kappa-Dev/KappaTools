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
(*contact map*)

let print_contact_map_aux parameter error result =
  Int2Map_CM_state.Map.iter
    (fun (x, y, s) (l1, l2) ->
      if l1 <> []
      then
        begin 
          let _ = fprintf parameter.log 
            "agent_type:%i@site_type:%i:state:%i" x y s in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.log ", ");
              fprintf parameter.log "agent_type:%i" x;
              true)
            false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter
	(fun (z, t, s') ->
	  Printf.fprintf parameter.log
            "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n"
            x y s z t s'
	) l2
    ) result

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
  error
   
(************************************************************************************)
(*update function [before] adding rule with side effects*)

(*let print_covering_classes_modification_aux parameter error result =
  Int2Map_CV_Modif.Map.iter
    ( fun (x, y, z) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log
              "agent_type:%i:covering_class_id:%i" x z
          in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.log ", ");
              fprintf parameter.log "agent_type:%i" x;
              true
            ) false l1
          in
          fprintf stdout "\n"
        end
      else ();
      let _ =
        fprintf parameter.log
          "agent_type:%i:site_type:%i:covering_class_id:%i:@set of rule_id:\n" x y z
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          fprintf parameter.log "rule_id:%i\n" rule_id
        ) s2
    ) result*)

(*TODO*)
(*Site_map_and_set.Set.iter
  (fun r ->
  fprintf parameter.log
  "agent_type:%i@site_type:%i:covering_class_id:%i:rule_id:%i\n"
  x y z r
  ) l2
  ) result*)

(*let print_covering_classes_modification parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "List of rules to awake when the state of a site is modified and tested:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_covering_classes_modification_aux
    parameter
    error
    result*)

let print_covering_classes_modification_aux parameter error result =
  Int2Map_CV_Modif.Map.iter
    ( fun (x, y) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log
              "agent_type:%i:covering_class_id:%i" x y
          in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.log ", ");
              fprintf parameter.log "agent_type:%i" x;
              true
            ) false l1
          in
          fprintf stdout "\n"
        end
      else ();
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
(*update function [after] adding rule with side effect when discovered the binding*)

(*let print_update_set_aux parameter error result =
  Int2Map_CV_Modif.Map.iter
    ( fun (x, y, z) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log
              "agent_type:%i:covering_class_id:%i" x z
          in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.log ", ");
              fprintf parameter.log "agent_type:%i" x;
              true
            ) false l1
          in
          fprintf stdout "\n"
        end
      else ();
      let _ =
        fprintf parameter.log
          "agent_type:%i:site_type:%i:covering_class_id:%i:\n" x y z
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          fprintf parameter.log "rule_id:%i\n" rule_id
        ) s2
    ) result*)

(*TODO: do not use site_type, only agent_type, cv_id and a set of rule_id*)
(*Site_map_and_set.Set.iter
  (fun r ->
  fprintf parameter.log
  "agent_type:%i@site_type:%i:covering_class_id:%i:rule_id:%i\n"
  x y z r
  ) s2
  ) result*)

(*let print_binding_update_aux parameter error result =
  let result_hb,
    result_remove,
    result_hb_remove,
    result_update_aux
    = result
  in
  let _ =
    fprintf stdout "half_break side effect (will be removed):\n";
    print_update_set_aux
      parameter
      error
      result_hb
  in
  let _ =
    fprintf stdout "remove side effect (will be removed):\n";
    print_update_set_aux
      parameter
      error
      result_remove
  in
  let _ =
    fprintf stdout "half break and remove side effect (will be removed):\n";
    print_update_set_aux
      parameter 
      error
      result_hb_remove
  in
  fprintf stdout "New update function:\n";
  print_update_set_aux
    parameter
    error
    result_update_aux*)

(*let print_binding_update parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Some potential binding type has been discovered list of rules that may break this bond:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_binding_update_aux
    parameter
    error
    result*)
  
(************************************************************************************)
(*working list*)

let print_wl_creation parameter result =
  IntWL.print_wl parameter result
    
(*let print_wl_creation parameter error result =
  AgentMap.print error
    (fun error parameter wl ->
      let _ =
	fprintf stdout "-List of rule_id in working list of creation:\n";
	IntWL.print_wl parameter wl
      in
      error
    ) parameter result

let print_wl_update parameter error result =
  AgentMap.print error
    (fun error parameter wl ->
      let _ =
        fprintf (Remanent_parameters.get_log parameter)
          "- List of rule_id in working list of update function:\n";
        IntWL.print_wl parameter wl
      in
      error
    ) parameter result

let print_wl_creation_update parameter error result =
  AgentMap.print error
    (fun error parameter wl ->
      let _ =
	fprintf stdout "-List of rule_id in working list of creation_update:\n";
	IntWL.print_wl parameter wl
      in
      error
    ) parameter result*)

(************************************************************************************)
(*main print*)

let print_result_dynamic parameter error result =
  fprintf (Remanent_parameters.get_log parameter) 
    "\n** Dynamic information:\n";
  let _ =
    print_contact_map
      parameter
      error 
      result.store_contact_map
  in
  (*let _ =
    print_covering_classes_modification
      parameter
      error
      result.store_covering_classes_modification_update
  in*)
  (*TEST*)
  let _ =
    print_covering_classes_modification
      parameter
      error
      result.store_covering_classes_modification_update
  in
  (*let _ =
    print_binding_update 
      parameter
      error
      result.store_update
  in*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Rules in the working list:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Working list creation:\n";
    print_wl_creation
      parameter
      result.store_wl_creation
  in
  (*let _ =
    let _ =
      fprintf (Remanent_parameters.get_log parameter)
        "- List of rules to wake up when the potential site of a covering class change:\n";
      print_wl_update
        parameter
        error 
        result.store_wl_update 
    in*)
    (*let _ =
      fprintf (Remanent_parameters.get_log parameter)
        "- Working list creation (will be removed):\n";
      print_wl_creation
        parameter
        error
        result.store_wl_creation 
    in*)
    (*fprintf (Remanent_parameters.get_log parameter)
      "- Working list update and creation:\n";
    print_wl_creation_update
      parameter
      error
      result.store_wl_creation_update
  in*)
  error
