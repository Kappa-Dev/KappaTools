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

let print_covering_classes_modification_aux parameter error result =
  Int2Map_CV_Modif.Map.iter
    ( fun (x, y) (l1, s2) ->
      if l1 <> []
      then ()
        (*begin
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
        end*)
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
(*main print*)

let print_result_dynamic parameter error result =
  fprintf (Remanent_parameters.get_log parameter) 
    "\n** Dynamic information:\n";
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
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Rules in the working list:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  error
