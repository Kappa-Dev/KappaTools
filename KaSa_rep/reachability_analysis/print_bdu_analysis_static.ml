(**
  * print_bdu_analysic_static.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 15th of July
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Bdu_analysis_type
open Remanent_parameters_sig
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

(************************************************************************************)
(*static information of covering classes id*)

let print_covering_classes_id_aux parameter error result =
  Int2Map_CV.Map.iter
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      List.iter
        (fun id ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:covering_class_id:%i\n"
            x y id
        ) l2
    ) result

let print_covering_classes_id parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Covering classes:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_covering_classes_id_aux
      parameter
      error
      result
  in
  error

(************************************************************************************)
(*side effects*)

let print_half_break_effect parameter error result =
  Int2Map_HalfBreak_effect.Map.iter
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      List.iter
        (fun (r, s) ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:(rule_id:%i * state:%i)\n"
            x y r s
        ) l2
    ) result

let print_remove_effect parameter error result =
  Int2Map_Remove_effect.Map.iter
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      List.iter
        (fun r ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:(rule_id:%i * state:no_information)\n"
            x y r
        ) l2
    ) result

let print_side_effects_aux parameter error result =
  let result_half_break, result_remove = result in
  let _ =
    print_half_break_effect
      parameter
      error
      result_half_break
  in
  print_remove_effect
    parameter
    error 
    result_remove

let print_side_effects parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter) 
    "Side effects action:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_side_effects_aux
      parameter
      error 
      result
  in
  error

(************************************************************************************)
(*modification sites*)

let print_modification_sites_aux parameter error result =
  Int2Map_Modif.Map.iter
    ( fun (x, y) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      Site_map_and_set.Set.iter
        (fun r ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:rule_id:%i\n"
            x y r
        ) s2
     ) result

let print_modification_sites parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Modification sites (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux
      parameter
      error
      result
  in
  error

(************************************************************************************)
(*test sites*)

let print_test_sites parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Test sites (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux 
      parameter
      error
      result
  in
  error  

(************************************************************************************)
(*test and modification*)

let print_test_modification_sites parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Test and modification sites (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux
      parameter
      error
      result
  in
  error

(************************************************************************************)
(*creation*)

let print_creation_sites parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Creation sites (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux
      parameter
      error 
      result
  in
  error

(************************************************************************************)
(*test/modification without creation*)

let print_modification_sites_without_creation parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Modification sites without creation:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux 
      parameter
      error 
      result
  in
  error

let print_test_modification_without_creation parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Test and modification sites without creation (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux 
      parameter
      error
      result;
  in
  error

(************************************************************************************)
(*main print*)

let print_result_static parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "============================================================\n";
    fprintf (Remanent_parameters.get_log parameter) "* BDU Analysis:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "============================================================\n";
    fprintf (Remanent_parameters.get_log parameter)
      "\n** Static information:\n";
  in
  let _ =
    print_covering_classes_id
      parameter 
      error
      result.store_covering_classes_id
  in
  let _ =
    print_side_effects 
      parameter 
      error 
      result.store_side_effects
  in
  let _ =
    print_modification_sites 
      parameter
      error 
      result.store_modification_sites
  in
  let _ =
    print_test_sites
      parameter
      error 
      result.store_test_sites
  in
  let _ =
    print_test_modification_sites
      parameter
      error 
      result.store_test_modification_sites
  in
  let _ =
    print_creation_sites
      parameter
      error
      result.store_creation_sites
  in
  let _ =
    print_modification_sites_without_creation
      parameter
      error
      result.store_modification_sites_without_creation
  in
  let _ = 
    print_test_modification_without_creation
      parameter
      error
      result.store_test_modification_without_creation
  in
  error
