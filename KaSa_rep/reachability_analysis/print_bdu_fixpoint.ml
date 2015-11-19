(**
  * print_bdu_fixpoint.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Bdu_analysis_type
open Print_bdu_build_map
open Remanent_parameters_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)

let print_bdu_update_map parameter error result =
  Map_bdu_update.Map.iter (fun (agent_type, cv_id) (l1, l2) ->
    if l1 <> []
    then ()
    else ();
    let _ =
      fprintf parameter.log "agent_type:%i:cv_id:%i\n" agent_type cv_id
    in
    List.iter (fun bdu_creation ->
      let _ =
        let _ = print_bdu parameter error bdu_creation in
        fprintf parameter.log "\n"
      in
      ()
    ) l2
  ) result

(*let print_bdu_update_map parameter error result =
  let _ = print_bdu parameter error result in
  fprintf parameter.log "\n"*)
    
(************************************************************************************)
(*main print*)

let print_bdu_fixpoint parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Fixpoint iteration :\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  let _ =
    print_bdu_update_map
      parameter
      error
      result.store_bdu_update_map    
  in
  (*let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- An array of bdu_test:\n";
    print_bdu_test_array
      parameter
      error
      result.store_bdu_test_array    
  in*)
  error
