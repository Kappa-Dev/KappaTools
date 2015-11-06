(**
  * print_bdu_build_map.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 6th of November
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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU print") message exn (fun () -> default)  

let trace = false

(************************************************************************************)

let print_remanent_test_map parameter error result =
  Map_test.Map.iter
    (fun (rule_id, cv_id, agent_type) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ = fprintf parameter.log
            "rule_id:%i:covering_class_id:%i@agent_type:%i\n"
            rule_id cv_id agent_type
          in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.log ", ");
              fprintf parameter.log "rule_id:%i" rule_id;
              true
            ) false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter (fun (list, set) ->
        (*print set*)
        Site_map_and_set.Set.iter (fun site ->
          fprintf parameter.log "rule_id:%i:covering_class_id:%i@agent_type:%i:site_type:%i\n"
            rule_id cv_id agent_type site
        ) set
      ) l2
    ) result

(************************************************************************************)
(*main print*)

let print_bdu_build_map parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Build BDU MAP:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- A map of covering classes with test rules:\n";
    print_remanent_test_map
      parameter
      error
      result.store_remanent_test_map
  in
  error
