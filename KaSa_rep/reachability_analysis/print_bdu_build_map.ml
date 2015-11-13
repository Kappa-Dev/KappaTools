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
    (fun (agent_type, rule_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      List.iter (fun (id, site, state) ->
        fprintf parameter.log 
          "agent_type:%i:rule_id:%i@covering_class_id:%i:site_type':%i:state:%i\n"
          agent_type rule_id id site state
      ) l2
    ) result

(************************************************************************************)

let print_remanent_creation_map parameter error result =
  Map_creation.Map.iter
    (fun (agent_type, rule_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      List.iter (fun (id, site, state) ->
        fprintf parameter.log 
          "agent_type:%i:rule_id:%i@covering_class_id:%i:site_type':%i:state:%i\n"
          agent_type rule_id id site state
      ) l2
    ) result

(************************************************************************************)

let print_remanent_modif_opt_map parameter error result =
   Map_modif_creation.Map.iter
      (fun (agent_type, rule_id) (l1, l2) ->
        if l1 <> []
        then ()
        else ();
        List.iter (fun (id, site, state) ->
          fprintf parameter.log 
            "agent_type:%i:rule_id:%i@covering_class_id:%i:site_type':%i:state:%i\n"
            agent_type rule_id id site state
        ) l2
      ) result

(************************************************************************************)
(*main print*)

let print_bdu_build_map parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Covering classes with new index :\n";
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
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- A map of covering classes with creation rules:\n";
    print_remanent_creation_map
      parameter
      error
      result.store_remanent_creation_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- A map of covering classes with modification rules (without creation rules):\n";
    print_remanent_modif_opt_map
      parameter
      error
      result.store_remanent_modif_opt_map
  in
  error
