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
    (fun (agent_id, agent_type, rule_id, cv_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ =
        fprintf parameter.log
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i@list of pair:\n" 
          agent_id agent_type rule_id cv_id
      in
      List.iter (fun (site, state) ->
        fprintf parameter.log 
          "site_type':%i:state:%i\n" site state
      ) l2
    ) result

(************************************************************************************)

let print_remanent_creation_map parameter error result =
  Map_creation.Map.iter
    (fun (agent_type, rule_id, cv_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ =
        fprintf parameter.log
          "agent_type:%i:rule_id:%i:covering_class_id:%i@list of pair:\n" 
          agent_type rule_id cv_id
      in
      List.iter (fun (site, state) ->
        fprintf parameter.log "site_type':%i:state:%i\n" site state
      ) l2
    ) result

(************************************************************************************)

let print_remanent_modif_map parameter error result =
  Map_modif.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ =
        fprintf parameter.log
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i@list of triple:\n"
          agent_id agent_type rule_id cv_id
      in
      List.iter (fun (site, state) ->
        fprintf parameter.log "site_type':%i:state:%i\n" site state
      ) l2
    ) result

(************************************************************************************)
(*BDU*)
(************************************************************************************)

let print_test_bdu_map parameter error result =
  Map_test_bdu.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ =
        fprintf parameter.log
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_id agent_type rule_id cv_id
      in
      List.iter (fun (_, bdu_test) ->
        Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_test
      ) l2
    ) result

let print_final_test_bdu_map parameter error result =
  Map_final_test_bdu.Map.iter
    (fun rule_id (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      List.iter (fun (agent_id, bdu_creation) ->
        let _ = fprintf parameter.log "agent_id:%i\n" agent_id in
        Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_creation
      ) l2
    ) result

(************************************************************************************)

let print_creation_bdu_map parameter error result =
  Map_creation_bdu.Map.iter
    (fun (agent_type, rule_id, cv_id) (l1,l2) ->
      if l1 <> []
      then ()
      else ();
      let _ =
        fprintf parameter.log "agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_type rule_id cv_id
      in
      List.iter (fun (_, bdu_creation) ->
        Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_creation
      ) l2
    ) result

(*projection*)

let print_final_creation_bdu_map parameter error result =
  Map_final_creation_bdu.Map.iter
    (fun rule_id (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      List.iter (fun (agent_type, bdu_creation) ->
        let _ = fprintf parameter.log "agent_type:%i\n" agent_type in
        Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_creation
      ) l2
    ) result

(************************************************************************************)

let print_modif_list_map parameter error result =
  Map_modif_list.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ =
        fprintf parameter.log 
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_id agent_type rule_id cv_id
      in
      List.iter (fun (_, l) ->
        List.iter (fun (site, state) ->
          fprintf parameter.log "site_type:%i:state:%i\n" site state
        ) l
      ) l2
    ) result

(*projection*)

let print_final_modif_list_map parameter error result =
  Map_final_modif_list.Map.iter
    (fun rule_id (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      List.iter (fun (agent_id, l) ->
        let _ = fprintf parameter.log "agent_id:%i:\n" agent_id in
        List.iter (fun (site, state) ->
          fprintf parameter.log "site_type:%i:state:%i\n" site state
        ) l
      ) l2
    ) result

(************************************************************************************)
(*main print*)

let print_bdu_build_map parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Bdu of covering classes with new indexes :\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  (*-----------------------------------------------------------------*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created (per rule, agent and covering class):\n\n";
    print_creation_bdu_map
      parameter
      error
      result.store_creation_bdu_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created (per rule_id; projection function):\n\n";
    print_final_creation_bdu_map
      parameter
      error
      result.store_final_creation_bdu_map
  in
  (*-----------------------------------------------------------------*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are tested (per rule, agent and covering class):\n\n";
    print_test_bdu_map
      parameter
      error
      result.store_test_bdu_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are tested (projection per rule):\n\n";
    print_final_test_bdu_map
      parameter
      error
      result.store_final_test_bdu_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- List for update of the views due to modification (per rule, agent and covering class):\n";
    print_modif_list_map
      parameter
      error
      result.store_modif_list_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- List for update of the views due to modification (projection per rule):\n";
    print_final_modif_list_map
      parameter
      error
      result.store_final_modif_list_map
  in
  error
