(**
  * print_bdu_build.ml
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
open Bdu_analysis_type
open SetMap
open Cckappa_sig
open Remanent_parameters_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)

let print_triple_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id, _, set) :: tl ->
      fprintf stdout "Covering_class_id:%i\n" id;
      Site_map_and_set.Set.iter (fun site ->
        fprintf stdout "site_type:%i\n" site
      ) set; aux tl
  in aux l

let print_remanent_triple parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
        print_triple_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*working list*)

let print_wl_creation parameter result =
  Fifo.IntWL.print_wl parameter result

(************************************************************************************)
(*test rule*)

let print_test_bdu_map parameter error result =
  Map_test_bdu.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) bdu_test ->
      let _ =
        fprintf parameter.log
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_id agent_type rule_id cv_id
      in
      Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_test
    ) result

let print_proj_test_bdu_map parameter error result =
  Map_final_test_bdu.Map.iter
    (fun rule_id map_b ->
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      Map_agent_id_test_bdu.Map.iter (fun agent_id bdu_test ->
        let _ = fprintf parameter.log "agent_id:%i\n" agent_id in
        Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_test
      ) map_b
    ) result

(************************************************************************************)
(*creation rule*)

let print_creation_bdu_map parameter error result =
  Map_creation_bdu.Map.iter
    (fun (agent_type, rule_id, cv_id) bdu_creation ->
      let _ =
        fprintf parameter.log "agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_type rule_id cv_id
      in
      Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_creation
    ) result

let print_proj_creation_bdu_map parameter error result =
  Map_final_creation_bdu.Map.iter
    (fun rule_id map_b ->
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      Map_agent_type_creation_bdu.Map.iter
        (fun agent_type bdu_creation ->
          let _ = fprintf parameter.log "agent_type:%i\n" agent_type in
          Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_creation
        ) map_b
    ) result

(************************************************************************************)
(*bdu initial state*)

let print_init_bdu_map parameter error result =
  Map_bdu_update.Map.iter
    (fun (agent_type, cv_id) bdu_init ->
      let _ =
        fprintf parameter.log "agent_type:%i:covering_class_id:%i\n"
          agent_type cv_id
      in
      Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_init
    ) result

(************************************************************************************)
(*modification list*)

let print_modif_list_map parameter error result =
  Map_modif_list.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) l2 ->
      let _ =
        fprintf parameter.log 
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_id agent_type rule_id cv_id
      in
      List.iter (fun (site, state) ->
        fprintf parameter.log "site_type:%i:state:%i\n" site state
      ) l2
    ) result

(*projection*)

let print_proj_modif_list_map parameter error result =
  Map_final_modif_list.Map.iter
    (fun rule_id map_b ->
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      Map_agent_id_modif_list.Map.iter
        (fun agent_id l ->
          let _ = fprintf parameter.log "agent_id:%i:\n" agent_id in
          List.iter (fun (site, state) ->
            fprintf parameter.log "site_type:%i:state:%i\n" site state
          ) l
        ) map_b
    ) result

(************************************************************************************)
(*main print*)

let print_bdu_build parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Covering classes with new indexes:\n";
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
  let _ =
     fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are tested (per rule, agent and covering class):\n\n";
    print_test_bdu_map
      parameter
      error
      result.store_bdu_test_restriction_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are tested (projection per rule):\n\n";
    print_proj_test_bdu_map
      parameter
      error
      result.store_proj_bdu_test_restriction_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created (per rule, agent and covering class):\n\n";
    print_creation_bdu_map
      parameter
      error
      result.store_bdu_creation_restriction_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created (per rule_id; projection function):\n\n";
    print_proj_creation_bdu_map
      parameter
      error
      result.store_proj_bdu_creation_restriction_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created in the initial state (per rule, agent and covering class):\n\n";
    print_init_bdu_map
      parameter
      error
      result.store_bdu_init_restriction_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- List for update of the views due to modification (per rule, agent and covering class):\n";
    print_modif_list_map
      parameter
      error
      result.store_modif_list_restriction_map
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- List for update of the views due to modification (projection per rule):\n";
    print_proj_modif_list_map
      parameter
      error
      result.store_proj_modif_list_restriction_map
  in
  error
