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
(*restriction of test rules*)

let print_pair_list parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (site, state) :: tl ->
      fprintf parameter.log "site_type':%i:state:%i\n" site state;
      aux tl
  in aux l

let print_four_list parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (agent_id, rule_id, cv_id, l) :: tl ->
      let _ = 
        fprintf parameter.log
          "agent_id:%i:rule_id:%i:covering_class_id:%i\n"
          agent_id rule_id cv_id
      in
      let _ = print_pair_list parameter l in
      aux tl
  in
  aux l

let print_remanent_test parameter error result =
  AgentMap.print error
    (fun error parameter four_list ->
      let _ =
        print_four_list parameter four_list
      in
      error
    ) parameter result

(*TEST*)
let print_remanent_test_map parameter error result =
  Map_test.Map.iter (fun (agent_id, agent_type, rule_id, cv_id) (l1, l2) ->
    if l1 <> []
    then ()
    else
      ();
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

let print_triple_list parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, cv_id, l) :: tl ->
      let _ = 
        fprintf parameter.log
          "rule_id:%i:covering_class_id:%i\n"
          rule_id cv_id
      in
      let _ = print_pair_list parameter l in
      aux tl
  in
  aux l

let print_remanent_creation parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
        print_triple_list parameter triple_list
      in
      error
    ) parameter result

let print_remanent_creation_map parameter error result =
  Map_creation.Map.iter (fun (agent_type, rule_id, cv_id) (l1, l2) ->
    if l1 <> []
    then ()
    else
      ();
    let _ =
      fprintf parameter.log
        "agent_type:%i:rule_id:%i:covering_class_id:%i@list of pair:\n" 
        agent_type rule_id cv_id
    in
    List.iter (fun (site, state) ->
      fprintf parameter.log 
        "site_type':%i:state:%i\n" site state
    ) l2
  ) result

(************************************************************************************)

let print_remanent_modif_map parameter error result =
  Map_modif.Map.iter (fun (agent_id, agent_type, rule_id, cv_id) (l1, l2) ->
    if l1 <> []
    then ()
    else
      ();
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
  (*------------------------------------------------------------------------------*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes triple (id, list, set):\n";
    print_remanent_triple
      parameter
      error
      result.store_remanent_triple
  in
  (*------------------------------------------------------------------------------*)
  (*let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Test rule with new index:\n";
    print_remanent_test
      parameter
      error
      result.store_remanent_test
  in*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Valuations of the views that are tested (per rule, agent and covering class):\n\n";
    print_remanent_test_map
      parameter
      error
      result.store_remanent_test_map
  in
  (*------------------------------------------------------------------------------*)
  let _ =
   fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Valuations of the views that are created (per rule, agent and covering class):\n\n";
    print_remanent_creation_map
      parameter
      error
      result.store_remanent_creation_map
  in
  (*------------------------------------------------------------------------------*)
  let _ =
   fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Update of the views due to modification (per rule, agent and covering class):\n\n";
    print_remanent_modif_map
      parameter
      error
      result.store_remanent_modif_map
  in
  error
