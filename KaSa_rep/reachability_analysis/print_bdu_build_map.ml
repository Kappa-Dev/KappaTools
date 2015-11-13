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

let print_bdu parameter error bdu =
  Boolean_mvbdu.print_boolean_mvbdu error
    (Remanent_parameters.update_prefix parameter "") bdu
    
(*let print_test_bdu_map parameter error result =
  Map_test_bdu.Map.iter
    (fun (agent_type, rule_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      List.iter (fun (handler, bdu) ->
        fprintf parameter.log 
          "agent_type:%i:rule_id:%i\n"
          agent_type rule_id;
        let _ = print_bdu parameter error bdu in
        ()
      ) l2
    ) result*)

let print_test_bdu parameter error result =
  AgentMap.print error
    (fun error parameter l ->
      let _ =
        List.iter (fun (rule_id, bdu_test) ->
          fprintf stdout "rule_id:%i\n" rule_id;
          let _ =
            print_bdu parameter error bdu_test
          in
          ()
        ) l
      in
      error
    ) parameter result

(************************************************************************************)

(*let print_creation_bdu_map parameter error result =
  Map_creation_bdu.Map.iter
    (fun (agent_type, rule_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      List.iter (fun (handler, bdu) ->
        fprintf parameter.log 
          "agent_type:%i:rule_id:%i\n"
          agent_type rule_id;
        let _ = print_bdu parameter error bdu in
        ()
      ) l2
    ) result*)

let print_creation_bdu parameter error result =
  AgentMap.print error
    (fun error parameter l ->
      let _ =
        List.iter (fun (rule_id, bdu_creation) ->
          fprintf stdout "rule_id:%i\n" rule_id;
          let _ =
            print_bdu parameter error bdu_creation
          in
          ()
        ) l
      in
      error
    ) parameter result

(************************************************************************************)

let print_modif_list_map parameter error result =
  Map_modif_list.Map.iter
    (fun (agent_type, rule_id) (l1, l2) ->
      if l1 <> []
      then ()
      else ();
      List.iter (fun site ->
          fprintf parameter.log 
            "agent_type:%i:rule_id:%i:site_type:%i\n"
            agent_type rule_id site;
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
  (*-----------------------------------------------------------------*)
  (*print bdu*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu test rules:\n";
    print_test_bdu
      parameter
      error
      result.store_test_bdu
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu creation rules:\n";
    print_creation_bdu
      parameter
      error
      result.store_creation_bdu
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- A map of modification rules:\n";
    print_modif_list_map
      parameter
      error
      result.store_modif_list_map
  in
  error
