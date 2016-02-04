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
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i"
          agent_id agent_type rule_id cv_id
      in
      let _ =
	Loggers.fprintf (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_test
    ) result

(*let print_proj_test_bdu_map parameter error result =
  Map_final_test_bdu.Map.iter
    (fun rule_id map_b ->
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_agent_id_test_bdu.Map.iter (fun agent_id bdu_test ->
        let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_id:%i" agent_id in
	let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
        Mvbdu_wrapper.Mvbdu.print parameter bdu_test
      ) map_b
    ) result*)

(************************************************************************************)
(*creation rule*)

let print_creation_bdu_map parameter error result =
  Map_creation_bdu.Map.iter
    (fun (agent_type, rule_id, cv_id) bdu_creation ->
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:rule_id:%i:covering_class_id:%i"
          agent_type rule_id cv_id
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_creation
    ) result

let print_proj_creation_bdu_map parameter error result =
  Map_final_creation_bdu.Map.iter
    (fun rule_id map_b ->
      let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      Map_agent_type_creation_bdu.Map.iter
        (fun (agent_type, cv_id) bdu_creation ->
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:cv_id:%i"
            agent_type cv_id
          in
	  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter)
	  in
          Mvbdu_wrapper.Mvbdu.print parameter bdu_creation
        ) map_b
    ) result

(************************************************************************************)
(*bdu initial state*)

let print_init_bdu_map parameter error result =
  Map_init_bdu.Map.iter
    (fun (agent_type, cv_id) bdu_init ->
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:covering_class_id:%i\n"
          agent_type cv_id
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_init
    ) result

(************************************************************************************)
(*modification list, this is list_a*)

let print_modif_list_map parameter error result =
  Map_modif_list.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) list_a ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i"
          agent_id agent_type rule_id cv_id
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print_association_list parameter list_a
    ) result

(************************************************************************************)
(*bdu of potential partner in side effects*)

let print_potential_bdu_map parameter error result =
  Map_potential_bdu.Map.iter
    (fun (agent_type, site_type, rule_id, cv_id) (bdu_potential,list) ->
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:new_site_name:%i:rule_id:%i:covering_class_id:%i\n"
          agent_type site_type rule_id cv_id
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_potential;
      Mvbdu_wrapper.Mvbdu.print_association_list parameter list
    ) result

(*projection*)

let print_proj_potential_bdu_map parameter error result =
  Map_final_potential_bdu.Map.iter
    (fun rule_id map_b ->
      let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "rule_id:%i" rule_id in
      let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_agent_type_potential_bdu.Map.iter
        (fun (agent_type, site_type, cv_id) (bdu_potential,list) ->
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_type:%i:new_site_name:%i:covering_class_id:%i"
            agent_type site_type cv_id
          in
	  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
          Mvbdu_wrapper.Mvbdu.print parameter bdu_potential;
	  Mvbdu_wrapper.Mvbdu.print_association_list parameter list
        ) map_b
    ) result

(************************************************************************************)
(*potential partner list of side effects*)

(*let print_potential_list_map parameter error result =
  Map_potential_list.Map.iter
    (fun (agent_type, rule_id, cv_id) l ->
      let _ =
        fprintf parameter.log "agent_type:%i:rule_id:%i:covering_class_id:%i\n"
          agent_type rule_id cv_id
      in
      List.iter (fun list_a ->
        Mvbdu_wrapper.Mvbdu.print_association_list parameter.log "" list_a
      ) l
    ) result*)

(*projection*)

(*let print_proj_potential_list_map parameter error result =
  Map_final_potential_list.Map.iter
    (fun rule_id map_b ->
      let _ = fprintf parameter.log "rule_id:%i\n" rule_id in
      Map_agent_type_potential_list.Map.iter
        (fun agent_type l ->
          let _ = fprintf parameter.log "agent_type:%i\n" agent_type in
          List.iter (fun list_a ->
            Mvbdu_wrapper.Mvbdu.print_association_list parameter.log "" list_a
          ) l
        ) map_b
    ) result*)

(************************************************************************************)
(*TODO: projection function will be used in is_enable function*)

let print_proj_bdu_views parameter error result =
  Map_rule_id_views.Map.iter
    (fun rule_id map_b ->
      let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_triple_views.Map.iter
        (fun (agent_id, agent_type, cv_id) bdu ->
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_id:%i:agent_type:%i:cv_id:%i" agent_id agent_type cv_id in
	  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter)  in
	  Mvbdu_wrapper.Mvbdu.print parameter bdu
        ) map_b
    ) result


(************************************************************************************)
(*main print*)

(*let print_bdu_pre_static parameter error result =
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "- Bdu for the valuations of the views that are created (per rule, agent and covering class):" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () =
    print_creation_bdu_map
      parameter
      error
      result.store_bdu_creation_restriction_map
  in
  error*)

let print_bdu_build parameter error result =
  let _ =
    Loggers.print_newline (Remanent_parameters.get_logger parameter) ;
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------";
    Loggers.print_newline (Remanent_parameters.get_logger parameter);
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "* Covering classes with new indexes:";
    Loggers.print_newline (Remanent_parameters.get_logger parameter);
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------";
    Loggers.print_newline (Remanent_parameters.get_logger parameter);
  in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "- Working list creation:";
    print_wl_creation
      parameter
      result.store_wl_creation
  in
   Loggers.print_newline (Remanent_parameters.get_logger parameter);
  (*print if one wants to debug*)
  (*let _ =
     fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created from potential partner in side effects (per rule, agent and covering class):\n\n";
    print_potential_bdu_map
      parameter
      error
      result.store_bdu_potential_effect_restriction_map
  in*)
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "\n------------------------------------------------------------\n";
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "- Bdu for the valuations of the views that are created from potential partner in side effects (projection per rule):\n\n";
    print_proj_potential_bdu_map
      parameter
      error
      result.store_proj_bdu_potential_restriction_map
  in
  (*print if one wants to debug*)
  (*let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- List for update of the views due to modification that are created from potential partner in side effects (per rule, agent and covering class):\n";
    print_potential_list_map
      parameter
      error
      result.store_potential_list_restriction_map
  in*)
  (*let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- List for update of the views due to modification that are created from potential partner in side effects (projection per rule):\n";
    print_proj_potential_list_map
      parameter
      error
      result.store_proj_potential_list_restriction_map
  in*)
  (*print if one wants to debug*)
  (*let _ =
     fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are tested (per rule, agent and covering class):\n\n";
    print_test_bdu_map
      parameter
      error
      result.store_bdu_test_restriction_map
  in*)
  (*REMOVE*)
  (*let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "- Bdu for the valuations of the views that are tested (projection per rule):" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () =
    print_proj_test_bdu_map
      parameter
      error
      result.store_proj_bdu_test_restriction_map
  in*)
(*print if one wants to debug*)
  (*let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "- Bdu for the valuations of the views that are created (per rule, agent and covering class):" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () =
    print_creation_bdu_map
      parameter
      error
      result.store_bdu_creation_restriction_map
  in*)
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------" in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "- Bdu for the valuations of the views that are created (per rule_id; projection function):" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      print_proj_creation_bdu_map
	parameter
	error
	result.store_proj_bdu_creation_restriction_map
    in
  (*print if one wants to debug*)
  (*let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu for the valuations of the views that are created in the initial state (per rule, agent and covering class):\n\n";
    print_init_bdu_map
      parameter
      error
      result.store_bdu_init_restriction_map
  in*)
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
	"------------------------------------------------------------" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
	"- List for update of the views due to modification (per rule, agent and covering class):" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      print_modif_list_map
	parameter
	error
	result.store_modif_list_restriction_map
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
	"------------------------------------------------------------" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
	"- Bdu for the valuation of the views that are tested (projection per rule):" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      print_proj_bdu_views
	parameter
	error
	result.store_proj_bdu_test_restriction
    in
    error
