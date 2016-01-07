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
            fprintf parameter.log "agent_type:%i:site_type:%i" x y
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
      let _ =
        fprintf parameter.log 
          "agent_type:%i:site_type:%i@list of covering_class_id:\n"
          x y
      in
      List.iter (fun id -> fprintf parameter.log "covering_class_id:%i\n" id)
        l2
    ) result

let print_covering_classes_id parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Mapping between sites and the covering classes they belong to:\n";
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
(*covering classes including type string*)

(*let print_covering_classes_id_string_aux parameter error result =
  Int2Map_CV_map.Map.iter
    ( fun (x, s, y, site_type) (l1, l2) ->
      let _ =
        match site_type with
        | Ckappa_sig.Internal a ->
          fprintf parameter.log 
            "agent_type:%i:%s:site_type:%i:%s(internal state)@list of covering_class_id:\n"
            x s y a
        | Ckappa_sig.Binding b ->
          fprintf parameter.log 
            "agent_type:%i:%s:site_type:%i:%s(binding state)@list of covering_class_id:\n"
            x s y b
      in
      List.iter (fun id -> fprintf parameter.log "covering_class_id:%i\n" id)
        l2
    ) result

let print_covering_classes_id_string parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Mapping between sites and the covering classes they belong to:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_covering_classes_id_string_aux
      parameter
      error
      result
  in
  error*)

(************************************************************************************)
(*side effects*)

let print_half_break_effect parameter error handler_kappa compiled result =
  Int2Map_HalfBreak_effect.Map.iter
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i:site_type:%i" x y
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
      let _ =
        fprintf parameter.log 
          "agent_type:%i:site_type:%i@list of pair (rule_id, binding state):\n"
          x y
      in
      List.iter (fun (rule_id, s) -> 
        (*mapping rule_id of type int to string*)
        let error, rule_id_string =
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        in
        fprintf parameter.log "(%s * state:%i)\n"
          rule_id_string s) l2
    ) result

let print_remove_effect parameter error handler_kappa compiled result =
  Int2Map_Remove_effect.Map.iter
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i:site_type:%i" x y
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
      let _ =
        fprintf parameter.log 
          "agent_type:%i:site_type:%i@list of pair (rule_id, binding state):\n"
          x y
      in
      List.iter (fun r ->
        let error, rule_id_string =
          Handler.string_of_rule parameter error handler_kappa
            compiled r
        in
        fprintf parameter.log "(%s * state:no_information)\n" rule_id_string
      ) l2
    ) result

let print_side_effects_aux parameter error handler_kappa compiled result =
  let result_half_break, result_remove = result in
  let _ =
    print_half_break_effect
      parameter
      error
      handler_kappa
      compiled
      result_half_break
  in
  print_remove_effect
    parameter
    error
    handler_kappa
    compiled
    result_remove

let print_side_effects parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter) 
    "Sites that may/must occurs side-effects:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_side_effects_aux
      parameter
      error 
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*Potential partner side-effects*)

let print_potential_partner_free parameter error handler_kappa compiled result =
  Int2Map_potential_effect.Map.iter 
    (fun (agent_type, rule_id) l ->
      let error, rule_id_string =
        Handler.string_of_rule parameter error handler_kappa
          compiled rule_id
      in
      let _ =
        fprintf stdout "agent_type:%i:%s@(site, free state)\n"
          agent_type rule_id_string
      in
      List.iter (fun (site, state) ->
        fprintf stdout "(site_type:%i * state:%i)\n" site state
      ) l
    ) result

let print_potential_partner_bind parameter error handler_kappa compiled result =
  Int2Map_potential_effect.Map.iter 
    (fun (agent_type, rule_id) l ->
      let error, rule_id_string =
        Handler.string_of_rule parameter error handler_kappa
          compiled rule_id
      in
      let _ =
        fprintf stdout "agent_type:%i:%s@(site, binding state)\n"
          agent_type rule_id_string
      in
      List.iter (fun (site, state) ->
        fprintf stdout "(site_type:%i * state:%i)\n" site state
      ) l
    ) result

let print_potential_side_effects parameter error handler_kappa compiled result =
  let result1, result2 = result in
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter) 
    "Sites that may/must occurs in the potential partner side-effects:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf stdout "- Potential partner has site that is free:\n";
  let _ =
    print_potential_partner_free
      parameter
      error 
      handler_kappa
      compiled
      result1
  in
  fprintf stdout "- Potential partner has site that is bind:\n";
  print_potential_partner_bind
    parameter
    error
    handler_kappa
    compiled
    result2

(************************************************************************************)
(*update of the views due to modification*)

(*with agent_id*)

let print_modification_sites_aux parameter error handler_kappa compiled result =
  Int2Map_Modif.Map.iter
    ( fun (x, y, z) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_id:%i:agent_type:%i:site_type:%i" x y z
          in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.log ", ");
              fprintf parameter.log "agent_id:%i" x;
              true
            ) false l1
            
          in
          fprintf stdout "\n"
        end
      else ();
      let _ =
        fprintf parameter.log 
          "agent_id:%i:agent_type:%i:site_type:%i@set of rule_id:\n" x y z
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          let error, rule_id_string =
            Handler.string_of_rule parameter error handler_kappa
              compiled rule_id
          in
          fprintf parameter.log "%s\n" rule_id_string
        ) s2
    ) result

let print_modification_sites parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Set of rules that may modify a given site (excluding created agents):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(*without agent_id*)
let print_modification_map_aux parameter error handler_kappa compiled result =
  Int2Map_Test_Modif.Map.iter
    ( fun (x, y) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i:site_type:%i" x y
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
      let _ =
        fprintf parameter.log 
          "agent_type:%i:site_type:%i@set of rules:\n" x y
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          let error, rule_id_string =
            Handler.string_of_rule parameter error handler_kappa
              compiled rule_id
          in
          fprintf parameter.log "%s\n" rule_id_string
        ) s2
    ) result

let print_modification_map parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Set of rules that may modify a given site (excluding created agents, without agent_id):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_map_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*valuation of the views that are tested*)

(*with agent_id*)
let print_test_sites parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Set of rules that may test a given site:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux 
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(*without agent_id*)
let print_test_map parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Set of rules that may test a given site (without agent_id):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_map_aux 
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*update and valuations of the views that are tested and modified.*)

(*with agent_id*)
let print_test_modification_sites parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Set of rules that may test and modify a given site (excluding created agents):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(*without agent_id*)

let print_test_modification_map parameter error handler_kappa compiled result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Set of rules that may test and modify a given site (excluding created agents, without agent_id):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_map_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*main print*)

let print_result_static parameter error handler_kappa compiled result =
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
  (*TODO*)
  (*let _ =
    print_covering_classes_id_string
      parameter
      error
      result.store_covering_classes_id_string
  in*)
  let _ =
    print_side_effects 
      parameter 
      error 
      handler_kappa
      compiled
      result.store_side_effects
  in
  let _ =
    print_potential_side_effects
      parameter
      error
      handler_kappa
      compiled
      result.store_potential_side_effects
  in
  let _ =
    print_modification_sites 
      parameter
      error
      handler_kappa
      compiled
      result.store_modification_sites
  in
  let _ =
    print_test_sites
      parameter
      error
      handler_kappa
      compiled
      result.store_test_sites
  in
  let _ =
    print_test_modification_sites
      parameter
      error
      handler_kappa
      compiled
      result.store_test_modification_sites
  in
  (*print if one wants to debug*)
  (*let _ =
    print_modification_map
      parameter
      error
    handler_kappa
    compiled
      result.store_modif_map
  in*)
  (*let _ =
    print_test_map
      parameter
      error 
    handler_kappa
    compiled
      result.store_test_map
  in*)
 (*let _ =
    print_test_modification_map
      parameter
      error 
   handler_kappa
   compiled
      result.store_test_modif_map
  in*)
  error
