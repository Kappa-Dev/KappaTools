(**
  * bdu_analysi.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
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
open Memo_sig
open Fifo
open Cckappa_sig

(************************************************************************************)
(*PRINT*)

let print_bdu_array_creation parameter error result =
  AgentMap.print error 
    (fun error parameter (l, (handler, bdu_array)) ->
      let _ =
        Array.iter (fun bdu ->
          let _ =
            fprintf stdout "-------------------------------------\n";
            Boolean_mvbdu.print_boolean_mvbdu error parameter bdu
          in
          ()
        ) bdu_array        
      in
      error
    ) parameter result

let print_list_rule parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | h :: tl -> 
      fprintf (Remanent_parameters.get_log parameter) "rule_id: %i \n" h;
      aux tl;
  in aux l

let print_rule_array parameter error rule_array =
  let error, store = AgentMap.create parameter error 0 in
  Array.iteri (fun index rule ->
    let _ =
      let error, store =
        Bdu_creation.collect_creation
          parameter
          error
          rule.rule_rhs.views
          rule.actions.creation
          store
      in
      print_bdu_array_creation parameter error store
    in 
    ()
  ) rule_array

let print_creation_rule parameter error result =
  AgentMap.print error
    (fun error parameter (l, wl, rule_array) ->
      let _ =
        fprintf (Remanent_parameters.get_log parameter) "- List of rule_id:\n";
        let _ = print_list_rule parameter l in
        fprintf (Remanent_parameters.get_log parameter)
          "- List of rule_id store inside a working list:\n";
        IntWL.print_wl parameter wl;
        fprintf (Remanent_parameters.get_log parameter)
          "- List of bdu_creation build from rule_id inside the working list:\n";
        print_rule_array parameter error rule_array
      in
      error
    ) parameter result

(************************************************************************************)
(*side effects*)

let print_triple parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, site, state_min) :: tl ->
      fprintf (Remanent_parameters.get_log parameter) "rule_id:%i:site_type:%i:state:%i\n"
        rule_id site state_min;
      aux tl
  in
  aux l

let print_pair parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, site) :: tl ->
      fprintf (Remanent_parameters.get_log parameter) "rule_id:%i:site_type:%i:state:no_information\n"
        rule_id site;
      aux tl
  in
  aux l

let print_side_effects parameter error result =
  let result_half_break, result_remove = result in
  (*let result_remove_with_info, result_remove_without_info = result_remove in*)
  let error =
    AgentMap.print error
      (fun error parameter l ->
        let _ =
          print_triple parameter l
        in
        error
      ) parameter result_half_break
  in
  (*NOTE: do not consider where site has state free*)
  let error =
    AgentMap.print error
      (fun error parameter l ->
        let _ =
          print_pair parameter l
        in
        error
      ) parameter result_remove
  in
  error

(************************************************************************************)
(*modification sites*)

let print_modification_sites parameter error result =
  AgentMap.print error
    (fun error parameter l ->
      let _ =
        print_triple parameter l
      in
      error      
    ) parameter result

let print_covering_classes_modified_sites parameter error result =
  AgentMap.print error
    (fun error parameter l ->
      let _ =
        print_triple parameter l
      in
      error
    ) parameter result

(************************************************************************************)
(*contact map*)

let print_contact_map parameter error result =
  Int2Map.iter
    (fun (x, y, s) (l1, l2) ->
      if l1 <> []
      then
        begin 
          let _ = fprintf parameter.Remanent_parameters_sig.log 
            "agent_type:%i@site_type:%i:state:%i" x y s in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.Remanent_parameters_sig.log ", ");
              fprintf parameter.Remanent_parameters_sig.log "agent_type:%i" x;
              true)
            false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter
	(fun (z, t, s') ->
	  Printf.fprintf parameter.Remanent_parameters_sig.log
            "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n"
            x y s z t s'
	) l2
    ) result

(*------------------------------------------------------------------------------*)
(*if agent A bond to B, then return A bond to B and B bond to A*)

let print_contact_map_binding_only_on_rhs parameter error result =
  let result_forward, result_reverse = result in
  (*A bond to B*)
  let _ =
    Int2Map_pair.iter
      (fun (x, y) (l1, l2) ->
        if l1 <> []
        then
          begin 
            let _ = fprintf parameter.Remanent_parameters_sig.log 
              "agent_type:%i@site_type:%i" x y in
            let _ = List.fold_left
              (fun bool x ->
                (if bool
                 then
                    fprintf parameter.Remanent_parameters_sig.log ", ");
                fprintf parameter.Remanent_parameters_sig.log "agent_type:%i" x;
                true)
              false l1
            in
            fprintf stdout "\n"
          end
        else ();
        List.iter
	  (fun (z,t) ->
	    Printf.fprintf parameter.Remanent_parameters_sig.log
              "agent_type:%i@site_type:%i--agent_type':%i@site_type':%i\n"
              x y z t 
	  ) l2
      ) result_forward
  in
  (*------------------------------------------------------------------------------*)
  (*B bond to A*)
  Int2Map_pair.iter
    (fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin 
          let _ = fprintf parameter.Remanent_parameters_sig.log 
            "agent_type':%i@site_type':%i" x y in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.Remanent_parameters_sig.log ", ");
              fprintf parameter.Remanent_parameters_sig.log "agent_type':%i" x;
              true)
            false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter
	(fun (z,t) ->
	  Printf.fprintf parameter.Remanent_parameters_sig.log
            "agent_type':%i@site_type':%i--agent_type:%i@site_type:%i\n"
            x y z t 
	) l2
    ) result_reverse

(*------------------------------------------------------------------------------*)

let print_precise_binding_dual parameter error result =
  let result_forward, result_reverse = result in
  let _ =
    Int2Map.iter
      (fun (x, y, s) (l1, l2) ->
        if l1 <> []
        then
          begin 
            let _ = fprintf parameter.Remanent_parameters_sig.log 
              "agent_type:%i@site_type:%i:state:%i" x y s in
            let _ = List.fold_left
              (fun bool x ->
                (if bool
                 then
                    fprintf parameter.Remanent_parameters_sig.log ", ");
                fprintf parameter.Remanent_parameters_sig.log "agent_type:%i" x;
                true)
              false l1
            in
            fprintf stdout "\n"
          end
        else ();
        List.iter
	  (fun (z, t, s') ->
	    Printf.fprintf parameter.Remanent_parameters_sig.log
              "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n"
              x y s z t s'
	  ) l2
      ) result_forward
  in
  (*------------------------------------------------------------------------------*)
  (*B bond to A*)
  Int2Map.iter
    (fun (x, y, s) (l1, l2) ->
      if l1 <> []
      then
        begin 
          let _ = fprintf parameter.Remanent_parameters_sig.log 
            "agent_type':%i@site_type':%i:state':%i" x y s in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.Remanent_parameters_sig.log ", ");
              fprintf parameter.Remanent_parameters_sig.log "agent_type':%i" x;
              true)
            false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter
	(fun (z, t, s') ->
	  Printf.fprintf parameter.Remanent_parameters_sig.log
            "agent_type':%i@site_type':%i:state':%i--agent_type:%i@site_type:%i:state:%i\n"
            x y s z t s'
	) l2
    ) result_reverse

(************************************************************************************)
(*update rule_id: side effects and covering classes and modified sites*)

let print_rule_id_set parameter error result =
  AgentMap.print error
    (fun error parameter rule_set ->
      let _ =
	Site_map_and_set.iter_set (fun elt ->
	  let _ =
	    fprintf stdout "rule_id:%i\n" elt
	  in
	  ()
	) rule_set
      in
      error
    ) parameter result

let print_rule_set result =
  Site_map_and_set.iter_set (fun elt ->
    let _ =
      fprintf stdout "rule_id:%i\n" elt
    in
    ()
  ) result

(************************************************************************************)

let print_fixpoint_iteration parameter error result =
  AgentMap.print error
    (fun error parameter (wl, rule_array) ->
      let _ =
	fprintf stdout "Working list:\n";
	IntWL.print_wl parameter wl
      in
      error
    ) parameter result
    
(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "============================================================\n";
    fprintf (Remanent_parameters.get_log parameter) "* BDU Analysis:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "============================================================\n";
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter) "* Side effects action:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_side =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_side_effects parameter_side error result.store_side_effects
    in
    error
  in    
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter) "* Modification sites:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_modif =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_modification_sites parameter_modif error result.store_modification_sites
    in
    error
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Covering classes and Modification sites:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_cv_m =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_covering_classes_modified_sites parameter_cv_m error
        result.store_covering_classes_modified_sites
    in
    error
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Contact map with binding both directions (with state):\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_cm =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_contact_map parameter_cm error result.store_contact_map
    in
    error
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Contact map with binding only on the rhs (there is no state information):\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_cm =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_contact_map_binding_only_on_rhs parameter_cm error
        result.store_binding_rhs
    in
    error
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Contact map with binding in the initial state and binding on the rhs (with state):\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_cm =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_precise_binding_dual parameter_cm error result.store_binding_dual
    in
    error    
  in
  (*let _ =
    let store_half_break_set, store_remove_set, store_cv_set, store_result_cv_set
      = result.store_update_bond_side_effects_set
    in
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* List of rules has binding sites and side effects set (TODO):\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_a =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      (*fprintf stdout "Half break set:\n";
      let _ = print_rule_id_set parameter_a error store_half_break_set in
      fprintf stdout "Remove set:\n";
      let _ = print_rule_id_set parameter_a error store_remove_set  in
      fprintf stdout "Covering Classes (cv1) set:\n";
      let _ = print_rule_id_set parameter_a error store_cv_set in*)
      fprintf stdout "Update rule_id list (cv) set:\n";
      print_rule_id_set parameter_a error store_result_cv_set
    in
    error
  in*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Update function:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_a =
      Remanent_parameters.update_prefix parameter "agent_type_" in
      print_rule_id_set parameter_a error result.store_update
  in
  let _ =
     fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Fixpoint iteration (TODO):\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_a =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      print_fixpoint_iteration parameter_a error result.store_fixpoint
    in
    error
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* List of rules has creation action:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    let parameter_a_rule =
      Remanent_parameters.update_prefix parameter "agent_type/rule_id_" in
    let error =
      print_creation_rule parameter_a_rule error result.store_creation_rule
    in
    error
  in
  let _ =
    let parameter_agent = Remanent_parameters.update_prefix parameter "agent_type_" in
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter) "* BDU creation in general:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    print_bdu_array_creation parameter_agent error result.store_creation
  in
  error
