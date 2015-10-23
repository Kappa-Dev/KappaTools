(**
  * bdu_analysi.ml
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
open Memo_sig
open Fifo
open Cckappa_sig
open Remanent_parameters_sig

(************************************************************************************)
(*PRINT*)

(*let print_bdu_array_creation_aux parameter error result =
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
    ) parameter result*)

let print_rule_list parameter l =
  let rec aux acc =
    match acc with
    | [] -> []
    | h :: tl -> 
      fprintf (Remanent_parameters.get_log parameter) "rule_id: %i \n" h;
      aux tl;
  in aux l

(*let print_rule_array parameter error rule_array =
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
      print_bdu_array_creation_aux parameter error store
    in 
    ()
  ) rule_array*)

(*let print_creation_rule_aux parameter error result =
  AgentMap.print error
    (fun error parameter (l, wl, rule_array) ->
      let _ =
        fprintf (Remanent_parameters.get_log parameter) "- List of rule_id:\n";
        let _ = print_rule_list parameter l in
        fprintf (Remanent_parameters.get_log parameter)
          "- List of rule_id store inside a working list:\n";
        IntWL.print_wl parameter wl;
        fprintf (Remanent_parameters.get_log parameter)
          "- List of bdu_creation build from rule_id inside the working list:\n";
        print_rule_array parameter error rule_array
      in
      error
    ) parameter result*)

(************************************************************************************)
(*static information of covering classes id*)

let print_covering_classes_id_aux parameter error result =
  Int2Map_CV.iter_map
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      List.iter
        (fun id ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:covering_class_id:%i\n"
            x y id
        ) l2
    ) result

(************************************************************************************)
(*print rule test; test inside the rule_array of update function*)

(*let print_bdu_array_test parameter error result =
  AgentMap.print error
    (fun error parameter (l, bdu_test, modif_list) ->
      
    ) parameter result

  
let print_rule_array_test parameter error rule_array = 
  let error, store = AgentMap.create parameter error 0 in
  Array.iteri (fun index rule ->
    let _ =
      let error, store =
	Bdu_fixpoint_iteration.build_bdu_test_list_direct
	  parameter
	  error
	  rule.rule_lhs.views
	  rule.diff_direct
	  store
      in
      print_bdu_array_test parameter error store
    in
    ()
  ) rule_array*)

(************************************************************************************)
(*side effects*)

let print_half_break_effect parameter error result =
  Int2Map_HalfBreak_effect.iter_map
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      List.iter
        (fun (r, s) ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:(rule_id:%i * state:%i)\n"
            x y r s
        ) l2
    ) result

let print_remove_effect parameter error result =
  Int2Map_Remove_effect.iter_map
    ( fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      List.iter
        (fun r ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:(rule_id:%i * state:no_information)\n"
            x y r
        ) l2
    ) result

let print_side_effects_aux parameter error result =
  let result_half_break, result_remove = result in
  let _ =
    print_half_break_effect parameter error result_half_break
  in
  print_remove_effect parameter error result_remove

(************************************************************************************)
(*modification sites*)

let print_modification_sites_aux parameter error result =
  Int2Map_Modif.iter_map
    ( fun (x, y) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log "agent_type:%i@site_type:%i" x y
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
      Site_map_and_set.iter_set
        (fun r ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:rule_id:%i\n"
            x y r
        ) s2
     ) result

(************************************************************************************)
(*contact map*)

let print_contact_map_aux parameter error result =
  Int2Map_CM_state.iter_map
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

(************************************************************************************)
(*TODO*)

(*let print_fixpoint_iteration parameter error result =
  AgentMap.print error
    (fun error parameter (wl, rule_array) ->
      let _ =
	fprintf stdout "Working list:\n";
	IntWL.print_wl parameter wl
      in
      error
    ) parameter result*)
    
(************************************************************************************)
(*update function before adding rule with side effects*)

let print_covering_classes_modification_aux parameter error result =
  Int2Map_CV_Modif.iter_map
    ( fun (x, y, z) (l1, l2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log
              "agent_type:%i@site_type:%i:covering_class_id:%i" x y z
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
      Site_map_and_set.iter_set
        (fun r ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:covering_class_id:%i:rule_id:%i\n"
            x y z r
        ) l2
    ) result

(************************************************************************************)
(*update function adding rule with side effect when discovered the binding*)

let print_update_set_aux parameter error result =
  Int2Map_CV_Modif.iter_map
    ( fun (x, y, z) (l1, s2) ->
      if l1 <> []
      then
        begin
          let _ =
            fprintf parameter.log
              "agent_type:%i@site_type:%i:covering_class_id:%i" x y z
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
      Site_map_and_set.iter_set
        (fun r ->
          fprintf parameter.log
            "agent_type:%i@site_type:%i:covering_class_id:%i:rule_id:%i\n"
            x y z r
        ) s2
    ) result

(*REMOVE*)
let print_binding_update_aux parameter error result =
  let result_hb,
    result_remove,
    result_hb_remove,
    result_update_aux
    = result
  in
  fprintf stdout "half_break side effect (will be removed):\n";
  print_update_set_aux parameter error result_hb;
  fprintf stdout "remove side effect (will be removed):\n";
  print_update_set_aux parameter error result_remove;
  fprintf stdout "half break and remove side effect (will be removed):\n";
  print_update_set_aux parameter error result_hb_remove;
  fprintf stdout "update function:\n";
  print_update_set_aux parameter error result_update_aux
  
(************************************************************************************)
(*auxilary functions*)

let print_covering_classes_id parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Covering classes:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_covering_classes_id_aux parameter error result
  in
  error

let print_side_effects parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter) 
    "Side effects action:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_side_effects_aux parameter error result
  in
  error

(*REMOVE*)
let print_modification_sites parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Modification sites (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux parameter error result
  in
  error

(*REMOVE*)
let print_creation_sites parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Creation sites (will be removed):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux parameter error result
  in
  error

let print_modification_sites_without_creation parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Modification sites without creation:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_modification_sites_aux parameter error result
  in
  error

let print_contact_map parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Contact map with state (there is no lhs binding):\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let error =
    print_contact_map_aux parameter error result
  in
  error

let print_covering_classes_modification parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Update function before adding the side effects rules:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_covering_classes_modification_aux parameter error result

let print_binding_update parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Update function after discovered binding sites, adding the side effects rules:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_binding_update_aux parameter error result
  

(*let print_creation_rule parameter error result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "* List of rules has creation action:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  let parameter_a_rule =
    Remanent_parameters.update_prefix parameter "agent_type/rule_id_" in
  let error =
    print_creation_rule_aux parameter_a_rule error result
  in
  error*)

(*REMOVE*)
(*let print_bdu_array_creation parameter error result =
  let parameter_agent = Remanent_parameters.update_prefix parameter "agent_type_" in
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "* BDU creation in general (this function used for testing) will be removed:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_bdu_array_creation_aux parameter_agent error result*)

(************************************************************************************)
(*Fixpoint iteration function*)

let print_wl_update parameter error result =
  AgentMap.print error
    (fun error parameter wl ->
      let _ =
        fprintf (Remanent_parameters.get_log parameter)
          "- List of rule_id in working list of update function:\n";
        IntWL.print_wl parameter wl
      in
      error
    ) parameter result

let print_wl_creation parameter error result =
  AgentMap.print error
    (fun error parameter wl ->
      let _ =
	fprintf stdout "-List of rule_id in working list of creation:\n";
	IntWL.print_wl parameter wl
      in
      error
    ) parameter result

let print_wl_creation_update parameter error result =
  AgentMap.print error
    (fun error parameter wl ->
      let _ =
	fprintf stdout "-List of rule_id in working list of creation_update:\n";
	IntWL.print_wl parameter wl
      in
      error
    ) parameter result
    
(*let print_fixpoint parameter error result =
  let parameter_agent = Remanent_parameters.update_prefix parameter "agent_type_" in
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "* Fixpoint iteration:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  print_wl_update parameter_agent error result;*)

(************************************************************************************)
(*fixpoint iteration*)

let print_bdu_creation_array parameter error result =
  AgentMap.print error
    (fun error parameter bdu_array ->
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
    
let print_fixpoint parameter error result =
  AgentMap.print error
    (fun error parameter bdu_array ->
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

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
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
    print_covering_classes_id parameter error result.store_covering_classes_id
  in
  let _ =
    print_side_effects parameter error result.store_side_effects
  in
  (*will be removed*)
  let _ =
      print_modification_sites parameter error result.store_modification_sites
  in
  (*will be removed*)
  let _ =
      print_creation_sites parameter error result.store_creation_sites
  in
  let _ =
    print_modification_sites_without_creation parameter error
      result.store_modification_sites_without_creation
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter) "\n** Dynamic information:\n";
  in
  let _ =
    print_contact_map parameter error result.store_contact_map
  in
  let _ =
    print_covering_classes_modification parameter error
      result.store_covering_classes_modification_update
  in
  let _ =
    print_binding_update parameter error result.store_update
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Fixpoint iteration:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  (*working list*)
  let _ =
    let parameter_agent = Remanent_parameters.update_prefix parameter "agent_type_" in
    fprintf (Remanent_parameters.get_log parameter)
      "- Working list update (will be removed):\n";
    (*REMOVE*)
    let _ = print_wl_update parameter_agent error result.store_wl_update in
    fprintf (Remanent_parameters.get_log parameter)
      "- Working list creation (will be removed):\n";
    (*REMOVE*)
    let _ = print_wl_creation parameter_agent error result.store_wl_creation in
    fprintf (Remanent_parameters.get_log parameter)
      "- Working list update and creation:\n";
    print_wl_creation_update parameter_agent error result.store_wl_creation_update
  in
  let _ =
    let parameter_agent = Remanent_parameters.update_prefix parameter "agent_type_" in
    
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu of creation iteration:\n";
    print_bdu_creation_array parameter_agent error result.store_bdu_creation_array;
    
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu of fixpoint iteration:\n";
    print_fixpoint parameter_agent error result.store_fixpoint_iteration
  in
  error
