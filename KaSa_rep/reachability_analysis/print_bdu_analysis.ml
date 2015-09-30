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

let print_list_rule l =
  let rec aux acc =
    match acc with
    | [] -> []
    | h :: tl -> 
      fprintf stdout "rule_id: %i \n" h;
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
        fprintf stdout "- List of rule_id:\n";
        let _ = print_list_rule l in
        fprintf stdout "- List of rule_id store inside a working list:\n";
        IntWL.print_wl wl;
        fprintf stdout
          "- List of bdu_creation build from rule_id inside the working list:\n";
        print_rule_array parameter error rule_array
      in
      error
    ) parameter result

(************************************************************************************)
(*side effects*)

let print_triple l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, site, state_min) :: tl ->
      fprintf stdout "rule_id:%i:site_type:%i:state_min:%i\n"
        rule_id site state_min;
      aux tl
  in
  aux l

let print_triple_option l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, site, op) :: tl ->
      match op with
      | None ->
        fprintf stdout "rule_id:%i:site_type:%i:state:none\n"
          rule_id site; aux tl
      | Some b ->
        fprintf stdout "rule_id:%i:site_type:%i:state_site_free:%b\n"
          rule_id site b;
        aux tl
  in
  aux l

let print_pair l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, site) :: tl ->
      fprintf stdout "rule_id:%i:site_type:%i:state:no_information\n"
        rule_id site;
      aux tl
  in
  aux l

let print_side_effects parameter error result =
  let result_half_break, result_remove = result in
  let result_remove_with_info, result_remove_without_info = result_remove in
  let error =
    AgentMap.print error
      (fun error parameter l ->
        let _ =
          print_triple l
        in
        error
      ) parameter result_half_break
  in
  let error =
    AgentMap.print error
      (fun error parameter l ->
        let _ =
          print_triple_option l
        in
        error
      ) parameter result_remove_with_info
  in
  let error =
    AgentMap.print error
      (fun error parameter l ->
        let _ =
          print_pair l
        in
        error
      ) parameter result_remove_without_info
  in
  error

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let error =
    fprintf stdout "------------------------------------------------------------\n";
    fprintf stdout "* Side effects action:\n";
    fprintf stdout "------------------------------------------------------------\n";
    let parameter_side =
      Remanent_parameters.update_prefix parameter "agent_type_" in
    let _ =
      print_side_effects parameter_side error result.store_side_effects
    in
    fprintf stdout "\n------------------------------------------------------------\n";
    fprintf stdout "* List of rules has creation action:\n";
    fprintf stdout "------------------------------------------------------------\n";
    let parameter_a_rule =
      Remanent_parameters.update_prefix parameter "agent_type/rule_id_" in
    let _ =
      print_creation_rule parameter_a_rule error result.store_creation_rule
    in
    let parameter_agent = Remanent_parameters.update_prefix parameter "agent_type_" in
    fprintf stdout "\n------------------------------------------------------------\n";
    fprintf stdout "* BDU creation in general:\n";
    fprintf stdout "------------------------------------------------------------\n";
    print_bdu_array_creation parameter_agent error result.store_creation
  in
  error
