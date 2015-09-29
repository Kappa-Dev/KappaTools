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
            Boolean_mvbdu.print_boolean_mvbdu error
              (Remanent_parameters.update_prefix parameter " ") bdu
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
    (fun error parameter (l, rule_array, wl) ->
      let _ =
        fprintf stdout "- List of rule_id:\n";
        let _ = print_list_rule l in
        fprintf stdout "- List of rule_id store inside a working list:\n";
        IntWL.print_wl wl;
        fprintf stdout "- List of bdu_creation build from rule_id inside the working list:\n";
        print_rule_array parameter error rule_array
      in
      error
    ) parameter result

    
(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "* List of rules has creation action:\n";
    let _ =
      print_creation_rule parameter error result.store_creation_rule
    in
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "*BDU creation in general:\n";
    print_bdu_array_creation parameter error result.store_creation
  in
  error
