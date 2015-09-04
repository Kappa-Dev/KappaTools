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

(************************************************************************************)
(*PRINT*)

let print_index_list parameter error store_result =
  let rec aux acc =
    match acc with
      | [] -> []
      | (i, i') :: tl ->
        let _ = fprintf stdout "succ: %i, %i\n" i i' in
        aux tl          
  in aux store_result

let print_bdu_array parameter error result =
  AgentMap.print error 
    (fun error parameter (handler, bdu_array) ->
      let _ =
        Array.iter (fun bdu ->
          let _ =
            fprintf stdout "-------------------------------------\n";
            (*handler.print_mvbdu stdout "" bdu*)
            Boolean_mvbdu.print_boolean_mvbdu error
              (Remanent_parameters.update_prefix parameter " ") bdu
          in
          ()
        ) bdu_array        
      in
      error
    ) parameter result

let print_iteration parameter error result =
  let _, store_bdu_array = result in
  print_bdu_array parameter error store_bdu_array
    

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "Succ_list\n";
    let _ = print_index_list parameter error result.store_succ_list in
    fprintf stdout "--------------------------------------------\n";
    print_iteration parameter error result.store_iteration
  in
  error
