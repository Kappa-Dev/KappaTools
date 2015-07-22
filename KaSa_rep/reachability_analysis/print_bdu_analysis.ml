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

(************************************************************************************)
(*PRINT*)
      
let print_half_break parameter error result =
  AgentMap.print error
    (fun error parameter (l, (handler, bdu)) ->
      let _ =
	let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	fprintf stdout "HALF_BREAK rules\n";
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (s, st) :: tl ->
	      fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	in
	aux l
      in
      error
    ) parameter result
   
let print_remove parameter error result =
  let (result_know, result_undoc, result_remove) = result in
  (*let _ =
    AgentMap.print error
      (fun error parameter (l, (handler, bdu)) ->
        let _ =
	  let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	  fprintf stdout "REMOVE KNOW SITE rules\n";
	  let rec aux acc =
	    match acc with
	      | [] -> ()
	      | (s,st) :: tl ->
	        fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	  in
	  aux l
        in
        error
      ) parameter result_know
  in
  let _ = 
    AgentMap.print error
    (fun error parameter (l, (handler, bdu)) ->
      let _ =
	let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	fprintf stdout "REMOVE UNDOCUMENT SITE rules\n";
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (s,st) :: tl ->
	      fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	in
	aux l
      in
      error
    ) parameter result_undoc
  in*)
  AgentMap.print error
    (fun error parameter (handler, bdu) ->
      let _ = handler.Memo_sig.print_mvbdu stdout "" bdu
      in
      error
    ) parameter result_remove
  
    
let print_test parameter error result =
  AgentMap.print error
    (fun error parameter (l, (handler, bdu)) ->
      let _ =
	let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	fprintf stdout "TEST rules\n";
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (s, st) :: tl ->
	      fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	in
	aux l
      in
      error
    ) parameter result
    

let print_iteration parameter error result =
  AgentMap.print error
    (fun error parameter (handler, bdu) ->
      let _ =
	handler.Memo_sig.print_mvbdu stdout "" bdu
      in
      error
    ) parameter result

let print_pair_iteration parameter error result =
  AgentMap.print error
    (fun error parameter (_, (handler, bdu)) ->
      let _ =
	handler.Memo_sig.print_mvbdu stdout "" bdu
      in
      error
    ) parameter result

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  (*let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU TEST rules\n";
    print_test parameter error result.store_test
  in*)
  (*let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU ITERATION rules\n";
    print_pair_iteration parameter error result.store_iteration
  in*)
  (*
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU HALF_BREAK rules\n";
    print_half_break parameter error result.store_half_break
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU REMOVE rules\n";
    print_remove parameter error result.store_remove
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU TEST_MODIFICATION rules\n";
    print_test_modif parameter error result.store_test_modif
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATION OF CREATION - COVERING CLASS rules\n";
    print_iteration parameter error result.store_iterate_created_cv
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATION OF HALF_BREAK - COVERING CLASS rules\n";
    print_iteration parameter error result.store_iterate_half_cv
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATION OF REMOVE - COVERING CLASS rules\n";
    print_iteration parameter error result.store_iterate_remove_cv
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATION OF HALF_BREAK - REMOVE - COVERING CLASS rules\n";
    print_iteration parameter error result.store_iterate_half_remove_cv
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATION OF HALF_BREAK - CREATION - COVERING CLASS rules\n";
    print_iteration parameter error result.store_iterate_half_created_cv
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATION OF HALF_BREAK - REMOVE - CREATION - COVERING CLASS rules\n";
    print_iteration parameter error result.store_iterate_half_remove_created_cv
  in*)
  error
