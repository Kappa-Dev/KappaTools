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

(************************************************************************************)
(*PRINT*)   

let rec print_bdu_list handler l =
  match l with
    | [] -> ()
    | bdu :: tl ->
      let _ =
        fprintf stdout "element of list\n";
        handler.print_mvbdu stdout "" bdu
      in
      print_bdu_list handler tl

(*let print_bdu_list handler l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | bdu :: tl ->
        let _ =
          fprintf stdout "element of list\n";
          handler.print_mvbdu stdout "" bdu
        in
        aux tl
  in aux l*)

let print_wl handler wl =
  let in_list, out_list, pool = wl in
  fprintf stdout "In_list:\n";
  print_bdu_list handler in_list;
  fprintf stdout "Out_list:\n";
  print_bdu_list handler out_list;
  fprintf stdout "Pool:\n";
  Fifo.BduWlist.WSet.iter_set (fun elt ->
    let _ =
      handler.print_mvbdu stdout "" elt
    in
    ()
  ) pool

let print_creation parameter error result =
  AgentMap.print error (fun error parameter (l, (handler, bdu)) ->
    let _ =
      handler.print_mvbdu stdout "" bdu
    in
    error
  ) parameter result

let print_iteration parameter error result =
  let wl_lhs, iteration = result in
  let _ =
    AgentMap.print error (fun error parameter (_, handler, wl) ->
      let _ =
        print_wl handler wl
      in
      error
    ) parameter wl_lhs
  in
  let _ = fprintf stdout "BDU DIRECT\n" in
  AgentMap.print error (fun error parameter (handler, bdu) ->
    let _ =
      handler.print_mvbdu stdout "" bdu
    in
    error
  ) parameter iteration


(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU creation rules\n";
    print_creation parameter error result.store_creation;
    fprintf stdout "BDU iteration rules\n";
    print_iteration parameter error result.store_iteration
  in
    error
