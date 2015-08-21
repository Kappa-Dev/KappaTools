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

let rec print_bdu_list handler l =
  match l with
    | [] -> ()
    | bdu :: tl ->
      let _ =
        fprintf stdout "element of list\n";
        handler.print_mvbdu stdout "" bdu
      in
      print_bdu_list handler tl

(*let print_wl handler wl =
  let in_list, out_list, pool = wl in
  fprintf stdout "In_list:\n";
  print_bdu_list handler in_list
  (*fprintf stdout "Out_list:\n";
  print_bdu_list handler out_list;
  fprintf stdout "Pool:\n";
  Fifo.BduWlist.WSet.iter_set (fun elt ->
    let _ =
      handler.print_mvbdu stdout "" elt
    in
    ()
  ) pool*)

let print_creation parameter error result =
  AgentMap.print error (fun error parameter (l, (handler, bdu)) ->
    let _ =
      handler.print_mvbdu stdout "" bdu
    in
    error
  ) parameter result

let print_wl_lhs parameter error result =
  AgentMap.print error (fun error parameter (_, handler, wl) ->
    let _ =
      print_wl handler wl
    in
    error
  ) parameter result

let print_wl_direct parameter error result =
  AgentMap.print error (fun error parameter (_, handler, wl) ->
    let _ =
      print_wl handler wl
    in
    error
  ) parameter result

let print_iteration parameter error result =
  AgentMap.print error (fun error parameter (handler, set) ->
    let _ =
      Bdu_iterate.iter_set (fun elt ->
        let _ =
          handler.print_mvbdu stdout "" elt
        in
        ()
      ) set
    in
    error
  ) parameter result

let print_result parameter error result =
  let wl_lhs, wl_direct, iteration = result in
  (*let _ = fprintf stdout "BDU LHS\n" in
  let _ = print_wl_lhs parameter error wl_lhs in
  let _ = fprintf stdout "BDU DIRECT\n" in
  let _ = print_wl_direct parameter error wl_direct in*)
  let _ = fprintf stdout "BDU Iteration\n" in
  print_iteration parameter error iteration

let print_succ_wl parameter error result =
  AgentMap.print error (fun error parameter (_, handler, wl) ->
    let _ =
      (*let in_list, _, set = wl in*)
      print_wl handler wl
      (*Fifo.BduWlist.WSet.iter_set (fun bdu ->
        let _ =
          handler.print_mvbdu stdout "" bdu
        in
        ()
      ) set*)
    in
    error
  ) parameter result*)

let print_index_list parameter error store_result =
  let rec aux acc =
    match acc with
      | [] -> []
      | (i, i') :: tl ->
        let _ = fprintf stdout "succ: %i, %i\n" i i' in
        aux tl          
  in aux store_result

(*let print_in_wl wl =
  let in_list, _, _ = wl in
  let rec aux acc =
    match acc with
      | [] -> []
      | i :: tl ->
        fprintf stdout "%i\n" i ;
        aux tl
  in
  aux in_list*)

let rec print_wl_list l =
  match l with
    | [] -> []
    | wl :: tl ->
      IntWL.print_wl wl;
      print_wl_list tl
        
let print_iteration parameter error result =
  AgentMap.print error 
    (fun error parameter (handler, bdu_array) ->
      let _ =
        Array.iter (fun bdu ->
          let _ =
            handler.print_mvbdu stdout "" bdu
          in
          ()
        ) bdu_array        
      in
      error
    ) parameter result

let print_array handler bdu_array =
  Array.iter (fun bdu ->
    let _ =
      handler.print_mvbdu stdout "" bdu
    in
    ()
  ) bdu_array        
    

(************************************************************************************)
(*MAIN PRINT*)

let rec add parameter error wl_list succ_list =
  match wl_list, succ_list with
    | [], [] | _, [] -> []
    | [], (i, i') :: tl -> 
      let error, wl =
        IntWL.push parameter error i' IntWL.empty in
      wl :: add parameter error [] tl
    | wl :: tl, (i,i') :: tl' ->
      let in_list, _, _ = wl in
      let rec aux acc =
        match acc with
          | [] -> []
          | r :: t -> 
            if r = i
            then
              let error, wlx =
                IntWL.push parameter error i' wl in
              let d = wlx :: add parameter error tl tl' in
              List.concat [d; aux t]
            else 
              List.concat [add parameter error tl tl' ; aux t]
      in aux in_list

let print_result parameter error result =
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "Succ_list\n";
      print_index_list parameter error result.store_succ_list;
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "rule wl\n";
    print_wl_list result.store_rule_wl;
    let wl =
      add parameter error result.store_rule_wl result.store_succ_list
    in
    fprintf stdout "rule add wl\n";
    print_wl_list wl;
    fprintf stdout "--------------------------------------------\n";
    print_iteration parameter error result.store_iteration
  in
  error
