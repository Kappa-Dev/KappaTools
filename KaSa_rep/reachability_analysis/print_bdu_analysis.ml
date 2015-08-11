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

let print_bdu_list handler l = (*REMOVE*)
  let rec aux acc =
    match acc with
      | [] -> ()
      | bdu :: tl ->
        let _ =
          handler.print_mvbdu stdout "" bdu
        in
        aux tl
  in aux l

let print_iteration parameter error result =
  AgentMap.print error (fun error parameter (handler, bdu) ->
    let _ = handler.print_mvbdu stdout "" bdu
    in
    error
  ) parameter result


(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU iteration rules\n";
    print_iteration parameter error result.store_iteration
  in
    error
