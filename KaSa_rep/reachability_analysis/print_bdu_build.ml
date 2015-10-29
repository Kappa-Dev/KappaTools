(**
  * print_bdu_build.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Bdu_analysis_type
open Set_and_map
open Cckappa_sig


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*restriction bdu_test*)

let rec print_list l =
  match l with
  | [] -> []
  | (s, state) :: tl ->
    fprintf stdout "site_type:%i:state:%i\n" s state;
    print_list tl
    
let print_restriction_bdu_test parameter error result =
  (*AgentMap.print error
    (fun error parameter map ->
      let _ =*)
        let rec aux acc =
          match acc with
          | [] -> []
          | h :: tl ->
            Site_map_and_set.iter_map (fun k port ->
              fprintf stdout "site:%i\n" k;
            ) h; aux tl
        in aux result
              
    (*  in
      error
    ) parameter result*)

(************************************************************************************)
(*main print*)

let print_bdu_build parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Build BDU:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  (*let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- BDU of test restriction:\n";
    print_restriction_bdu_test
      parameter
      error
      result.store_restriction_bdu_test
  in*)
  error
