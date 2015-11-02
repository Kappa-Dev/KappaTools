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

let print_list_restriction result =
  let rec aux acc =
    match acc with
    | [] -> []
    | map :: tl ->
      Int2Map_Modif.iter_map (fun (agent_type, site) (l1, set_state) ->
        if l1 <> []
        then ()
        else ();
        Site_map_and_set.iter_set (fun state -> 
          fprintf stdout "site:%i:state:%i\n" site state
        ) set_state;
      ) map;
      aux tl
  in
  aux result

(*let print_with_id result =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id, map) :: tl ->
      fprintf stdout "Covering_class_id:%i\n" id;
      let _ =
        print_list_restriction map
      in aux tl
  in aux result*)

let print_restriction parameter error result =
  AgentMap.print error
    (fun error parameter l ->
      let _ =
        print_list_restriction l
      in
      error
    ) parameter result
              
(*TEST*)
(*let print_map l l' =
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | id :: tl, map :: tl' ->
      fprintf stdout "Covering_class_id:%i\n" id;
      Site_map_and_set.iter_map (fun site state ->
        fprintf stdout "site:%i:state:%i\n" site state
      ) map; aux tl tl'
  in
  aux l l'*)

let print_map parameter error l l' =
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | id :: tl, (site, state) :: tl' ->
      fprintf stdout "Covering_class_id:%i\n" id;
      fprintf stdout "site:%i:state:%i\n" site state;
      aux tl tl'
  in
  aux l l'

let print_list_pair parameter error result =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id_list, l) :: tl ->
      print_map parameter error id_list l;
      aux tl
  in
  aux result

let print_test parameter error result =
  AgentMap.print error
    (fun error parameter l ->
      let _ =
        print_list_pair parameter error l
      in
      error
    ) parameter result

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
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- BDU of test restriction:\n";
    print_restriction
      parameter
      error
      result.store_restriction_bdu_test
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- BDU of test restriction:\n";
    print_test
      parameter
      error
      result.store_test
  in
  error
