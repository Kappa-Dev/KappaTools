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

let print_triple_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id, _, set) :: tl ->
      fprintf stdout "Covering_class_id:%i\n" id;
      Site_map_and_set.iter_set (fun site ->
        fprintf stdout "site_type:%i\n" site
      ) set; aux tl
  in aux l

let print_remanent_test parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
        print_triple_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*restriction of test rules*)

let print_pair_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id, m) :: tl ->
      fprintf stdout "Covering_class_id:%i\n" id;
      Site_map_and_set.iter_map (fun site state ->
        fprintf stdout "site':%i:state:%i\n" site state
      ) m; aux tl
  in aux l

let print_test_restriction parameter error result =
  AgentMap.print error
    (fun error parameter pair_list ->
      let _ =
        print_pair_list pair_list
      in
      error
    ) parameter result

(************************************************************************************)
(*bdu_test*)

let print_bdu parameter error bdu =
  Boolean_mvbdu.print_boolean_mvbdu error
    (Remanent_parameters.update_prefix parameter "") bdu

let print_site_state_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (site, state) :: tl ->
      fprintf stdout "site':%i:state:%i\n" site state;
      aux tl
  in aux l

let print_bdu_list parameter error l l' =
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | rule_id :: tl, (l, id, (handler, bdu_test)) :: tl' ->
      fprintf stdout "rule_id:%i\n" rule_id;
      fprintf stdout "Covering_class_id:%i\n" id;
      let _ = print_site_state_list l in
      fprintf stdout "\n";
      let _ = print_bdu parameter error bdu_test in
      fprintf stdout "\n";
      aux tl tl'
  in aux l l'

let print_bdu_test parameter error result =
  AgentMap.print error
    (fun error parameter (rule_id_list, triple_list) ->
      let _ =
        print_bdu_list parameter error rule_id_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*remanent creation*)

let print_remanent_creation parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
	print_triple_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*remanent modification*)
    
let print_remanent_modif parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
	print_triple_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*modification restriction*)

let print_modif_restriction parameter error result =
  AgentMap.print error
    (fun error parameter pair_list ->
      let _ =
	print_pair_list pair_list
      in
      error
    ) parameter result
    
(************************************************************************************)
(*modification restriction list*)

let print_pair_modif l l' =
  let rec aux acc acc' =
    match acc, acc' with
      | [], [] | _, [] | [], _ -> []
      | rule_id :: tl, site :: tl' ->
	fprintf stdout "rule_id:%i\n" rule_id;
	fprintf stdout "site_type':%i\n" site;
	aux tl tl'
  in
  aux l l'
    
let print_modif_restriction_list parameter error result =
  AgentMap.print error
    (fun error parameter (rule_id_list, site_list) ->
      let _ =
	print_pair_modif rule_id_list site_list
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
      "- Covering classes of test:\n";
    print_remanent_test
      parameter
      error
      result.store_remanent_test
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes of test restriction:\n";
    print_test_restriction
      parameter
      error
      result.store_test_restriction
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Bdu of test restriction:\n";
    print_bdu_test
      parameter
      error
      result.store_bdu_test
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Creation restriction:\n";
    print_remanent_creation
      parameter
      error
      result.store_remanent_creation
  in
   let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes of modification:\n";
    print_remanent_modif
      parameter
      error
      result.store_remanent_modif
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes of modification restriction:\n";
    print_modif_restriction
      parameter
      error
      result.store_modif_restriction
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Modification restriction list:\n";
    print_modif_restriction_list
      parameter
      error
      result.store_modif_restriction_list
  in
  error
