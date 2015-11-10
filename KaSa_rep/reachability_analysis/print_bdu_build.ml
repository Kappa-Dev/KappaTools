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
open SetMap
open Cckappa_sig
open Remanent_parameters_sig

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
      Site_map_and_set.Set.iter (fun site ->
        fprintf stdout "site_type:%i\n" site
      ) set; aux tl
  in aux l

let print_remanent_triple parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
        print_triple_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*restriction of test rules*)

let print_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id, site, state) :: tl ->
      fprintf stdout "covering_class_id:%i:site_type':%i:state:%i\n" id site state;
      aux tl
  in aux l

let print_pair_list parameter p =
  let rec aux acc =
    match acc with
    | [] -> []
    | (rule_id, l) :: tl ->
      fprintf stdout "rule_id:%i:\n" rule_id; print_list l;
      aux tl
  in
  aux p

let print_remanent_test parameter error result =
  AgentMap.print error
    (fun error parameter pair_list ->
      let _ =
        print_pair_list parameter pair_list
      in
      error
    ) parameter result

(************************************************************************************)

let print_remanent_creation parameter error result =
  AgentMap.print error
    (fun error parameter pair_list ->
      let _ =
        print_pair_list pair_list
      in
      error
    ) parameter result

(************************************************************************************)

let print_remanent_modif parameter error result =
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
   
(************************************************************************************)
(*main print*)

let print_bdu_build parameter error result =
  (*print if one would like to test*)
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Taking information of covering class:\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes triple (id, list, set):\n";
    print_remanent_triple
      parameter
      error
      result.store_remanent_triple
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes fourth (rule_id, cv_id, site', state) of test rule:\n";
    print_remanent_test
      parameter
      error
      result.store_remanent_test
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes fourth (rule_id, cv_id, site', state) of creation rule:\n";
    print_remanent_creation
      parameter
      error
      result.store_remanent_creation
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- Covering classes fourth (rule_id, cv_id, site', state) of modification rule:\n";
    print_remanent_modif
      parameter
      error
      result.store_remanent_modif
  in
  error
