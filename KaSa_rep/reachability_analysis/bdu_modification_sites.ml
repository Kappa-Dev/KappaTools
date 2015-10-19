(**
  * bdu_side_effects.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 30th of September
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Bdu_analysis_type
open Cckappa_sig
open Int_storage
open Covering_classes_type
open Covering_classes

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU modification sites") message exn (fun () -> default)

let trace = false

(************************************************************************************)
(* return a list of rule_id has sites that are modified.
   For example:
   'r0' A() ->
   'r1' A(x) ->
   'r2' A(x!B.x) -> A(x)
   'r3' A(x), B(x) -> A(x!1), B(x!1)
   'r4' A(x,y), C(x) -> A(x, y!1), C(x!1)

   result:
   - A(x): [r2; r3]
   - A(y): [r4]
   - B(x): [r3]
   - C(x): [r4]
*)

let collect_modification_sites parameter error rule_id diff_direct store_result =
  (*from a pair of Map (agent_type, site) -> rule_id :: old_result)*)
  let add_link (agent_type, site_type) rule_id store_result =
    let l, old =
      try Int2Map_Modif.find (agent_type, site_type) store_result
      with Not_found -> [], []
    in
    Int2Map_Modif.add (agent_type, site_type) (l, rule_id :: old) store_result
  in
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent_modif store_result ->
        if Site_map_and_set.is_empty_map agent_modif.agent_interface
        then error, store_result
        else
          let agent_type = agent_modif.agent_name in
          (*return*)
          let store_result =
            Site_map_and_set.fold_map
              (fun site_type _ store_result ->
                let store_result =
                  add_link (agent_type, site_type) rule_id store_result
                in
                store_result
              ) agent_modif.agent_interface store_result
          in
          error, store_result
      ) diff_direct store_result
  in
  let store_result =
    Int2Map_Modif.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*a pair (agent_type_cv, site_cv) in covering classes
  return a list of covering_classes_id*)

let site_covering_classes parameter error covering_classes (*store_result*) =
  let add_link (agent_type, site_type) cv_id store_result =
    let l, old =
      try Int2Map_CV.find (agent_type, site_type) store_result
      with Not_found -> [], []
    in
    (*add the fresh signature into the old result and store them*)
    Int2Map_CV.add (agent_type, site_type) (l, cv_id :: old) store_result
  in
  let error, store_result =
    (*From sites return a list of covering_class_id*)
    AgentMap.fold parameter error
      (fun parameter error agent_type_cv remanent store_result ->
        (*get a list of covering_class_id from remanent*)
        let cv_dic = remanent.store_dic in
        (*fold a dictionary*)
        let store_result =
          Dictionary_of_Covering_class.fold
            (fun value_list ((),()) cv_id store_result ->
              (*get site_cv in value*)
              List.fold_left (fun store_result site_type_cv ->
                add_link (agent_type_cv, site_type_cv) cv_id store_result
              ) store_result value_list
            ) cv_dic store_result
        in
        error, store_result
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) covering_classes Int2Map_CV.empty
  in
  let store_result =
    Int2Map_CV.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result
