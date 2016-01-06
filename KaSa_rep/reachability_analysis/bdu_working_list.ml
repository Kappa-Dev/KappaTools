(**
  * bdu_working_list.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Fifo
open Bdu_analysis_type
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

(*----------------------------------------------------------------------------*)
(*working list content only creation rule_id: this is the initial working list, 
  this creation has no lhs.
  Only this case:
  'r1' -> A(x,y)
  will add 'r1' into a working list.

  - There is case when it has a lhs and created a new agent. 
    For instance:
    'r1' A(x,y) -> D(x,y)
  will also add 'r1' into a working list. (lhs of D is Ghost. Rhs of A is Ghost)
*)

(*CHECK ME*)

let collect_wl_creation parameter error rule_id rule store_result =
  (*add rule_id that has no lhs into a working list*)
  let error, wl_creation =
    List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
      let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
      match agent with
      | Some Dead_agent _ | Some Ghost  -> error, store_result
      | None ->  warn parameter error (Some "line 45") Exit store_result
      | Some Unknown_agent _
      | Some Agent _ -> 
        let error, wl = IntWL.push parameter error rule_id store_result in
        error, wl
    ) (error, store_result) rule.actions.creation
  in
  error, wl_creation

(************************************************************************************)
(*working list of initial state*)

let collect_wl_init_creation parameter handler error store_bdu_init_restriction_map 
    store_covering_classes_modification_update_full wl_creation =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error
  in
  (*-----------------------------------------------------------------------*)
  let add_link handler (agent_type, cv_id) bdu store_result =
    let bdu_old =
      Map_bdu_update.Map.find_default bdu_false (agent_type, cv_id) store_result
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
    in
    let result_map =
      Map_bdu_update.Map.add (agent_type, cv_id) bdu_union store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------------*)
  let error, store_bdu_fixpoint_init_map =
    Map_bdu_update.Map.fold
      (fun (agent_type, cv_id) bdu (error, store_result) ->
        let error, store_result =
          add_link handler (agent_type, cv_id) bdu store_result
        in
        error, store_result
      )
      store_bdu_init_restriction_map
      (error, Map_bdu_update.Map.empty)
  in
  (*-----------------------------------------------------------------------*)
  (*add update(c) into working list*)
  let error, wl_init_creation =
    Map_bdu_update.Map.fold
      (fun (agent_type, cv_id) _ (error, wl_init_creation) ->
        Bdu_fixpoint_iteration.add_update_to_wl
          parameter
          error
          agent_type
          cv_id
          store_covering_classes_modification_update_full
          wl_init_creation
      ) store_bdu_fixpoint_init_map
      (error, wl_creation)
  in
  error, wl_init_creation
