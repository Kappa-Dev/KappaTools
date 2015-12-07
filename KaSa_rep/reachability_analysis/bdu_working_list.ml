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
(*working list content only creation rule_id: this is the initial working list*)

let collect_wl_creation parameter error rule_id rule store_result =
  (*add rule_id of creation inside a working list*)
  let error, wl_creation =
    List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
      let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
      match agent with
      | Some Dead_agent _
      | Some Unknown_agent _ 
      | None -> warn parameter error (Some "line 36") Exit store_result
      | Some Ghost -> error, store_result			     
      | Some Agent agent ->
        let error, wl = IntWL.push parameter error rule_id store_result in
        error, wl
    ) (error, store_result) rule.actions.creation
  in
  error, wl_creation

(*let error, wl_init =
    Int_storage.Nearly_inf_Imperatif.fold parameter error
      (fun parameter error rule_id rule store_result ->
        AgentMap.fold parameter error
          (fun parameter error agent_id agent store_result ->
            match agent with
            | Unknown_agent _ | Ghost -> error, store_result
            | Dead_agent (agent, _, _, _)
            | Agent agent ->
              (*push rule_id of initial state into working list*)
              let error, wl = IntWL.push parameter error rule_id store_result in
              error, wl
          ) rule.e_init_c_mixture.views store_result
      ) compil.init store_result
  in
  (*initial state will be adding into working list as well if it declared.*)
  let error, store_result =
    match IntWL.is_empty wl_creation, IntWL.is_empty wl_init with
    | true, true -> error, store_result
      (*if both are not given then return empty*)
    | true, false ->
      (*if creation is empty and init is not empty then return init*)
      error, wl_init
    | false, true ->
      (*if init is empty and creation is not then return creation*)
      error, wl_creation
    | false, false ->
      (*if both are existed then union them*)
      let error, wl_creation =
        List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
          let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
          match agent with
          | Some Dead_agent _
          | Some Unknown_agent _ 
          | None -> warn parameter error (Some "line 34") Exit store_result
          | Some Ghost -> error, store_result			     
          | Some Agent agent ->
            let error, wl = IntWL.push parameter error rule_id store_result in
            error, wl
        ) (error, store_result) rule.actions.creation
      in
      let error, wl_init =
        Int_storage.Nearly_inf_Imperatif.fold parameter error
          (fun parameter error rule_id rule store_result ->
            AgentMap.fold parameter error
              (fun parameter error agent_id agent store_result ->
                match agent with
                | Unknown_agent _ | Ghost -> error, store_result
                | Dead_agent (agent, _, _, _)
                | Agent agent ->
                  (*push rule_id of initial state into working list*)
                  let error, wl = IntWL.push parameter error rule_id store_result in
                  error, wl
              ) rule.e_init_c_mixture.views store_result
          ) compil.init wl_creation
      in
      error, wl_init
  in
  error, store_result*)

