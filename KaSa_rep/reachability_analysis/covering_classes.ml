(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Compute the relations between the left hand site of a rule and its sites.
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Covering_classes_type
open Cckappa_sig
open Int_storage
open Site_map_and_set

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false
let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Covering_classes") message exn (fun () -> default) 
  
	      
(************************************************************************************)   
(*RULE*)

(*compute modified (actions) site*)

let collect_modified_map parameter error diff_reverse store_modified_map =
  Agent_id_quick_nearly_inf_Imperatif.fold parameter error
    (fun parameter error agent_id site_modif store_modified_map ->
      (*if there is no modified sites then do nothing*)
      if Map.is_empty
        site_modif.agent_interface
      then error, store_modified_map
      else
        let agent_type = site_modif.agent_name in
        let error',store_site =
          Map.fold
            (fun site port (error,current_map) ->
              (*store site map*)
              let error,site_map =
                Map.add
		  parameter
		  error 
                  site
                  site
                  current_map
              in
              error,site_map)
            site_modif.agent_interface (error,Map.empty)
        in
	let error = Exception.check warn parameter error error' (Some "line 58") Exit in 
        (*compute site_map*)
        let error, old_map =
          match
            Agent_type_quick_nearly_inf_Imperatif.unsafe_get
            parameter
            error
            agent_type
            store_modified_map
          with
            | error, None -> error, Map.empty
            | error, Some m -> error, m
        in
        (*store*)
        let error,final_map =
          Map.union parameter error 
            old_map
            store_site
        in
        let error', store_modified_map =
          Agent_type_quick_nearly_inf_Imperatif.set
            parameter
            error
            agent_type
            final_map
            store_modified_map
        in
	let error = Exception.check warn parameter error error' (Some "line 85") Exit in
 
        error, store_modified_map
    ) diff_reverse
    store_modified_map

(************************************************************************************)   
(*compute covering_class*)

let add_covering_class parameter error agent_type list store_covering_classes =
  match list with
    | [] -> error, store_covering_classes
    | _ ->
      let error, old_list =
        match 
          Agent_type_quick_nearly_inf_Imperatif.unsafe_get
            parameter 
            error
            agent_type
            store_covering_classes
        with
          | error, None -> error, []
          | error, Some sites -> error, sites
      in
      (* store the new list of covering classes *)
      let new_pair_list = (List.rev list) :: old_list in
      Agent_type_quick_nearly_inf_Imperatif.set 
        parameter
        error
        agent_type
        new_pair_list
        store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute covering classes, site test and bdu*)

let collect_covering_classes parameter error views diff_reverse store_covering_classes =
  let error, store_covering_classes =
    Cckappa_sig.Agent_id_quick_nearly_inf_Imperatif.fold2_common parameter error
      (fun parameter error agent_id agent site_modif store_covering_classes ->
        (* if in the interface there is no site modified then do nothing *)
        if Map.is_empty site_modif.agent_interface
        then error, store_covering_classes
        else
          match agent with
	  | Ghost | Unknown_agent _ -> error, store_covering_classes
	  | Dead_agent (agent,_,_,_) 
	  | Agent agent ->
              let agent_type = agent.agent_name in
              (*get a list of sites from an interface at each rule*)
              let site_list =
                Map.fold (fun site _ current_list ->
                  site :: current_list
                ) agent.agent_interface []
              in
              (*compute covering_class*)
              let error, covering_classes =
                add_covering_class
                  parameter
                  error
                  agent_type
                  site_list
                  store_covering_classes
              in
              (*store*)
              error, covering_classes
      ) views diff_reverse store_covering_classes
  in error, store_covering_classes
