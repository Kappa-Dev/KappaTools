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

(*REMOVE*)
let collect_modification_sites parameter error rule_id diff_direct store_result =
  (*from a pair of Map (agent_type, site) -> rule_id :: old_result)*)
  let add_link (agent_type, site_type) rule_id store_result =
    let (l, old) =
      Int2Map_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, site_type) store_result in
    let current_set =
      Site_map_and_set.Set.add rule_id old
    in
    let new_set =
      Site_map_and_set.Set.union current_set old
    in
    error,
    Int2Map_Modif.Map.add (agent_type, site_type) (l, new_set) store_result
  in
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent_modif store_result ->
        if Site_map_and_set.Map.is_empty agent_modif.agent_interface
        then error, store_result
        else
          let agent_type = agent_modif.agent_name in
          (*return*)
          let error, store_result =
            Site_map_and_set.Map.fold
              (fun site_type _ (error, store_result) ->
                let error, store_result =
                  add_link (agent_type, site_type) rule_id store_result
                in
                error, store_result
              ) agent_modif.agent_interface (error, store_result)
          in
          error, store_result
      ) diff_direct store_result
  in
  let store_result =
    Int2Map_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*collect a set of rule_id of test rule and modification *)

let collect_test_sites parameter error rule_id viewslhs 
    store_result =
  let add_link (agent_type, site_type) rule_id store_result =
    let (l, old) =
      Int2Map_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, site_type) store_result in
    let current_set = Site_map_and_set.Set.add rule_id old in
    let new_set = Site_map_and_set.Set.union current_set old  in
    let result =
      Int2Map_Modif.Map.add (agent_type, site_type) (l, new_set) store_result
    in
    error, result
  in
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent store_result ->
       match agent with
       | Ghost -> error, store_result
       | Agent agent ->
         let agent_type = agent.agent_name in
         let error, store_result_test =
           Site_map_and_set.Map.fold
             (fun site_type _ (error, store_result) ->
               let error, store_result_test =
                 add_link (agent_type, site_type) rule_id store_result
               in
               error, store_result_test
             ) agent.agent_interface (error, store_result)
         in
         error, store_result_test
      ) viewslhs store_result
  in
  let store_result =
    Int2Map_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*TODO: modification and test rule that has rule_id union together.
For example:
modification: agent_type:0:site_type:0:[5;6]
test: agent_type:0:site_type:0:[4;5;6;7]
=> result: agent_type:0:site_type:0:[4;5;6;7]
*)

(*let collect_test_modification_sites'
    parameter error store_modification_map store_test_map store_result =
  let add_link (agent_type, site_type) rule_id_set store_result =
    let (l, old) =
      Int2Map_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, site_type) store_result in
    let result =
      Int2Map_Modif.Map.add (agent_type, site_type) (l, rule_id_set) store_result
    in
    error, result
  in
  Int2Map_Modif.Map.fold2z_with_logs
    (fun parameter error str1 str_opt1 exn1 ->
      let error, ex = warn parameter error str_opt1 exn1 Not_found in
      error
    )
    parameter error
    (fun parameter error (agent_type, site_type) (l1, s1) (l2, s2) store_result ->
      let union = Site_map_and_set.Set.union s1 s2 in
      let error, store_result =
        add_link (agent_type, site_type) union store_result
      in
      error, store_result
    ) store_modification_map store_test_map store_result*)

(*TEST*)

let collect_test_modification_sites
    parameter error store_modification_map store_test_map store_result =
  let add_link (agent_type, site_type) rule_id_set store_result =
    let (l, old) =
      Int2Map_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, site_type) store_result in
    let result =
      Int2Map_Modif.Map.add (agent_type, site_type) (l, rule_id_set) store_result
    in
    error, result
  in  
  Int2Map_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found in
      error
    )
    parameter error
    (*exists in 'a t*)
    (fun parameter error (agent_type, site_type) (l1, s1) store_result ->
      let error, store_result =
        add_link (agent_type, site_type) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, site_type) (l2, s2) store_result ->
      let error, store_result =
        add_link (agent_type, site_type) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, site_type) (l1, s1) (l2, s2) store_result ->
      let union = Site_map_and_set.Set.union s1 s2 in
      let error, store_result =
        add_link (agent_type, site_type) union store_result
      in
      error, store_result
    ) store_modification_map store_test_map store_result

(************************************************************************************)
(*creation rule_id*)

(*let collect_creation_sites parameter error rule_id viewsrhs creation store_result =
  (*map (agent, site) -> rule_id list*)
  let add_link (agent_type, site_type) rule_id store_result =
    let (l, old) =
      Int2Map_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, site_type) store_result in
    let current_set =
      Site_map_and_set.Set.add rule_id old
    in
    let new_set =
      Site_map_and_set.Set.union current_set old
    in
    let store_result =
      Int2Map_Modif.Map.add
	(agent_type, site_type) (l, new_set) store_result
    in
    error, store_result
  in
  let error, store_result =
    List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
      let error, agent = AgentMap.get parameter error agent_id viewsrhs in
      match agent with
	| None -> warn parameter error (Some "line 29") Exit store_result
	| Some Ghost -> error, store_result
	| Some Agent agent ->
	  let error, store_result =
	    Site_map_and_set.Map.fold
	      (fun site _ (error, store_result) ->
		let error, store_result =
		  add_link (agent_type, site) rule_id store_result
		in
		error, store_result
	      ) agent.agent_interface (error, store_result)
	  in
	  error, store_result
    ) (error, store_result) creation
  in
  let store_result =
    Int2Map_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result*)
    
(************************************************************************************)
(*the modification sites if it discover there is a creation rule then
  do not add this rule into the result*)

(*let collect_modification_sites_without_creation parameter error
    rule_id diff_direct store_creation_sites store_result =
  (*map (agent, site) -> rule_id list*)
  let add_link (agent_type, site_type) rule_id store_result =
    let (l, old) =
      Int2Map_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, site_type) store_result in
    let current_set =
      Site_map_and_set.Set.add rule_id old in
    let new_set =
      Site_map_and_set.Set.union current_set old in
    let store_result =
      Int2Map_Modif.Map.add (agent_type, site_type) (l, new_set) store_result
    in
    error, store_result
  in
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent_modif store_result ->
	if Site_map_and_set.Map.is_empty agent_modif.agent_interface
	then error, store_result
	else
	  let agent_type = agent_modif.agent_name in
	  let error, store_result =
	    Site_map_and_set.Map.fold
	      (fun site_type _ (error, store_result) ->
		let error, store_result =
		  if Int2Map_Modif.Map.is_empty store_creation_sites
		  then
		    let error, store_result =
		      add_link (agent_type, site_type) rule_id store_result
		    in
		    error, store_result
		  else
		  (*if agent_type, site_type of creation and rule_id is inside
		    the result then remove them*)
		  Int2Map_Modif.Map.fold (fun (agent_type', site_type') (l1, s2)
		    (error, store_result) ->
		      if Site_map_and_set.Set.mem rule_id s2
		      then
			let store_result =
			  Int2Map_Modif.Map.remove
			    (agent_type', site_type') store_result
			in
			error, store_result
		      else
			let error, store_result =
			  add_link (agent_type, site_type) rule_id store_result
			in
			error, store_result
		  ) store_creation_sites (error, store_result)
		in
		error, store_result
	      ) agent_modif.agent_interface (error, store_result)
	  in
	  error, store_result
      ) diff_direct store_result
  in
  let store_result =
    Int2Map_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result*)

(************************************************************************************)
(*test + modification without creation *)

(*let collect_test_modification_without_creation parameter error 
    store_modification_without_creation store_test_sites =
  Int2Map_Modif.Map.merge store_modification_without_creation
    store_test_sites*)

(************************************************************************************)
(*a pair (agent_type_cv, site_cv) in covering classes
  return a list of covering_classes_id*)

let site_covering_classes parameter error covering_classes (*store_result*) =
  let add_link (agent_type, site_type) cv_id store_result =
    let (l, old) =
      Int2Map_CV.Map.find_default ([],[]) (agent_type, site_type) store_result in
    (*add the fresh signature into the old result and store them*)
    let result =
      Int2Map_CV.Map.add
        (agent_type, site_type) (l, cv_id :: old) store_result
    in error, result
  in
  let error, store_result =
    (*From sites return a list of covering_class_id*)
    AgentMap.fold parameter error
      (fun parameter error agent_type_cv remanent store_result ->
        (*get a list of covering_class_id from remanent*)
        let cv_dic = remanent.store_dic in
        (*fold a dictionary*)
        let error, store_result =
          Dictionary_of_Covering_class.fold
            (fun value_list ((),()) cv_id (error, store_result) ->
              (*get site_cv in value*)
              List.fold_left (fun (error, store_result) site_type_cv ->
                let error, result =
                  add_link (agent_type_cv, site_type_cv) cv_id store_result
                in 
                error, result
              ) (error, store_result) value_list
            ) cv_dic (error, store_result)
        in
        error, store_result
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) covering_classes Int2Map_CV.Map.empty
  in
  let store_result =
    Int2Map_CV.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result
