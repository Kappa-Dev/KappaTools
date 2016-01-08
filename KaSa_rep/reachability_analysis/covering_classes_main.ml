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
open Print_covering_classes
open Covering_classes
open Covering_classes_new_index
open Site_map_and_set
open Clean_covering_classes

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*compute covering class: it is a covering class whenever there is a
  modified site in that agent. (CHECK on their left-hand side)

  For example: A(x~u,y~u) -> A(x~u,y~p) (where y is a modified site), then
  there is one covering class for agent A: CV_1: (x,y)
  
  - If the rule is: A(x~u), A(y~u) -> A(x~p), A(y~p), (x,y are modified
  sites), then agent A has two covering classes: CV_1: x; CV_2: y
  
  - If the rule is: A(x~u), A(y~u) -> A(x~u), A(y~p), (y is a modified
  site), then agent A has only one covering class: CV_1: y
*)

let scan_rule_covering_classes parameter error handler rule classes =
  (*------------------------------------------------------------------------------*)
  (*compute modified map*)
  let error, store_modified_map =
    collect_modified_map
      parameter
      error
      rule.diff_reverse
      classes.store_modified_map
  in
  (*------------------------------------------------------------------------------*)
  (*compute covering_class*)
  let error, store_covering_classes =
    collect_covering_classes parameter error
      rule.rule_lhs.views
      rule.diff_reverse
      classes.store_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    store_modified_map     = store_modified_map;
    store_covering_classes = store_covering_classes;
  }           

(************************************************************************************)   
(*RULES*)

let scan_rule_set_covering_classes parameter error handler rules =
  let n_agents = handler.nagents in
  let error, init_modif_map =  AgentMap.create parameter error n_agents in
  let error, init_class     =  AgentMap.create parameter error n_agents in
  (*------------------------------------------------------------------------------*)
  (* add each singleton as a covering class *)
  let error, init_class = 
    Int_storage.Nearly_inf_Imperatif.fold
      parameter 
      error
      (fun parameters error a b init_class -> 
	let error,bool,init_class = 
	  Ckappa_sig.Dictionary_of_sites.fold 
	    (fun _ _ b (error,bool,init_class) -> 
	      let error,l' = 
		match 
		  AgentMap.unsafe_get parameters error a init_class
		with 
		| error,None -> error,[[b]]
		| error,Some l -> error,[b]::l
	      in 
	      let error,map = AgentMap.set parameters error a l' init_class in 
	      error,true,map)
	    b (error,false,init_class)
	in 
	if bool then error,init_class
	else 
	  (* add an empty covering class for the agents without sites *)
	  AgentMap.set parameters error a [[]] init_class
      )
      handler.Cckappa_sig.sites
      init_class
  in 
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      store_modified_map     = init_modif_map;
      store_covering_classes = init_class;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a list of covering classes*)
  let error, store_covering_classes =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        let error, result =
          scan_rule_covering_classes
            parameter
            error
            handler
            rule.e_rule_c_rule
            classes
        in
        error, result
      ) rules init_class
  in
  error, store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute covering classes in the set of rules*)

let scan_rule_set_remanent parameter error handler rules =
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = AgentMap.create parameter error 0 in
  let error, store_covering_classes = 
    scan_rule_set_covering_classes parameter error handler rules
  in
  let result_covering_classes = store_covering_classes.store_covering_classes in
  let error, remanent_dictionary =
    AgentMap.fold parameter error
      (fun parameters error agent_type covering_class init_remanent ->
        (*------------------------------------------------------------------------------*)
        (*get modified site*)
        let error, modified_map =
          match
            Quick_Nearly_inf_Imperatif.unsafe_get
              parameter 
              error 
              agent_type
              store_covering_classes.store_modified_map
          with
            | error, None -> error, Map.empty
            | error, Some m -> error, m
        in
        (*------------------------------------------------------------------------------*)
        (*clean the covering classes, removed duplicate of covering classes*)
        let error, store_remanent_dic =
          clean_classes
            parameters 
            error
            covering_class
            modified_map
        in
        (*------------------------------------------------------------------------------*)
        (*compute the number of covering classes*)
        let error, get_number_cv =
          (Dictionary_of_Covering_class.last_entry parameter error
            store_remanent_dic.Covering_classes_type.store_dic)
        in
        let number_cv = get_number_cv + 1 in
        (*------------------------------------------------------------------------------*)
        (*print covering classes*)
        let _ =
          if Remanent_parameters.get_dump_site_dependencies parameter
          then
            let parameter =
              Remanent_parameters.update_prefix parameter ""
            in
            let error, agent_string =
              Handler.string_of_agent parameter error handler agent_type
            in
            let _ =
              Dictionary_of_Covering_class.print parameter error
                (fun parameter error elt_id site_type_list _ _ ->
                  let _ =
                    Printf.fprintf stdout 
                      "Potential dependencies between sites:Number of covering classes:%i\n"
                      number_cv
                  in
                  let _ =
                    (*print covering_class_id*)
                    Printf.fprintf stdout "Potential dependencies between sites:\nagent_type:%i:%s:covering_class_id:%i\n"
                      agent_type agent_string elt_id
                  in
                  let _ =
                    List.iter (fun site_type ->
                      let error, site_string =
                        Handler.string_of_site parameter error handler agent_type site_type
                      in
                      Printf.fprintf stdout "site_type:%i:%s\n" site_type site_string
                    ) site_type_list
                  in
                  error
                ) store_remanent_dic.Covering_classes_type.store_dic
            in
            ()
        in
        (*------------------------------------------------------------------------------*)
        (*store the covering classes after cleaning theirs duplicate classes*)
        let error, store_remanent =
          AgentMap.set 
            parameters
            error
            agent_type
            store_remanent_dic
            init_remanent
        in
        (*------------------------------------------------------------------------------*)
        (*result*)
        error, store_remanent
      )
      result_covering_classes
      init_result
  in
  error, remanent_dictionary

(************************************************************************************)   
(*MAIN*)

let covering_classes parameter error handler cc_compil =
  let error, init = AgentMap.create parameter error 0 in
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result = scan_rule_set_remanent parameter error handler cc_compil.rules in
  (*convert a list of site inside result [remanent] to a set of site*)
  let error =
    if (Remanent_parameters.get_trace parameter) || trace
    then
      let error, covering_classes_set2list =
        Covering_classes_list2set.collect_remanent_list2set parameter error handler result
      in
      error
    else
      error
  in
  error, result
