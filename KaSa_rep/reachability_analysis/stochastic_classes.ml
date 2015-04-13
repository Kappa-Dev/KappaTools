(**
  * stochastic_classes.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of March
  * Last modification: 
  * 
  * Compute the relations between sites in an agent.
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic_classes") message exn
                 (fun () -> default)

let trace = false
              
let get_sites_list parameter error agent_type agent stochastic_class =
  let sites_list =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_list ->
       site :: current_list)
      agent.Cckappa_sig.agent_interface []
  in
  let error, old_list =
    Stochastic_classes_type.AgentMap.unsafe_get
      parameter
      error
      agent_type
      stochastic_class
  in
  let old_list =
    match old_list with
    | None -> []
    | Some old_list -> old_list
  in
  let new_list = List.concat [sites_list;old_list] in
  let error, stochastic_class =
    Stochastic_classes_type.AgentMap.set
      parameter
      error
      agent_type
      new_list
      stochastic_class
  in
  error, stochastic_class
             
let scan_rule parameter error handler rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let viewsrhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let creation = rule.Cckappa_sig.actions.Cckappa_sig.creation in
  let error,stochastic_class_rhs = Stochastic_classes_type.AgentMap.create parameter error 0 in
  (*creation*)
  let error, stochastic_class_rhs =
    List.fold_left
      (fun (error, stochastic_class_rhs) (agent_id, agent_type) ->
	let error, agent =
	  Stochastic_classes_type.AgentMap.get
	    parameter
	    error
	    agent_id
	    viewsrhs
	in
	match agent with
	  | None
	  | Some Cckappa_sig.Ghost -> error, stochastic_class_rhs
	  | Some Cckappa_sig.Agent agent ->
             get_sites_list parameter error agent_type agent stochastic_class_rhs
      ) (error, stochastic_class_rhs) creation
  in
  let error, stochastic_classes =
    Stochastic_classes_type.AgentMap.fold
      parameter error
      (fun parameter error agent_id agent stochastic_class ->
       match agent with
       | Cckappa_sig.Ghost -> error, stochastic_class
       | Cckappa_sig.Agent agent ->
	  let agent_type = agent.Cckappa_sig.agent_name in
          get_sites_list parameter error agent_type agent stochastic_class
      ) viewslhs stochastic_class_rhs
  in
  error,
  {
    Stochastic_classes_type.stochastic_class = stochastic_classes;
  }

let empty_stochastic_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, empty_stochastic = 
    Stochastic_classes_type.AgentMap.create parameter error n_agents in
  error, 
  {
    Stochastic_classes_type.stochastic_class = empty_stochastic;
  }

let get_nsites parameter error key handler =
  let error, get_nsites =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      key
      handler.Cckappa_sig.sites
  in
  let error, sites_dic =
    match get_nsites with
    | None -> warn parameter error (Some "line 141") Exit
                   (Ckappa_sig.Dictionary_of_sites.init())
    | Some dic -> error, dic
  in                  
  let error, nsites =
    Ckappa_sig.Dictionary_of_sites.last_entry
      parameter
      error
      sites_dic
  in
  nsites + 1
  
let scan_rule_set parameter error handler rules =
  let error, empty_stochastic_type = empty_stochastic_classes parameter error handler in
  let error, init_stochastic = Stochastic_classes_type.AgentMap.create parameter error 0 in
  (*map each agent to a stochastic classes*)
  let error, stochastic_class =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule stochastic_class ->
       let error, map =
         scan_rule
           parameter
           error
           handler
           rule.Cckappa_sig.e_rule_c_rule
           empty_stochastic_type
       in
       let error, result =
         Stochastic_classes_type.AgentMap.fold
           parameter error
           (fun parameter error id sites_list store_union ->
            let nsites = get_nsites parameter error id handler in
            match sites_list with
            | [] | _ :: [] -> error, store_union
            | _ ->
               let error,get_array =
                 Stochastic_classes_type.AgentMap.unsafe_get
                   parameter error id store_union
               in
               let array =
                 match get_array with
                 | None -> Union_find.create nsites
                 | Some a -> a
               in
               let error, union_array =
                 Union_find.union_list parameter error array sites_list
               in
               (*store*)
               let error, result =
                 Stochastic_classes_type.AgentMap.set
                   parameter
                   error
                   id
                   union_array
                   store_union
               in error, result
           )
           map.Stochastic_classes_type.stochastic_class
           stochastic_class
       in
       error, result
      ) rules init_stochastic
  in
  error, stochastic_class
    
let sprintf_array a =
  let acc = ref "[|" in
  Array.iteri (fun i x ->
      acc := !acc ^
             if i <> 0
             then Printf.sprintf "; %d" x
             else Printf.sprintf "%d" x
    ) a;
  !acc ^ "|]"
           
let print_array a =
  let output = sprintf_array a in
  Printf.fprintf stdout "%s\n" output
          
let print_result parameter error result =
  Stochastic_classes_type.AgentMap.print
    error
    (fun error parameter a ->
     let _ =
       print_string "site_type:";
       print_array a
     in
     error) parameter result
    
let stochastic_classes parameter error handler cc_compil =
  let parameter =  Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print_result parameter error result in
  error, result
