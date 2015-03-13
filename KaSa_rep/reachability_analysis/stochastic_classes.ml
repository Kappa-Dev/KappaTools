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
              
let empty_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, stochastic_classes =
    Stochastic_classes_type.AgentMap.create parameter error n_agents in
  error,
  {
    Stochastic_classes_type.stochastic_classes = stochastic_classes
  }

let add_stochastic_class parameter error agent_type new_stochastic_class stochastic_classes =
  match new_stochastic_class with
  | [] -> error, stochastic_classes
  | _ ->
     let error, agent =
       Stochastic_classes_type.AgentMap.unsafe_get
         parameter
         error
         agent_type
         stochastic_classes
     in
     (*fetch the fomer list of stochastic classes*)
     let old_list =
       match agent with
       | None -> []
       | Some a -> a
     in
     (*store the new list of stochastic classes*)
     let new_list = (List.rev new_stochastic_class) :: old_list in
     (*add this new list into the result list*)
     Stochastic_classes_type.AgentMap.set
       parameter
       error
       agent_type
       new_list
       stochastic_classes                                                         

(*TODO*)
let scan_rule parameter error handler rule classes =
  let stochastic_classes = classes.Stochastic_classes_type.stochastic_classes in
  let error, stochastic_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error agent_id agent stochastic_classes ->
       (*TODO:?if the set of agent_interface is empty then do nothing*)
       match agent with
       | Cckappa_sig.Ghost -> error, stochastic_classes
       | Cckappa_sig.Agent agent ->
          (*compute a new class in the same interface; if there are 2
            site: site, site' are of the same set then they are equivance*)
          let new_stochastic_class =
            Cckappa_sig.Site_map_and_set.fold2_map
              parameter error
              (fun k site site' (error, current_class) ->
               (*test:create an empty array*)
               let empty_arr = Union_find.makeSets 0 in
               if Union_find.is_equivalence site site' empty_arr
               then
                 error, site :: current_class
               else
                 error, current_class
              )
              agent.Cckappa_sig.agent_interface
              agent.Cckappa_sig.agent_interface
              []
          in
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, stochastic_classes =
            add_stochastic_class
              parameter
              error
              agent_type
              new_stochastic_class
              stochastic_classes
          in
          error, stochastic_classes
      )
      rule stochastic_classes
  in
  error,
  {
    Stochastic_classes_type.stochastic_classes = stochastic_classes
  }   

let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  (*map each agent to this class*)
  let error, agent_map =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
       scan_rule
         parameter error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         classes
      )
      rules init
  in
  (*create a new init for result *)
  let error, init_result =
    Stochastic_classes_type.AgentMap.create parameter error 0 in
  let error, result =
    Stochastic_classes_type.AgentMap.fold
      parameter error
      (fun parameter eror id list init ->
       (* clean list?*)
       
      (*add id list into the init*)
         Stochastic_classes_type.AgentMap.set
         parameter error id list init
      )
      agent_map.Stochastic_classes_type.stochastic_classes
      init_result
  in
  error, result

let stochastic_classes parameter error handler cc_compil =
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  error, result
