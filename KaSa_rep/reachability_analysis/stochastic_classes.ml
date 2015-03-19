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

let rec print_list l =
  (*print_string "site_type:{";*)
  match l with
  | [] -> print_string "empty"
  | h :: [] ->  print_int h; print_string " "
  | h :: tl ->
     let _ = print_int h; print_string "," in
     print_list tl
                     
let rec print_list_list ls =
  match ls with
  | [] -> ()
  | h :: tl ->
     let _ = print_list h in
     print_list_list tl

let scan_rule parameter error handler rule classes =
  let viewlhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let stochastic_classes = classes.Stochastic_classes_type.stochastic_classes in
  let error, stochastic_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent stochastic_classes ->
       match agent with
       | Cckappa_sig.Ghost -> error, stochastic_classes
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*collect all the sites in an agent *)
          let sites_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_list ->
               site :: current_list)
              agent.Cckappa_sig.agent_interface []
          in
          (*compute a map from agent_type to sites_list*)
          let error, init =
            Stochastic_classes_type.AgentMap.create parameter error 0 in
          let error, sites_visited =
            Stochastic_classes_type.AgentMap.fold
              parameter error
              (fun parameter error _ _ _ ->
               Stochastic_classes_type.AgentMap.set
                 parameter
                 error
                 agent_type
                 sites_list
                 stochastic_classes
              )
              stochastic_classes
              init              
          in         
          (*compute the stochastic class by getting from the list of
            sites_visited and return the set of sites that are equivalence*)
          let error, get_sites_agent =
            Stochastic_classes_type.AgentMap.unsafe_get
              parameter
              error
              agent_type
              sites_visited
          in
          let get_sites =
            match get_sites_agent with
            | None -> []
            | Some sites -> sites
          in
          (*compute the equivalence of sites in that agent by using the
         union_find algorithm from a list of get_sites*)
          let error, stochastic_classes =
            match get_sites with
            | [] -> error, stochastic_classes
            | _ :: _ as l ->
               let new_equivalence_class =
                 Union_find.union_list l (Union_find.union_of_list get_sites)
               in
               (*convert this class from union_type into dicionary type*)
               let new_equivalence_dic =
                 Union_find.list_of_union new_equivalence_class
               in
               (*store this new class into the classes corresponding to its
               agent*)
               let error, stochastic_classes =
                 Stochastic_classes_type.AgentMap.set
                   parameter
                   error
                   agent_type
                   new_equivalence_dic
                   stochastic_classes
               in
               error, stochastic_classes
          in error, stochastic_classes
      ) viewlhs stochastic_classes
  in
  error,
  {
    Stochastic_classes_type.stochastic_classes = stochastic_classes
  }
    
let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  (*map each agent to a stochastic classes*)
  let error, agent_map =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
       scan_rule
         parameter
         error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         classes
      ) rules init
  in
  let error, init = Stochastic_classes_type.AgentMap.create
                      parameter error 0 in
  let error, result =
    Stochastic_classes_type.AgentMap.fold
      parameter
      error
      (fun parameter error id list init ->
       Stochastic_classes_type.AgentMap.set
         parameter error id list init)
      agent_map.Stochastic_classes_type.stochastic_classes
      init
  in
  error, result
  
let stochastic_classes parameter error handler cc_compil =
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print_string "START\n";
          Stochastic_classes_type.AgentMap.print
            error
            (fun error parameter l ->
             let _ = print_string "List:";
                     print_list l
             in
             let _ = print_newline() in
             error)
            parameter
            result
  in
  error, result
