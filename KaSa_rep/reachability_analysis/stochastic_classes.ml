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
  match l with
  | [] -> print_string "empty"
  | h :: [] ->  print_int h; print_string "}"
  | h :: tl ->
     let _ = print_int h; print_string "," in
     print_list tl
                     
let rec print_list_list ls =
  match ls with
  | [] -> ()
  | h :: [] -> print_list h; print_string "}"
  | h :: tl ->
     let _ =  print_string "{"; print_list h;
              print_string "; {" in
     print_list_list tl
 
let add_stochastic_class parameter error agent_type sites_list stochastic_classes =
  match List.rev sites_list with
  | [] | [_] -> error, stochastic_classes
  | l ->
     (*fetch the former list of stochastic classes*)
     (*let error, agent =
       Stochastic_classes_type.AgentMap.unsafe_get
         parameter
         error
         agent_type
         stochastic_classes in
     let old_list =
       match agent with
       | None -> []
       | Some sites -> sites
     in *)          
     let union_list = Union_find.union_list l in
     (*let new_list = union_list :: old_list in*)
     let new_list = union_list :: [] in
     (*TEST*)
     let _ = print_string "union_list:{";
             print_list union_list; print_string "\n";
             print_string "new_list:{{";
             print_list_list new_list; print_string "\n"
     in
     Stochastic_classes_type.AgentMap.set
       parameter
       error
       agent_type
       new_list
       stochastic_classes
     
(*List.fold_left (fun _ _ ->
  let union_list = Union_find.union_list l in
  error, union_list)
  (error, []) stochastic_classes*)

let scan_rule parameter error handler rule classes =
  let viewlhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  (* let creation = rule.Cckappa_sig.actions.Cckappa_sig.creation in*)
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
          (* get the list of sites associated with agent_type, or empty if
          it does not exist *)
          let error, init =
            Stochastic_classes_type.AgentMap.create parameter error 0 in
          let error, get_agent_sites =
            Stochastic_classes_type.AgentMap.unsafe_get
              parameter
              error
              agent_type
              init
          in
          let agent_sites_list =
            match get_agent_sites with
            | None -> []
            | Some sites -> sites
          in
          (*collect all sites in an agent *)
          let sites_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_list ->
               site :: current_list)
              agent.Cckappa_sig.agent_interface agent_sites_list in
          let error, stochastic_classes =
            add_stochastic_class
              parameter
              error
              agent_type
              sites_list
              stochastic_classes
          in
          error, stochastic_classes
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
  let parameter =  Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print_string "START\n";
          Stochastic_classes_type.AgentMap.print
            error
            (fun error parameter ls ->
             let _ = print_string "site_type:{{"  in
             print_list_list ls;              
             let _ = print_newline () in
             error)
            parameter
            result
  in
  error, result
