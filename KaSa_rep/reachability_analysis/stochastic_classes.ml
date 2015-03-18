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

let print_stochastic_classes parameter error stochastic_classes =
  Stochastic_classes_type.AgentMap.print
    error
    (fun error parameter u ->
     let _ = print_string "\nPrint union";
             Union_find.print_union u; 
             print_string "\n"
     in
     error)
    parameter
    stochastic_classes

(*MOVE*)
module Stochastic_classes =
  struct
    type t = Union_find.union_find
    let compare = compare
  end

module Dictionary_of_Stochastic_classes =
  Dictionary.Dictionary_of_Ord (Stochastic_classes)
                               
type remanent_dic = (unit, unit) Dictionary_of_Stochastic_classes.dictionary
type remanent = {dic : remanent_dic}

(*
let equivalence_classes parameter error classes =
  let init_dic = Dictionary_of_Stochastic_classes.init () in
  let init_remanent = {dic = init_dic} in
  (**)
 *)
                                       
    
let add_stochastic_class parameter error agent_type sites_list stochastic_classes =
  match sites_list with
  | [] -> error, stochastic_classes                         
  | t :: tl ->
     let error, agent =
       Stochastic_classes_type.AgentMap.unsafe_get
         parameter
         error
         agent_type
         stochastic_classes
     in
     (*fetch the fomer list of stochastic classes*)
     let empty_ufind = Union_find.create 0 in
     let old_list =
       match agent with
       | None -> empty_ufind
       | Some sites -> sites
     in
     (*store the new list of stochastic classes*)
     let new_list = List.fold_left (fun union t' ->
                                    Union_find.union t t' union) old_list tl
     in
     (*TEST*)
     (*let _ = print_string "\nPRINT old_list:";
             Union_find.print_union old_list;
             print_string "\n"
     in
     let _ = print_string "\nPRINT new_list:";
             Union_find.print_union new_list;
             print_string "\n"
     in*)
     (*add this new list into the result list*)
     Stochastic_classes_type.AgentMap.set
       parameter
       error
       agent_type
       new_list
       stochastic_classes
               
let scan_rule parameter error handler rule classes =
  let stochastic_classes = classes.Stochastic_classes_type.stochastic_classes in
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let error, stochastic_classes  =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error agent_id agent stochastic_classes ->
       match agent with
       | Cckappa_sig.Ghost -> error, stochastic_classes
       | Cckappa_sig.Agent agent ->
          let sites_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_class ->
               site :: current_class)
              agent.Cckappa_sig.agent_interface []
          in
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, stochastic_classes =
            add_stochastic_class
              parameter
              error
              agent_type
              sites_list
              stochastic_classes
          in
          error, stochastic_classes
       ) viewslhs stochastic_classes
  in
  error,
  {
    Stochastic_classes_type.stochastic_classes = stochastic_classes
  }

let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
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
  let error, init = Stochastic_classes_type.AgentMap.create
                      parameter error 0 in
  let error, result =
    Stochastic_classes_type.AgentMap.fold
      parameter
      error
      (fun parameter error id list init ->
       Stochastic_classes_type.AgentMap.set
         parameter
         error
         id
         list
         init)
      agent_map.Stochastic_classes_type.stochastic_classes
      init
  in
  error, result

let stochastic_classes parameter error handler cc_compil =
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_string "START\n";
          print_stochastic_classes parameter error result
  in
  error, result
