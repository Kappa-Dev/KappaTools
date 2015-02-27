 (**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Compute the relations between the left hand site of a rule and its sites.
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn (fun () -> default)

let local_trace = false

let empty_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, covering_classes = 
    Covering_classes_type.AgentMap.create parameter error n_agents in
  error,
  {
     Covering_classes_type.covering_classes  = covering_classes
  }

let add_covering_class parameter error agent_type new_covering_class covering_classes =
  match new_covering_class with
    | [] -> error, covering_classes
    | _ ->
       let error, agent =
         Covering_classes_type.AgentMap.unsafe_get
           parameter
           error
           agent_type
           covering_classes in
       (* fetch the former list of covering classes *)
       let old_list =
         match agent with
         | None -> []
         | Some a -> a
       in
       (* store the new list of covering classes *)
       let new_list = new_covering_class::old_list in
       Covering_classes_type.AgentMap.set
         parameter
         error
         agent_type
         new_list
         covering_classes

let scan_rule parameter error handler rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let rule_diff = rule.Cckappa_sig.diff_reverse in
  let covering_classes = classes.Covering_classes_type.covering_classes in
  let _ = Misc_sa.trace parameter (fun () -> "TEST\n") in
  let error, covering_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold2_common
      parameter error
    (fun parameter error agent_id agent site_modif covering_classes ->
       match agent with
       | Cckappa_sig.Ghost -> error, covering_classes
       | Cckappa_sig.Agent agent ->
          let new_covering_class =
            Cckappa_sig.Site_map_and_set.fold_map
	      (fun site _ current_covering_class ->
               site::current_covering_class)
              agent.Cckappa_sig.agent_interface []
          in
          let agent_type = agent.Cckappa_sig.agent_name in
          (* store new_covering class in the classes of the agent type
               agent_type *)
          let error,covering_classes =
            add_covering_class
              parameter
              error
              agent_type
              new_covering_class
              covering_classes 
          in
          error, covering_classes
    ) viewslhs rule_diff covering_classes
  in
  error,
  {
    Covering_classes_type.covering_classes = covering_classes
  }

(*TODO*)
let length_covering_classes lists =
  List.map (fun covering_class ->
            covering_class, List.length covering_class) lists

let length_sort lists =
  let lists = List.map (fun list -> length_covering_classes list, list) lists in
  let lists = List.sort (fun a b -> compare (fst a) (fst b)) lists in
  List.map snd lists
  

let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  Int_storage.Nearly_inf_Imperatif.fold
    parameter error
    (fun parameter error rule_id rule classes ->
     let _ = Misc_sa.trace parameter
      (fun () -> "Rule " ^ (string_of_int rule_id) ^ "\n") in
     scan_rule
       parameter
       error
       handler
       rule.Cckappa_sig.e_rule_c_rule
       classes
    ) rules init
      
let covering_classes parameters error handler cc_compil =
  let _ = Misc_sa.trace parameters (fun () -> "Covering_classes \n") in
    scan_rule_set parameters error handler cc_compil.Cckappa_sig.rules
