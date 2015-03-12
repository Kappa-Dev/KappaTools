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
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)
                 
let trace = false
(*let local_trace = false*)

let empty_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, covering_classes = 
    Covering_classes_type.AgentMap.create parameter error n_agents in
  error,
  {
     Covering_classes_type.covering_classes  = covering_classes
  }
    
let length_sorted lists =
  let list_length = List.rev_map (fun list -> list, List.length list) lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(*let print_remanent parameter error remanent =
  let _ = print_string "pointers\n" in 
  let error =
    Int_storage.Nearly_inf_Imperatif.print
      error
      (fun error parameter set_pointer_backward ->
       let error =
         Covering_classes_type.Set_list_id.fold_set
           (fun elt error ->
             let _ = Printf.fprintf (Remanent_parameters.get_log parameter)
                            "sites_type:" in
            let _ = Printf.fprintf (Remanent_parameters.get_log parameter)
                                   "%i" elt in
            let _ = Printf.fprintf (Remanent_parameters.get_log parameter) "\n"
            in
            error
           ) set_pointer_backward error
       in
       error)
      parameter
      remanent.Covering_classes_type.pointer_backward
  in
  let _ = print_string "dictionary\n" in 
  let _ = Covering_classes_type.Dictionary_of_Covering_classes.print
                 parameter error
                 (fun parameter error elt list _ _ ->
                  let _ = Printf.printf "%i:" elt in
                  let _ = List.iter (fun x -> print_int x;print_string ",")
                                    list in
                  let _ = print_newline () in error
                 ) remanent.Covering_classes_type.dic
  in error*)
      
let store_new_class parameter error l remanent =
  let good_lists = remanent.Covering_classes_type.dic in
  let pointer_backward = remanent.Covering_classes_type.pointer_backward in
  match l with
  | [] -> error, remanent
  | _ ->
     let error, output =
       Covering_classes_type.Dictionary_of_Covering_classes.allocate
         parameter
         error
         (fun _ _ -> 0)
         l
         ()
         Misc_sa.const_unit
         good_lists
     in
     let error,(allocate_id,dic) =
       match output
       with Some (al,_,_,d) -> error,(al,d)
          | None ->
             warn
               parameter
               error
               (Some "line 106")
               Exit
               (0,good_lists)
     in
     (*let _ = print_string "AFTER UNSAFE ALLOCATION\n" in
     let _ = Covering_classes_type.Dictionary_of_Covering_classes.print
                 parameter error
                 (fun parameter error elt list _ _ ->
                  let _ = Printf.printf "%i:" elt in
                  let _ = List.iter (fun x -> print_int x;print_string ",")
                                    list in
                  let _ = print_newline () in error
                 ) dic
     in *)
     (*store pointer backward*)
     let error,pointer_backward =
       List.fold_left
         (fun (error,pointer_backward) elt ->
           let error, old_set_id =   
            match Int_storage.Nearly_inf_Imperatif.unsafe_get
                    parameter
                    error
                    elt 
                    pointer_backward
            with
            | error, None ->
               error, Covering_classes_type.Set_list_id.empty_set
            | error, Some set_list_id -> error, set_list_id
          in
          let error,new_set_id =
            Covering_classes_type.Set_list_id.add_set
              parameter error allocate_id old_set_id
          in
           Int_storage.Nearly_inf_Imperatif.set
            parameter
            error
            elt
            new_set_id
            pointer_backward)
         (error,pointer_backward)
         l
     in
     error, {
         Covering_classes_type.dic = dic; 
         Covering_classes_type.pointer_backward = pointer_backward}
      
let clean_new parameter error classes =
  (*let _ = print_string "START PRINT_NEW \n" in *)
  let good_lists =
    Covering_classes_type.Dictionary_of_Covering_classes.init () in
  let error,pbw = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in 
  let empty_acc =
    { Covering_classes_type.dic = good_lists ;
      Covering_classes_type.pointer_backward = pbw }
  in        
  let lists_to_deal_with = length_sorted classes in
  List.fold_left (fun (error,acc) list ->
                  match list with
                  | [] -> (error, acc)
                  | t::q ->(*
                     let _ = print_string "\nTHIS IS THE REMANENT STATE\n" in 
                     let _ = print_remanent parameter error acc in
                     let _ = print_string "\nLIST TO BE HANDLED WITH\n" in 
                     let _ = List.iter (fun x -> print_int x;print_string "," )
                                       list in
                     let _ = print_newline () in *)
                     let pointer_backward = acc.Covering_classes_type.pointer_backward in 
                     (* get the set of list(id) containing t *)
                     let error, potential_supersets =
                       match Int_storage.Nearly_inf_Imperatif.unsafe_get
                               parameter
                               error
                               t
                               pointer_backward
                       with
                       | error, None ->
                          error, Covering_classes_type.Set_list_id.empty_set
                       | error, Some set_list_id -> error, set_list_id
                     in
                     let rec aux to_visit potential_supersets =
                       match to_visit
                       with
                       | [] ->
                          (*let _ = print_string "THIS IS ACTUALLY A SUBSET\n" in*)
                          error,acc
                       | t'::q' ->
                           (* get the set of list(id) containing t *)
                           let error, potential_supersets' =
                             match Int_storage.Nearly_inf_Imperatif.unsafe_get
                                     parameter
                                     error
                                     t'
                                     pointer_backward
                             with
                             | error, None ->
                                error, Covering_classes_type.Set_list_id.empty_set
                             | error, Some set_list_id ->
                                error, set_list_id
                           in           
                           (* intersection of two sets *)
                           let error, potential_superset =
                             Covering_classes_type.Set_list_id.inter
                               parameter
                               error
                               potential_supersets
                               potential_supersets'
                           in
                           if Covering_classes_type.Set_list_id.is_empty_set
                                potential_superset
                           then
                             store_new_class parameter error list acc
                           else
                             aux q' potential_superset
                     in
                     if Covering_classes_type.Set_list_id.is_empty_set
                          potential_supersets
                     then
                       store_new_class parameter error list acc
                     else
                       aux q potential_supersets)
                 (error, empty_acc) lists_to_deal_with

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
       let new_list = (List.rev new_covering_class) :: old_list in
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
  let error, covering_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold2_common
      parameter error
      (fun parameter error agent_id agent site_modif covering_classes ->
       (* if the site does not modify then do nothing  *)
       if Cckappa_sig.Site_map_and_set.is_empty_map
            site_modif.Cckappa_sig.agent_interface
       then
         error, covering_classes
       else
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
            (* store new_covering_class in the classes of the agent type
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

let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  let error,agent_map =
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
  let error, init = Covering_classes_type.AgentMap.create parameter error 0 in
  let error, result =
    Covering_classes_type.AgentMap.fold
      parameter error
      (fun parameters error id list init ->
       let error,list = clean_new parameters error list in
       Covering_classes_type.AgentMap.set parameters error id list init)
      agent_map.Covering_classes_type.covering_classes
      init
  in
  error,result
          
let covering_classes parameters error handler cc_compil =
  let error,result = scan_rule_set parameters error handler cc_compil.Cckappa_sig.rules in
(*  let _ = print_string "START PRINTING\n" in*)
  let _ =
    Covering_classes_type.AgentMap.print
      error
      (fun error parameter dic ->
       let _ = Covering_classes_type.Dictionary_of_Covering_classes.print
                 parameter error
                 (fun parameter error elt l _ _ ->
                  let _ = Printf.printf "Covering_class_id:%i:" elt in
                  let _ =
                    print_string "site_type:{";
                    let rec print_list l =
                      match l with
                      | [] -> ()
                      | h :: [] -> print_int h; print_string "}"
                      | h :: tl ->
                         let _ = print_int h; print_string "," in
                         print_list tl in
                    print_list l
                  in
                  let _ = print_newline () in
                  error
                 ) dic.Covering_classes_type.dic
       in error )
      parameters
      result
  in error, result

(*test union_find function*)
(*let test parameter error =
  Union_find.Union_find_array.create parameter error 0*)
