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

let store_new_class parameter error l remanent =
  let dic = remanent.Stochastic_classes_type.dic in
  let key = remanent.Stochastic_classes_type.key in
  match l with
  | [] -> error, remanent
  | _ ->
     let error, output =
       Stochastic_classes_type.Dictionary_of_Stochastic_classes.allocate
         parameter
         error
         Misc_sa.compare_unit
         l
         ()
         Misc_sa.const_unit
         dic
     in
     let error, (id, dic) =
       match output with
       | Some (id, _, _, dic) -> error, (id, dic)
       | None -> warn parameter error (Some "line 64") Exit (0, dic)
     in
     (*get new key and store it into a result*)
     let error, key =
       List.fold_left (fun (error, key) elt ->
                       let error, old_set_key =
                           match Int_storage.Nearly_inf_Imperatif.unsafe_get
                                   parameter error elt key
                           with
                           | error, None ->
                              error, Stochastic_classes_type.Set_list_keys.empty_set
                           | error, Some set_list_keys -> error, set_list_keys
                       in
                       let error,new_set_key =
                         Stochastic_classes_type.Set_list_keys.add_set
                           parameter error id old_set_key
                       in         
                       Int_storage.Nearly_inf_Imperatif.set
                         parameter
                         error
                         elt
                         new_set_key
                         key
                      ) (error, key) l

     in
     error, {Stochastic_classes_type.dic = dic;
            Stochastic_classes_type.key = key}
  
let clean_classes parameter error classes =
  let init_dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init() in
  let error, init_key = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_acc = {Stochastic_classes_type.dic = init_dic;
                  Stochastic_classes_type.key = init_key
                 }
  in
  List.fold_left (fun (error, acc) list ->
                  match list with
                  | [] -> (error, acc)
                  | t :: q ->
                     let (*fst_class*)pointer_backward = acc.Stochastic_classes_type.key in
                     (*get a fist class*)
                     let error,  potential_supersets (*get_fst_elt*) =
                       match Int_storage.Nearly_inf_Imperatif.unsafe_get
                               parameter error
                               t  pointer_backward
                               (*fst_class*)
                       with
                       | error, None ->
                          error, Stochastic_classes_type.Set_list_keys.empty_set
                       | error, Some set_list_keys -> error, set_list_keys
                     in
                     (*check the second element in this class with the fst until the class is empty*)
                     let rec aux to_visit potential_supersets  (*fst_class*) =
                       match to_visit with
                       | [] -> error, acc
                       | t' :: q' ->
                          (*get the second element*)
                          let error, potential_supersets'(*get_snd_elt*) =
                            match Int_storage.Nearly_inf_Imperatif.unsafe_get
                                    parameter
                                    error
                                    t'  pointer_backward
                                    (*fst_class*)
                            with
                            | error, None ->
                               error, Stochastic_classes_type.Set_list_keys.empty_set
                            | error, Some set_list_keys -> error, set_list_keys
                          in
                          (*join then together*)
                          let error, potential_superset(*result_join*) =
                            Stochastic_classes_type.Set_list_keys.inter
                              parameter
                              error
                              potential_supersets
                              potential_supersets'
                              (*
                              get_fst_elt
                              get_snd_elt*)
                          in
                          if Stochastic_classes_type.Set_list_keys.is_empty_set
                               potential_superset (*result_join*)
                          then
                            store_new_class parameter error list acc
                          else
                            (*continue to the rest of the list*)
                            aux q' potential_superset (*result_join*)
                     in
                     (*check the big set*)
                     if Stochastic_classes_type.Set_list_keys.is_empty_set
                          (*get_fst_elt*) potential_supersets
                     then
                       store_new_class parameter error list acc
                     else
                       aux q (*get_fst_elt*) potential_supersets)
                 (error, init_acc) classes
                   
let add_sites_class parameter error agent_type sites_list stochastic_classes =
  match (*List.rev*) sites_list with
  | [] -> error, stochastic_classes
  | l ->
     (*fetch the former list of stochastic classes*)
     let error, agent =
       Stochastic_classes_type.AgentMap.unsafe_get
         parameter
         error
         agent_type
         stochastic_classes in
     let old_list =
       match agent with
       | None -> []
       | Some sites -> sites
     in
     (*add the normal sites without union first*)
     let new_list = (List.rev sites_list) :: old_list in
     (*let union_list = Union_find.union_list l in
     let new_list = union_list :: old_list in*)
     (*let new_list = union_list :: [] in*)
     Stochastic_classes_type.AgentMap.set
       parameter
       error
       agent_type
       new_list
       stochastic_classes

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
          (*store all sites associated with agent_type*)
          let error, stochastic_classes =
            add_sites_class
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
      (fun parameter error id classes init ->
       (*reunion sites to get only one list of sites for each agent_type*)
       let error, cleaned_classes =
         clean_classes parameter error classes in
       (*store the result*)
       Stochastic_classes_type.AgentMap.set
         parameter error id cleaned_classes init)
      agent_map.Stochastic_classes_type.stochastic_classes
      init
  in
  error, result
 
let stochastic_classes parameter error handler cc_compil =
  let parameter =  Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = Stochastic_classes_type.AgentMap.print
            error
            (fun error parameter dic ->
             let _ = Stochastic_classes_type.Dictionary_of_Stochastic_classes.print
                       parameter
                       error
                       (fun parameter error elt l _ _ ->
                        let _ = Printf.printf "Stochastic_class_id:%i:" elt in
                        let _ =
                          print_string "site_type:{";
                          let l = Union_find.union_list l in
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
                       ) dic.Stochastic_classes_type.dic
             in error )
            parameter
            result

  in
  error, result
