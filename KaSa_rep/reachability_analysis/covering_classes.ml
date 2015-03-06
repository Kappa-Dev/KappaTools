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

let print_agent parameter error handler =
  Ckappa_sig.Dictionary_of_agents.print
    parameter
    error
    (fun parameter error i agent_name () () ->
     let _ = Printf.fprintf (Remanent_parameters.get_log parameter)
                            "agent_type:%d:%s:\n" i agent_name
     in error)
    handler.Cckappa_sig.agents_dic
      
let print_site_type parameters site =
  match site with
  | Ckappa_sig.Internal a ->
     Printf.fprintf (Remanent_parameters.get_log parameters) "%s%s(internal state)" (Remanent_parameters.get_prefix parameters) a
  | Ckappa_sig.Binding a ->
     Printf.fprintf (Remanent_parameters.get_log parameters) "%s%s(binding state)" (Remanent_parameters.get_prefix parameters) a
                    
let print_site parameters error handler =
  let print print_aux =
    (fun parameters error i site () () ->
     let parameters = Remanent_parameters.update_prefix
                        parameters                      
                        ("site_type:"^(string_of_int i)^"->")
     in
     let _ = print_aux parameters site in
     let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "\n" in
     error)
  in
  let parameters_site = Remanent_parameters.update_prefix parameters "sites:" in
  Int_storage.Nearly_inf_Imperatif.print_site_f
    error
    (fun error parameters s ->
     let _ = Ckappa_sig.Dictionary_of_sites.print
               parameters
               error
               (print print_site_type) s
     in error)
    parameters_site
    handler.Cckappa_sig.sites
    
let rec print_class parameter l =
  match l with
  | [] -> ()
  | site :: tl ->
     let _ = Printf.fprintf (Remanent_parameters.get_log parameter)
                            "%i " site in
     print_class parameter tl

let rec print_classes parameter ls =
  match ls with
  | [] -> ()
  | l :: tl ->
     let _ = Printf.fprintf (Remanent_parameters.get_log parameter)
                            "sites_type:" in
     let _ = print_class parameter l in
     let _ = Printf.fprintf (Remanent_parameters.get_log parameter) "\n" in
     print_classes parameter tl

let rec remove_dups l =
  match l with
  | [] -> [] 
  | h :: t -> h :: (remove_dups (List.filter (fun x -> x <> h)t))
            
let rec remove_dups_lists ls =
  match ls with
  | [] -> []
  | h :: t -> let h' = remove_dups h in
              h' :: (remove_dups_lists
                       (List.filter
                          (fun x -> let x' = remove_dups x in x' <> h') t))
          
let length_sort_remove_dups lists =
  let remove_lists = remove_dups_lists lists in
  let length_lists = List.rev_map (fun list ->
                                   list, List.length list) remove_lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) length_lists in
  List.rev_map fst lists

let subset l1 ls =
  List.map (List.filter (fun x -> List.mem x l1))ls

let result_subset ls =
  match ls with
  | [] -> []
  | l1 :: ls' -> subset l1 ls'
  
let remove_subset ls =
  let ls' = result_subset ls in
  List.filter (fun l -> not (List.mem l ls')) ls

let clean ls =
  let remove_dups = length_sort_remove_dups ls in
  let remove_sub = remove_subset remove_dups in
  remove_sub

(*TODO*) 
let length_sorted lists =
  let list_length = List.rev_map (fun list -> list, List.length list) lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

let length_longest lists =
  List.rev_map (fun list -> list, List.length list) lists

(*compute the size of agent*)
(*let length_agent agent =
  let rec aux agent size =
    match agent with
    | Cckappa_sig.Ghost -> size
    | Cckappa_sig.Agent _ -> aux agent (size + 1)
  in aux agent 0*)
  (* array: int -> 'b?
  build a data structure for a new array, this one will be use to update
  the image. The size of this array will be the sites of the agents*)

(*TODO: new cleaning function with the dictionary*)
(*
let clean_new parameter error classes = (*return 'a list list*)
  (* create an initial good lists *)
  let init_good_lists =
    Covering_classes_type.Dictionary_of_Covering_classes.init () in
  (*a list to deal with is the list has longest size in a descreasing order*)
  let lists_to_deal_with = length_sorted classes in
  (*allocate the value in the dictionary*)
  let error, good_lists =
    Covering_classes_type.Dictionary_of_Covering_classes.unsafe_allocate
      parameter
      error
      (*value type*)
      Covering_classes_type.Covering_classes.t
      (*'a*)
      ()
      (*int -> 'b*)
      Misc_sa.const_unit
      (*dictionary*)
      init_good_lists
  in
  (*good_lists*)
    (*return the value in a good list here*)
  let error, value_good_list = (*int list *)
    match good_lists with
    | [] -> warn parameter error (Some "line 45") Exit 0
    | is -> error, is
  in
  (*Iterate in the list_to_deal_with, and check the member of this list
  with the good_lists*)
   List.iter (fun f ->
              (*checking each element of l and value in a value_good_lists*)
              (*if it is a member of value_good_lists*)
              if not (Hashtbl.mem value_good_list f)
              then (*replace value_good_lists by f*)
                
              (*if not: throw away*)
             ) (*something here*)
   *)
let add_covering_class parameter error agent_type new_covering_class covering_classes =
  match new_covering_class with
    | [] -> error, covering_classes
    | _ ->
       let _ = Misc_sa.trace parameter (fun () -> "agent_type:" ^
                              (string_of_int agent_type) ^":") in
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
       (*let clean_new_list = clean_new parameter error new_list in*)
       let clean_new_list = clean new_list in
       let _ = print_classes
                 parameter
                 clean_new_list
       in
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
  scan_rule_set parameters error handler cc_compil.Cckappa_sig.rules
