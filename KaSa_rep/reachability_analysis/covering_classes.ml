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

(*FIXME*)
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

(*REMOVE*)
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

let length_sorted lists =
  let list_length = List.rev_map (fun list -> list, List.length list) lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(*MOVE*)
(* key(#id): int; 'a t = infinite array of list(#id) *)
module Inf_array = Int_storage.Nearly_inf_Imperatif

module Inf_array_set = Set_and_map.Make
                         (struct
                             type t = Inf_array
                             let compare = compare
                           end)

(* create a type set for an infinite list(id) *)
type set_list_id = Inf_array_set.set

type remanent_dic =
  (unit, unit)
    Covering_classes_type.Dictionary_of_Covering_classes.dictionary

type remanent =
  {dic: remanent_dic;
   pointer_backward: (int*set_list_id) Inf_array.t (* map infinite array into a set *) }
  
let clean_new parameter error classes remanent = (*return 'a list*)
  let good_lists =
    Covering_classes_type.Dictionary_of_Covering_classes.init () in
  (*a list to deal with is the list has longest size in a descreasing order*)
  let lists_to_deal_with = length_sorted classes in
  let store_new_class l = (*'a list *)
    match l with
    | [] -> []
    | list -> []
       (* allocate *)
      (* let error, allocate_id =
         Covering_classes_type.Dictionary_of_Covering_classes.unsafe_allocate
           parameter
           error
           (*value type*)
           list
           (*'a:asso*)
           ()
           (*int -> 'b:build*)
           Misc_sa.const_unit
           (*dictionary*)
           good_lists
       in
       (*get #id in the dictionary*)
       let error, get_id =
         match allocate_id with
         | (id, _, _, _) -> error, id
       in
       (*get the set_list(id) *)
        let error, old_set_list_id =
          match Int_storage.Nearly_inf_Imperatif.get
                  parameter
                  error
                  (*key: #id*)
                  get_id
                  (*'a t: set of (t, list(id)) *)
                  remanent.pointer_backward
          with
          | error, None ->
             warn
               parameter
               error
               (Some "line 52")
               Exit
               Inf_array_set.empty_set
          | error, Some (_, set_list_id) -> error, set_list_id
        in
        (*add id to the set*)(*FIXME*)
        Inf_array_set.fold_set (fun id current_set ->
                                id :: current_set
                               ) old_set_list_id list
  in*)
  in
  List.fold_left (fun list elem ->
                  match list with
                  | [] -> elem (*'a list*)
                  | t::q ->
                     (* get the set of list(id) containing t *)
                     let error, potential_supersets =
                       match Int_storage.Nearly_inf_Imperatif.get
                               parameter
                               error
                               (*key: #id*)
                               t
                               (*'a t: set of (t, list(id)) *)
                               remanent.pointer_backward
                       with
                       | error, None ->
                          warn
                            parameter
                            error
                            (Some "line 52")
                            Exit
                            Inf_array_set.empty_set
                       | error, Some (t, set_list_id) -> error, set_list_id
                     in
                     let rec aux to_visit potential_supersets =
                       match to_visit
                       with
                       | [] -> store_new_class list
                       | t'::q' ->
                           (* get the set of list(id) containing t *)
                           let error, potential_supersets' =
                             match Int_storage.Nearly_inf_Imperatif.get
                                     parameter
                                     error
                                     (*key*)
                                     t'
                                     (*'a t: set of (t, list(id)) *)
                                     remanent.pointer_backward
                             with
                             | error, None ->
                                warn
                                  parameter
                                  error
                                  (Some "line 52")
                                  Exit
                                  Inf_array_set.empty_set
                             | error, Some (t', set_list_id) ->
                                error, set_list_id
                           in           
                           (* intersection of two sets *)
                           let error, potential_superset =
                             Inf_array_set.inter
                               parameter
                               error
                               potential_supersets
                               potential_supersets'
                           in
                           if Inf_array_set.is_empty_set
                                potential_superset
                           then
                             elem (*'a list *)
                           else
                             aux q' potential_superset
                     in
                     aux q potential_supersets)
                 [] lists_to_deal_with

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
       (*let clean_new_covering_class =
         clean_new parameter error (List.rev new_covering_class)
       in
       let new_list = clean_new_covering_class :: old_list in*)
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
