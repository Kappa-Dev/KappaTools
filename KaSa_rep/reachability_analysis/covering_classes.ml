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

open Covering_classes_type
open Cckappa_sig
open Int_storage
open Printf
open Print_covering_classes

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*UTILITIES FUNCTION*)

let empty_set = Site_map_and_set.empty_set
let empty_map = Site_map_and_set.empty_map
let add_set = Site_map_and_set.add_set
let union = Site_map_and_set.union

(************************************************************************************)
(*sorted and ordered a covering classes in an descreasing order *)

let length_sorted (l: int list list): int list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists       = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(************************************************************************************)
(*port information for site (state)*)

let int_of_port port = port.site_state.min
    
(************************************************************************************)
(*common function for getting an id in pointer backward*)

let get_id_common_set parameter error t set =
  match Nearly_inf_Imperatif.unsafe_get
    parameter
    error 
    t 
    set
  with
    | error, None -> error, empty_set      
    | error, Some id -> error, id

(************************************************************************************)
(*CLEANING*)

(*------------------------------------------------------------------------------*)
(*store pointer backward *)

let store_pointer_backward parameter error id pointer_backward covering_class =
  List.fold_left (fun (error, pointer_backward) old_id ->
    (*getting an old id in the set*)
    let error, old_id_set =
      get_id_common_set 
        parameter 
        error
        old_id 
        pointer_backward
    in
    (*add the current set of elt into the old set*)
    let error, new_id_set =
      add_set
        parameter
        error
        id
        old_id_set
    in
    (*store the result into pointer backward*)
    Nearly_inf_Imperatif.set
      parameter
      error
      old_id
      new_id_set
      pointer_backward)
    (error, pointer_backward)
    covering_class

(*------------------------------------------------------------------------------*)
(*PART II:
  Re-index the sites in each rule, each agent type and each covering class *)

let position x = (*TODO: change to use array instead*)
  let rec aux k = function
    | [] -> raise Not_found
    | y :: ys ->
      if x = y
      then k
      else aux (k+1) ys
  in aux 0;;

let is_empty_list l =
  match l with
    | [] -> true
    | _ -> false

let re_index_value_list list =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: tl ->
        let nth = position x list in
        (nth + 1) :: aux tl
  in aux list

(*------------------------------------------------------------------------------*)
(*projection site that are modified with new index*)

let project_modified_site value_list modified_map = (*TODO:add state information*)
  let rec aux acc =
    match acc with
      | [] -> []
      | x :: tl ->
        if not (Site_map_and_set.is_empty_map modified_map)
        then
          if Site_map_and_set.mem_map x modified_map
          then
            begin
              if not (is_empty_list value_list)
              then
                let nth_1 = (position x value_list) + 1 in
                let l = nth_1 :: aux tl in
                l
              else []
            end
          else aux tl
        else [] (*if modified_set is empty then nothing*)
  in aux value_list

(************************************************************************************)
(*DICTIONARY*)

let common_allocate_dic parameter error value good_dic =
  let error, output =
    Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      value
      ()
      Misc_sa.const_unit
      good_dic
  in
  (*return id and result as a dictionary type*)
  let error, (id_dic, store_dic) =
    match output with
      | Some (id, _, _, dic) -> error, (id, dic)
      | None -> warn parameter error (Some "line 197") Exit (0, good_dic)
  in
  error, (id_dic, store_dic)
  
(*------------------------------------------------------------------------------*)
(*compute covering class dictionary*)

let covering_class_dic parameter error covering_class good_covering_class =
  common_allocate_dic
    parameter
    error
    covering_class 
    good_covering_class
    
(*------------------------------------------------------------------------------*)
(*compute new index dictionary*)

let new_index_dic parameter error new_index_covering_class good_index =
  common_allocate_dic 
    parameter
    error 
    new_index_covering_class
    good_index

(*------------------------------------------------------------------------------*)
(*compute tested dictionary with new index *)

let test_new_index_dic parameter error new_id new_dic good_test_dic =
  let error, (value_index_dic, _, _) =
    Misc_sa.unsome
      (Dictionary_of_Covering_class.translate
         parameter
         error
         new_id
         new_dic)
      (fun error -> warn parameter error (Some "line 236") Exit ([], (),()))
  in
  (*return site_test_dic*)
  common_allocate_dic 
    parameter 
    error 
    value_index_dic 
    good_test_dic

(*------------------------------------------------------------------------------*)
(*compute modified dictionary with new_index*)

let modified_index_dic parameter error covering_class modified_map 
    good_new_index_modif_dic =
  let modified_value = project_modified_site covering_class modified_map in
  let error, out_modif_dic =
    Dictionary_of_Modified_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      modified_value
      ()
      Misc_sa.const_unit
      good_new_index_modif_dic
  in
  let error, (new_modif_id, modif_index_dic) =
    match out_modif_dic with
      | None -> warn parameter error (Some "line 252") Exit (0, good_new_index_modif_dic)
      | Some (id, _, _, dic) -> error, (id, dic)        
  in
  error, (new_modif_id, modif_index_dic)

(*------------------------------------------------------------------------------*)
(*store remanent*)

(*let remove_duplicates_hashtable l =
  let open List in
  let tbl = Hashtbl.create (length l) in
  let f l (e,e') = 
    try 
      let _ = Hashtbl.find tbl (e,e') in l
    with
    | Not_found -> 
      Hashtbl.add tbl (e,e') ();
      (e,e') :: l
  in
  List.rev (List.fold_left f [] l)

let combine x l =
  let rec aux acc =
    match acc with
      | [] -> []
      | (x', s) :: tl ->
        if x = x'
        then
          (x,s) :: aux tl
        else
          aux tl
  in aux l

let combine_site_state pair_list =
  let rec aux acc =
    match acc with
      | [] -> []
      | (x, l) :: tl ->
        let comb = combine x l in
        comb :: aux tl
  in
  aux pair_list
    
let pair_site_state pair_list =
  let l = List.flatten (combine_site_state pair_list) in
  remove_duplicates_hashtable l*)

(*------------------------------------------------------------------------------*)

let store_remanent parameter error covering_class modified_map remanent =
  (* current state of remanent*)
  let pointer_backward    = remanent.store_pointer_backward in
  let good_covering_class = remanent.store_dic in
  let good_index          = remanent.store_new_index_dic in
  let good_test_dic       = remanent.store_test_new_index_dic in
  let good_modif_dic      = remanent.store_modif_new_index_dic in
  (*------------------------------------------------------------------------------*)
  match covering_class with
    | [] -> error, remanent
    | _ ->
      (*------------------------------------------------------------------------------*)
      (*covering class dictionary*)
      let error, (covering_class_id, store_dic) =
        covering_class_dic 
          parameter 
          error 
          covering_class
          good_covering_class 
      in
      (*------------------------------------------------------------------------------*)
      (*store pointer backward*)
      let error, pointer_backward =
        store_pointer_backward 
          parameter
          error
          covering_class_id 
          pointer_backward
          covering_class 
      in
      (*------------------------------------------------------------------------------*)
      (*PART II: compute new_index in covering_class*)
      let new_index_covering_class = re_index_value_list covering_class in
      let error, (new_id, new_dic) =
        new_index_dic 
          parameter 
          error 
          new_index_covering_class 
          good_index 
      in
      (*------------------------------------------------------------------------------*)
      (*PART II: site test with new index*)
      let error, (new_test_id, test_new_index_dic) =
        test_new_index_dic 
          parameter 
          error
          new_id 
          new_dic 
          good_test_dic 
      in     
      (*------------------------------------------------------------------------------*)
      (*PART II: site modified with new_index*)
      let error, (new_modif_id, modif_index_dic) =
        modified_index_dic 
          parameter
          error 
          covering_class
          modified_map
          good_modif_dic 
      in
      (*------------------------------------------------------------------------------*)
      (*result*)
      error,
      {
        store_pointer_backward    = pointer_backward;
        store_dic                 = store_dic;
        store_new_index_dic       = new_dic;
        store_test_new_index_dic  = test_new_index_dic;
        store_modif_new_index_dic = modif_index_dic;
      }

(*------------------------------------------------------------------------------*)
(*CLEAN: In a covering class, it will store the old result of the previous
  covering class of an agent.

  For example:
  - rule 0: agent A has a covering class: (0)
  - rule 1: agent A has a covering class: (0,1)
  => Then do the intersection of two covering classes of agent A:
  (0) inter (0,1) -> 0
*)

let init_dic = Dictionary_of_Covering_class.init ()

let clean_classes parameter error covering_classes modified_map =
  let error, init_pointer = Nearly_inf_Imperatif.create parameter error 0 in
  let init_index          = init_dic in
  let init_store_dic      = init_dic in
  let init_test_index     = init_dic in
  let init_modif_index    = Dictionary_of_Modified_class.init() in
  (*------------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent = 
    {
      store_pointer_backward    = init_pointer;
      store_dic                 = init_store_dic;
      store_new_index_dic       = init_index;
      store_test_new_index_dic  = init_test_index;
      store_modif_new_index_dic = init_modif_index;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted covering_classes in
  let is_empty_set = Site_map_and_set.is_empty_set in
  List.fold_left (fun (error, remanent) covering_class ->
    match covering_class with
      | [] -> error, remanent
      | t :: tl ->
        let pointer_backward = remanent.store_pointer_backward in
        (* return the set of list(id) containing t.
           For example: current_covering_classes: [[0;1];[0]]
           t = 0 => (id:1;id:2) of type set;
           remanent_type: [(id:1,[0;1]);(id:2,[0])];
           (id:pointer_backward, dic: int list)
        *)
        let error, potential_supersets =
          get_id_common_set
            parameter
            error
            t
            pointer_backward 
        in
        let rec aux to_visit potential_supersets =
          match to_visit with
	    | [] -> error, remanent
	    | t' :: tl' ->
              (* get the set of list(id) containing t' *)
              let error, potential_supersets' =
		get_id_common_set
                  parameter 
                  error
                  t' 
                  pointer_backward 
              in
              (*-------------------------------------------------------------------*)
              (* intersection of two sets *)
              let error, potential_superset =
                Site_map_and_set.inter
                  parameter 
                  error
                  potential_supersets
                  potential_supersets'
              in
              if is_empty_set potential_superset
              then
                let error, result_covering_dic =
                  store_remanent 
                    parameter 
                    error
                    covering_class
                    modified_map
                    remanent
                in
                error, result_covering_dic
              else
                aux tl' potential_superset
        in
        (*-------------------------------------------------------------------*)
        (*check the beginning state of a superset*)
        if is_empty_set potential_supersets
        then
          (*if it is empty then store it to remanent*)
          let error, result_covering_dic =
            store_remanent 
              parameter
              error
              covering_class
              modified_map
              remanent
          in
          error, result_covering_dic
        else
          aux tl potential_supersets
          )
    (error, init_remanent)
    current_covering_classes

(************************************************************************************)   
(*RULE*)

(*compute modified (actions) site*)

let collect_modified_map parameter error diff_reverse store_modified_map =
  AgentMap.fold parameter error
    (fun parameter error agent_id site_modif store_modified_map ->
      (*if there is no modified sites then do nothing*)
      if Site_map_and_set.is_empty_map
        site_modif.agent_interface
      then error, store_modified_map
      else
        let agent_type = site_modif.agent_name in
        let store_site =
          Site_map_and_set.fold_map
            (fun site port current_map ->
              (*store site map*)
              let error, site_map =
                Site_map_and_set.add_map
                  parameter
                  error
                  site
                  site
                  current_map
              in
              site_map)
            site_modif.agent_interface empty_map
        in
        (*compute site_map*)
        let error, old_map =
          match
            AgentMap.unsafe_get
            parameter
            error
            agent_type
            store_modified_map
          with
            | error, None -> error, empty_map
            | error, Some m -> error, m
        in
        (*store*)
        let error, final_map =
          Site_map_and_set.union_map
            parameter
            error
            old_map
            store_site
        in
        let error, store_modified_map =
          AgentMap.set
            parameter
            error
            agent_type
            final_map
            store_modified_map
        in
        error, store_modified_map
    ) diff_reverse
    store_modified_map

(*------------------------------------------------------------------------------*)
(*compute covering_class*)

let add_covering_class parameter error agent_type list store_covering_classes =
  match list with
    | [] -> error, store_covering_classes
    | _ ->
      let error, old_list =
        match 
          AgentMap.unsafe_get
            parameter 
            error
            agent_type
            store_covering_classes
        with
          | error, None -> error, []
          | error, Some sites -> error, sites
      in
      (* store the new list of covering classes *)
      let new_pair_list = (List.rev list) :: old_list in
      AgentMap.set 
        parameter
        error
        agent_type
        new_pair_list
        store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute covering classes, site test and bdu*)

let collect_covering_classes parameter error views diff_reverse store_covering_classes =
  let error, store_covering_classes =
    Quick_Nearly_inf_Imperatif.fold2_common parameter error
      (fun parameter error agent_id agent site_modif store_covering_classes ->
        (* if in the interface there is no site modified then do nothing *)
        if Site_map_and_set.is_empty_map
          site_modif.agent_interface
        then error, store_covering_classes
        else
          match agent with
            | Ghost -> error, store_covering_classes
            | Agent agent ->
              let agent_type = agent.agent_name in
              (*get a list of sites from an interface at each rule*)
              let site_list =
                Site_map_and_set.fold_map
	          (fun site _ current_list ->
                    site :: current_list
                  ) agent.agent_interface []
              in
              (*compute covering_class*)
              let error, covering_classes =
                add_covering_class
                  parameter
                  error
                  agent_type
                  site_list
                  store_covering_classes
              in
              (*store*)
              error, covering_classes
      ) views diff_reverse store_covering_classes
  in error, store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute covering class: it is a covering class whenever there is a
  modified site in that agent. (CHECK on their left-hand side)

  For example: A(x~u,y~u) -> A(x~u,y~p) (where y is a modified site), then
  there is one covering class for agent A: CV_1: (x,y)
  
  - If the rule is: A(x~u), A(y~u) -> A(x~p), A(y~p), (x,y are modified
  sites), then agent A has two covering classes: CV_1: x; CV_2: y
  
  - If the rule is: A(x~u), A(y~u) -> A(x~u), A(y~p), (y is a modified
  site), then agent A has only one covering class: CV_1: y
*)

let scan_rule parameter error handler rule classes =
  (*------------------------------------------------------------------------------*)
  (*compute modified map*)
  let error, store_modified_map =
    collect_modified_map
      parameter
      error
      rule.diff_reverse
      classes.store_modified_map
  in
  (*------------------------------------------------------------------------------*)
  (*compute covering_class*)
  let error, store_covering_classes =
    collect_covering_classes parameter error
      rule.rule_lhs.views
      rule.diff_reverse
      classes.store_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    store_modified_map     = store_modified_map;
    store_covering_classes = store_covering_classes;
  }           

(************************************************************************************)   
(*PART II. COMPUTE properties related to covering classes*)

(*------------------------------------------------------------------------------*)
(*return the number of covering classes for each agent type*)

let number_of_covering_classes parameter error store_dic =
  let error, num =
    Dictionary_of_Covering_class.last_entry
      parameter error store_dic
  in num + 1

(************************************************************************************)   
(*RULES*)

let create_map parameter error n_agents = AgentMap.create parameter error n_agents

let scan_rule_set parameter error handler rules =
  let n_agents = handler.nagents in
  let error, init_modif_map = create_map parameter error n_agents in
  let error, init_class     = create_map parameter error n_agents in
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      store_modified_map     = init_modif_map;
      store_covering_classes = init_class;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_covering_classes =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
        let error, result =
          scan_rule
            parameter
            error
            handler
            rule.e_rule_c_rule
            classes
        in
        error, result
      ) rules init_class
  in
  error, store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute covering classes in the set of rules*)

let scan_rule_set_cv parameter error handler rules =
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = AgentMap.create parameter error 0 in
  let error, store_covering_classes = 
    scan_rule_set parameter error handler rules
  in
  let result_covering_classes = store_covering_classes.store_covering_classes in
  let error, remanent_dictionary =
    AgentMap.fold parameter error
      (fun parameters error agent_type covering_class init_remanent ->
        (*------------------------------------------------------------------------------*)
        (*get modified site*)
        let error, modified_map =
          match
            Quick_Nearly_inf_Imperatif.unsafe_get
              parameter 
              error 
              agent_type
              store_covering_classes.store_modified_map
          with
            | error, None -> error, empty_map
            | error, Some m -> error, m
        in
        (*------------------------------------------------------------------------------*)
        (*clean the covering classes, removed duplicate of covering classes*)
        let error, store_remanent_dic =
          clean_classes
            parameters 
            error
            covering_class
            modified_map
        in
        (*------------------------------------------------------------------------------*)
        (*store the covering classes after cleaning theirs duplicate classes*)
        let error, store_remanent =
          AgentMap.set 
            parameters
            error
            agent_type
            store_remanent_dic
            init_remanent
        in
        (*------------------------------------------------------------------------------*)
        (*result*)
        error, store_remanent
      )
      result_covering_classes
      init_result
  in
  error, remanent_dictionary

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result_remanent =
  AgentMap.print
    error
    (fun error parameter remanent ->
      let _ =
        (*------------------------------------------------------------------------------*)
        (* number of covering classes*)
        let number =
          number_of_covering_classes 
            parameter
            error
            remanent.store_dic
        in
        let _ = fprintf stdout
          "Potential dependencies between sites:Number of covering classes:%i\n" number
        in
        (*------------------------------------------------------------------------------*)
        (*print covering class and theirs new-index*)
        let _ =
          print_dic_and_new_index parameter error
            remanent.store_new_index_dic
            remanent.store_test_new_index_dic
            remanent.store_modif_new_index_dic
            remanent.store_dic
        in
        (*------------------------------------------------------------------------------*)
        error
      in
      error) parameter result_remanent

(************************************************************************************)   
(*MAIN*)

let covering_classes parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set_cv parameter error handler cc_compil.rules in
  let _ = print_result parameter error result in
  error, result
