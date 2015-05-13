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

(************************************************************************************)
(*UTILITIES FUNCTION*)

let sprintf_string_list l =
  let acc = ref "{" in
  List.iteri (fun i x ->
    acc := !acc ^
      if i <> 0
      then Printf.sprintf "; %s" x
      else Printf.sprintf "%s" x
  ) l;
  !acc ^ "}"
    
let print_string_list l =
  let output = sprintf_string_list l in
  Printf.fprintf stdout "%s\n" output

(*------------------------------------------------------------------------------*)

let sprintf_list l =
  let acc = ref "{" in
  List.iteri (fun i x ->
    acc := !acc ^
      if i <> 0
      then Printf.sprintf "; %i" x
      else Printf.sprintf "%i" x
  ) l;
  !acc ^ "}"
    
let print_list l =
  let output = sprintf_list l in
  Printf.fprintf stdout "%s\n" output

let print_pair_list l =
  let rec aux acc =
    match acc with
      | [] -> acc
      | (s, i) :: tl ->
        Printf.fprintf stdout "s:%s:i%i" s i;
        aux tl
  in aux l

let print_list_list l =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: tl -> print_list x; aux tl
  in
  aux l

(*------------------------------------------------------------------------------*)

let print_dic parameter error remanent_field =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error i v _ _ ->
      let _ =
        Printf.fprintf stdout "int:%i\n" i;
        print_string "value_list:";
        print_list v; print_string "\n"
      in
      error
    ) remanent_field

(************************************************************************************)
(*sorted and ordered a covering classes in an descreasing order *)

let length_sorted lists =
  let list_length = List.rev_map (fun list -> list, List.length list) lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(************************************************************************************)
(*common function for getting an id in pointer backward*)

let get_id_common_set parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None -> error, 
      Cckappa_sig.Site_map_and_set.empty_set
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
    let error,new_id_set =
      Cckappa_sig.Site_map_and_set.add_set
        parameter
        error
        id
        old_id_set
    in
    (*store the result into pointer backward*)
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      old_id
      new_id_set
      pointer_backward)
    (error, pointer_backward)
    covering_class
    
(*------------------------------------------------------------------------------*)
(*store remanent*)

let store_remanent parameter error covering_class remanent =
  (* current state of remanent*)
  let pointer_backward    = remanent.Covering_classes_type.store_pointer_backward in
  let good_covering_class = remanent.Covering_classes_type.store_dic in
  (*------------------------------------------------------------------------------*)
  match covering_class with
  | [] -> error, remanent
  | _ ->
     (*get allocate_id from a dictionary*)
     let error, output =
       Covering_classes_type.Dictionary_of_Covering_class.allocate
         parameter
         error
         Misc_sa.compare_unit
         covering_class
         ()
         Misc_sa.const_unit
         good_covering_class
     in
     (*return id and result of covering class as a dictionary type*)
     let error, (covering_class_id, store_dic) =
       match output with
       | Some (id, _, _, dic) -> error, (id, dic)
       | None -> warn parameter error (Some "line 154") Exit (0, good_covering_class)
     in
     (*store pointer backward*)
     let error, pointer_backward =
       store_pointer_backward
         parameter
         error
         covering_class_id
         pointer_backward
         covering_class
     in
     (*result*)
     error,
     {
       Covering_classes_type.store_pointer_backward = pointer_backward;
       Covering_classes_type.store_dic              = store_dic
     }

(*------------------------------------------------------------------------------*)
(*CLEAN: In a covering class, it will store the old result of the previous
  covering class of an agent.

  For example:
  - rule 0: agent A has a covering class: (0)
  - rule 1: agent A has a covering class: (0,1)
  => Then do the intersection of two covering class of agent A: (0) inter (0,1)
  -> 0
*)

let clean_classes parameter error classes =
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_dic = Covering_classes_type.Dictionary_of_Covering_class.init () in
  (*------------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent = 
    {
      Covering_classes_type.store_pointer_backward = init_pointer;
      Covering_classes_type.store_dic              = init_dic
    }
  in
  (*------------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted classes in
  let is_empty_set = Cckappa_sig.Site_map_and_set.is_empty_set in
  List.fold_left (fun (error, remanent) covering_class ->
    match covering_class with
      | [] -> error, remanent
      | t :: tl ->
        let pointer_backward = remanent.Covering_classes_type.store_pointer_backward in
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
              (* intersection of two sets *)
              let error, potential_superset =
                Cckappa_sig.Site_map_and_set.inter
                  parameter
                  error
                  potential_supersets
                  potential_supersets'
              in
              if is_empty_set potential_superset
              then
                store_remanent
                  parameter
                  error
                  covering_class
                  remanent
              else
                aux tl' potential_superset
        in
        (*check the beginning state of a superset*)
        if is_empty_set potential_supersets
        then
          (*if it is empty then store it to remanent*)
          store_remanent
            parameter
            error
            covering_class
            remanent
        else
          aux tl potential_supersets)
    (error, init_remanent)
    current_covering_classes

(************************************************************************************)   
(*RULE*)

(*------------------------------------------------------------------------------*)
(*store the covering class by getting the old result and combine it with
  the current result *)

let add_covering_class parameter error agent_type sites_list covering_classes =
  match sites_list with
    | [] -> error, covering_classes
    | _ ->
      let error, get_old =
        Covering_classes_type.AgentMap.unsafe_get
          parameter
          error
          agent_type
          covering_classes
      in		 
       (* fetch the former list of covering classes *)
       let old_list =
         match get_old with
         | None -> []
         | Some sites -> sites
       in
       (* store the new list of covering classes *)
       let new_list = (List.rev sites_list) :: old_list in
       Covering_classes_type.AgentMap.set
         parameter
         error
         agent_type
         new_list
         covering_classes

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
  let error, store_covering_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold2_common
      parameter error
      (fun parameter error agent_id agent site_modif covering_classes ->
       (* if in the interface there is no site modified then do nothing *)
       if Cckappa_sig.Site_map_and_set.is_empty_map
            site_modif.Cckappa_sig.agent_interface
       then
         error, covering_classes
       else
         match agent with
         | Cckappa_sig.Ghost -> error, covering_classes
         | Cckappa_sig.Agent agent ->
            let sites_list =
              Cckappa_sig.Site_map_and_set.fold_map
	        (fun site _ current_class ->
                 site :: current_class)
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
                sites_list
                covering_classes
            in
            error, covering_classes                   
      )
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  error,
  {
    Covering_classes_type.store_covering_classes = store_covering_classes
  }           

(************************************************************************************)   
(*PART II. COMPUTE properties related to covering classes*)

(*------------------------------------------------------------------------------*)
(*Return the number of covering classes for each agent type*)

let number_of_covering_classes parameter error store_dic =
  let error, num =
    Covering_classes_type.Dictionary_of_Covering_class.last_entry
      parameter
      error
      store_dic
  in
  num + 1

(*------------------------------------------------------------------------------*)
(*Re-index the sites in each rule, each agent type and each covering class *)

let position x =
  let rec aux k = function
    | [] -> raise Not_found
    | y :: ys ->
      if x = y
      then k
      else aux (k+1) ys
  in aux 0;;

let re_index_value_list value_list =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: tl ->
        let nth = position x value_list in
        (nth + 1) :: aux tl
  in aux value_list

(*TEST*)


(*------------------------------------------------------------------------------*)
(*Projection*)(*TODO*)

let project_modified_site parameter error value_list store_modified_set =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: tl ->
        (*if x is a member of value_list; if yes then return the new position of
          x; otherwise continue to the rest of the list*)
        if List.mem x value_list (*TODO: change value_list to value_set*)
        then
          let n = (position x value_list) + 1 in
          n :: aux tl
        else aux tl        
  in aux store_modified_set


(************************************************************************************)   
(*PRINT*)

let print_remanent_dic parameter error store_dic =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_list _ _ ->
      let _ = Printf.printf "Covering_class_id:%i:" elt in
      let _ = print_string "site_type:";
        print_list value_list
      in
      error
    ) store_dic

(*------------------------------------------------------------------------------*)

let print_re_index parameter error store_dic =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_list _ _ ->
      let _ = 
        Printf.fprintf stdout "Re-index the sites in Covering_class_id:%i:site_type:" elt;
        let re_index = re_index_value_list value_list in
        print_list re_index
      in
      error      
    ) store_dic

(*------------------------------------------------------------------------------*)

let print_site_test parameter error store_dic =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_list _ _ ->
      let _ = 
        Printf.fprintf stdout "TEST:Covering_class_id:%i:\n" elt;
        let re_index = re_index_value_list value_list in
        let site_test =
          let rec aux acc =
            match acc with
              | [] -> acc
              | x :: tl ->
                Printf.fprintf stdout "site_type:%i\n" x;
                aux tl
          in
          aux re_index
        in
        site_test
      in
      error      
    ) store_dic

(*------------------------------------------------------------------------------*)
(*FIXME*)
let print_modification parameter error store_dic store_modified_set =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_list _ _ ->
      let _ =
        let modified =
          project_modified_site
            parameter
            error
            value_list
            store_modified_set
        in
        let _ = 
          Printf.fprintf stdout "\nCovering_class_id:%i:site_type:" elt;
          print_list modified
        in
        error
      in
      error
    ) store_dic

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result_remanent =
  Covering_classes_type.AgentMap.print
    error
    (fun error parameter remanent ->
      let _ =
        (* information of covering classes store in store_dic *)
        let _ =
          print_remanent_dic
            parameter
            error
            remanent.Covering_classes_type.store_dic
        in
        (*------------------------------------------------------------------------------*)
        (* number of covering classes*)
        let number =
          number_of_covering_classes
            parameter
            error
            remanent.Covering_classes_type.store_dic
        in
        let _ = Printf.fprintf stdout "Number of covering classes:%i\n" number in
        (*------------------------------------------------------------------------------*)
        (* re-index site in covering classes*)
        let _ =
          print_re_index
            parameter
            error
            remanent.Covering_classes_type.store_dic          
        in
        (*------------------------------------------------------------------------------*)
        (*sites that are tested in covering classes*)
        let _ =
          print_site_test
            parameter
            error
            remanent.Covering_classes_type.store_dic
        in
        (*------------------------------------------------------------------------------*)
        (*sites that are modified in covering classes*)
      
        (*------------------------------------------------------------------------------*)
        error
      in
      error)
    parameter
    result_remanent

(*------------------------------------------------------------------------------*)

let dump_agent parameter error handler =
  let agent_dic = handler.Cckappa_sig.agents_dic in
  let acc = ref [] in
  let _ = 
    Ckappa_sig.Dictionary_of_agents.print
      parameter
      error
      (fun parameter error i v a b ->
        acc := v::!acc ;
        error)
      agent_dic
  in ();
  !acc

(************************************************************************************)   
(*RULES*)

let scan_rule_set parameter error handler rules =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, init_class = Covering_classes_type.AgentMap.create parameter error n_agents
  in
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      Covering_classes_type.store_covering_classes = init_class
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, scan_rule =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        (*let _ = Printf.fprintf stdout "rule_id:%i\n" rule_id in*)
        scan_rule
          parameter
          error
          handler
          rule.Cckappa_sig.e_rule_c_rule
          classes
      )
      rules
      init_class
  in
  (*------------------------------------------------------------------------------*)
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = Covering_classes_type.AgentMap.create parameter error 0 in
  let error, remanent_dictionary =
    Covering_classes_type.AgentMap.fold
      parameter
      error
      (fun parameters error agent_type value_list init_remanent ->
        (*TEST site_dic*)
        let get_sites = handler.Cckappa_sig.sites in
        let error, get_site = 
          Int_storage.Nearly_inf_Imperatif.unsafe_get
            parameter
            error
            agent_type
            get_sites
        in
        let site =
          match get_site with
            | None -> Ckappa_sig.Dictionary_of_sites.init ()
            | Some site_dic -> site_dic
        in
        let _ =
          Ckappa_sig.Dictionary_of_sites.print
            parameter
            error
            (fun parameter error i v _ _ ->
              match v with
                | Ckappa_sig.Internal site_name ->
                  let _ =
                    Printf.fprintf stdout "internal:site_name:%s:%i\n" site_name i
                  in
                  error
                | Ckappa_sig.Binding site_name ->
                  let _ =
                  Printf.fprintf stdout "binding:site_name:%s:%i\n" site_name i
                  in error
            )
            site
        in
       (*clean the covering classes, removed duplicate covering classes*)
        let error, store_remanent =
          clean_classes
            parameters
            error
            value_list
        in
        (*store the covering classes after cleaning the duplicate*)
        let error, store_dic =
          Covering_classes_type.AgentMap.set
            parameters
            error
            agent_type
            store_remanent
            init_remanent
        in 
        (*result*)
        error, store_dic
      )
      scan_rule.Covering_classes_type.store_covering_classes
      init_result
  in
  error, remanent_dictionary (*FIXME: find a better return type*)

(************************************************************************************)   
(*MAIN*)

let covering_classes parameter error handler cc_compil =
  let agent_list = dump_agent parameter error handler in
  let _ = print_string "Agents:"; print_string_list (List.rev agent_list) in
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in        
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_result parameter error result in
  error, result
