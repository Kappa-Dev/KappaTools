(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
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

let collect_modified_map parameter error diff_reverse store_modified_map =
  Ckappa_sig.Agent_id_quick_nearly_inf_Imperatif.fold parameter error
    (fun parameter error agent_id site_modif store_modified_map ->
      (*if there is no modified sites then do nothing*)
      if Ckappa_sig.Site_map_and_set.Map.is_empty site_modif.Cckappa_sig.agent_interface
      then error, store_modified_map
      else
        let agent_type = site_modif.Cckappa_sig.agent_name in
        let error', store_site =
          Ckappa_sig.Site_map_and_set.Map.fold
            (fun site port (error,current_map) ->
              (*store site map*)
              let error,site_map =
                Ckappa_sig.Site_map_and_set.Map.add
		  parameter
		  error 
                  site
                  site
                  current_map
              in
              error,site_map)
            site_modif.Cckappa_sig.agent_interface
            (error, Ckappa_sig.Site_map_and_set.Map.empty)
        in
	let error = Exception.check warn parameter error error' (Some "line 58") Exit in 
        (*compute site_map*)
        let error, old_map =
          match
            Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.unsafe_get
            parameter
            error
            agent_type
            store_modified_map
          with
            | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
            | error, Some m -> error, m
        in
        (*store*)
        let error,final_map =
          Ckappa_sig.Site_map_and_set.Map.union 
            parameter 
            error 
            old_map
            store_site
        in
        let error', store_modified_map =
          Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.set
            parameter
            error
            agent_type
            final_map
            store_modified_map
        in
	let error = Exception.check warn parameter error error' (Some "line 85") Exit in
 
        error, store_modified_map
    ) diff_reverse
    store_modified_map

(************************************************************************************)   
(*compute covering_class*)

let add_covering_class parameter error agent_type list store_covering_classes =
  match list with
    | [] -> error, store_covering_classes
    | _ ->
      let error, old_list =
        match 
          Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.unsafe_get
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
      Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.set 
        parameter
        error
        agent_type
        new_pair_list
        store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute covering classes, site test and bdu*)

let collect_covering_classes parameter error views diff_reverse store_covering_classes =
  let error, store_covering_classes =
    Ckappa_sig.Agent_id_quick_nearly_inf_Imperatif.fold2_common parameter error
      (fun parameter error agent_id agent site_modif store_covering_classes ->
        (* if in the interface there is no site modified then do nothing *)
        if Ckappa_sig.Site_map_and_set.Map.is_empty 
          site_modif.Cckappa_sig.agent_interface
        then error, store_covering_classes
        else
          match agent with
	  | Cckappa_sig.Ghost 
          | Cckappa_sig.Unknown_agent _ -> error, store_covering_classes
	  | Cckappa_sig.Dead_agent (agent, _, _, _) 
	  | Cckappa_sig.Agent agent ->
              let agent_type = agent.Cckappa_sig.agent_name in
              (*get a list of sites from an interface at each rule*)
              let site_list =
                Ckappa_sig.Site_map_and_set.Map.fold
                  (fun site _ current_list ->
                    site :: current_list
                  ) agent.Cckappa_sig.agent_interface []
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

(************************************************************************************)
(*compute covering class: it is a covering class whenever there is a
  modified site in that agent. (CHECK on their left-hand side)

  For example: A(x~u,y~u) -> A(x~u,y~p) (where y is a modified site), then
  there is one covering class for agent A: CV_1: (x,y)
  
  - If the rule is: A(x~u), A(y~u) -> A(x~p), A(y~p), (x,y are modified
  sites), then agent A has two covering classes: CV_1: x; CV_2: y
  
  - If the rule is: A(x~u), A(y~u) -> A(x~u), A(y~p), (y is a modified
  site), then agent A has only one covering class: CV_1: y
*)

let scan_rule_covering_classes parameter error handler rule classes =
  (*------------------------------------------------------------------------------*)
  (*compute modified map*)
  let error, store_modified_map =
    collect_modified_map
      parameter
      error
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_modified_map
  in
  (*------------------------------------------------------------------------------*)
  (*compute covering_class*)
  let error, store_covering_classes =
    collect_covering_classes
      parameter
      error
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    Covering_classes_type.store_modified_map     = store_modified_map;
    Covering_classes_type.store_covering_classes = store_covering_classes;
  }           

(************************************************************************************)   
(*RULES*)

let scan_rule_set_covering_classes parameter error handler rules =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, init_modif_map = 
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.create parameter error n_agents in
  let error, init_class =
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.create parameter error n_agents in
  (*------------------------------------------------------------------------------*)
  (* add each singleton as a covering class *)
  let error, init_class = 
      (*Int_storage.Nearly_inf_Imperatif.fold*)
    Ckappa_sig.Agent_type_nearly_inf_Imperatif.fold
      parameter 
      error
      (fun parameters error a b init_class -> 
       Ckappa_sig.Dictionary_of_sites.fold 
	 (fun _ _ b (error, init_class) ->
	  let error, l' =
	    match
	      Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.unsafe_get
                parameters 
                error
                a 
                init_class
	    with 
	    | error,None -> error,[[b]]
	    | error,Some l -> error,[b]::l
	  in
	  Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.set
            parameters 
            error
            a 
            l' 
            init_class
	 )
	 b (error,init_class)
      )
      handler.Cckappa_sig.sites
      init_class
  in
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      Covering_classes_type.store_modified_map     = init_modif_map;
      Covering_classes_type.store_covering_classes = init_class;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a list of covering classes*)
  let error, store_covering_classes =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        let error, result =
          scan_rule_covering_classes
            parameter
            error
            handler
            rule.Cckappa_sig.e_rule_c_rule
            classes
        in
        error, result
      ) rules init_class
  in
  error, store_covering_classes

(************************************************************************************)
(*clean covering classes*)

let length_sorted (l: int list list): int list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists       = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(************************************************************************************)
(*port information for site (state)*)

let int_of_port port = port.Cckappa_sig.site_state.Cckappa_sig.min
    
(************************************************************************************)
(*common function for getting an id in pointer backward*)

let get_id_common_set parameter error t set =
  match 
    (*Nearly_inf_Imperatif.unsafe_get*)
    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
    parameter
    error 
    t 
    set
  with
    | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
    | error, Some id -> error, id

(************************************************************************************)
(*new indexes*)

(*
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
      *)
(*let re_index_value_list list = (*TO BE FIXED*)
  let l =
    let rec aux acc =
      match acc with
      | [] -> acc
      | x :: tl ->
        let nth = position x list in
        (nth + 1) :: aux tl
    in aux list
  in
  l*)
  (*List.fold_left (fun l x ->
    (Cckappa_sig.site_name_of_int x) :: l
  ) [] l*)

(*------------------------------------------------------------------------------*)
(*projection site that are modified with new index*)

(*let project_modified_site value_list modified_map =
  (*TODO:add state information*)
  let l =
    let rec aux acc =
      match acc with
      | [] -> []
      | x :: tl ->
        if not (Ckappa_sig.Site_map_and_set.Map.is_empty modified_map)
        then
          if Ckappa_sig.Site_map_and_set.Map.mem x modified_map
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
  in
  l*)
  (*List.fold_left (fun l x ->
    (Cckappa_sig.site_name_of_int x) :: l
  ) [] l*)
    
(*------------------------------------------------------------------------------*)
(*compute new index dictionary*)

(*let new_index_dic parameter error new_index_covering_class good_index =
  let error, output =
    Covering_classes_type.Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      new_index_covering_class
      ()
      Misc_sa.const_unit
      good_index
  in
  let error, (id_dic, store_dic) =
    match output with
    | Some (id, _, _, dic) -> error, (id, dic)
    | None -> warn parameter error (Some "95") Exit 
      (0, good_index)
  in
  error, (id_dic, store_dic)*)

(*------------------------------------------------------------------------------*)
(*compute tested dictionary with new index *)

(*let test_new_index_dic parameter error new_id new_dic good_test_dic =
  let error, (value_index_dic, _, _) =
    Misc_sa.unsome
      (Covering_classes_type.Dictionary_of_Covering_class.translate
         parameter
         error
         new_id
         new_dic)
      (fun error -> warn parameter error (Some "line 236") Exit ([], (),()))
  in
  (*return site_test_dic*)
  let error, output =
    Covering_classes_type.Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      value_index_dic
      ()
      Misc_sa.const_unit
      good_test_dic
  in
  let error, (id_dic, store_dic) =
    match output with
    | Some (id, _, _, dic) -> error, (id, dic)
    | None -> warn parameter error (Some "131") Exit
      (0, good_test_dic)
  in
  error, (id_dic, store_dic)*)

(*------------------------------------------------------------------------------*)
(*compute modified dictionary with new_index*)

(*let modified_index_dic parameter error covering_class modified_map 
    good_new_index_modif_dic =
  let modified_value = project_modified_site covering_class modified_map in
  (*let modified_value = List.fold_left (fun l x ->
    (Cckappa_sig.site_name_of_int x) :: l
  ) [] modified_value
  in*)
  let error, out_modif_dic =
    Covering_classes_type.Dictionary_of_Modified_class.allocate
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
      | None -> warn parameter error (Some "line 155") Exit
        (0, good_new_index_modif_dic)
      | Some (id, _, _, dic) -> error, (id, dic)        
  in
  error, (new_modif_id, modif_index_dic)*)

(************************************************************************************)   
(*PART II. COMPUTE properties related to covering classes*)

(*------------------------------------------------------------------------------*)
(*return the number of covering classes for each agent type*)

let number_of_covering_classes parameter error store_dic =
  let error, num =
    Covering_classes_type.Dictionary_of_Covering_class.last_entry
      parameter error store_dic
  in num + 1

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
    let error', new_id_set = 
      Ckappa_sig.Site_map_and_set.Set.add 
        parameter
        error
        id (*site_name*)
        old_id_set
    in
    let error = Exception.check warn parameter error error' (Some "line 71") Exit in 
    (*store the result into pointer backward*)
    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.set
      parameter
      error
      old_id (*site_name*)
      new_id_set
      pointer_backward
  )
    (error, pointer_backward)
    covering_class
  
(*------------------------------------------------------------------------------*)
(*compute covering class dictionary*)

let covering_class_dic parameter error covering_class good_covering_class =
  let error, output =
    Covering_classes_type.Dictionary_of_Covering_class.allocate
      parameter
      error
      Misc_sa.compare_unit_site_name
      covering_class
      ()
      Misc_sa.const_unit
      good_covering_class
  in
  (*return id and result as a dictionary type*)
  let error, (id_dic, store_dic) =
    match output with
      | Some (id, _, _, dic) -> error, (id, dic)
      | None -> warn parameter error (Some "line 105") Exit
        (0, good_covering_class)
  in
  error, (id_dic, store_dic)

(*------------------------------------------------------------------------------*)

let store_remanent parameter error covering_class modified_map remanent =
  (* current state of remanent*)
  let pointer_backward    = remanent.Covering_classes_type.store_pointer_backward in
  let good_covering_class = remanent.Covering_classes_type.store_dic in
  (*let good_index          = remanent.Covering_classes_type.store_new_index_dic in
  let good_test_dic       = remanent.Covering_classes_type.store_test_new_index_dic in
  let good_modif_dic      = remanent.Covering_classes_type.store_modif_new_index_dic in*)
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
  (*let new_index_covering_class = 
    re_index_value_list 
      covering_class
  in
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
  in*)
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    Covering_classes_type.store_pointer_backward    = pointer_backward;
    Covering_classes_type.store_dic                 = store_dic;
    (*Covering_classes_type.store_new_index_dic       = new_dic;
    Covering_classes_type.store_test_new_index_dic  = test_new_index_dic;
    Covering_classes_type.store_modif_new_index_dic = modif_index_dic;*)
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
  
let clean_classes parameter error covering_classes modified_map =
  let error, init_pointer = 
    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create parameter error 0 
  in
  (*let init_index          = Covering_classes_type.Dictionary_of_Covering_class.init () in*)
  let init_store_dic      = Covering_classes_type.Dictionary_of_Covering_class.init () in
  (*let init_test_index     = Covering_classes_type.Dictionary_of_Covering_class.init () in
  let init_modif_index    = Covering_classes_type.Dictionary_of_Modified_class.init() in*)
  (*------------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent = 
    {
      Covering_classes_type.store_pointer_backward    = init_pointer;
      Covering_classes_type.store_dic                 = init_store_dic;
      (*Covering_classes_type.store_new_index_dic       = init_index;
      Covering_classes_type.store_test_new_index_dic  = init_test_index;
      Covering_classes_type.store_modif_new_index_dic = init_modif_index;*)
    }
  in
  (*------------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted covering_classes in
  let is_empty_set = Ckappa_sig.Site_map_and_set.Set.is_empty in
  List.fold_left (fun (error, remanent) covering_class ->
    match covering_class with
      | [] -> error,remanent
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
              (*-------------------------------------------------------------------*)
              (* intersection of two sets *)
              let error',potential_superset =
                Ckappa_sig.Site_map_and_set.Set.inter 
                  parameter 
                  error 
                  potential_supersets
                  potential_supersets'
              in
	      let error = Exception.check warn parameter error error'
                (Some "line 71") Exit
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

(*------------------------------------------------------------------------------*)
(*compute covering classes in the set of rules*)

let scan_rule_set_remanent parameter error handler rules =
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = 
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.create parameter error 0 
  in
  let error, store_covering_classes = 
    scan_rule_set_covering_classes parameter error handler rules
  in
  let result_covering_classes = 
    store_covering_classes.Covering_classes_type.store_covering_classes
  in
  let error, remanent_dictionary =
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.fold parameter error
      (fun parameters error agent_type covering_class init_remanent ->
        (*------------------------------------------------------------------------------*)
        (*get modified site*)
        let error, modified_map =
          match
            Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.unsafe_get
              parameter 
              error 
              agent_type
              store_covering_classes.Covering_classes_type.store_modified_map
          with
            | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
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
        (*compute the number of covering classes*)
        let error, get_number_cv =
          (Covering_classes_type.Dictionary_of_Covering_class.last_entry 
             parameter error
            store_remanent_dic.Covering_classes_type.store_dic)
        in
        let number_cv = get_number_cv + 1 in
        (*------------------------------------------------------------------------------*)
        (*print covering classes*)
        let _ =
          if Remanent_parameters.get_dump_site_dependencies parameter
          then
            let parameter =
              Remanent_parameters.update_prefix parameter ""
            in
            let error, agent_string =
              Handler.string_of_agent parameter error handler agent_type
            in
            let _ =
              Covering_classes_type.Dictionary_of_Covering_class.print parameter error
                (fun parameter error elt_id site_type_list _ _ ->
                  let _ =
                    Printf.fprintf stdout 
                      "Potential dependencies between sites:Number of covering classes:%i\n"
                      number_cv
                  in
                  let _ =
                    (*print covering_class_id*)
                    Printf.fprintf stdout 
                      "Potential dependencies between sites:\nagent_type:%i:%s:covering_class_id:%i\n"
                      (Ckappa_sig.int_of_agent_name agent_type)
                      agent_string elt_id
                  in
                  let _ =
                    List.iter (fun site_type ->
                      let error, site_string =
                        Handler.string_of_site parameter error handler agent_type site_type
                      in
                      Printf.fprintf stdout "site_type:%i:%s\n" site_type site_string
                    ) site_type_list
                  in
                  error
                ) store_remanent_dic.Covering_classes_type.store_dic
            in
            ()
        in
        (*------------------------------------------------------------------------------*)
        (*store the covering classes after cleaning theirs duplicate classes*)
        let error, store_remanent =
          Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.set 
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
(*list to set*)

let new_index_pair_map parameter error l =
  let rec aux acc k map1 map2 error =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error, map1 = 
        Ckappa_sig.Site_map_and_set.Map.add 
          parameter
          error
          h 
          k
          map1 
      in
      let error, map2 =
        Ckappa_sig.Site_map_and_set.Map.add
          parameter
          error
          k 
          h
          map2 
      in
      aux
        tl
        (k + 1)
        map1
        map2
        error 
  in
  let error', (map1, map2) = 
    aux 
      l
      1
      Ckappa_sig.Site_map_and_set.Map.empty 
      Ckappa_sig.Site_map_and_set.Map.empty 
      error
  in
  let error = Exception.check warn parameter error error' (Some "line 49") Exit in
  error, (map1, map2)

(************************************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  let error',list =
    List.fold_left
      (fun (error, current_set) elt ->
       let error, add_set =
	 Ckappa_sig.Site_map_and_set.Set.add 
           parameter
           error
           elt 
           current_set
       in
       error, add_set
      ) (error, Ckappa_sig.Site_map_and_set.Set.empty) list
  in
  let error = Exception.check warn parameter error error' (Some "line 66") Exit in
  error, list 

(************************************************************************************)

let collect_remanent_list2set parameter error handler_kappa store_remanent  =
  let error, init = 
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.create parameter error 0 
  in
  Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.fold parameter error 
    (fun paramter error agent_type remanent store_result ->
     (*-------------------------------------------------------------------------*)
     (*get covering classes dictionary*)
     let store_dic = remanent.Covering_classes_type.store_dic in
     let error, (id_list, site_set_list) =
       Covering_classes_type.Dictionary_of_Covering_class.fold
	 (fun list _ index (error, (index_list, current_list)) ->
	  let error, list2set = list2set parameter error list in
	  let list_set = list2set :: current_list in
	  let list_index = index :: index_list in
	  error, (List.rev list_index, List.rev list_set)
	 ) store_dic (error, ([], []))
     in
      (*-------------------------------------------------------------------------*)
      (*store a mapping function from a list of covering class into a list
        of new index and a pair of map*)
     let error, (id_list_map, list_of_map) =
       Covering_classes_type.Dictionary_of_Covering_class.fold
         (fun list _ index (error, (index_list, current_list)) ->
           let error, store_map =
             new_index_pair_map
               parameter
               error 
               list
           in
           let new_list = store_map :: current_list in
           let index_list = index :: index_list in
           error, (List.rev index_list, List.rev new_list)
          ) store_dic (error, ([], []))
     in
     (*-------------------------------------------------------------------------*)
     (*print covering classes*)
     let _ =
       if Remanent_parameters.get_dump_reachability_analysis_covering_classes parameter
       then
         (*let _ = Format.printf "Reachability analysis potential dependencies...@." in *)
         let parameter =
           Remanent_parameters.update_prefix parameter ""
         in
         if Remanent_parameters.get_trace parameter
         then
           let error, agent_string = 
             try
               Handler.string_of_agent parameter error handler_kappa agent_type
             with
               _ -> warn parameter error (Some "line 118") Exit 
                 (Ckappa_sig.string_of_agent_name agent_type)
           in
           List.iter (fun id ->
             List.iter (fun site_set ->
               let _ =
                 Printf.fprintf stdout 
                   "Potential dependencies between sites:\nagent_type:%i:%s:covering_class_id:%i\n" 
                   (Ckappa_sig.int_of_agent_name agent_type)
                   agent_string
                   id
               in
               Ckappa_sig.Site_map_and_set.Set.iter (fun site_type ->
                 let error, site_string =
                   try
                     Handler.string_of_site parameter error handler_kappa 
                       agent_type site_type
                   with
                     _ -> warn parameter error (Some "line 132") Exit 
                       (string_of_int site_type)
                 in
                 Printf.fprintf stdout "site_type:%i:%s\n" site_type site_string
               ) site_set
             ) site_set_list
           ) id_list
         else ()
     in
     (*-------------------------------------------------------------------------*)
     (*print mapping with new indexes of site_type*)
     let _ =
        if Remanent_parameters.get_dump_reachability_analysis_covering_classes parameter
        then
          let parameter =
            Remanent_parameters.update_prefix parameter ""
          in
          if Remanent_parameters.get_trace parameter
          then
            let error, agent_string = 
              try
                Handler.string_of_agent parameter error handler_kappa 
                  agent_type
              with
                _ -> warn parameter error (Some "line 155") Exit 
                  ((Ckappa_sig.string_of_agent_name agent_type))
            in
            List.iter (fun id ->
              let _ =
                Printf.fprintf stdout
                  "Mapping between global identifier of sites (per agent) and local identifier of sites (per covering classes):\nagent_type:%i:%s:covering_class_id:%i\n" 
                  (Ckappa_sig.int_of_agent_name agent_type)
                  agent_string
                  id
              in
              List.iter (fun (map1, map2) ->
                let _ =
                  Ckappa_sig.Site_map_and_set.Map.iter
                    (fun site site_new ->
                      let error, site_string =
                        try
                          Handler.string_of_site parameter error handler_kappa
                            agent_type site
                        with
                          _ -> warn parameter error (Some "line 172") Exit 
                            (string_of_int site)
                      in
                      Printf.fprintf stdout
                        "Global:site_type:%i:%s  => Local:site_type':%i:%s\n" 
                        site site_string site_new site_string
                    ) map1
                in
                Ckappa_sig.Site_map_and_set.Map.iter
                  (fun site_new site ->
                    let error, site_string =
                      try
                        Handler.string_of_site parameter error handler_kappa agent_type site
                      with
                        _ -> warn parameter error (Some "line 186") Exit 
                          (string_of_int site)
                    in
                    Printf.fprintf stdout 
                      "Local:site_type':%i:%s  => Global:site_type:%i:%s\n"
                      site_new site_string site site_string
                  ) map2
              ) list_of_map
            ) id_list_map
          else ()
     in
     (*-------------------------------------------------------------------------*)
     (*store*)
     let error, store_result =
       Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.set
         parameter
         error
         agent_type
         ((id_list, site_set_list),
          (id_list_map, list_of_map))
         store_result
     in
     error, store_result
    ) store_remanent init

(************************************************************************************)   
(*MAIN*)

let covering_classes parameter error handler cc_compil =
  let error, init =
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.create parameter error 0 
  in
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result = scan_rule_set_remanent parameter error handler 
    cc_compil.Cckappa_sig.rules 
  in
  (*convert a list of site inside result [remanent] to a set of site*)
  let error =
    if (Remanent_parameters.get_trace parameter) || trace
    then
      let error, covering_classes_set2list =
        collect_remanent_list2set 
          parameter
          error
          handler
          result
      in
      error
    else
      error
  in
  error, result
