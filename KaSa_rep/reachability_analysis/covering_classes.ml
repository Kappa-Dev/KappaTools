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

let empty_set = Cckappa_sig.Site_map_and_set.empty_set
let empty_map = Cckappa_sig.Site_map_and_set.empty_map
let add_set = Cckappa_sig.Site_map_and_set.add_set
let union = Cckappa_sig.Site_map_and_set.union

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

(************************************************************************************)
(*sorted and ordered a covering classes in an descreasing order *)

let length_sorted (l: (int * Cckappa_sig.Site_map_and_set.set) list list):
    (int * Cckappa_sig.Site_map_and_set.set) list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(************************************************************************************)
(*print port information for site*)

let int_of_port port =
  port.Cckappa_sig.site_state.Cckappa_sig.min
    
(************************************************************************************)
(*common function for getting an id in pointer backward*)

let get_id_common_set parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get
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
    Int_storage.Nearly_inf_Imperatif.set
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

let fst_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (x,_) :: tl -> x :: aux tl
  in aux l

let re_index_value_list pair_list =
  let rec aux acc =
    match acc with
      | [] -> acc
      | (x, s) :: tl ->
        let value_list = fst_list pair_list in
        let nth = position x value_list in
        ((nth + 1), s) :: aux tl
  in aux pair_list

(*------------------------------------------------------------------------------*)
(*projection site that are modified with new index*)

let project_modified_site value_list modified_map = (*TODO:add state information*)
  let rec aux acc =
    match acc with
      | [] -> []
      | x :: tl ->
        if not (Cckappa_sig.Site_map_and_set.is_empty_map modified_map)
        then
          if Cckappa_sig.Site_map_and_set.mem_map x modified_map
          then
            let nth_1 = (position x value_list) + 1 in
            let l = nth_1 :: aux tl in
            l
          else aux tl
        else [] (*if modified_set is empty then nothing*)
  in aux value_list

(************************************************************************************)
(*DICTIONARY*)

let common_allocate_dic parameter error value good_dic =
  let error, output =
    Covering_classes_type.Dictionary_of_Covering_class.allocate
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

let covering_class_dic parameter error pair_covering_class good_covering_class =
  common_allocate_dic
    parameter
    error
    pair_covering_class 
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
      (Covering_classes_type.Dictionary_of_Covering_class.translate
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
      | None -> warn parameter error (Some "line 252") Exit (0, good_new_index_modif_dic)
      | Some (id, _, _, dic) -> error, (id, dic)        
  in
  error, (new_modif_id, modif_index_dic)

(*------------------------------------------------------------------------------*)
(*store remanent*)
let print_pair l = (*TODO*)
  let rec aux acc =
    match acc with
      | [] -> ()
      | (x,y) :: tl ->
        Printf.fprintf stdout "site_type:%i:site_state:%i\n"
          x y; aux tl
  in
  aux l

let store_remanent parameter error pair_covering_class modified_map remanent =
  (* current state of remanent*)
  let pointer_backward    = remanent.Covering_classes_type.store_pointer_backward in
  let good_covering_class = remanent.Covering_classes_type.store_dic in
  let good_index          = remanent.Covering_classes_type.store_new_index_dic in
  let good_test_dic       = remanent.Covering_classes_type.store_test_new_index_dic in
  let good_modif_dic      = remanent.Covering_classes_type.store_modif_new_index_dic in
  (*------------------------------------------------------------------------------*)
  match pair_covering_class with
    | [] -> error, remanent
    | _ ->
      (*------------------------------------------------------------------------------*)
      (*covering class dictionary*)
      let error, (covering_class_id, store_dic) =
        covering_class_dic 
          parameter 
          error 
          pair_covering_class
          good_covering_class 
      in
      (*------------------------------------------------------------------------------*)
      (*store pointer backward*)
      let covering_class = fst_list pair_covering_class in
      let error, pointer_backward =
        store_pointer_backward 
          parameter
          error
          covering_class_id 
          pointer_backward
          covering_class 
      in
      (*------------------------------------------------------------------------------*)
      (* BDU*)
      (*let _ =
        let rec aux acc =
          match acc with
            | [] -> []
            | (site, set) :: tl ->
              let list = Cckappa_sig.Site_map_and_set.elements set in
              let rec aux' acc' =
                match acc' with
                  | [] -> []
                  | x :: tl' ->
                    let l = (site, x) :: aux' tl' in
                    Printf.fprintf stdout "list:";
                    print_pair l; print_string "\n";
                    aux tl
              in
              aux' list
        in
        aux pair_covering_class      
      in*)
      (*------------------------------------------------------------------------------*)
      (*PART II: compute new_index in covering_class*)
      let new_index_covering_class = re_index_value_list pair_covering_class in
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
        Covering_classes_type.store_pointer_backward    = pointer_backward;
        Covering_classes_type.store_dic                 = store_dic;
        Covering_classes_type.store_new_index_dic       = new_dic;
        Covering_classes_type.store_test_new_index_dic  = test_new_index_dic;
        Covering_classes_type.store_modif_new_index_dic = modif_index_dic;
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

let init_dic = Covering_classes_type.Dictionary_of_Covering_class.init ()

let clean_classes parameter error pair_covering_classes modified_map =
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_index       = init_dic in
  let init_store_dic   = init_dic in
  let init_test_index  = init_dic in
  let init_modif_index = Covering_classes_type.Dictionary_of_Modified_class.init() in
  (*------------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent = 
    {
      Covering_classes_type.store_pointer_backward    = init_pointer;
      Covering_classes_type.store_dic                 = init_store_dic;
      Covering_classes_type.store_new_index_dic       = init_index;
      Covering_classes_type.store_test_new_index_dic  = init_test_index;
      Covering_classes_type.store_modif_new_index_dic = init_modif_index;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted pair_covering_classes in
  let is_empty_set = Cckappa_sig.Site_map_and_set.is_empty_set in
  List.fold_left (fun (error, remanent) covering_class ->
    match covering_class with
      | [] -> error, remanent
      | (t, s) :: tl ->
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
	    | (t', s') :: tl' ->
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
                let error, result_covering_dic =
                  store_remanent parameter error
                    covering_class
                    modified_map
                    remanent
                in
                error, result_covering_dic
              else
                aux tl' potential_superset
        in
        (*check the beginning state of a superset*)
        if is_empty_set potential_supersets
        then
          (*if it is empty then store it to remanent*)
          let error, result_covering_dic =
            store_remanent parameter error
              covering_class
              modified_map
              remanent
          in
          error, result_covering_dic
        else
          aux tl potential_supersets)
    (error, init_remanent)
    current_covering_classes

(************************************************************************************)   
(*RULE -- VAR *)

let get_old_port parameter error agent_type port_set store_port =
  let old_set = 
    match 
      Covering_classes_type.AgentMap.unsafe_get 
        parameter 
        error 
        agent_type
        store_port 
    with
      | error, None -> empty_set
      | error, Some s -> s
  in
  let error, new_set = 
    union 
    parameter 
      error
      port_set
      old_set
  in
  error, new_set

(*------------------------------------------------------------------------------*)

let add_state_class parameter error agent_type port_set store_port =
  let error, set =
    get_old_port 
      parameter
      error 
      agent_type
      port_set 
      store_port 
  in
  let error, store_port =
    Covering_classes_type.AgentMap.set
      parameter
      error
      agent_type
      set
      store_port
  in error, store_port

(*------------------------------------------------------------------------------*)

let collect_modified_map parameter error diff_reverse store_modified_map =
  Covering_classes_type.AgentMap.fold parameter error
    (fun parameter error agent_id site_modif store_modified_map ->
      (*if there is no modified sites then do nothing*)
      if Cckappa_sig.Site_map_and_set.is_empty_map
        site_modif.Cckappa_sig.agent_interface
      then error, store_modified_map
      else
        let agent_type = site_modif.Cckappa_sig.agent_name in
        let store_site =
          Cckappa_sig.Site_map_and_set.fold_map
            (fun site port current_map ->
              (*store site map*)
              let error, site_map =
                Cckappa_sig.Site_map_and_set.add_map
                  parameter
                  error
                  site
                  site
                  current_map
              in
              site_map)
            site_modif.Cckappa_sig.agent_interface empty_map
        in
        (*compute site_map*)
        let error, old_map =
          match
          Covering_classes_type.AgentMap.unsafe_get
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
          Cckappa_sig.Site_map_and_set.union_map
            parameter
            error
            old_map
            store_site
        in
        let error, store_modified_map =
          Covering_classes_type.AgentMap.set
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

let add_covering_class parameter error agent_type pair_site store_covering_classes =
  match pair_site with
    | [] -> error, store_covering_classes
    | _ ->
      let error, old_pair =
        match 
          Covering_classes_type.AgentMap.unsafe_get
            parameter 
            error
            agent_type
            store_covering_classes
        with
          | error, None -> error, []
          | error, Some sites -> error, sites
       in
       (* store the new list of covering classes *)
      let new_site_list = (List.rev pair_site) :: old_pair in
      Covering_classes_type.AgentMap.set 
        parameter
        error
        agent_type
        new_site_list
        store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute the state information*)

let store_port_state parameter error agent_type port_min current_port_min
    store_state_min =
  let error, port_set_min = 
    add_set 
      parameter
      error 
      port_min
      current_port_min
  in
  let error, set_min = 
    get_old_port
      parameter
      error 
      agent_type
      port_set_min
      store_state_min
  in set_min

(*------------------------------------------------------------------------------*)
(*store covering class and port state class*)

let store_covering_port_state_class parameter error agent_type site_list fst_cv
    port_set_min snd_port_min =
  (*store covering class*)
  let error, covering_classes =
    add_covering_class
      parameter 
      error
      agent_type
      site_list 
      fst_cv
  in
  (*store port min*)
  let error, store_port_min =
    add_state_class
      parameter 
      error
      agent_type
      port_set_min 
      snd_port_min
  in error, (covering_classes, store_port_min)

(*------------------------------------------------------------------------------*)

let collect_covering_classes parameter error views diff_reverse store_covering_classes =
  let error, store_covering_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold2_common parameter error
      (fun parameter error agent_id agent site_modif store_covering_classes ->
        (* if in the interface there is no site modified then do nothing *)
        if Cckappa_sig.Site_map_and_set.is_empty_map
          site_modif.Cckappa_sig.agent_interface
        then error, store_covering_classes
        else
          match agent with
            | Cckappa_sig.Ghost -> error, store_covering_classes
            | Cckappa_sig.Agent agent ->
              let agent_type = agent.Cckappa_sig.agent_name in
              let (store_cv, store_state_min) = store_covering_classes in
              (*get a list of sites from an interface at each rule*)
              let pair_site =
                Cckappa_sig.Site_map_and_set.fold_map
	          (fun site port (store_current_class, store_current_port_min) ->
                    (*get state min*)
                    let port_min = int_of_port port in
                    let set_min =
                      store_port_state
                        parameter
                        error
                        agent_type
                        port_min
                        store_current_port_min
                        store_state_min
                    in
                    (*store*)
                    let site_list = (site, set_min) :: store_current_class in
                    (site_list, set_min)
                  ) agent.Cckappa_sig.agent_interface ([], empty_set)
              in
              (* store new_covering_class in the classes of the agent type
                 agent_type *)
              let (site_list, port_min_set) = pair_site in
              (*store covering class*)
              let error, (covering_classes, store_port_min) =
                store_covering_port_state_class 
                  parameter
                  error
                  agent_type
                  site_list 
                  store_cv
                  port_min_set
                  store_state_min
              in
              (*store*)
              error, (covering_classes, store_port_min)
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
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_modified_map
  in
  (*------------------------------------------------------------------------------*)
  (*compute covering_class*)
  let error, store_covering_classes =
    collect_covering_classes parameter error
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    Covering_classes_type.store_modified_map     = store_modified_map;
    Covering_classes_type.store_covering_classes = store_covering_classes
  }           

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
(*PRINT*)

let print_pair_list l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | (x,s) :: tl ->
        let l = Cckappa_sig.Site_map_and_set.elements s in
        Printf.fprintf stdout "site_type:%i->state_min:" 
          x; print_list l;
        aux tl
  in aux l

(*------------------------------------------------------------------------------*)
(*print new index*)

let print_new_index_dic parameter error elt_id store_index =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt pair_index _ _ ->
      let _ = 
        Printf.fprintf stdout
          "Potential dependencies between sites:New-index:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_pair_list pair_index
      in
      error
    ) store_index
    
(*------------------------------------------------------------------------------*)
(*print test*)

let print_test_new_index_dic parameter error elt_id store_test =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_index _ _ ->
      let _ = Printf.fprintf stdout
        "Potential dependencies between sites:New-index:TEST:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_pair_list value_index
      in
      error
    ) store_test

(*------------------------------------------------------------------------------*)
(*print modified*)

let print_modified_dic parameter error elt_id store_modif =(*FIXME:add state*)
  Covering_classes_type.Dictionary_of_Modified_class.print
    parameter
    error
    (fun parameter error elt l _ _ ->
      let _ =
        Printf.fprintf stdout 
          "Potential dependencies between sites:New-index:MODIFICATION-:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_list l
      in
      error
    ) store_modif

(*------------------------------------------------------------------------------*)
(*print remove action*)

let print_dic_and_new_index parameter error store_index store_test store_modif 
    store_dic =
  Covering_classes_type.Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt_id pair_list _ _ ->
      let _ =
        let _ =
        (*print covering class in dictionary*)
          Printf.printf "Potential dependencies between sites:Covering_class_id:%i:\n"
            elt_id; 
          print_pair_list pair_list
        in
        (*print new_index for covering class*)
        let _ = print_new_index_dic
          parameter
          error 
          elt_id 
          store_index 
        in
        (*print site that is tested with its new_index*)
        let _ = print_test_new_index_dic
          parameter 
          error 
          elt_id
          store_test 
        in
        (*print site that is modified with its new_index*)
        print_modified_dic 
          parameter
          error 
          elt_id 
          store_modif
      in error)
    store_dic

(*------------------------------------------------------------------------------*)

let print_value_site parameter error elt site value_site =
  Int_storage.Quick_Nearly_inf_Imperatif.print
    error
    (fun error parameter value_site_list ->
      let _ =
        let rec aux_value acc' =
          match acc' with
            | [] -> acc'
            | vsite :: tl' ->
              let _ =
                Printf.fprintf stdout 
                  "Potential dependencies between sites:New-index:Covering_class_id:%i:" elt;
                match vsite with
                  | Ckappa_sig.Internal s ->
		    Printf.fprintf stdout "site_modified:%i->%s(internal state)\n"
                      site s
	          | Ckappa_sig.Binding s ->
		    Printf.fprintf stdout "site_modified:%i->%s(binding state)\n"
                      site s                                     
              in aux_value tl'
        in aux_value value_site_list
      in
      error
    ) parameter value_site

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result_remanent =
  Covering_classes_type.AgentMap.print
    error
    (fun error parameter remanent ->
      let _ =
        (*------------------------------------------------------------------------------*)
        (* number of covering classes*)
        let number =
          number_of_covering_classes 
            parameter
            error
            remanent.Covering_classes_type.store_dic
        in
        let _ = Printf.fprintf stdout
          "Potential dependencies between sites:Number of covering classes:%i\n" number
        in
        (*------------------------------------------------------------------------------*)
        (*print covering class and theirs new-index*)
        let _ =
          print_dic_and_new_index parameter error
            remanent.Covering_classes_type.store_new_index_dic
            remanent.Covering_classes_type.store_test_new_index_dic
            remanent.Covering_classes_type.store_modif_new_index_dic
            remanent.Covering_classes_type.store_dic
        in
        (*------------------------------------------------------------------------------*)
        error
      in
      error) parameter result_remanent

(************************************************************************************)   
(*RULES - VARS*)

let create_map parameter error n_agents =
  Covering_classes_type.AgentMap.create parameter error n_agents

let scan_rule_set parameter error handler rules =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, init_modif_map  = create_map parameter error n_agents in
  let error, init_class = create_map parameter error n_agents in
  let error, init_min   = create_map parameter error n_agents in
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      Covering_classes_type.store_modified_map     = init_modif_map;
      Covering_classes_type.store_covering_classes = (init_class, init_min)
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_covering_classes =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
        scan_rule 
          parameter
          error
          handler
          rule.Cckappa_sig.e_rule_c_rule
          classes
      ) rules init_class
  in
  let (result_covering_classes, port_min) =
      store_covering_classes.Covering_classes_type.store_covering_classes in
  (*------------------------------------------------------------------------------*)
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = Covering_classes_type.AgentMap.create parameter error 0 in
  let error, remanent_dictionary =
    Covering_classes_type.AgentMap.fold parameter error
      (fun parameters error agent_type pair_covering_class init_remanent ->
        (*------------------------------------------------------------------------------*)
        (*get modified site*)
        let error, modified_map =
          match
            Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get
              parameter 
              error 
              agent_type
              store_covering_classes.Covering_classes_type.store_modified_map
          with
            | error, None -> error, empty_map
            | error, Some m -> error, m
        in
        (*------------------------------------------------------------------------------*)
        (*clean the covering classes, removed duplicate covering classes*)
        let error, store_remanent_dic =
          clean_classes
            parameters 
            error
            pair_covering_class
            modified_map
        in
        (*------------------------------------------------------------------------------*)
        (*store the covering classes after cleaning the duplicate class*)
        let error, store_remanent =
          Covering_classes_type.AgentMap.set 
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
(*MAIN*)

let covering_classes parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = 
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_result parameter error result in
  error, result
