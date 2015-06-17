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

(*------------------------------------------------------------------------------*)

let sprintf_list l =
  let acc = ref "{" in
  List.iteri (fun i x ->
    acc := !acc ^
      if i <> 0
      then sprintf "; %i" x
      else sprintf "%i" x
  ) l;
  !acc ^ "}"
    
let print_list l =
  let output = sprintf_list l in
  fprintf stdout "%s\n" output

(************************************************************************************)
(*sorted and ordered a covering classes in an descreasing order *)

let length_sorted (l: int list list): int list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
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
        if not (is_empty_list list)
        then
        let nth = position x list in
        (nth + 1) :: aux tl
        else []
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
  let init_index       = init_dic in
  let init_store_dic   = init_dic in
  let init_test_index  = init_dic in
  let init_modif_index = Dictionary_of_Modified_class.init() in
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
    match covering_class with (*TODO: add state in length*)
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
(*compute BDU for each rule in a covering class*)

let rec print_a_list (l: int List_sig.list) =
  fprintf stdout "list_id:%i:" l.List_sig.id;
  let v = l.List_sig.value in
  match v with
    | List_sig.Empty -> print_string "\n"
    | List_sig.Cons precell ->
      Printf.fprintf stdout "value:[";
      print_precell precell
      
and print_precell p =
  fprintf stdout "site_type:%i:site_state:%i]\n" 
    p.List_sig.variable  p.List_sig.association;
  print_a_list p.List_sig.tail

(*---------------------------------------------------------------------------*)

let bdu_covering parameter error agent_type pair_list store_bdu_covering_class =
  (*build bdu for this list*)
  let remanent_bdu = (Sanity_test.remanent parameter) in
  let error = remanent_bdu.Sanity_test_sig.error in
  let allocate = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  (*'b: memo_tables; 'a: mvbdu_dic; 'c: list_dic*)
  let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
    remanent_bdu.Sanity_test_sig.mvbdu_handler
  in
  let a_val = Mvbdu_sig.Leaf true in
  let b_val = Mvbdu_sig.Leaf false in
  (*build bdu from a_val: 
    a',a'_id: output of build_already_compressed_cell;
    a'', a''_id: output of compress_node
  *)
  let error, handler, a', a'_id, a'', a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val (*bdu_skel*)
      a_val (*bdu_val*)
  in
  (*define function f*)
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> a')
        in error, handler, a
  in
  (*build bdu_b from b_val*)
  let error, handler, b', b'_id, b'', b''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      b_val
      b_val
  in
  (*---------------------------------------------------------------------------*)
  (*compute bdu relation*)
  (*check the Leaf is true*)
  let error, handler, bmvbdu_true0 =
    f (Boolean_mvbdu.boolean_mvbdu_true parameter handler error) parameter
  in
  (*---------------------------------------------------------------------------*)    
  (*build bdu_list from a list of pair [site, state] computed above in cv*)
  let error, (handler, list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      pair_list
  in
  (*let _ =
    Printf.fprintf stdout "\nBuild list(list_a):\n";
    print_a_list list_a
  in*)
  (*compute redefine in a list_a, a': mvbdu_input*)
  let error, handler, mvbdu_redefine =
    f (Boolean_mvbdu.redefine parameter error parameter handler a') list_a
  in
  (*---------------------------------------------------------------------------*)
  (*store redefine*)
  AgentMap.set
    parameter
    error
    agent_type
    mvbdu_redefine
    store_bdu_covering_class 

(*------------------------------------------------------------------------------*)
(*compute covering_class*)

let add_covering_class parameter error agent_type site_list store_covering_classes =
  match site_list with
    | [] -> error, store_covering_classes
    | _ ->
      let error, old_pair =
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
      let new_site_list = (List.rev site_list) :: old_pair in
      AgentMap.set 
        parameter
        error
        agent_type
        new_site_list
        store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute site test*)

let add_site_test parameter error agent_type pair_list store_state =
  match pair_list with
    | [] -> error, store_state
    | _ ->
      let error, old_list =
        match 
          AgentMap.unsafe_get
            parameter
            error
            agent_type
            store_state
        with
          | error, None -> error, []
          | error, Some s -> error, s
      in
      let new_list = List.concat [pair_list; old_list] in
      AgentMap.set
        parameter
        error
        agent_type
        (List.rev new_list)
        store_state      
      
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
              let (store_cv, store_state, store_bdu) = store_covering_classes in
              (*get a list of sites from an interface at each rule*)
              let error, init = AgentMap.create parameter error 0 in
              let store_triple =
                Site_map_and_set.fold_map
	          (fun site port (current_class, current_list, _) ->
                    (*get state min*)
                    let state = int_of_port port in
                    (*---------------------------------------------------------*)
                    (*BDU*)
                    let pair_list = (site, state) :: current_list in
                    let error, bdu =
                      bdu_covering parameter error agent_type
                        pair_list
                        store_bdu                        
                    in
                    (*---------------------------------------------------------*)
                    (*store*)
                    let site_list = site :: current_class in
                    (site_list, pair_list, bdu)
                  ) agent.agent_interface ([], [], init)
              in
              (* store new_covering_class in the classes of the agent type
                 agent_type *)
              let (site_list, pair_list, store_bdu) = store_triple in
              (*compute covering_class*)
              let error, covering_classes =
                add_covering_class
                  parameter
                  error
                  agent_type
                  site_list
                  store_cv
              in
              (*compute site test*)
              let error, store_state =
                add_site_test
                  parameter
                  error
                  agent_type
                  pair_list
                  store_state
              in
              (*store*)
              error, (covering_classes, store_state, store_bdu)
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
(*PRINT*)

let print_covering_class l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | x :: tl ->
        fprintf stdout "site_type:%i\n" x;
        aux tl
  in
  aux l

(*------------------------------------------------------------------------------*)
(*print new index*)

let print_new_index_dic parameter error elt_id store_index =
  Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt pair_index _ _ ->
      let _ = 
        fprintf stdout
          "Potential dependencies between sites:New-index:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_covering_class pair_index
      in
      error
    ) store_index
    
(*------------------------------------------------------------------------------*)
(*print test with new index*)

let print_test_new_index_dic parameter error elt_id store_test =
  Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_index _ _ ->
      let _ = fprintf stdout
        "Potential dependencies between sites:New-index:TEST:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_covering_class value_index
      in
      error
    ) store_test

(*------------------------------------------------------------------------------*)
(*print modified site (action) with new index*)

let print_modified_dic parameter error elt_id store_modif =(*FIXME:add state*)
  Dictionary_of_Modified_class.print
    parameter
    error
    (fun parameter error elt l _ _ ->
      let _ =
        fprintf stdout 
          "Potential dependencies between sites:New-index:MODIFICATION-:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_covering_class l
      in
      error
    ) store_modif

(*------------------------------------------------------------------------------*)
(*print remove action*)

let print_dic_and_new_index parameter error store_index store_test 
    store_modif store_dic =
  Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt_id pair_list _ _ ->
      let _ =
        let _ =
        (*print covering class in dictionary*)
          printf "Potential dependencies between sites:Covering_class_id:%i:\n"
            elt_id; 
          print_covering_class pair_list
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
  Quick_Nearly_inf_Imperatif.print
    error
    (fun error parameter value_site_list ->
      let _ =
        let rec aux_value acc' =
          match acc' with
            | [] -> acc'
            | vsite :: tl' ->
              let _ =
                fprintf stdout 
                  "Potential dependencies between sites:New-index:Covering_class_id:%i:" elt;
                match vsite with
                  | Ckappa_sig.Internal s ->
		    fprintf stdout "site_modified:%i->%s(internal state)\n"
                      site s
	          | Ckappa_sig.Binding s ->
		    fprintf stdout "site_modified:%i->%s(binding state)\n"
                      site s                                     
              in aux_value tl'
        in aux_value value_site_list
      in
      error
    ) parameter value_site

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

(*------------------------------------------------------------------------------*)
(*print test: state of site at each rule*)

let print_pair l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | (x,s) :: tl ->
        fprintf stdout "TEST:[site_type:%i:state:%i]\n"
          x s; aux tl
  in aux l

let print_test_site parameter error result_test =
  AgentMap.print
    error
    (fun error parameter l -> let _ = print_pair l in
                              error)
    parameter
    result_test

(*------------------------------------------------------------------------------*)
(*PRINT BDU*)

let print_bdu parameter error result_bdu =
  AgentMap.print error
    (fun error parameter mvbdu_redefine ->
      let _ =
        Boolean_mvbdu.print_boolean_mvbdu
          error
          (Remanent_parameters.update_prefix parameter "mvbdu_redefine:")
          mvbdu_redefine
      in
      error
    )
    parameter
    result_bdu

(************************************************************************************)   
(*RULES - VARS*)

let create_map parameter error n_agents = AgentMap.create parameter error n_agents

let scan_rule_set parameter error handler rules =
  let n_agents = handler.nagents in
  let error, init_modif_map  = create_map parameter error n_agents in
  let error, init_class = create_map parameter error n_agents in
  let error, init_map   = create_map parameter error n_agents in
  let error, init_bdu   = create_map parameter error n_agents in
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      store_modified_map     = init_modif_map;
      store_covering_classes = init_class, init_map, init_bdu
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_covering_classes =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
        scan_rule 
          parameter
          error
          handler
          rule.e_rule_c_rule
          classes
      ) rules init_class
  in
  let (result_covering_classes, result_state, result_bdu) =
    store_covering_classes.store_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = AgentMap.create parameter error 0 in
  let error, remanent_dictionary =
    AgentMap.fold parameter error
      (fun parameters error agent_type pair_covering_class init_remanent ->
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
        (*clean the covering classes, removed duplicate of covering classes: TODO example*)
        let error, store_remanent_dic =
          clean_classes
            parameters 
            error
            pair_covering_class
            modified_map
        in
        (*------------------------------------------------------------------------------*)
        (*store the covering classes after cleaning theirs duplicate class*)
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
  error, (remanent_dictionary, result_state, result_bdu)

(************************************************************************************)   
(*MAIN*)

let common_result parameter error handler cc_compil =
  let error, result = 
    scan_rule_set parameter error handler cc_compil.rules in
  error, result
  
let covering_classes parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = common_result parameter error handler cc_compil in
  let (result_covering_class, _, _) = result in
  let _ = print_result parameter error result_covering_class in
  error, result_covering_class

let result_test parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = common_result parameter error handler cc_compil in
  let (_, result_test, _) = result in
  let _ = print_test_site parameter error result_test in
  error, result_test

let result_bdu parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = common_result parameter error handler cc_compil in
  let (_, _, result_bdu) = result in
  let _ = print_bdu parameter error result_bdu in
  error, result_bdu  
