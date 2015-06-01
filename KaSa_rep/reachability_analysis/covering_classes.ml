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

let print_pair_list l = (*REMOVE*)
  let rec aux acc =
    match acc with
      | [] -> acc
      | (i, s) :: tl ->
        Printf.fprintf stdout "site_type:%i:state:%s\n" i s;
        aux tl
  in aux l

let print_list_list l =
  let rec aux acc =
    match acc with
      | [] -> acc
      | x :: tl ->
        print_pair_list x;
        aux tl
  in aux l

(************************************************************************************)
(*sorted and ordered a covering classes in an descreasing order *)

let length_sorted (l: (int * Cckappa_sig.Site_map_and_set.set *
                         Cckappa_sig.Site_map_and_set.set) list list):
    (int * Cckappa_sig.Site_map_and_set.set * Cckappa_sig.Site_map_and_set.set) list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(************************************************************************************)
(*print port information for site*)

let string_of_port port = (*REMOVE*)
  "[state_min:"^(string_of_int port.Cckappa_sig.site_state.Cckappa_sig.min)^
    ";state_max:"^(string_of_int port.Cckappa_sig.site_state.Cckappa_sig.max)^"]"

let int_of_port port =
  (port.Cckappa_sig.site_state.Cckappa_sig.min,
   port.Cckappa_sig.site_state.Cckappa_sig.max)
    
(************************************************************************************)
(*common function for getting an id in pointer backward*)

let get_id_common_set parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None -> error, empty_set      
    | error, Some id -> error, id

(************************************************************************************)
(*CLEANING*)

(*------------------------------------------------------------------------------*)
(*store pointer backward *)

let store_pointer_backward parameter error id pointer_backward covering_class =
  List.fold_left (fun (error, pointer_backward) old_id ->
    (*getting an old id in the set*)
    let error, old_id_set = get_id_common_set parameter error old_id pointer_backward in
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
    | (x,_,_) :: tl -> x :: aux tl
  in aux l

let re_index_value_list pair_list =
  let rec aux acc =
    match acc with
      | [] -> acc
      | (x,s,s') :: tl ->
        let value_list = fst_list pair_list in
        let nth = position x value_list in
        ((nth + 1), s, s') :: aux tl
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

(*let project_modified_site value_list modified_map = (*TODO:add state information*)
  let rec aux acc =
    match acc with
      | [] -> []
      | x :: tl ->
        if not (Cckappa_sig.Site_map_and_set.is_empty_map modified_map)
        then
          if Cckappa_sig.Site_map_and_set.mem_map x modified_map
          then
            Cckappa_sig.Site_map_and_set.fold_map
              (fun k (site, min) _ ->
                let nth_1 = (position x value_list) + 1 in
                let l = (nth_1, min) :: aux tl in
                l
              ) modified_map []
          else aux tl
        else [] (*if modified_set is empty then nothing*)
  in aux value_list*)

(*------------------------------------------------------------------------------*)
(*check if in covering class there is a site that belong to site effect set*)

let collect_site_effect value_list effect_set = (*TODO*)
  let rec aux acc =
    match acc with
      | [] -> []
      | x :: tl ->
        if not (Cckappa_sig.Site_map_and_set.is_empty_set effect_set)
        then
          if Cckappa_sig.Site_map_and_set.mem_set x effect_set
          then
            let nth_1 = (position x value_list) + 1 in
            let l = (x, nth_1) :: aux tl in
            l
          else aux tl
        else [] (*if effect_set is empty then nothing*)
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
  common_allocate_dic parameter error pair_covering_class good_covering_class
    
(*------------------------------------------------------------------------------*)
(*compute new index dictionary*)

let new_index_dic parameter error new_index_covering_class good_index =
  common_allocate_dic parameter error new_index_covering_class good_index

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
  common_allocate_dic parameter error value_index_dic good_test_dic

(*------------------------------------------------------------------------------*)
(*compute modified dictionary with new_index*)

let modified_index_dic parameter error covering_class modified_map good_new_index_modif_dic =
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
(*compute site effect with and without new_index*)

let halfbreak_dic parameter error covering_class effect_set good_halfbreak_dic =(*TODO*)
  let halfbreak_value = collect_site_effect covering_class effect_set in
  let error, out_effect_dic =
    Covering_classes_type.Dictionary_of_Halfbreak_class.allocate
      parameter
      error
      Misc_sa.compare_unit
      halfbreak_value
      ()
      Misc_sa.const_unit
      good_halfbreak_dic
  in
  let error, (new_id, store_halfbreak_dic) =
    match out_effect_dic with
      | None -> warn parameter error (Some "line 339") Exit (0, good_halfbreak_dic)
      | Some (id, _, _, dic) -> error, (id, dic)
  in
  error, (new_id, store_halfbreak_dic)

(*------------------------------------------------------------------------------*)
(*store remanent*)

let store_remanent parameter error pair_covering_class modified_map effect_set
    remanent =
  (* current state of remanent*)
  let pointer_backward    = remanent.Covering_classes_type.store_pointer_backward in
  let good_covering_class = remanent.Covering_classes_type.store_dic in
  let good_index          = remanent.Covering_classes_type.store_new_index_dic in
  let good_test_dic       = remanent.Covering_classes_type.store_test_new_index_dic in
  let good_modif_dic      = remanent.Covering_classes_type.store_modif_new_index_dic in
  let good_halfbreak_dic  = remanent.Covering_classes_type.store_halfbreak_dic in
  (*------------------------------------------------------------------------------*)
  match pair_covering_class with
    | [] -> error, remanent
    | _ ->
      (*------------------------------------------------------------------------------*)
      (*covering class dictionary*)
      let error, (covering_class_id, store_dic) =
        covering_class_dic parameter error pair_covering_class good_covering_class in
      (*------------------------------------------------------------------------------*)
      (*store pointer backward*)
      let covering_class = fst_list pair_covering_class in
      let error, pointer_backward =
        store_pointer_backward parameter error covering_class_id pointer_backward
          covering_class in
      (*------------------------------------------------------------------------------*)
      (*PART II: compute new_index in covering_class*)
      let new_index_covering_class = re_index_value_list pair_covering_class in
      let error, (new_id, new_dic) =
        new_index_dic parameter error new_index_covering_class good_index in
      (*------------------------------------------------------------------------------*)
      (*PART II: site test with new index*)
      let error, (new_test_id, test_new_index_dic) =
        test_new_index_dic parameter error new_id new_dic good_test_dic in     
      (*------------------------------------------------------------------------------*)
      (*PART II: site modified with new_index*)
      let error, (new_modif_id, modif_index_dic) =
        modified_index_dic parameter error covering_class modified_map good_modif_dic in
      (*------------------------------------------------------------------------------*)
      (*collect site effect in covering class *)
      let error, (new_halfbreak_id, store_halfbreak_dic) =
        halfbreak_dic parameter error covering_class effect_set good_halfbreak_dic in
      (*------------------------------------------------------------------------------*)
      (*result*)
      error,
      {
        Covering_classes_type.store_pointer_backward    = pointer_backward;
        Covering_classes_type.store_dic                 = store_dic;
        Covering_classes_type.store_new_index_dic       = new_dic;
        Covering_classes_type.store_test_new_index_dic  = test_new_index_dic;
        Covering_classes_type.store_modif_new_index_dic = modif_index_dic;
        Covering_classes_type.store_halfbreak_dic       = store_halfbreak_dic
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

let clean_classes parameter error pair_covering_classes modified_map effect_set =
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_index       = init_dic in
  let init_store_dic   = init_dic in
  let init_test_index  = init_dic in
  let init_modif_index = Covering_classes_type.Dictionary_of_Modified_class.init() in
  let init_halfbreak_dic = Covering_classes_type.Dictionary_of_Halfbreak_class.init() in
  (*------------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent = 
    {
      Covering_classes_type.store_pointer_backward    = init_pointer;
      Covering_classes_type.store_dic                 = init_store_dic;
      Covering_classes_type.store_new_index_dic       = init_index;
      Covering_classes_type.store_test_new_index_dic  = init_test_index;
      Covering_classes_type.store_modif_new_index_dic = init_modif_index;
      Covering_classes_type.store_halfbreak_dic       = init_halfbreak_dic;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted pair_covering_classes in
  let is_empty_set = Cckappa_sig.Site_map_and_set.is_empty_set in
  List.fold_left (fun (error, remanent) covering_class ->
    match covering_class with
      | [] -> error, remanent
      | (t, s, y) :: tl ->
        let pointer_backward = remanent.Covering_classes_type.store_pointer_backward in
        (* return the set of list(id) containing t.
           For example: current_covering_classes: [[0;1];[0]]
           t = 0 => (id:1;id:2) of type set;
           remanent_type: [(id:1,[0;1]);(id:2,[0])];
           (id:pointer_backward, dic: int list)
        *)
        let error, potential_supersets =
          get_id_common_set parameter error t pointer_backward in
        let rec aux to_visit potential_supersets =
          match to_visit with
	    | [] -> error, remanent
	    | (t', s', y') :: tl' ->
              (* get the set of list(id) containing t' *)
              let error, potential_supersets' =
		get_id_common_set parameter error t' pointer_backward in
              (* intersection of two sets *)
              let error, potential_superset =
                Cckappa_sig.Site_map_and_set.inter parameter error
                  potential_supersets
                  potential_supersets'
              in
              if is_empty_set potential_superset
              then
                let error, result_covering_dic =
                  store_remanent parameter error
                    covering_class
                    modified_map
                    effect_set
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
              effect_set
              remanent
          in
          error, result_covering_dic
        else
          aux tl potential_supersets)
    (error, init_remanent)
    current_covering_classes

(************************************************************************************)   
(*RULE*)

let collect_modified_map parameter error diff_reverse store_modified_map =
  Covering_classes_type.AgentMap.fold parameter error
    (fun parameter error agent_id site_modif store_modified_map ->
      if Cckappa_sig.Site_map_and_set.is_empty_map
        site_modif.Cckappa_sig.agent_interface
      then error, store_modified_map
      else
        let agent_type = site_modif.Cckappa_sig.agent_name in
        let store_site =
          Cckappa_sig.Site_map_and_set.fold_map
            (fun site port current_map ->
              let (port_min, port_max) = int_of_port port in
              (*store site map*)
              let error, site_map =
                Cckappa_sig.Site_map_and_set.add_map
                  parameter
                  error
                  site
                  site
                  current_map
              in
              site_map
            )
            site_modif.Cckappa_sig.agent_interface
            Cckappa_sig.Site_map_and_set.empty_map
        in
        (*get old*)
        let error, old =
          Covering_classes_type.AgentMap.unsafe_get
            parameter
            error
            agent_type
            store_modified_map
        in
        let old_map = 
          match old with
            | None -> Cckappa_sig.Site_map_and_set.empty_map
            | Some m -> m
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

let get_old_port parameter error agent_type port_set store_port =
  let error, out_port =
    Covering_classes_type.AgentMap.unsafe_get parameter error agent_type
      store_port in
  let old =
    match out_port with
      | None -> empty_set
      | Some s -> s
  in
  let error, set =
    Cckappa_sig.Site_map_and_set.union parameter error
      port_set old                  
  in error, set

(*------------------------------------------------------------------------------*)

let add_state_class parameter error agent_type port_set store_port =
  let error, set =
    get_old_port parameter error agent_type port_set store_port in
  let error, store_port =
    Covering_classes_type.AgentMap.set parameter error agent_type
      set store_port
  in error, store_port

(*------------------------------------------------------------------------------*)
(*compute site effect*)

(*unknow unbinding*)
let collect_half_break parameter error handler store_half_break half_break =
  List.fold_left (fun (error, store_half_break) (site_add, state) ->
    (*get information of site and agent_type in site_address*)
    let site = site_add.Cckappa_sig.site in
    let agent_type = site_add.Cckappa_sig.agent_type in
    let error, (min, max) = (*TO BE USED*)
      match state with
        | None ->
          begin
            let error, value_state =
              Misc_sa.unsome
                (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                   parameter
                   error
                   (agent_type, site)
                   handler.Cckappa_sig.states_dic)
                (fun error -> warn parameter error (Some "line 782")
                  Exit (Cckappa_sig.Dictionary_of_States.init ()))
            in
            let error, last_entry =
              Cckappa_sig.Dictionary_of_States.last_entry parameter error value_state in
            error, (1, last_entry)
          end
        | Some interval -> error, (interval.Cckappa_sig.min, interval.Cckappa_sig.max)
    in
    (*get the old one*)
    let error, out_old =
      Covering_classes_type.AgentMap.unsafe_get parameter error agent_type
        store_half_break in
    let old_set =
      match out_old with
        | None -> empty_set
        | Some s -> s
    in
    (*new set*)
    let error, add_set =
      Cckappa_sig.Site_map_and_set.add_set
        parameter
        error
        site
        old_set
    in
    let error, new_set =
      Cckappa_sig.Site_map_and_set.union
        parameter
        error
        add_set
        old_set
    in
    (*store*)
    Covering_classes_type.AgentMap.set
      parameter
      error
      agent_type
      new_set
      store_half_break
  )(error, store_half_break) half_break

(*know unbinding*)
let collect_know_binding error store_unbinding release =
  let error, store_unbinding =
    List.fold_left (fun (error, store_unbinding) (site_add_1, site_add_2) ->
      (*get the first binding information*)
      let agent_index_1 = site_add_1.Cckappa_sig.agent_index in
      let agent_type_1 = site_add_1.Cckappa_sig.agent_type in
      let site_1 = site_add_1.Cckappa_sig.site in
      (*get the second binding information*)
      let agent_index_2 = site_add_2.Cckappa_sig.agent_index in
      let agent_type_2 = site_add_2.Cckappa_sig.agent_type in
      let site_2 = site_add_2.Cckappa_sig.site in
      (*store unbinding information*)
      let unbinding_list =
        (agent_index_1, agent_type_1, site_1, 
         agent_index_2, agent_type_2, site_2) :: store_unbinding
      in error, unbinding_list
    )(error, store_unbinding) release
  in
  error, store_unbinding

(*------------------------------------------------------------------------------*)
(*collect remove actions-it is one of the property of site effect*)

(*collect document site*)
let collect_document_site parameter error index agent agent_type store_doc =
  let site_map =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_map ->
        let error, site_map =
          Cckappa_sig.Site_map_and_set.add_map
            parameter
            error
            site
            (index, agent_type, site)
            current_map
        in
        site_map
      )
      agent.Cckappa_sig.agent_interface
      Cckappa_sig.Site_map_and_set.empty_map
  in
  let error, old =
    Covering_classes_type.AgentMap.unsafe_get
      parameter
      error
      agent_type
      store_doc
  in
  let old_map =
    match old with
      | None -> Cckappa_sig.Site_map_and_set.empty_map
      | Some m -> m
  in
  let error, final_map =
    Cckappa_sig.Site_map_and_set.union_map
      parameter
      error
      old_map
      site_map  
  in
  Covering_classes_type.AgentMap.set
    parameter
    error
    agent_type
    final_map
    store_doc

(*collect undocument site*)
let collect_undocument_site parameter error index agent_type list_undoc store_undoc =
  let undoc_map =
    List.fold_left (fun current_map site ->
      let error, site_map =
        Cckappa_sig.Site_map_and_set.add_map
          parameter
          error
          site
          (index, agent_type, site)
          current_map
      in site_map
    ) Cckappa_sig.Site_map_and_set.empty_map list_undoc
  in
  let error, old = 
    Covering_classes_type.AgentMap.unsafe_get
      parameter
      error
      agent_type
      store_undoc
  in
  let old_map =
    match old with
      | None -> Cckappa_sig.Site_map_and_set.empty_map
      | Some m -> m
  in
  let error, final_map =
    Cckappa_sig.Site_map_and_set.union_map
      parameter
      error
      old_map
      undoc_map          
  in
  Covering_classes_type.AgentMap.set
    parameter
    error
    agent_type
    final_map
    store_undoc

let collect_remove parameter error store_remove_map remove =
  List.fold_left (fun (error, store_remove_map) (index, agent, list_undoc) ->
    let agent_type = agent.Cckappa_sig.agent_name in
    let error, document_site =
      collect_document_site
        parameter
        error
        index agent agent_type (fst store_remove_map)
    in
    let error, undocument_site =
      collect_undocument_site
        parameter error index agent_type list_undoc
        (snd store_remove_map)
    in
    error, (document_site, undocument_site)      
  ) (error, store_remove_map) remove

(*------------------------------------------------------------------------------*)
(*compute covering_class*)

let add_covering_class parameter error agent_type pair_site store_covering_classes =
  match pair_site with
    | [] -> error, store_covering_classes
    | _ ->
      let error, get_old =
        Covering_classes_type.AgentMap.unsafe_get parameter error agent_type
          store_covering_classes in		 
       (* fetch the former list of covering classes *)
       let old_pair =
         match get_old with
           | None -> []
           | Some sites -> sites
       in
       (* store the new list of covering classes *)
       let new_site_list = (List.rev pair_site) :: old_pair in
       Covering_classes_type.AgentMap.set parameter error agent_type
         new_site_list store_covering_classes

(*------------------------------------------------------------------------------*)
(*compute the state information*)

let store_port_state parameter error agent_type port_min current_port_min
    port_max current_port_max state_min state_max =
  let error, port_set_min =
    Cckappa_sig.Site_map_and_set.add_set parameter error
      port_min current_port_min
  in
  let error, set_min =
    get_old_port parameter error agent_type
      port_set_min state_min
  in
  (*get state max*)
  let error, port_set_max =
    Cckappa_sig.Site_map_and_set.add_set parameter error
      port_max
      current_port_max
  in
  let error, set_max =
    get_old_port parameter error agent_type
      port_set_max state_max
  in (set_min, set_max)

(*------------------------------------------------------------------------------*)
(*store covering class and port state class*)

let store_covering_port_state_class parameter error agent_type site_list fst_cv
    port_set_min snd_cv port_set_max thrd_cv =
  (*store covering class*)
  let error, covering_classes =
    add_covering_class parameter error agent_type
      site_list fst_cv
  in
  (*store port min*)
  let error, store_port_min =
    add_state_class parameter error agent_type
      port_set_min snd_cv
  in
  (*store port max*)
  let error, store_port_max =
    add_state_class parameter error agent_type
      port_set_max thrd_cv
  in error, (covering_classes, store_port_min, store_port_max)

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
              let (store_cv, state_min, state_max) = store_covering_classes in
              (*get a list of sites from an interface at each rule*)
              let pair_site =
                Cckappa_sig.Site_map_and_set.fold_map
	          (fun site port (current_class, current_port_min, current_port_max) ->
                    (*get state min*)
                    let (port_min, port_max) = int_of_port port in
                    let (set_min, set_max) =
                      store_port_state parameter error agent_type
                        port_min current_port_min
                        port_max current_port_max
                        state_min state_max
                    in
                    (*store*) (*FIXME:add agent_type?*)
                    let site_list = (site, set_min, set_max) :: current_class in
                    (site_list, set_min, set_max)
                  ) agent.Cckappa_sig.agent_interface ([], empty_set, empty_set)
              in
              (* store new_covering_class in the classes of the agent type
                 agent_type *)
              let (site_list, port_set_min, port_set_max) = pair_site in
              (*store covering class*)
              let error, (covering_classes, store_port_min, store_port_max) =
                store_covering_port_state_class parameter error agent_type
                  site_list store_cv
                  port_set_min state_min
                  port_set_max state_max
              in
              (*store*)
              error, (covering_classes, store_port_min, store_port_max)
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
  (*compute site effects - half break actions*)
  let error, store_half_break =
    collect_half_break parameter error
      handler
      classes.Covering_classes_type.store_half_break
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
  in
  (*------------------------------------------------------------------------------*)
  (*compute site effects - whole break, release actions*)
  let error, store_unbinding =
    collect_know_binding error 
      classes.Covering_classes_type.store_unbinding
      rule.Cckappa_sig.actions.Cckappa_sig.release
  in
  (*------------------------------------------------------------------------------*)
  (*compute site effects - remove action*)
  let error, store_remove_map =
    collect_remove
      parameter
      error
      classes.Covering_classes_type.store_remove_map
      rule.Cckappa_sig.actions.Cckappa_sig.remove
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
    Covering_classes_type.store_half_break       = store_half_break;
    Covering_classes_type.store_unbinding        = store_unbinding;
    Covering_classes_type.store_remove_map       = store_remove_map;
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

let print_state l l' =
  let rec aux acc acc' =
    match acc, acc' with
      | [], [] | _, [] | [], _ -> acc
      | x :: tl, y :: tl' ->
        Printf.fprintf stdout "[state_min:%i; state_max:%i]" x y;
        aux tl tl'
  in aux l l'

let print_pair_list l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | (x,s,s') :: tl ->
        let l = Cckappa_sig.Site_map_and_set.elements s in
        let l' = Cckappa_sig.Site_map_and_set.elements s' in
        Printf.fprintf stdout "site_type:%i->state:" x;
        print_state l l'; print_string "\n";
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

let print_modified_dic parameter error elt_id store_modif =(*FIXME*)
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
(*print site effect in covering class*)

let print_halfbreak_dic parameter error store_half_break =
  Covering_classes_type.Dictionary_of_Halfbreak_class.print
    parameter
    error
    (fun parameter error elt unknow_unbinding _ _ ->
      let _ =
        (*print 1/2 unbinding*)
        let rec aux acc =
          match acc with
            | [] -> []
            | (x,y) :: tl ->
              let _ =
                Printf.fprintf stdout
                  "Potential dependencies between sites:Covering_classes:class_id:%i:1/2unbinding:site_type:%i\n" elt x; aux tl
              in
              Printf.fprintf stdout
                "Potential dependencies between sites:Covering_classes:class_id:%i:New_index:1/2unbinding:site_type:%i\n" elt y; aux tl
        in
        aux unknow_unbinding
      in
      error)
    store_half_break

(*------------------------------------------------------------------------------*)
(*print remove action*)


(*------------------------------------------------------------------------------*)

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
            elt_id; print_pair_list pair_list in
        (*print new_index for covering class*)
        let _ = print_new_index_dic parameter error elt_id store_index in
        (*print site that is tested with its new_index*)
        let _ = print_test_new_index_dic parameter error elt_id store_test in
        (*print site that is modified with its new_index*)
        print_modified_dic parameter error elt_id store_modif
        (*print_unbinding_dic parameter error elt_id store_unbinding*)
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
          number_of_covering_classes parameter error
            remanent.Covering_classes_type.store_dic
        in
        let _ = Printf.fprintf stdout
          "Potential dependencies between sites:Number of covering classes:%i\n" number
        in
        (*------------------------------------------------------------------------------*)
        (* site effect of covering classes*)
        let _ =
          print_halfbreak_dic parameter error
            remanent.Covering_classes_type.store_halfbreak_dic
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

(*------------------------------------------------------------------------------*)
(*This unbinding information getting no information of covering_class*)
let print_unbinding store_unbinding =
  Printf.fprintf stdout "Potential dependencies between sites:unbinding:\n";
    let rec aux acc =
      match acc with
        | [] -> []
        | (i,a,x,u,b,y) :: tl ->
          Printf.fprintf stdout
            "agent_id:%i:agent_type:%i:site_type:%i -> agent_id:%i:agent_type:%i:site_type:%i\n"
            i a x u b y;
          aux tl
    in aux store_unbinding

(*------------------------------------------------------------------------------*)
(*print remove action*)
let print_remove parameter error store_remove =
  (*print document*)
  let _ =
    Printf.fprintf stdout "Potential dependencies between sites:deletion:\n";
    Covering_classes_type.AgentMap.print error
      (fun error parameter map  ->
        let _ =
          Printf.fprintf stdout "Potential dependencies between sites:document_site:\n";
          Cckappa_sig.Site_map_and_set.iter_map (fun k (i,a,s) ->
            let _ =
              Printf.fprintf stdout "agent_id:%i:agent_type:%i:site_type:%i\n"
                i a s
            in
            ()
          ) map
        in error)
      parameter
      (fst store_remove)
  in
  (*print undocument*)
  Covering_classes_type.AgentMap.print error
    (fun error parameter map  ->
      let _ =
        Printf.fprintf stdout "Potential dependencies between sites:undocument_site:\n";
        Cckappa_sig.Site_map_and_set.iter_map (fun k (i,a,s) ->
          let _ =
            Printf.fprintf stdout "agent_id:%i:agent_type:%i:site_type:%i\n"
              i a s
          in
          ()) map
      in error)
    parameter
    (snd store_remove)

(************************************************************************************)   
(*RULES*)

let create_map parameter error n_agents =
  Covering_classes_type.AgentMap.create parameter error n_agents

let scan_rule_set parameter error handler rules =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, init_modif_map  = create_map parameter error n_agents in
  let error, init_half_break = create_map parameter error n_agents in
  let error, init_remove_doc = create_map parameter error n_agents in
  let error, init_remove_undoc = create_map parameter error n_agents in
  let init_remove_map = (init_remove_doc, init_remove_undoc) in
  let error, init_class = create_map parameter error n_agents in
  let error, init_min   = create_map parameter error n_agents in
  let error, init_max   = create_map parameter error n_agents in
  (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      Covering_classes_type.store_modified_map     = init_modif_map;
      Covering_classes_type.store_half_break       = init_half_break;
      Covering_classes_type.store_unbinding        = [];
      Covering_classes_type.store_remove_map       = init_remove_map;
      Covering_classes_type.store_covering_classes = (init_class, init_min, init_max)
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_covering_classes =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
        scan_rule parameter error
          handler
          rule.Cckappa_sig.e_rule_c_rule
          classes
      ) rules init_class
  in
  let (result_covering_classes, port_min, port_max) =
      store_covering_classes.Covering_classes_type.store_covering_classes in
  (*------------------------------------------------------------------------------*)
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result = Covering_classes_type.AgentMap.create parameter error 0 in
  let error, remanent_dictionary =
    Covering_classes_type.AgentMap.fold parameter error
      (fun parameters error agent_type pair_covering_class init_remanent ->
        (*------------------------------------------------------------------------------*)
        (*get modified site*)
        let error, out_modified =
          Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get parameter error agent_type
            store_covering_classes.Covering_classes_type.store_modified_map
        in
        let modified_map =
          match out_modified with
            | None -> Cckappa_sig.Site_map_and_set.empty_map
            | Some s -> s
        in
        (*------------------------------------------------------------------------------*)
        (*get site effect - half break, unknow unbinding*)
        let error, out_effect =
          Covering_classes_type.AgentMap.unsafe_get parameter error agent_type
            store_covering_classes.Covering_classes_type.store_half_break
        in
        let effect_set =
          match out_effect with
            | None -> empty_set
            | Some s -> s
        in
        (*------------------------------------------------------------------------------*)
        (*clean the covering classes, removed duplicate covering classes*)
        let error, store_remanent =
          clean_classes parameters error
            pair_covering_class
            modified_map
            effect_set
        in
        (*store the covering classes after cleaning the duplicate class*)
        let error, store_remanent_dic =
          Covering_classes_type.AgentMap.set parameters error agent_type
            store_remanent init_remanent
        in
        (*------------------------------------------------------------------------------*)
        (*result*)
        error, store_remanent_dic
      )
      result_covering_classes
      init_result
  in
  (*print unbinding information*)
  let _ = print_unbinding store_covering_classes.Covering_classes_type.store_unbinding in
  (*print store remove map*)
  let _ = print_remove parameter error 
    store_covering_classes.Covering_classes_type.store_remove_map
  in
  error, remanent_dictionary

(************************************************************************************)   
(*MAIN*)

let covering_classes parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_result parameter error result in
  error, result
